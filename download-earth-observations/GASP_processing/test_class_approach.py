import argparse
import glob, os, re
import multiprocessing
import boto.s3.connection
from boto.s3.key import Key

import gzip, shutil, struct
import csv, subprocess
import shapefile as shp
import pandas as pd
#import geopandas as gpd
import datetime
import pytz


class Test:
    def __init__(self):
        args = self._setup()
        self.start_year = args.start_year
        self.end_year = args.end_year
        self.access_key = args.access_key
        self.secret_key = args.secret_key
        self.s3_bucket = args.s3_bucket
        self.data_directory = args.data_directory
        # connection here
        self.conn = boto.connect_s3(
            aws_access_key_id = self.access_key,
            aws_secret_access_key =self.secret_key
        )

    def _setup(self):
        parser = argparse.ArgumentParser(description='Pass in AWS credentials.')
        parser.add_argument('--start_year', type=int, required=True,
                            help='starting year for data download (starts with Jan 1 of that year)')
        parser.add_argument('--end_year', type=int, required=True,
                            help='ending year for data download (ends with Dec 31 of that year)')
        parser.add_argument('--access_key', type=str, required=True,
                            help='aws access key')
        parser.add_argument('--secret_key', type=str, required=True,
                            help='secret access key')
        parser.add_argument('--s3_bucket', type=str, required=True, help='s3 bucket name')
        parser.add_argument('--data_directory', type=str, required=True,
                            help='directory path where data is stored, including lat and lon files')
        # print(args.access_key, args.secret_key, args.s3_bucket, args.data_directory)

        args = parser.parse_args()
        return args

    def upload_to_AWS(self, subdir, file):
        bucket = self.conn.get_bucket(self.s3Bucketname)
        k = Key(bucket)
        k.key = subdir + os.path.basename(file)
        k.set_contents_from_filename(file)  # rewind = True if from file

    def adjust_datetime(self, dt, timezone_str):
        timezone = pytz.timezone(timezone_str)
        adjusted_dt = pytz.timezone('UTC').localize(datetime.datetime(
            dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second)).astimezone(timezone)
        return adjusted_dt

    def zero(self, origpath, outpath, item):  # Unzip from .gz to binary
        item = os.path.basename(item)
        # while i < 10:
        with gzip.open(origpath + item, 'rb') as f_in:
            with open(outpath + item[:-3], 'wb') as f_out:
                shutil.copyfileobj(f_in, f_out)
                # i = i + 1
            f_out.close()
        f_in.close()
        f_out.close()
        # Note: not re-uploading zipped data to AWS
        os.remove(origpath + item)

    def one(self, origpath, outpath, item):  # Read from binary
        item = os.path.basename(item)
        if os.path.isfile(origpath + item):
            print(item)
            type = item[-3:]
            if type == 'dat':
                with open(origpath + item, 'rb') as datafile:
                    f_data = datafile.read()
                    new_file = open(outpath + item[:3] + '.txt', 'w')
                    for i in range(1, 2280001):  # array is size 2500 * 912
                        d = struct.unpack('f', f_data[((i - 1) * 4):(i * 4)])
                        d = str(d)
                        d = d.replace("(", "")
                        d = d.replace(")", "")
                        d = d.replace(",", "")
                        d = d.replace(" ", "")
                        new_file.write(str(d) + '\n')
                new_file.close()
                print(item + " done")

            # Need to read in only the first 9120000 values (aod ones)

            elif item[0] == 'G':  # aod files
                slice = item[-14:-1]

                with open(origpath + item, 'rb') as datafile:
                    new_file = open(outpath + "GASP_" + slice + '.txt', 'w')
                    data = datafile.read()
                    # data = datafile.read(4)
                    max = 0
                    min = 300
                    j = 0
                    for d in data:
                        if j >= 2280000:
                            break
                        else:
                            j += 1
                            d = d / 100. - 0.5  # convert from 0-255 range as specified by Chuanyu
                            if d < 0:
                                d = -9.99  # Zev's convention
                            new_file.write(str(d) + '\n')
                            # data = datafile.read(4)
                            if d > max:
                                max = d
                            if d < min:
                                min = d
                    print("Min: " + str(min))
                    print("Max: " + str(max))
                    print(j)
                    new_file.close()
            self.upload_to_AWS("GASP_processed/step0/", origpath + item)
            os.remove(origpath + item)

    def two(self, origpath, outpath, item):  # Write valid lat, lon, aod values to file with local UTC name (requires time conversion)
        item = os.path.basename(item)
        print(item)
        line_num = 1
        timestamp = item[-17:]  # we need this because the files have different beginnings, such as GOESW versus GOES11...
        year = timestamp[:4]
        day = timestamp[4:7]
        time = timestamp[9:13]
        # print("Y = ", year)
        # print("D = ", day)
        # print("T = ", time)

        # Convert time stamp to local UTC, create array for four regions
        timezones = ['America/Boise', 'America/Denver', 'America/Los_Angeles', 'America/Phoenix']
        dt_str = year + day + time
        dt = datetime.datetime.strptime(dt_str, '%Y%j%H%M')

        adjusted_day_per_tz_array = []
        for tz_str in timezones:
            adjusted_dt = self.adjust_datetime(dt, tz_str)
            adjusted_julian_day = adjusted_dt.strftime('%j')
            adjusted_day_per_tz_array.append(adjusted_julian_day)

        # print(adjusted_day_per_tz_array)

        file_LL = open(self.data_directory + "lat_lon_tzid_lookup.txt", 'r') #need to put this in the correct folder before starting
        aod_table = pd.read_table(origpath + item, header= None)
        # print(aod_table.head())

        today = int(day)
        if(today != 1): #If it's the start of a new year
            yesterday = int(day) - 1
        else:
            if(int(year) in [2009, 2013]): #if last year was a leap year
                yesterday = 366
            else:
                yesterday = 365

        # print(today, yesterday)

        yesteryear = year

        if(yesterday in [365, 366]):
            yesteryear = int(year) - 1

        # print(year, yesteryear)

        exists = os.path.isfile(outpath + "GASP_" + yesteryear + "." + str(yesterday) + ".txt")
        file_yesterday = open(outpath + "GASP_" + yesteryear + "." + str(yesterday) + ".txt", 'a+') #appending if the file already exists
        file_today = open(outpath + "GASP_" + year + "." + str(today) + ".txt", 'w+')

        if(exists == False):
            file_yesterday.write("Point, Lon, Lat, AOD \n")
        file_today.write("Point, Lon, Lat, AOD \n")

        for line in file_LL.readlines():
            elements = line.split(",")
            if(elements[0] != ''):
                # print(elements)
                AOD = aod_table.loc[int(elements[0])].values[0]
                if(AOD != -9.99):
                    print(AOD)
                    new_line = elements[0] + ", " + elements[1] + ", " + elements[2] + ", " + str(AOD)
                    # print(new_line)
                    write_day = adjusted_day_per_tz_array[timezones.index(elements[3].rstrip('\n'))]
                    # print(write_day)
                    if(write_day == today):
                        file_today.write(new_line + '\n')
                    else:
                        file_yesterday.write(new_line + '\n')

        file_today.close()
        file_yesterday.close()
        file_LL.close()

        self.upload_to_AWS("GASP_processed/step1/", origpath + item)
        os.remove(origpath + item)

    def three(self, origpath, outpath, item):  # Average aod values for each day at each lat, lon location
        vals = dict()
        if(os.path.getsize(item) > 1000): #checking if it only has a header
            infile = open(item, "r")
            reader = infile.readlines()[1:]  # skip the header
            for line in reader:
                line = line.strip('\n')
                sep = re.split(',', line)
                key = sep[1] + ',' + sep[2]  # Lon, Lat
                # print("Key = " + key)
                aod = float(sep[3])
                # print("AOD = " + aod)
                if (key in vals):
                    vals[key].append(aod)
                else:
                    vals[key] = [aod]
            infile.close()
            # print(vals)

            item = os.path.basename(item)

            file_new = open(outpath + item[-4:] + "_avg.txt", 'w')
            file_new.write("Point, Lon, Lat, AOD \n")
            i = 1
            for key, values in vals.items():
                avg_aod = sum(values) / float(len(values))
                # print(avg_aod)
                file_new.write(str(i) + "," + key + ", " + str(avg_aod) + "\n")
                i += 1
            file_new.close()

            self.upload_to_AWS("GASP_processed/step2/", origpath + item)
        os.remove(origpath + item)

    def four(self, origpath, outpath, item):  # Write average aod values to shapefile
        epsg = 'GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295],AUTHORITY["EPSG", 4269]]'
        item = os.path.basename(item)

        try:
            print(item)
            outfile = os.path.join(outpath, item).replace('_avg.txt', '.shp')
            # print outfile

            # Set up blank lists for data
            long, lat, id_no, aod = [], [], [], []

            # Read data from csv file and store in lists
            with open(origpath + item, 'r') as csvfile:  # had to change to 'r' from 'rb'
                r = csv.reader(csvfile, delimiter=',')

                for i, row in enumerate(r):
                    if i > 0:  # skip header
                        # print(row)
                        long.append(float(row[1]))
                        lat.append(float(row[2]))
                        id_no.append(row[0])
                        aod.append(float(row[3]))

                # Set up shapefile writer and create empty fields
                w = shp.Writer(shp.POINT)
                w.autoBalance = 1  # ensures gemoetry and attributes match
                # check out http://pygis.blogspot.com/2012/10/pyshp-attribute-types-and-point-files.html
                w.field('long', 'F', 10, 8)  # F for float
                w.field('lat', 'F', 10, 8)
                w.field('id_no', 'N')  # N for double precision integer
                w.field('aod', 'F', 10, 8)

                # Loop through the data and write the shapefile
                for j, k in enumerate(long):
                    w.point(k, lat[j])  # Write the geometry
                    w.record(k, lat[j], id_no[j], aod[j])  # Write the attributes

                # Save shapefile
                w.save(outfile)
                print("saved")

                # Create the PRJ file
                prj = open("%s.prj" % outfile[:-4], "w")
                prj.write(epsg)
                prj.close()

        except Exception as e:
            print(str(e))

        self.upload_to_AWS("GASP_processed/step3/", origpath + item)
        os.remove(origpath + item)

    def five(self, origpath, outpath, item):  # Reproject shapefile to ESRI 102003, then interpolate to raster
        SHP = gpd.read_file(item)
        basename = os.path.basename(item)
        print(basename)
        # reproject shapefile
        new_SHP = SHP.to_crs({'init': 'esri:102003'})
        proj_str = 'PROJCS["USA_Contiguous_Albers_Equal_Area_Conic",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Albers"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["central_meridian",-96],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["latitude_of_origin",37.5],UNIT["Meter",1]]'
        # write reprojected shapefile
        intermediate = outpath + "new_" + basename
        new_SHP.to_file(intermediate, driver='ESRI Shapefile', crs_wkt=proj_str)
        # write raster
        outfile = outpath + basename[:-4] + ".tif"
        subprocess.call(
            ['gdal_grid', '-zfield', 'aod', '-a', 'linear', '-txe', '-2380056.81286844', '-480056.81286844425', '-tye',
             '-638166.9912686478', '1581833.0087313522', '-outsize', '555', '475', '-of', 'GTiff', '-ot', 'Float64',
             intermediate, outfile])

        # deal with unprojected shapefiles
        dbf = item[:-4] + ".dbf"
        cpg = item[:-4] + ".cpg"
        prj = item[:-4] + ".prj"
        shx = item[:-4] + ".shx"

        subdir = "GASP_processed/step4/"
        self.upload_to_AWS(subdir, item)
        self.upload_to_AWS(subdir, dbf)
        self.upload_to_AWS(subdir, cpg)
        self.upload_to_AWS(subdir, prj)
        self.upload_to_AWS(subdir, shx)

        os.remove(item)
        os.remove(dbf)
        os.remove(cpg)
        os.remove(prj)
        os.remove(shx)

        # deal with intermediate shapefiles
        dbf = intermediate[:-4] + ".dbf"
        cpg = intermediate[:-4] + ".cpg"
        prj = intermediate[:-4] + ".prj"
        shx = intermediate[:-4] + ".shx"

        subdir = "GASP_processed/step4.5/"
        self.upload_to_AWS(subdir, intermediate)
        self.upload_to_AWS(subdir, dbf)
        self.upload_to_AWS(subdir, cpg)
        self.upload_to_AWS(subdir, prj)
        self.upload_to_AWS(subdir, shx)

        os.remove(intermediate)
        os.remove(dbf)
        os.remove(cpg)
        os.remove(prj)
        os.remove(shx)

    def main(self):
        # Step0
        outpath0 = '/home/jovyan/GASP_processed/step0/'
        pool = multiprocessing.Pool()
        for item in os.listdir(self.data_directory):  # includes aod and lat, lon files
            pool.apply_async(self.zero, [self.data_directory, outpath0, item])
        pool.close()
        pool.join()
        
        # Step1
        outpath1 = '/home/jovyan/GASP_processed/step1/'
        pool = multiprocessing.Pool()
        for item in os.listdir(outpath0):
            pool.apply_async(self.one, [outpath0, outpath1, item])
        pool.close()
        pool.join()

        # Step2
        outpath2 = '/home/jovyan/GASP_processed/step2/'
        pool = multiprocessing.Pool()
        for item in os.listdir(outpath1):
            pool.apply_async(self.two, [outpath1, outpath2, item])
        pool.close()
        pool.join()

        # Step3
        outpath3 = '/home/jovyan/GASP_processed/step3/'
        pool = multiprocessing.Pool()
        for item in os.listdir(outpath2):
            pool.apply_async(self.three, [outpath2, outpath3, item])
        pool.close()
        pool.join()
        
        # Step4
        outpath4 = '/home/jovyan/GASP_processed/step4/'
        pool = multiprocessing.Pool()
        for item in os.listdir(outpath3):
            pool.apply_async(self.four, [outpath3, outpath4, item])
        pool.close()
        pool.join()
        
        # Step5
        outpath5 = '/home/jovyan/GASP_processed/step5/'
        pool = multiprocessing.Pool()
        for item in os.listdir(outpath4):
            pool.apply_async(self.five, [outpath4, outpath5, item])
        pool.close()
        pool.join()


if __name__ == "__main__":
    Test().main()
