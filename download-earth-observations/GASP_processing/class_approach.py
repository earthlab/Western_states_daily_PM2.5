import argparse
import glob, os
import multiprocessing
import boto.s3.connection
from boto.s3.key import Key

import gzip, shutil, struct


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
            aws_access_key=self.access_key,
            aws_secret_access_key=self.secret_key
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
        parser.add_argument('--data_directory', type=str, required=True, help='directory path where data is stored, including lat and lon files')
        #print(args.access_key, args.secret_key, args.s3_bucket, args.data_directory)

        args = parser.parse_args()
        return args

    def upload_to_AWS(self, subdir, file):
        bucket = self.conn.get_bucket(self.s3Bucketname)
        k = Key(bucket)
        k.key = subdir + os.path.basename(file)
        k.set_contents_from_filename(file)  # rewind = True if from file

    def zero(self, origpath, outpath, item): #Unzip from .gz to binary
        item = os.path.basename(item)
        # while i < 10:
        with gzip.open(origpath + item, 'rb') as f_in:
            with open(outpath + item[:-3], 'wb') as f_out:
                shutil.copyfileobj(f_in, f_out)
                # i = i + 1
            f_out.close()
        f_in.close()
        f_out.close()
        #Note: not re-uploading zipped data to AWS
        os.remove(origpath + item)

    def one(self, origpath, outpath, item): #Read from binary
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

    def two(self, origpath, outpath, item): #Write valid lat, lon, aod values to file with local UTC name (requires time conversion)
        item = os.path.basename(item)
        print(item)
        line_num = 1
        timestamp = item[-17:] #we need this because the files have different beginnings, such as GOESW versus GOES11...
        year = timestamp[:4]
        day = timestamp[4:7]
        time = timestamp[9:13]

        #Convert time stamp to local UTC, create array for four regions

        #Instead, read in Lat/Lon/TimeZone file from Gina... write to correct day file

        file_lat = open(origpath + 'lat.txt', 'r')
        file_lon = open(origpath + 'lon.txt', 'r')

        file_aod = open(origpath + item, 'r')
        file_new = open(outpath + item, 'w')
        file_new.write("Point, Lon, Lat, AOD \n")

        line_lat = iter(file_lat)
        line_lon = iter(file_lon)

        for line in file_aod:
            lon = next(line_lon)
            lat = next(line_lat)
            # Don't include missing values:
            if (line.rstrip('\n') != "-9.99") & (lon.rstrip('\n') != "-200") & (lat.rstrip('\n') != "-200"):
                # Study area bounding box:
                if (float(lon.rstrip('\n')) >= -126) & (float(lon.rstrip('\n')) <= -101) & (
                        float(lat.rstrip('\n')) >= 25) & (float(lat.rstrip('\n')) <= 50):
                    new_line = str(line_num) + ", " + lon.rstrip('\n') + ", " + lat.rstrip('\n') + ", " + line.rstrip(
                        '\n')
                    file_new.write(new_line + '\n')
                    line_num += 1

        file_aod.close()
        file_new.close()
        file_lat.close()
        file_lon.close()

        self.upload_to_AWS("GASP_processed/step1/", origpath + item)
        os.remove(origpath + item)

    def three(self): #Average aod values for each day at each lat, lon location
        print(self.end_year)

    def four(self): #Write average aod values to shapefile
        epsg = 'GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295],AUTHORITY["EPSG", 4269]]'
        print(self.end_year)

    def five(self): #Reproject shapefile to ESRI 102003, then interpolate to raster
        pass

    def main(self):
        #Step0
        outpath0 = '/home/jovyan/GASP_processed/step0/'
        pool = multiprocessing.Pool()
        for item in os.listdir(self.data_directory): #includes aod and lat, lon files
            pool.apply_async(self.zero, [self.data_directory, outpath0, item])
        pool.close()
        pool.join()

        #Step1
        outpath1 = '/home/jovyan/GASP_processed/step1/'
        pool = multiprocessing.Pool()
        for item in os.listdir(outpath0):
            pool.apply_async(self.one, [outpath0, outpath1, item])
        pool.close()
        pool.join()

        #Step2
        outpath2 = '/home/jovyan/GASP_processed/step2/'
        pool = multiprocessing.Pool()
        for item in os.listdir(outpath0):
            pool.apply_async(self.two, [outpath1, outpath2, item])
        pool.close()
        pool.join()

        #Step3
        outpath3 = '/home/jovyan/GASP_processed/step3/'
        # Loop through files
        self.three(outpath2, outpath3)
        outpath4 = '/home/jovyan/GASP_processed/step4/'
        # Loop through files
        self.four(outpath3, outpath4)
        outpath5 = '/home/jovyan/GASP_processed/step5/'
        # Loop through files
        self.five(outpath4, outpath5)



if __name__ == "__main__":
    Test().main()
