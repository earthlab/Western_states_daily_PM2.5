import argparse
import glob, os, re, sys
import multiprocessing
import boto.s3.connection
from boto.s3.key import Key

import gzip, shutil, struct
import csv, subprocess
import pandas as pd
import geopandas as gpd
import rasterio as rio
import numpy as np
import scipy.interpolate
from rasterio.transform import from_origin
from rasterio.warp import calculate_default_transform, reproject
from rasterio.enums import Resampling
from rasterio.crs import CRS
import datetime
import pytz
import IPython


# sys.argv = ['--start_year', '2008', '--end_year', '2014', '--access_key',
#             '', '--secret_key', '',
#             '--s3_bucket', 's3://earthlab-reid-group/',
#             '--data_directory', 'C:/Users/ellen/OneDrive/MyDocs/Earth Lab Internship/Spatial_Processing/test_data/GASP/test_results/']


class GASP:
    def __init__(self):
        # args = self._setup()
        self.start_year = 2008  # args.start_year
        self.end_year = 2014  # args.end_year
        self.access_key = 'AKIAJQKU7FLSOKSHUQ4A'  # args.access_key
        self.secret_key = '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'  # args.secret_key
        self.s3_bucket = 's3://earthlab-reid-group/'  # args.s3_bucket
        self.data_directory = 'C:/Users/ellen/OneDrive/MyDocs/Earth Lab Internship/Spatial_Processing/test_data/GASP/test_results/'  # args.data_directory
        # connection here
        self.conn = boto.connect_s3(
            aws_access_key_id=self.access_key,
            aws_secret_access_key=self.secret_key
        )

    def _setup(self):
        parser = argparse.ArgumentParser(description='Pass in AWS credentials.')
        parser.add_argument('--start_year', type=int,
                            help='starting year for data download (starts with Jan 1 of that year)')
        parser.add_argument('--end_year', type=int,
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

    def unzip_GZ(self, origpath, outpath, item):  # Unzip from .gz to binary
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

    def read_from_binary(self, origpath, outpath, item):  # Read from binary
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
            # self.upload_to_AWS("GASP_processed/step0/", origpath + item)
            # os.remove(origpath + item)

    def sort_data(self, origpath, outpath, item):  # Write valid lat, lon, aod values to file with local UTC name (requires time conversion)
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

        today = int(day)
        if (today != 1):  # If it's the start of a new year
            yesterday = int(day) - 1
        else:
            if (int(year) in [2009, 2013]):  # if last year was a leap year
                yesterday = 366
            else:
                yesterday = 365

        # print(today, yesterday)

        yesteryear = year

        if (yesterday in [365, 366]):
            yesteryear = int(year) - 1

        # print(year, yesteryear)

        LL_df = pd.read_csv('C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\test_data\\GASP\\' + "lat_lon_tzid_lookup.txt")  # need to put this in the correct folder before starting
        # print(LL_df.head())
        aod_table = pd.read_table(origpath + item, header=None)
        aod_df = pd.DataFrame(aod_table.loc[LL_df["point"].values])
        aod_df = aod_df.reset_index(drop=True)
        # print(aod_df.head())

        today_tz = [t for t in timezones if adjusted_day_per_tz_array[timezones.index(t)] == str(today).zfill(3)]
        today_ind = [j for j in range(len(LL_df['tzid'])) if LL_df['tzid'][j] in today_tz]
        today_aod = aod_df.loc[today_ind]
        today_LL = LL_df[["lon", "lat"]].loc[today_ind]
        today_df = today_LL.join(today_aod)
        today_df.columns = ["Lon", "Lat", "AOD"]
        not_null = [i for i in range(len(today_df["AOD"])) if float(today_df["AOD"][i]) != -9.99]
        today_df = today_df.loc[not_null,:]
        # print(today_df.head())

        yesterday_tz = [t for t in timezones if adjusted_day_per_tz_array[timezones.index(t)] == str(yesterday).zfill(3)]
        yesterday_ind = [j for j in range(len(LL_df['tzid'])) if LL_df['tzid'][j] in yesterday_tz]
        yesterday_aod = aod_df.loc[yesterday_ind]
        yesterday_LL = LL_df[["lon", "lat"]].loc[yesterday_ind]
        yesterday_df = yesterday_LL.join(yesterday_aod)
        yesterday_df.columns = ["Lon", "Lat", "AOD"]
        not_null = [i for i in range(len(yesterday_df["AOD"])) if float(yesterday_df["AOD"][i]) != -9.99]
        yesterday_df = yesterday_df.loc[not_null, :]
        # print(yesterday_df.head())

        yesterfile = outpath + "GASP_" + yesteryear + "." + str(yesterday).zfill(3) + ".csv"
        exists = os.path.isfile(yesterfile)
        todayfile = outpath + "GASP_" + year + "." + str(today).zfill(3) + ".csv"

        if(exists):
            with open(yesterfile, 'a') as f:
                yesterday_df.to_csv(f, header=False)
        else:
            yesterday_df.to_csv(yesterfile, header=False)

        today_df.to_csv(todayfile, header=False)


        # self.upload_to_AWS("GASP_processed/step1/", origpath + item)
        # os.remove(origpath + item)

    def average_days(self, origpath, outpath, item):  # Average aod values for each day at each lat, lon location
        vals = dict()
        if (os.path.getsize(item) > 1000):  # checking if it only has a header
            infile = open(item, "r")
            reader = csv.reader(infile, delimiter=',')
            for row in reader: #format: ['0', '-104.96376037597656', '30.64215850830078', '1.36']
                key = row[1] + ',' + row[2]  # Lon, Lat
                # print("Key = " + key)
                aod = float(row[3])
                # print("AOD = " + aod)
                if (key in vals):
                    vals[key].append(aod)
                else:
                    vals[key] = [aod]
            infile.close()
            # print(vals)

            item = os.path.basename(item)

            file_new = open(outpath + item[:-4] + "_avg.txt", 'w')
            file_new.write("Point, Lon, Lat, AOD \n")
            i = 1
            for key, values in vals.items():
                avg_aod = sum(values) / float(len(values))
                # print(avg_aod)
                file_new.write(str(i) + "," + key + ", " + str(avg_aod) + "\n")
                i += 1
            file_new.close()

        #     self.upload_to_AWS("GASP_processed/step2/", origpath + item)
        # os.remove(origpath + item)

    def csv_to_geotiff(self, origpath, outpath, item):  # thanks to Max Joseph, Earth Lab Analytics Hub
        pts = pd.read_csv(item)
        pts.rename(columns=lambda x: x.strip(), inplace=True)
        # print(pts.head())

        # Interpolate

        interp = scipy.interpolate.LinearNDInterpolator(pts[['Lon', 'Lat']].values,
                                                        pts[['AOD']].values)
        res = 0.02

        X = np.arange(pts.Lon.min(), pts.Lon.max(), step=res)
        Y = np.arange(pts.Lat.min(), pts.Lat.max(), step=res)
        Xg, Yg = np.meshgrid(X, Y)

        Z = interp(Xg, Yg)
        Z = Z.astype(np.float32)

        # Write geotiff with interpolated values
        transform = from_origin(X[0] - res / 2, Y[-1] + res / 2, res, res)

        basename = os.path.basename(item)

        filename = outpath + basename[:-4] + ".tif"

        new_dataset = rio.open(filename, 'w', driver='GTiff',
                               height=Z.shape[0], width=Z.shape[1],
                               count=1, dtype='float32',
                               crs='+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs', transform=transform)

        new_dataset.write(np.flip(Z[:, :, 0], axis=0), 1)

        new_dataset.close()

        # # reproject:
        #
        # dst_crs = CRS(init = 'ESRI:102003')
        #
        # with rio.open(filename, 'r') as src:
        #     transform, width, height = calculate_default_transform(
        #         src.crs, dst_crs, src.width, src.height, *src.bounds)
        #     kwargs = src.meta.copy()
        #     kwargs.update({
        #         'crs': dst_crs,
        #         'transform': transform,
        #         'width': width,
        #         'height': height
        #     })
        #     with rio.open(filename, 'w', **kwargs) as dst:  # is it ok to write over the file here?
        #         for i in range(1, src.count + 1):
        #             reproject(
        #                 source=rio.band(src, i),
        #                 destination=rio.band(dst, i),
        #                 src_transform=src.transform,
        #                 src_crs=src.crs,
        #                 dst_transform=transform,
        #                 dst_crs=dst_crs,
        #                 resampling=Resampling.bilinear)

        # self.upload_to_AWS("GASP_processed/step3/", origpath + item)
        # os.remove(origpath + item)



    def main(self):
        #         # Step0
        #         outpath0 = '/home/jovyan/GASP_processed/step0/'
        #         pool = multiprocessing.Pool()
        #         for item in os.listdir(self.data_directory):  # includes aod and lat, lon files
        #             pool.apply_async(self.unzip_GZ, [self.data_directory, outpath0, item])
        #         pool.close()
        #         pool.join()

        #         # Step1
        outpath1 = 'C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\test_data\\GASP\\test_results\\step1\\'
        # pool = multiprocessing.Pool()
        # for item in os.listdir(outpath0):
        #     pool.apply_async(self.read_from_binary, [outpath0, outpath1, item])
        # pool.close()
        # pool.join()

        # Step2
        outpath2 = 'C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\test_data\\GASP\\test_results\\step2\\'
        pool = multiprocessing.Pool()
        for item in sorted(glob.glob(outpath1 + "GASP*.txt")):
            print("Sorting")
            pool.apply_async(self.sort_data, [outpath1, outpath2, item])
        pool.close()
        pool.join()


        # Step3
        outpath3 = 'C:/Users/ellen/OneDrive/MyDocs/Earth Lab Internship/Spatial_Processing/test_data/GASP/test_results/step3/'
        pool = multiprocessing.Pool()
        for item in sorted(glob.glob(outpath2 + "GASP*.csv")):
            print("Averaging ")
            pool.apply_async(self.average_days, [outpath2, outpath3, item])
        pool.close()
        pool.join()

        #NEW final step:
        outpath4 = 'C:/Users/ellen/OneDrive/MyDocs/Earth Lab Internship/Spatial_Processing/test_data/GASP/test_results/step4/'
        pool = multiprocessing.Pool()
        for item in sorted(glob.glob(outpath3 + "*avg.txt")):
            print("Rasterizing")
            pool.apply_async(self.csv_to_geotiff, [outpath3, outpath4, item])
        pool.close()
        pool.join()


if __name__ == "__main__":
    GASP().main()
