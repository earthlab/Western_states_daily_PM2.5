# This script downloads data sets from NASA LAADS HTTPS server

from urllib2 import urlopen, URLError, HTTPError
import json
import calendar
import boto
from boto.s3.key import Key
import os
import argparse
from retrying import retry

@retry
def retry_urlopen():
    response = urlopen(hdf_list)

class HTTPSDownloader:
    def __init__(self):
        args = self._setup()
        self.start_year = args.start_year
        self.end_year = args.end_year
        self.access_key = args.access_key
        self.secret_key = args.secret_key
        self.s3_bucket = args.s3_bucket
        self.output_path = args.output_path
        self.collection_number = args.collection_number
        self.data_set_name = args.data_set_name
        self.tiles = ['h08v04', 'h09v04', 'h08v05', 'h10v04', 'h11v04', 'h09v05', 'h10v05']
        self.date_cadence = args.date_cadence

        self.conn = boto.connect_s3(
            aws_access_key_id=self.access_key,
            aws_secret_access_key=self.secret_key
        )

    def _setup(self):
            parser = argparse.ArgumentParser(description='arguments for HTTPS download script')
            parser.add_argument('--start_year', type=int, required=True,
                                help='starting year for data download (starts with Jan 1 of that year)')
            parser.add_argument('--end_year', type=int, required=True,
                                help='ending year for data download (ends with Dec 31 of that year)')
            parser.add_argument('--access_key', type=str, required=True,
                                help='aws access key')
            parser.add_argument('--secret_key', type=str, required=True,
                                help='secret access key')
            parser.add_argument('--data_set_name', type=str, required=True,
                                help='name of data set as it appears in NASA LAADS DAAC site i.e. MOD04')
            parser.add_argument('--collection_number', type=str, required=True,
                                help='collection number as it appears in NASA LAADS DAAC site i.e. 61 for colelction 6.1')
            parser.add_argument('--s3_bucket', type=str, required=True, help='s3 bucket name')
            parser.add_argument('--output_path', type=str, required=True,
                                help='path for saving out data')
            parser.add_argument('--date_cadence', type=str, default='daily', help='Data collection frequency. Either monthly or daily, depending on the data set')
            args = parser.parse_args()
            return args

    def dlfile(self, url, hdf_filename):
        # Open the url
        try:
            f = urlopen(url)
            print "downloading " + url

            # Open our local file for writing
            with open(self.output_path + hdf_filename, "wb+") as local_file:
                local_file.write(f.read())
                #uploadToS3Bucket(hdf_filename, local_file, subdir)

        #handle errors
        except HTTPError, e:
            print "HTTP Error:", e.code, url
        except URLError, e:
            print "URL Error:", e.reason, url


    def uploadToS3Bucket(self, hdf_filename, file, subdir):
        conn = boto.connect_s3(self.access_key, self.secret_key)
        bucket = conn.get_bucket(self.s3_bucket)
        # Get the Key object of the bucket
        k = Key(bucket)
        # Crete a new key with id as the name of the file
        k.key = subdir + hdf_filename
        # Upload the file
        result = k.set_contents_from_file(file, rewind=True)
        os.remove(self.output_path + hdf_filename)
    

    def main(self):
        # Iterate over years of interest
        for year in range(self.start_year, self.end_year+1):
            if calendar.isleap(year):
                end_date = 366
                month_end_list = [1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336]
            else:
                end_date = 365
                month_end_list = [1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335]
            if self.date_cadence == 'monthly':
                cadence = month_end_list
            elif self.date_cadence == 'daily':
                cadence = range(1, end_date+1)
            # Iterate over all dates in year
            for julian_day in cadence:
                print("Downloading data sets for year " + str(year) + " and julian day " + str(julian_day))
                julian_day = str(julian_day).zfill(3)
                # construct base URL with correct year and date
                base_url = ("https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/%s/%s/%d/%s" %
                    (self.collection_number, self.data_set_name, year, julian_day))
                # construct link to json file with list of all HDF files for a given date
                hdf_list = base_url + ".json"
                retry_urlopen()
                # Read in list of all HDF files for given date
                json_str = response.read()
                parsed_json = json.loads(json_str)

                # Get the name of each HDF file
                for j in parsed_json:
                    hdf_filename = j['name']
                    tile = hdf_filename.split(".")[2]
                    if tile in self.tiles:
                        print("hdf_filename: " + hdf_filename)
                        url = base_url + "/" + hdf_filename
                        self.dlfile(url, hdf_filename)



if __name__ == '__main__':
    HTTPSDownloader().main()
