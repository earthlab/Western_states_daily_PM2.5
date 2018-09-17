# This script downloads data sets from NASA LAADS HTTPS server

from urllib2 import urlopen, URLError, HTTPError
import json

import boto
from boto.s3.key import Key
import os

class HTTPSDownloader:
    def __init__(self):
        args = self._setup()
        self.start_year = args.start_year
        self.end_year = args.end_year
        self.access_key = args.access_key
        self.secret_key = args.secret_key
        self.s3_bucket = args.s3_bucket
        self.data_directory = args.data_directory
        self.output_path = args.output_path
        
        self.conn = boto.connect_s3(
            aws_access_key=self.access_key,
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
            parser.add_argument('--data_directory', type=str, required=True, help='directory path where data is stored, including lat and lon files')
            parser.add_argument('--output_path', type=str, required=True,
                                help='path for saving out data')
'''
output_path = "C:\Users\User\Documents\MA\RA_2018\collected_data\MODIS_AOD\\"
start_year = 2008
end_year = 2014
data_set_name = "MOD04_L2"
# Note that collection 6.1 is now available, so can change this to 61
collection_number = 6

# Amazon Key ID and Secret Key ID
keyId = ""
sKeyId = ""
bucketName = "earthlab-reid-group"
subdir = "MODIS-AOD/collected_data/"
'''
    def dlfile(self, url, hdf_filename):
        # Open the url
        try:
            f = urlopen(url)
            print "downloading " + url

            # Open our local file for writing
            with open(output_path + hdf_filename, "wb+") as local_file:
                local_file.write(f.read())
                #uploadToS3Bucket(hdf_filename, local_file, subdir)

        #handle errors
        except HTTPError, e:
            print "HTTP Error:", e.code, url
        except URLError, e:
            print "URL Error:", e.reason, url


    def uploadToS3Bucket(self, hdf_filename, file, subdir):
        conn = boto.connect_s3(keyId, sKeyId)
        bucket = conn.get_bucket(bucketName)
        # Get the Key object of the bucket
        k = Key(bucket)
        # Crete a new key with id as the name of the file
        k.key = subdir + hdf_filename
        # Upload the file
        result = k.set_contents_from_file(file, rewind=True)
        os.remove(output_path + hdf_filename)


    def isLeapYear(self, year):
        if year%4 == 0:
            if year%100 == 0:
            if year%400 == 0:
                return True
            else:
                return False
            else:
                return True
        else:
            return False


    def main():
        # Iterate over years of interest
        for year in range(self.start_year, self.end_year+1):
            if self.isLeapYear(year):
                end_date = 366
            else:
                end_date = 365
            # Iterate over all dates in year
            for julian_day in range(1, self.end_date+1):
                print("Downloading data sets for year " + str(year) + " and julian day " + str(julian_day))
                julian_day = str(julian_day).zfill(3)
                # construct base URL with correct year and date
                base_url = ("https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/%d/%s/%d/%s" %
                    (collection_number, data_set_name, year, julian_day))
                # construct link to json file with list of all HDF files for a given date
                hdf_list = base_url + ".json"
                response = urlopen(hdf_list)
                # Read in list of all HDF files for given date
                json_str = response.read()
                parsed_json = json.loads(json_str)

                # Get the name of each HDF file
                for j in parsed_json:
                    hdf_filename = j['name']
                    print("hdf_filename: " + hdf_filename)
                    url = base_url + "/" + hdf_filename
                    dlfile(url, hdf_filename)



if __name__ == '__main__':
    main()
