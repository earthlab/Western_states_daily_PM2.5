# Download all smoke plume and fire .zip files for study time period

import ftplib
import sys, os, re
import argparse
import boto
from boto.s3.key import Key
from bs4 import BeautifulSoup
import requests
from urllib2 import urlopen
import gzip
import shutil

__credits__ = "Gina Li"
__email__ = "gina.li@colorado.edu"

class HMSDownloader():
    def __init__(self):
        args = self._setup()
        self.start_year = args.start_year
        self.end_year = args.end_year
        self.base_url = "http://satepsanone.nesdis.noaa.gov"
        self.access_key = args.access_key
        self.secret_key = args.secret_key
        self.bucket_name = args.bucket_name
        self.s3_folder_path = args.s3_folder_path
        self.outpath = args.outpath
        self.delete_local = args.delete_local

    def download_data(self):

        # Each year for study time period
        for year in range(self.start_year, self.end_year+1):
            subdir_paths = ["/pub/volcano/FIRE/HMS_ARCHIVE/%d/GIS/FIRE" % year, "/pub/volcano/FIRE/HMS_ARCHIVE/%d/GIS/SMOKE" % year]
            for subdir_path in subdir_paths:
                url = self.base_url + subdir_path
                
                for url in self.listFD(url, "gz"):
                    data_file = url.split("/")[-1]
                    # Retrieve data
                    self.dlfile(url, data_file)

    def gunzip_all(self):
        unzipped_directory_path = os.path.join(self.outpath, 'unzipped')
        zipped_directory_path = os.path.join(self.outpath)
        if not os.path.exists(unzipped_directory_path):
            os.makedirs(unzipped_directory_path)
        for gz_file in os.listdir(zipped_directory_path):
            with gzip.open(os.path.join(zipped_directory_path, gz_file), 'rb') as f_in, open(os.path.join(unzipped_directory_path, gz_file.rsplit(".", 1)[0]), 'wb') as f_out:
                shutil.copyfileobj(f_in, f_out)
                #f_out.close()
                #f_in.close()
                


    def main(self):
        self.download_data()
        #self.gunzip_all()


    def _setup(self):
        parser = argparse.ArgumentParser(description='Pass in arguments for FTP download script')
        parser.add_argument('--start_year', type=int, required=True, help='starting year for data download (starts with Jan 1 of that year)')
        parser.add_argument('--end_year', type=int, required=True, help='ending year for data download (ends with Dec 31 of that year)')
        parser.add_argument('--access_key', type=str, required=True, help='AWS access key')
        parser.add_argument('--secret_key', type=str, required=True, help='AWS secret access key')
        parser.add_argument('--bucket_name', type=str, required=True, help='AWS S3 bucket name to upload final data product')
        parser.add_argument('--s3_folder_path', type=str, required=True, help='path within S3 bucket where data will be uploaded')
        parser.add_argument('--outpath', type=str, required=True, help='output directory for saved data locally')
        parser.add_argument('--delete_local', type=bool, default=True, help='delete intermediary data from local workspace after process finishes')
        args = parser.parse_args()
        return args
    
    def listFD(self, url, ext=''):
        page = requests.get(url).text
        print page
        soup = BeautifulSoup(page, 'html.parser')
        return [url + '/' + node.get('href') for node in soup.find_all('a') if node.get('href').endswith(ext)]

    def dlfile(self, url, filename):
        # Open the url
        try:
            f = urlopen(url)
            print("downloading " + url)
            # Open our local file for writing
            with open(self.outpath + filename, "w+") as local_file:
                local_file.write(f.read())
                print("downloaded " + filename)

        #handle errors
        except Exception as e:
            print(str(e))

if __name__ == "__main__":
    HMSDownloader().main()





