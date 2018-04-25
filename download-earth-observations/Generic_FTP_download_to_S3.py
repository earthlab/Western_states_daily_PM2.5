# This script takes in an available data set on the NASA FTP download site and downloads all data from 2008-2014.

'''
Prerequisites:
Have the URL of the FTP site
i.e. `ncdc.noaa.gov`

Objective:
This script downloads all datasets in the Western U.S. from 2008-2014 of the data set of interest from the NASA FTP
site. Default data set is the North American Regional Reanalysis

Results: .hdf files for the data set of interest

'''

import ftplib
import sys, os, re
import boto
from boto.s3.key import Key
import argparse

__credits__ = "Gina Li, Colleen Reid, Melissa Maestas, Ellen Considine"
__email__ = "gina.li@colorado.edu"

import boto
from boto.s3.key import Key

keyId = "AKIAJI4EQVEOL3JMBWLA"
sKeyId= "j4z5C4p+OUhidT19W8ayTU0h08kXJI33hBLcss3M"


# argv[1] is the the FTP URL including sub-directories (i.e. "ftp.ncdc.noaa.gov/pub/has/HAS011070577/" make sure to omit the "ftp://" from the beginning)
# argv[2] is the local file location to write out to

subdirs = []

for x in range(2008, 2014):
    for y in range(1, 13):
        x = str(x)
        month = x + ("%02d" % (y,))
        subdirs.append(month)

print(subdirs)



CLI=argparse.ArgumentParser()

CLI.add_argument(
  "--ftp_url",  # name on the CLI - drop the `--` for positional/required parameters
  nargs="*",  # 0 or more values expected => creates a list
  type=str,
  default="nomads.ncdc.noaa.gov/NARR/",  # default if nothing is provided
)

CLI.add_argument(
  "--subdirs",  # name on the CLI - drop the `--` for positional/required parameters
  nargs="*",  # 0 or more values expected => creates a list
  type=str,
  default=subdirs,  # default if nothing is provided
)

CLI.add_argument(
  "--local_path",  # name on the CLI - drop the `--` for positional/required parameters
  nargs="*",  # 0 or more values expected => creates a list
  type=str,
  default="H:\\NARR",  # default if nothing is provided
)

args = CLI.parse_args()

#Open ftp connection

print(args.ftp_url)

split_url = args.ftp_url.split('/')
base_url = split_url[0]
str = '/'
sub_directories = split_url[1:]
sub_directories_str = str.join(sub_directories)

ftp = ftplib.FTP(base_url)

ftp.login()


ftp.cwd(sub_directories_str)

count = 0

for subdir in args.subdirs:
    ftp.cwd(subdir)
    subdirs2 = ftp.nlst()
    for subdir2 in subdirs2:
        ftp.cwd(subdir2)
        data_files = ftp.nlst()
        print(data_files)
        for data_file in data_files:

            local_path = args.local_path

            if not os.path.exists(local_path):
                os.makedirs(local_path)
                os.chdir(local_path)

            # Retrieve data
            if data_file.startswith("narr-a") and data_file.endswith(".grb"):
                print("this is the data file: " + data_file)
                local_filename = os.path.join(local_path, data_file)
                print(data_file)
                file = open(local_filename, 'wb+')

                ftp.retrbinary('RETR ' + data_file, file.write)

                bucketName="earthlab-reid-group"

                conn = boto.connect_s3(keyId,sKeyId)
                bucket = conn.get_bucket(bucketName)
                #Get the Key object of the bucket
                k = Key(bucket)
                #Crete a new key with id as the name of the file
                k.key="NARR/" + data_file
                #Upload the file
                result = k.set_contents_from_file(file, rewind=True)
                #result contains the size of the file uploaded
                count+=1

                # delete the downloaded file locally after uploading to S3
                #os.remove(local_filename)
        ftp.cwd('..')
    ftp.cwd('..')


print("Number of files successfully downloaded from FTP site:")
print(count)
print("Done downloading all data sets")
ftp.quit()

