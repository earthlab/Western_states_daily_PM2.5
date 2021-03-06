# This is the main file which calls all the MAIAC data processing steps as functions on EC2
# Author: Ellen
# Date: 8/15/18

#Processing:
#Convert from HDF to average-value CSV
#Convert from average-value CSV to projected SHP
#Convert from projected SHP to raster
#Upload everything to AWS S3

import glob, os
import multiprocessing
import boto.s3.connection
from boto.s3.key import Key
from pyhdf.SD import SD

from maiac_create_csv import CSV

# Setting up AWS S3 Connection
access_key = ''
secret_key = ''

conn = boto.connect_s3(
    aws_access_key_id=access_key,
    aws_secret_access_key=secret_key
)

s3Bucketname = "earthlab-reid-group"


def upload_to_AWS(subdir, file):
    bucket = conn.get_bucket(s3Bucketname)
    k = Key(bucket)
    k.key = subdir + os.path.basename(file)
    k.set_contents_from_filename(file)  # rewind = True if from file


    
#MAIN code

# Getting latitude, longitude files
latlon_dir = '/home/jovyan/MAIAC-AOD/LL_files/'  # change this, eventually

# Tiles we're using:
tiles = ['h08v04', 'h08v05', 'h08v06', 'h09v04', 'h09v05', 'h09v06', 'h10v04', 'h10v05']

# Download relevant files from ftp://dataportal.nccs.nasa.gov/DataRelease/MODISTile_lat-lon/
# Note: we would need to have a nasa login to pull the lat/lon files directly from ftp...

coord_list = []  # will eventually be eight lists, with lon, lat lists inside each

for LLfile in sorted(glob.glob(latlon_dir + '*.hdf')):
    if not os.stat(LLfile).st_size == 0:
        # Create an SD object from the .hdf file
        hdffile = SD(LLfile)
        attrs = hdffile.datasets()
        lon = hdffile.select('lon')[:]
        lat = hdffile.select('lat')[:]
        coord_list.append([lon, lat])
# print(coord_list)

#Turn HDF files into average-value CSV files

origpath = "/home/jovyan/MAIAC-AOD/collected_data/"
outpath = "/home/jovyan/MAIAC-AOD/avg_csv/"

# create a list of all the days
days = []
for item in os.listdir(origpath):
    # print(item)
    this_day = item[9:16]
    # print(this_day)
    if (this_day in days):
        pass
    else:
        days.append(this_day)


# Loop through each day, with multiprocessing:
pool = multiprocessing.Pool()
for day in days:
    pool.apply_async(CSV, [origpath, outpath, day, coord_list])
pool.close()
pool.join()

for file in sorted(glob.glob(origpath + '*.hdf')):
    os.remove(file)


#Upload CSVs to S3
subdir = "MAIAC-AOD/avg_csv/"

for file in sorted(glob.glob(outpath + '*.csv')):
    upload_to_AWS(subdir, file)
    #os.remove(file)


print ('Calculations complete')
