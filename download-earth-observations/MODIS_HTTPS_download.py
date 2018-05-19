# This script downloads data sets from NASA LAADS HTTPS server
'''
Prerequisites:
1) Download and install Python 2.7 (I recommend Anaconda 2)

Objective:
In this script, daily data sets from NASA LAADS are downloaded, then uploaded to the S3 bucket.

To Run:
1) Adjust the output_path, start_year, end_date, and collection_number
2) Run script with the correct Python interpreter that arcpy is installed on (the Python 2.7 that comes with ArcGIS)

Output:
Downloaded MODIS data sets (option to upload to S3 commented out, but can uncomment to implement)

'''

from urllib2 import urlopen, URLError, HTTPError
import json

import boto
from boto.s3.key import Key
import os

output_path = "C:\Users\User\Documents\MA\RA_2018\collected_data\MODIS_AOD\\"
start_year = 2008
end_year = 2014
# Note that collection 6.1 is now available, so can change this to 61
collection_number = 6

# Amazon Key ID and Secret Key ID
keyId = "AKIAJI4EQVEOL3JMBWLA"
sKeyId = "j4z5C4p+OUhidT19W8ayTU0h08kXJI33hBLcss3M"
bucketName = "earthlab-reid-group"
subdir = "MODIS-AOD/collected_data/"

def dlfile(url, hdf_filename):
    # Open the url
    try:
        f = urlopen(url)
        print "downloading " + url

        # Open our local file for writing
        with open(output_path + hdf_filename, "wb+") as local_file:
            local_file.write(f.read())
            uploadToS3Bucket(hdf_filename, local_file, subdir)

    #handle errors
    except HTTPError, e:
        print "HTTP Error:", e.code, url
    except URLError, e:
        print "URL Error:", e.reason, url


def uploadToS3Bucket(hdf_filename, file, subdir):
    conn = boto.connect_s3(keyId, sKeyId)
    bucket = conn.get_bucket(bucketName)
    # Get the Key object of the bucket
    k = Key(bucket)
    # Crete a new key with id as the name of the file
    k.key = subdir + hdf_filename
    # Upload the file
    result = k.set_contents_from_file(file, rewind=True)
    os.remove(output_path + hdf_filename)


def isLeapYear(year):
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
    for year in range(start_year, end_year+1):
        if isLeapYear(year):
            end_date = 366
        else:
            end_date = 365
        # Iterate over all dates in year
        for julian_day in range(1, end_date+1):
            print("Downloading data sets for year " + str(year) + " and julian day " + str(julian_day))
            julian_day = str(julian_day).zfill(3)
            # construct base URL with correct year and date
            base_url = ("https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/%d/MOD04_L2/%d/%s" %
                   (collection_number, year, julian_day))
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
