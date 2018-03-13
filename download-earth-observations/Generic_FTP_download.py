# This script takes in an available data set on the NASA FTP download site and downloads all data from 2008-2014.

'''
Prerequisites:
Have the URL of the FTP site
i.e. `ncdc.noaa.gov`

Objective:
This script downloads all datasets in the Western U.S. from 2008-2014 of the data set of interest from the NASA FTP
site. We specify the spatial range with a regular expression that chooses the correct tiles from the  MODIS Sinusoidal
Tile Grid scheme.

Results: .hdf files for the data set of interest

'''

import ftplib
import sys, os, re
import boto
from boto.s3.key import Key

__credits__ = "Gina Li, Colleen Reid, Melissa Maestas, Ellen Considine"
__email__ = "gina.li@colorado.edu"

import boto
from boto.s3.key import Key

keyId = "AKIAJI4EQVEOL3JMBWLA"
sKeyId= "j4z5C4p+OUhidT19W8ayTU0h08kXJI33hBLcss3M"


# argv[1] is the the FTP URL including sub-directories (i.e. "ftp.ncdc.noaa.gov/pub/has/HAS011070577/" make sure to omit the "ftp://" from the beginning)
# argv[2] is the local file location to write out to



#Open ftp connection

split_url = sys.argv[1].split('/')
base_url = split_url[0]
str = '/'
sub_directories = split_url[1:]
sub_directories_str = str.join(sub_directories)

ftp = ftplib.FTP(base_url)

ftp.login()


ftp.cwd(sub_directories_str)

data_files = ftp.nlst()
print("Number of files available for download on FTP site:")
print(len(data_files))

count = 0
for data_file in data_files:

    print(data_file)

    local_path = sys.argv[2]

    if not os.path.exists(local_path):
        os.makedirs(local_path)
        os.chdir(local_path)

    # Retrieve data
    local_filename = os.path.join(local_path, data_file)
    file = open(local_filename, 'wb+')
    
    ftp.retrbinary('RETR ' + data_file, file.write)

    bucketName="earthlab-reid-group"

    conn = boto.connect_s3(keyId,sKeyId)
    bucket = conn.get_bucket(bucketName)
    #Get the Key object of the bucket
    k = Key(bucket)
    #Crete a new key with id as the name of the file
    k.key="GASP-AOD/" + data_file
    #Upload the file
    result = k.set_contents_from_file(file, rewind=True)
    #result contains the size of the file uploaded
    count+=1

    # delete the downloaded file locally after uploading to S3
    os.remove(local_filename)

print("Number of files successfully downloaded from FTP site:")
print(count)
print("Done downloading all data sets")
ftp.quit()

