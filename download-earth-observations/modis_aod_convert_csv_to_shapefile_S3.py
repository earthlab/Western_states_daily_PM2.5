# This script converts the .csv file containing AOD values and lat/long information into shapefiles

'''
Prerequisites:
1) Download MODIS AOD data from NASA
2) Obtain .csv files containing AOD data (by running script `modis_aod_create_csv_file.py`)

Objective:
This script reads in each .csv file (containing AOD values and lat/long data) and converts each row into a point.
Results: corresponding .shp files for each .csv file with id, lat, long, and aod as field variables for each file.
Note: This script was inspired by Zev Ross's code which was adapted from Phil Moorefield's code.
'''

import csv, shapefile, glob, datetime, os
from os import path
import boto
from boto.s3.key import Key

__credits__ = "Gina Li, Colleen Reid, Melissa Maestas, Ellen Considine"
__email__ = "gina.li@colorado.edu"

keyId = "AKIAJI4EQVEOL3JMBWLA"
sKeyId = "j4z5C4p+OUhidT19W8ayTU0h08kXJI33hBLcss3M"

#processed_data = 'H:\MODIS_AOD\processed_data\csv_files\\'
output_location = 'H:\MODIS_AOD\processed_data\shapefiles2\\'
temp_output_location = 'H:\MODIS_AOD\processed_data\output_temp\\'


# function to generate .prj file information using spatialreference.org
def getWKT_PRJ (epsg_code):
     import urllib
     # access projection information
     wkt = urllib.urlopen("http://spatialreference.org/ref/epsg/{0}/prettywkt/".format(epsg_code))
     # remove spaces between charachters
     remove_spaces = wkt.read().replace(" ","")
     # place all the text on one line
     output = remove_spaces.replace("\n", "")
     return output

# Define attributes for point shapefile
points = shapefile.Writer(shapefile.POINT)
points.autoBalance = 1

points.field("aod", "F", 10, 5)
points.field("lat", "F", 10, 5)
points.field("long", "F", 10, 5)
points.field("year", "C")
points.field("month", "C")
points.field("day", "C")
points.field("hour_min", "C")

# Specify S3 bucket connection
bucketName = "earthlab-reid-group"
conn = boto.connect_s3(keyId, sKeyId)
bucket = conn.get_bucket(bucketName)


counter = 1

for l in bucket.list(prefix='MODIS-AOD/CSV/M'):
    # Get file names of .csv files (without prefixes or extension)
    keyString = str(l.key)
    key_str = str(l)
    key_split = key_str.split("/")
    filename = key_split[2][:-5]


    # Download each .csv file to local (this is temporary, wil be deleted at end)
    # check if file exists locally, if not: download it
    if not os.path.exists(temp_output_location + keyString):
        csv_file_name = temp_output_location + filename + ".csv"
        f = open(csv_file_name, "wb+")
        l.get_contents_to_file(f)

    with open(csv_file_name, 'rb') as csv_file:
        print("THIS IS THE CSV FILE BEING PROCESSED: " + csv_file_name)

        # Get the whole UTC date/time information string from the file name
        date_time_label = path.basename(csv_file_name).split(".")
        print(date_time_label)

        # Extract the UTC date portion
        date_label = date_time_label[1][1:]
        # Extract the UTC time portion
        time_label = date_time_label[2]

        # Extract the UTC year
        year_str = date_label[:4]
        # Extract the UTC Julian day
        julian_day_str = date_label[4:]
        # Extract the UTC month and day from Julian day

        file_dt = datetime.datetime.strptime(year_str + julian_day_str, '%Y%j').date()

        # Extract the UTC hour
        hour_str = time_label[:2]
        # Extract the UTC minute
        minute_str = time_label[2:]

        year = str(file_dt.year)
        month = str(file_dt.month)
        day = str(file_dt.day)
        hour_min = hour_str + ":" + minute_str

        csv_reader = csv.reader(csv_file, delimiter=',')
        next(csv_reader, None)

        for row in csv_reader:
            print(row)

            if len(row) <= 3:
                raise AttributeError("Not all columns present")


            long = row[1]
            lat = row[2]
            aod = row[3]

            points.point(float(long), float(lat))
            points.record(aod, lat, long, year, month, day, hour_min)


            print "Feature added to Shapefile."


        points.save(output_location + filename + ".csv")
        print("stamp: " + filename)


        # Give .prj filename without the acquisition date. For some reason, the output .shp and associated files
        # do not include the aquisition date (truncates after the last ".". So, we will do the same here in order to
        # have a correctly associated .prj file

        print("filename: " + filename)
        prj = open(output_location + filename + ".prj", "w")

        epsg = getWKT_PRJ("4326")
        prj.write(epsg)
        prj.close()


        # Get the Key object of the bucket
        k = Key(bucket)
        # Crete a new key with id as the name of the file
        ext_list = ['.shp', '.dbf', '.prj', '.shx']
        for ext in ext_list:
            k.key = "MODIS-AOD/Shapefile/" + filename + ext

            # Upload the file
            print(output_location + filename + ext)
            result = k.set_contents_from_filename(output_location + filename + ext)
            # result contains the size of the file uploaded

        counter += 1

        # Delete the .csv, .shp, .dbf, .prj, and .shx off of local
        os.remove(output_location + filename + ".shx")
        os.remove(output_location + filename + ".shp")
        os.remove(output_location + filename + ".dbf")
        os.remove(output_location + filename + ".prj")

        os.remove(temp_output_location + filename + ".csv")


        print(str(counter) + " files uploaded")

