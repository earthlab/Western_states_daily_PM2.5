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

import csv, shapefile, glob
from os import path

__credits__ = "Gina Li, Colleen Reid, Melissa Maestas, Ellen Considine"
__email__ = "gina.li@colorado.edu"

processed_data = 'H:\MODIS_AOD\processed_data\csv_files\\'
output_location = 'H:\MODIS_AOD\processed_data\shapefiles\\'

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

points = shapefile.Writer(shapefile.POINT)
points.autoBalance = 1

points.field("aod", "F", 10, 5)

counter = 1

for file in sorted(glob.glob(processed_data + "\\*.csv")):
    stamp = path.basename(file)[:-4]
    with open(file, 'rb') as csv_file:
        print(stamp)

        csv_reader = csv.reader(csv_file, delimiter=',')
        next(csv_reader, None)

        for row in csv_reader:
            print(row)
            if len(row) <= 1:
                break
            long = row[1]
            lat = row[2]
            aod = row[3]

            points.point(float(long), float(lat))
            points.record(aod)
            print "Feature " + str(counter) + " added to Shapefile."
            counter = counter + 1

        points.save(output_location + stamp)

        # Give .prj filename without the acquisition date. For some reason, the output .shp and associated files
        # do not include the aquisition date (truncates after the last ".". So, we will do the same here in order to
        # have a correctly associated .prj file

        stamp2 = path.basename(file)[:-18]
        print("stamp2: " + str(stamp2))
        prj = open(output_location + stamp2 + ".prj", "w")

        epsg = getWKT_PRJ("4326")
        prj.write(epsg)
        prj.close()