# This script converts the .csv file containing AOD values and lat/long information into shapefiles

'''
Prerequisites:
1) Download MODIS AOD data from NASA
2) Obtain .csv files containing AOD data (by running script `modis_aod_create_csv_file.py`)

Objective:
This script reads in each .csv file (containing AOD values and lat/long data) and converts each row into a point.
Uses multiprocessing to utilize all cores and make for faster processing.

Results: corresponding .shp files for each .csv file with id, lat, long, and aod as field variables for each file.

Note: This script was inspired by Zev Ross's code which was adapted from Phil Moorefield's code.

'''
from utilities import getWKT_PRJ
import shapefile as shp
import csv, glob, os
import concurrent.futures

processed_data = 'C:\\Users\\User\Documents\hard_drive_backup\MODIS_AOD\csv_files\\'
output_location = 'C:\\Users\\User\Documents\hard_drive_backup\MODIS_AOD\shapefiles_4326\\'

prj_info = getWKT_PRJ("epsg", 4326)
print(prj_info)

def makeShapefiles(filename):
    date_time_label = os.path.basename(filename).split(".")
    stamp = date_time_label[1][1:] + "_" + date_time_label[2]

    out_file = output_location + stamp + ".shp"
    print(stamp)

    #Set up blank lists for data
    long,lat,aod=[],[],[]

    #read data from csv file and store in lists
    with open(filename, 'rb') as csvfile:
        r = csv.reader(csvfile, delimiter=',')
        for i,row in enumerate(r):
            if i > 0: #skip header
                long.append(float(row[1]))
                lat.append(float(row[2]))
                aod.append(row[3])

    #Set up shapefile writer and create empty fields
    w = shp.Writer(shp.POINT)
    w.autoBalance = 1 #ensures gemoetry and attributes match
    w.field('long','F',10,8)
    w.field('lat','F',10,8)
    w.field('aod')

    #loop through the data and write the shapefile
    for j,k in enumerate(long):
        w.point(k,lat[j]) #write the geometry
        w.record(k,lat[j],aod[j]) #write the attributes

    #Save shapefile
    w.save(out_file)

    # Create the PRJ file
    prj = open(output_location + stamp + ".prj", "w")
    prj.write(prj_info)
    prj.close()



if __name__ == '__main__':
    with concurrent.futures.ProcessPoolExecutor() as executor:
        # Get list of .csv files to process
        filelist = glob.glob(processed_data + "\\*.csv")

        # Process list of .csv files, but split the work across the process pool to use all CPUs
        for csv_file, shp_file in zip(filelist, executor.map(makeShapefiles, filelist)):
            print("a shapefile was created")
