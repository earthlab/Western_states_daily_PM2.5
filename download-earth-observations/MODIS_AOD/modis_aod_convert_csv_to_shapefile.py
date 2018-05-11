# This script converts each .csv file to a corresponding .shp file

'''
Prerequisites:
1) Download MODIS AOD data from NASA via FTP (MOD04_L2 and MYD04_L2)
2) Create .csv files containing lat/long and AOD values (by running modis_aod_create_csv_file.py script)

Objective:
Read in each .csv file (containing AOD values and lat/long data) and convert each row/observation into a point. One
.shp file created per .csv file. Use multiprocessing to utilize all cores and make for faster processing.

To Run:
1) Adjust the processed_data and output_path to reflect your local directories
2) Install pyshp library (https://pypi.org/project/pyshp/). I recommend installing it on either your
Anaconda2 or Python 2.7.X  distribution. This is because the script does a call to getWKT_PRJ which contains syntax for
Python 2.7.X (the urlopen function).
3) Run script with the correct Python interpreter that you installed pyHDF library on

Output:
Corresponding .shp files for each .csv file with fid, lat, long, and aod as fields/columns for each file.

Additional Notes/Resources:
1) GIS Stack exchange question to understand .csv to .shp conversion:
https://gis.stackexchange.com/questions/35593/using-pyshp-to-convert-csv-file-to-shp for
2) Understand multiprocess implemetation:
https://medium.com/@ageitgey/quick-tip-speed-up-your-python-data-processing-scripts-with-process-pools-cf275350163a


'''

# import necessary libraries for the script to run
from utilities import getWKT_PRJ
import shapefile as shp
import csv, glob, os
import concurrent.futures

# metadata
__credits__ = "Gina Li, Colleen Reid, Melissa Maestas, Ellen Considine"
__email__ = "gina.li@colorado.edu"

# processed_data refers to the path where the .csv files are located
processed_data = 'C:\\Users\\User\Documents\hard_drive_backup\MODIS_AOD\csv_files\\'
# output_path refers to the path where the .shp files will be output
output_path = 'C:\\Users\\User\Documents\hard_drive_backup\MODIS_AOD\shapefiles_4326\\'

# Get WKT string that denotes the EPSG 4326 projection information
prj_info = getWKT_PRJ("epsg", 4326)
print(prj_info)

# function that converts the .csv file to a .shp file
def makeShapefiles(filename):
    # create an array of the different components in the filename string
    date_time_label = os.path.basename(filename).split(".")
    # make variable stamp, which is a string of the date and acquisition time (i.e. "2008001_1745")
    stamp = date_time_label[1][1:] + "_" + date_time_label[2]
    # If terra, add a "t" to the stamp (i.e. "2008001_1745t")
    if date_time_label[0][1] == "O":
        stamp = stamp + "t"
    # if aqua, add an "a" to the stamp (i.e. "2008001_1745a")
    if date_time_label[0][1] == "Y":
        stamp = stamp + "a"

    # create new, empty output .shp file
    out_file = output_path + stamp + ".shp"
    print(stamp)

    #Set up blank lists for data
    long,lat,aod=[],[],[]

    #read data from .csv file and store in lists
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
    w.field('aod', 'F',10,5)

    #loop through the data and write the shapefile
    for j,k in enumerate(long):
        w.point(k,lat[j]) #write the geometry
        w.record(k,lat[j],aod[j]) #write the attributes

    #Save shapefile
    w.save(out_file)

    # Create the PRJ file
    prj = open(output_path + stamp + ".prj", "w")
    prj.write(prj_info)
    prj.close()



if __name__ == '__main__':
    # Implements multiprocessing
    with concurrent.futures.ProcessPoolExecutor() as executor:
        # Get list of .csv files to process
        filelist = glob.glob(processed_data + "\\*.csv")

        # Process list of .csv files, but split the work across the process pool to use all CPUs
        for csv_file, shp_file in zip(filelist, executor.map(makeShapefiles, filelist)):
            print("a shapefile was created")
