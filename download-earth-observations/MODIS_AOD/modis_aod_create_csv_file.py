# This script converts the raw MODIS AOD .hdf files to corresponding .csv files

'''
Prerequisites:
Download MODIS AOD data from NASA via FTP (MOD04_L2 and MYD04_L2)

Objective:
Read in each MODIS AOD .hdf file and stores the latitude, longitude, and best estimate AOD value  from the
Deep_Blue_Aerosol_Optical_Depth_550_Land_Best_Estimate SDS into a new, corresponding .csv file.

To Run:
1) Adjust the collected_data and output_path to reflect your local directories
2) Install pyHDF library (https://hdfeos.org/software/pyhdf.php). The source recommend using Anaconda2 distribution.
3) Run script the correct Python interpreter that you installed pyHDF library on (Anaconda2)

Output:
Corresponding .csv files for each original MODIS AOD .hdf file with columns id, lat, long, and aod in each file.

'''

# import necessary libraries for the script to run
import glob, os
from os import path
from pyhdf.SD import SD

# metadata
__credits__ = "Gina Li, Colleen Reid, Melissa Maestas, Ellen Considine"
__email__ = "gina.li@colorado.edu"

# collected_data refers to the path where the .hdf files are located
collected_data = 'H:\MODIS_AOD\collected_data\\'
# output_path refers to the path where the .csv files will be output
output_path = 'H:\MODIS_AOD\processed_data\csv_files\\'

# these folders are subdirectories in collected_data and have the respective terra and aqua .hdf files downloaded into them
# these data folders were populated in the first download step
folders = ['MOD04_terra', 'MYD04_aqua']

# loop through each of the folders
for folder in folders:
    # loop through each .hdf file
    for rawfile in sorted(glob.glob(collected_data + folder + "\\*.hdf")):
        # grab the name of the file (minus the file extension) and save it as stamp
        stamp = path.basename(rawfile)[:-4]
        line_num = 1
        # if the .hdf file is not empty, proceed
        if not os.stat(rawfile).st_size == 0:
            # Create an SD object from the .hdf file
            hdffile = SD(rawfile)

            print(stamp)
            # get best estimate AOD, latitude, and longitude from .hdf file
            AOD = hdffile.select("Deep_Blue_Aerosol_Optical_Depth_550_Land_Best_Estimate")[:]
            LAT = hdffile.select("Latitude")[:]
            LON = hdffile.select("Longitude")[:]

            # store 2D array values into 1D tuple
            aod = tuple(AOD.reshape(AOD.shape[0] * AOD.shape[1]))
            lat = tuple(LAT.reshape(LAT.shape[0] * LAT.shape[1]))
            lon = tuple(LON.reshape(LON.shape[0] * LON.shape[1]))

            # zip tuples together where each element in the array is a complete observation with lat, lon, aod
            allvars = zip(lat, lon, aod)

            # create a new, empty .csv file and write header into it
            bestfile = open(path.join(output_path + "\\" + stamp + ".csv"), "w")
            bestfile.write(",".join(["point", "long", "lat", "aod" + "\n"]))

            # for each item in the array, get the lat, long, and aod value
            for v in allvars:
                lats = v[0]
                lons = v[1]
                aod = v[2]

                # append each non-null observation to the .hdf file
                if aod != -9999:
                    bestfile.write(",".join([str(line_num), str(lons), str(lats), str(aod * 0.001) + "\n"]))
                line_num += 1

            bestfile.close()

        else:
            print(stamp + ' is empty')

print ('Calculations complete')

