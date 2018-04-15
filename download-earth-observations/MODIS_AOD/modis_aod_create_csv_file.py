# This script converts the raw MODIS AOD .hdf data into .csv format

'''
Prerequisites:
1) Download MODIS AOD data from NASA

Objective:
This script reads in each MODIS AOD .hdf file (that has been previously downloaded via FTP from NASA) and stores the
latitude, longitude, and best estimate AOD value into a new, corresponding .csv file. Adjust the output_path and
collected_data path to reflect your local directories.

Results:
Corresponding .csv files for each original MODIS AOD .hdf file with id, lat, long, and aod as column headers
for each file.

Note: This script was adapted from Zev Ross's code which was adapted from Phil Moorefield's code.

'''

import glob, os
from os import path
from pyhdf.SD import SD

__credits__ = "Gina Li, Colleen Reid, Melissa Maestas, Ellen Considine"
__email__ = "gina.li@colorado.edu"

output_path = 'H:\MODIS_AOD\processed_data\csv_files\\'
collected_data = 'H:\MODIS_AOD\collected_data\\'

folders = ['MOD04_terra', 'MYD04_aqua']

for folder in folders:
    for rawfile in sorted(glob.glob(collected_data + folder + "\\*.hdf")):
        stamp = path.basename(rawfile)[:-4]
        line_num = 1

        if not os.stat(rawfile).st_size == 0:
            hdffile = SD(rawfile)

            print(stamp)

            AOD = hdffile.select("Deep_Blue_Aerosol_Optical_Depth_550_Land_Best_Estimate")[:]
            LAT = hdffile.select("Latitude")[:]
            LON = hdffile.select("Longitude")[:]

            aod = tuple(AOD.reshape(AOD.shape[0] * AOD.shape[1]))
            lat = tuple(LAT.reshape(LAT.shape[0] * LAT.shape[1]))
            lon = tuple(LON.reshape(LON.shape[0] * LON.shape[1]))

            allvars = zip(lat, lon, aod)

            bestfile = open(path.join(output_path + "\\" + stamp + ".csv"), "w")
            bestfile.write(",".join(["point", "long", "lat", "aod" + "\n"]))

            for v in allvars:
                lats = v[0]
                lons = v[1]
                aod = v[2]

                if aod != -9999:
                    bestfile.write(",".join([str(line_num), str(lons), str(lats), str(aod * 0.001) + "\n"]))
                line_num += 1

            bestfile.close()

        else:
            print(stamp + ' is empty')

print ('Calculations complete')

