#Author: Ellen Considine
#Date: 8/11/18
#Purpose: Create CSV from HDF, as in Gina's MODIS script

#PLAN:

#For each tile:
#Download lon/lat coordinates from ftp://dataportal.nccs.nasa.gov/DataRelease/MODISTile_lat-lon/

#For each day:
#We want to create a csv with all data from that day
    #For each hdf file:
    #Assign each orbit's worth of AOD data to an array
    #Take the average of those arrays, to get the avg aod
    #write each coordinate and corresponding avg aod value to the csv

#LATER...
#Run GASP step4a.py, and shp_to_raster.py

#---------------------------------------------------------------------------------------
import glob, os
from os import path
from pyhdf.SD import SD
import pandas as pd
import numpy as np

import ftplib
import sys, re

#Get latitude, longitude
latlon_dir = 'C:\\Users\\elco2649\\Documents\\MAIAC\\' #change this, eventually

#Tiles we're using:
tiles = ['h08v04', 'h08v05', 'h08v06', 'h09v04', 'h09v05', 'h09v06', 'h10v04', 'h10v05']

#Download relevant files from ftp://dataportal.nccs.nasa.gov/DataRelease/MODISTile_lat-lon/
#Note: we would need to have a nasa login to pull the lat/lon files directly from ftp...

coord_list = [] # will eventually be eight lists, with lon, lat lists inside each

for LLfile in sorted(glob.glob(latlon_dir + '*.hdf')):
    if not os.stat(LLfile).st_size == 0:
        # Create an SD object from the .hdf file
        hdffile = SD(LLfile)
        attrs = hdffile.datasets()
        lon = hdffile.select('lon')[:]
        lat = hdffile.select('lat')[:]
        coord_list.append([lon, lat])

#print(coord_list)

# collected_data refers to the path where the .hdf files are located
origpath = 'C:\\Users\\elco2649\\Documents\\MAIAC_DATA\\'
# output_path refers to the path where the .csv files will be output
outpath = 'C:\\Users\\elco2649\\Documents\\MAIAC\\output_csvs\\'


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

#Loop through each day:
for day in days:
    print(day)
    csvpath = outpath + day + ".csv"
    csvfile = open(csvpath, 'w')
    csvfile.write(",".join(["point", "long", "lat", "aod" + "\n"]))
    csvlines = 0

    #Loop through each hdf file:
    for rawfile in sorted(glob.glob(origpath + '*%s.*.hdf' % (day))):
        # grab the name of the file (minus the file extension) and save it as stamp
        stamp = path.basename(rawfile)[:-4]

        tile = stamp[17:23]
        T = [i for i, x in enumerate(tiles) if x == tile]
        t = T[0]
        # print(t)

        line_num = 1
        # if the .hdf file is not empty, proceed
        if not os.stat(rawfile).st_size == 0:
            print(stamp)
            # Create an SD object from the .hdf file
            hdffile = SD(rawfile)
            AOD = hdffile.select("Optical_Depth_047")[:]
            # Get number of orbits
            dims = AOD.shape
            norbits = dims[0]
            AOD_list = []
            for i in range(1, norbits + 1):
                #print("Orbit: " + str(i))
                # get best estimate AOD, latitude, and longitude from .hdf file
                AOD = hdffile.select("Optical_Depth_047")[i - 1][:]
                AOD_list.append(AOD)
            #print(AOD_list)

            #Take the average of the aod values
            data = np.array(AOD_list) #shape=(norbits,1200,1200)
            #print(data.shape)
            avg_AOD = np.average(data, axis=0)
            #print(avg_AOD)

            #get lat/lon corresponding to the right tile
            lon_array = coord_list[t][0]
            lat_array = coord_list[t][1]

            # Turn 2D arrays into 1D arrays
            AOD = avg_AOD.flatten()
            LON = lon_array.flatten()
            LAT = lat_array.flatten()

            allvars = zip(LAT, LON, AOD)

            for v in allvars:
                lats = v[0]
                lons = v[1]
                aod = v[2]

                # append each non-null observation to the .hdf file
                if aod > 0:
                    # Study area bounding box:
                    if (lons >= -126) & (lons <= -101) & (
                            lats >= 25) & (lats <= 50):
                        csvfile.write(",".join([str(line_num), str(lons), str(lats), str(aod * 0.001) + "\n"]))
                        csvlines += 1
                line_num += 1

        else:
            print(stamp + ' is empty')

    csvfile.close()

    #Remove empty csvs
    if(csvlines == 0):
        os.remove(csvpath)

print ('Calculations complete')