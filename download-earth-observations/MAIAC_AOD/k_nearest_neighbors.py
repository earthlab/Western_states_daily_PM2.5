#Author: Ellen
#Date: 8/23/18
#Purpose: instead of creating shapefiles and rasters (which is computationally intensive), let's try extracting points nearest to the monitor locations

from sklearn.neighbors import NearestNeighbors
import pandas as pd
import numpy as np
from csv import writer
import glob
import os

# date conversion code from: https://www.science-emergence.com/Articles/MODIS-Convert-Julian-Date-to-MMDDYYYY-Date-format-using-python/
import calendar

def JulianDate_to_MMDDYYY(y,jd):
    month = 1
    day = 0
    while jd - calendar.monthrange(y,month)[1] > 0 and month <= 12:
        jd = jd - calendar.monthrange(y,month)[1]
        month = month + 1
    if jd < 10:
        jd = str(jd)
        jd = "0" + jd
    if month < 10:
        month = str(month)
        month = "0" + month
    return(str(y) + '-' + str(month) + '-' + str(jd))
    #print(month,jd,y)

# These are the monitoring locations:
M_CSV = "/home/jovyan/MAIAC-AOD/Locations_Dates_of_PM25_Obs_DeDuplicate.csv" # 'C:/Users/elco2649/Documents/MAIAC/Locations_Dates_of_PM25_Obs_DeDuplicate.csv'
monitors = pd.read_csv(M_CSV)
monitors['Date'].astype('str')

origpath = "/home/jovyan/MAIAC-AOD/avg_csv/" # 'C:/Users/elco2649/Documents/MAIAC/output_csvs/'
outpath = "/home/jovyan/MAIAC-AOD/extracted/" # 'C:/Users/elco2649/Documents/MAIAC/extracted/'

# This is the output file
with open(outpath + "MAIAC_extracted.csv", 'w') as dst:
    writer = writer(dst, lineterminator='\n')
    writer.writerow(["Latitude", "Longitude", "Datum", "Date", "MAIAC_AOD"])

    # Loop through the MAIAC data (each file contains average values for a given day)
    for csvfile in sorted(glob.glob(origpath + "*.csv")):
        basename = os.path.basename(csvfile)
        print(basename)
        # convert date to useful format
        day_string = basename[:-4]
        year = day_string[:4]
        JD = day_string[4:7]
        #print(year + ", " + JD)
        date = JulianDate_to_MMDDYYY(int(year), int(JD))
        print(date)

        # get monitor locations for this day
        day_monts = monitors.loc[monitors['Date'] == date,:]
        mont_coords = day_monts.loc[:,['Latitude', 'Longitude']]
        # print(mont_coords.shape)
        numObs = mont_coords.shape[0]

        # read in the maiac data for this day
        data = pd.read_csv(csvfile)
        data_coords = data.loc[:, ['lat', 'long']]
        # print(data_coords.shape)

        #Nearest Neighbors Classification: pick k
        k = 8 # this approximates Queen Contiguity?
        nbrs = NearestNeighbors(n_neighbors = k, algorithm = 'kd_tree').fit(data_coords, mont_coords)
        distances, indices = nbrs.kneighbors(mont_coords)
        # print("Distances = ", distances)
        # print("Indices = ", indices)

        #collect and average the aod values of the k nearest neighbors
        mont_aod = []
        for i in range(0, numObs):
            #print(csv[indices[i],3])
            weights = 1 - (distances[i] - distances[i][0])/distances[i][0] #or figure out some other inverse-distance weighting system
            #print(weights)
            mont_aod.append(np.average(data.loc[indices[i],'aod'], weights = weights))
        # print("AOD = ", mont_aod)

        for j in range(0, numObs):
            writer.writerow([mont_coords['Latitude'].iloc[j], mont_coords['Longitude'].iloc[j], 'ESRI102003', date, mont_aod[j]])


# Eventually, upload "MAIAC_extracted.csv" to S3