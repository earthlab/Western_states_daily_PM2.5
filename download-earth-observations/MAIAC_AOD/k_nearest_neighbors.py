#Author: Ellen
#Date: 8/23/18
#Purpose: instead of creating shapefiles and rasters (which is computationally intensive), let's try extracting points nearest to the monitor locations

from sklearn.neighbors import NearestNeighbors
import pandas as pd
import numpy as np
import csv
import glob
import os, shutil
import datetime

# date conversion code from: https://www.science-emergence.com/Articles/MODIS-Convert-Julian-Date-to-MMDDYYYY-Date-format-using-python/
import calendar

start = datetime.datetime.now()

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

def square(list):
    return [i ** 2 for i in list]

def main():
    # These are the monitoring locations:
    M_CSV = 'C:/Users/elco2649/Documents/MAIAC/Projected_locations_with_dates_part_a.csv' # '/home/jovyan/Projected_locations_with_dates_part_a.csv'
    monitors = pd.read_csv(M_CSV)
    monitors['Date'].astype('str')

    origpath = 'C:/Users/elco2649/Documents/MAIAC/avg_csvs/'
    outpath = 'C:/Users/elco2649/Documents/MAIAC/used_csvs/'

    # This is the output file
    with open("C:/Users/elco2649/Documents/MAIAC/MAIAC_extracted.csv", 'a+') as dst:
        writer = csv.writer(dst, lineterminator='\n')
        writer.writerow(["Lat", "Lon", "Date", "MAIAC_AOD"])

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
            if(day_monts.shape[0] > 1):
                mont_coords = day_monts.loc[:,['Lat', 'Lon']]
                # print(mont_coords.shape)
                numObs = mont_coords.shape[0]

                # read in the maiac data for this day
                data = pd.read_csv(csvfile)
                if(data.shape[0] > 1):
                    data_coords = data.loc[:, ['lat', 'long']]
                    # print(data_coords.shape)

                    #Nearest Neighbors Classification: pick k
                    k = 1 # this approximates Queen Contiguity?
                    nbrs = NearestNeighbors(n_neighbors = k, algorithm = 'kd_tree').fit(data_coords, mont_coords)
                    distances, indices = nbrs.kneighbors(mont_coords)
                    # print("Distances = ", distances)
                    # print("Indices = ", indices)

                    #collect and average the aod values of the k nearest neighbors
                    mont_aod = []
                    for i in range(0, numObs):
                        # #print(csv[indices[i],3])
                        # sq = np.array(square(distances[i]))
                        # weights = 1. / sq #or figure out some other inverse-distance weighting system
                        # # print(weights)
                        # mont_aod.append(np.average(data.loc[indices[i],'aod'], weights = weights))
                        mont_aod.append((data.loc[indices[i],'aod']).values[0])
                    # print("AOD = ", mont_aod)
    
                    for j in range(0, numObs):
                        writer.writerow([mont_coords['Lat'].iloc[j], mont_coords['Lon'].iloc[j], date, mont_aod[j]])

            shutil.move(csvfile, outpath + basename)

    end = datetime.datetime.now()
    print("Time for all files: ", end - start)
    # Eventually, upload "MAIAC_extracted.csv" to S3


if __name__ == "__main__":
    main()
