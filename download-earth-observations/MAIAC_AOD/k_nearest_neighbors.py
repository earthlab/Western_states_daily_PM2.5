#instead of creating shapefiles and rasters, let's try extracting points nearest to the monitor locations:

from sklearn.neighbors import NearestNeighbors
import numpy as np
from csv import writer
import glob
import os

#These are the monitoring locations:
mont_file = np.genfromtxt('C:/Users/elco2649/Documents/Reproject_monitors/Final_monitors.csv', delimiter= ",", skip_header = True)
monitors = mont_file[:,[1,0]]
numObs = monitors.shape[0]

origpath = 'C:/Users/elco2649/Documents/MAIAC/output_csvs/'
outpath = 'C:/Users/elco2649/Documents/MAIAC/extracted/'

#This is the MAIAC data (eventually, loop through each day)
for csvfile in sorted(glob.glob(origpath + "*.csv")):
    basename = os.path.basename(csvfile)
    print(basename)
    csv = np.genfromtxt(csvfile, delimiter= ",", skip_header = True)
    coords = csv[:,1:3]

    #Nearest Neighbors Classification: pick k
    k = 8 # Queen Contiguity
    nbrs = NearestNeighbors(n_neighbors = k, algorithm = 'kd_tree').fit(coords, monitors)
    distances, indices = nbrs.kneighbors(monitors)
    #print("Distances = ", distances)
    #print("Indices = ", indices)

    #mont_aod = np.empty( (numObs, 3), dtype = float)
    mont_aod = []
    for i in range(0, numObs):
        #print(csv[indices[i],3])
        weights = 1 - (distances[i] - distances[i][0])/distances[i][0] #or figure out some other inverse-distance weighting system
        #print(weights)
        mont_aod.append(np.average(csv[indices[i],3], weights = weights))
    #print("AOD = ", mont_aod)

    with open(outpath + "extracted_" + basename, 'w') as dst:
        writer = writer(dst, lineterminator = '\n')
        writer.writerow(["Long", "Lat", "AOD"])
        for j in range(0, numObs):
            writer.writerow([monitors[j,0], monitors[j,1], mont_aod[j]])


#Eventually, upload to S3