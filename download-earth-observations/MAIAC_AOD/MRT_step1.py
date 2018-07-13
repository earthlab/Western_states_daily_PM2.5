#Sorts hdf files with regard to number of orbits

import os, glob
from pyhdf.SD import SD
from numpy import *
import shutil

origpath = 'C:\\Users\\elco2649\\Documents\\MAIAC_DATA\\'
outpath = 'C:\\Users\\elco2649\\Documents\\MAIAC\\input_HDFs\\'
#origpath = '/home/jovyan/MAIAC/collected_data/'
#outpath = '/home/jovyan/MAIAC/processed_data/TIFs/'

for rawfile in sorted(glob.glob(origpath + "\\*.hdf")):
    print(rawfile)
    # if the .hdf file is not empty, proceed
    if not os.stat(rawfile).st_size == 0:
        # Create an SD object from the .hdf file
        hdffile = SD(rawfile)
        sds = hdffile.select("Optical_Depth_047")[:]
        dims = sds.shape
        norbits = dims[0]
        print("Orbits = " + str(norbits))

        try:
            shutil.move(rawfile, outpath + str(norbits) + "_Orbits\\")
        except(e):
            print(e)


