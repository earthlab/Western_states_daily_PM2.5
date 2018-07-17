#Author: Ellen Considine
#Date: 7/17/18
#Purpose: This script runs the MRT on each folder (with different numbers of orbits), through the MRT command line interface
import os, glob, shutil
import subprocess

origpath = 'C:\\Users\\elco2649\\Documents\\MAIAC\\input_HDFs\\'
outpath = 'C:\\Users\\elco2649\\Documents\\MAIAC\\output_TIFs\\'
#origpath = '/home/jovyan/MAIAC/collected_data/'
#outpath = '/home/jovyan/MAIAC/processed_data/TIFs/'

folderlist = sorted(glob.glob(origpath + "*Orbits"))

i = 1
for folder in folderlist:
    n = 1
    if(i >= 3 and i <= 3): #change this depending on the number of daily observations we want
        orbits = str(i) + "_Orbits"
        folderpath = ('C:\\Users\\elco2649\\Documents\\MAIAC\\input_HDFs\\%s\\' % (orbits) )
        PRMpath = ('C:\\Users\\elco2649\\Documents\\MAIAC\\Parameters\\%d_orbits_inputLL.prm' % (i) )
        print(PRMpath)

        #Call MRT through the command line
        subprocess.call(['java', '-jar', 'C:\\Users\\elco2649\\MRT\\bin\\MRTbatch.jar', '-d', folderpath, '-p', PRMpath, '-o', outpath])
        subprocess.call(['C:\\Users\\elco2649\\MRT\\bin\\mrtbatch.bat'])

        #Move files to the appropriate folder so they are not overwritten
        for file in sorted(glob.glob(outpath + "*tif")):
            base = os.path.basename(file)[:16]
            newname = base + "_" + str(i) + "_" + str(n) + ".tif"
            os.rename(file, outpath + newname)
            dir = outpath + base[-7:] + '\\'
            try:
                os.mkdir(dir)
            except:
                pass # directory already exists
            shutil.move(outpath + newname, dir + newname)
            n = n + 1
    i = i + 1

#Move parameter files so they do not clutter the data space -- we shouldn't need these for anything later
for PRM in sorted(glob.glob(outpath + "MCD*")):
    shutil.move(PRM, 'C:\\Users\\elco2649\\Documents\\MAIAC\\Parameters\\days\\')