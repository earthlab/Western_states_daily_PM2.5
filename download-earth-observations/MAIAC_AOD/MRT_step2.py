#Runs MRT on each folder, through the command line
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
    if(i >= 3 and i <= 3): #change this depending on the number of observations we want
        orbits = str(i) + "_Orbits"
        folderpath = ('C:\\Users\\elco2649\\Documents\\MAIAC\\input_HDFs\\%s\\' % (orbits) )
        PRMpath = ('C:\\Users\\elco2649\\Documents\\MAIAC\\Parameters\\%d_orbits_inputLL.prm' % (i) )
        print(PRMpath)
        #subprocess.call(['cd', 'C:\\Users\\elco2649\\MRT\\bin\\'])
        subprocess.call(['java', '-jar', 'C:\\Users\\elco2649\\MRT\\bin\\MRTbatch.jar', '-d', folderpath, '-p', PRMpath, '-o', outpath])
        subprocess.call(['C:\\Users\\elco2649\\MRT\\bin\\mrtbatch.bat'])
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

for PRM in sorted(glob.glob(outpath + "MCD*")):
    shutil.move(PRM, 'C:\\Users\\elco2649\\Documents\\MAIAC\\Parameters\\')