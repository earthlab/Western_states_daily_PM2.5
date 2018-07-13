
# -----------------------------------------------------------------
# Author: Ellen
# Date: 5/17/18
# Purpose: This is an adaptation of Zev's script, which is an adaptation of Phil's original GASP script
# (client_data\20141217_gasp_modis_scripts\GASP_scripts\gasp_
# process_step1.py). The script cycles through the original
# files (downloaded from FTP) and creates a copy of each
# file (lat, lon and aod) with simplified name.
# -----------------------------------------------------------------
import gzip
import shutil
import glob, os


#This is actually encountering some errors -- PyCharm overheating?

#Instead use:
#"C:\Program Files\7-Zip\7z.exe" e C:\Users\elco2649\Documents\GASP_AOD\original_compressed_1\*.gz -oC:\Users\elco2649\Documents\GASP_DATA\
#On the command line (Windows)

#I separated the compressed data into 6 subfolders to be able to see where it breaks, if it breaks...

def zero(origpath, outpath, item):
    item = os.path.basename(item)
    #while i < 10:
    with gzip.open(origpath + item, 'rb') as f_in:
         with open(outpath + item[:-3], 'wb') as f_out:
            shutil.copyfileobj(f_in, f_out)
            #i = i + 1
         f_out.close()
    f_in.close()
    f_out.close()
    return(f_out)
    #break

    print("Unzipped.")