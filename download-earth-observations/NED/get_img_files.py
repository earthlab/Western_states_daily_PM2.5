import os
import glob
import shutil

directory = 'C:\\Users\\ginal\\Documents\\EarthLab\\NED_uget\\'
output_directory = 'C:\\Users\\ginal\\Documents\\EarthLab\\NED_uget\\img_files\\'
ned_directories = [x[0] for x in os.walk(directory)]

count = 0
for folder in ned_directories:
    count += 1
    if count%2 == 0:
        for img_file in glob.glob(folder + '\\*.img'):
            shutil.copy(img_file, output_directory)
