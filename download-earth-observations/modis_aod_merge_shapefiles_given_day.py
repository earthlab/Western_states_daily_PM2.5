# This script merges .shp files that occur on the same day into a single .shp file

'''
Prerequisites:
1) Download MODIS AOD data from NASA
2) Obtain .csv files containing AOD data (by running script `modis_aod_create_csv_file.py`)
3) Convert .csv files to .shp files (by running script `modis_aod_convert_csv_to_shapefile.py`)

Objective:
This script loops through each .shp file and combines the files that contain data from the same day
(detailed in file name)

Results: .shp files that contain all combined AOD data for a given day.

Note: This script was inspired by Zev Ross's code which was adapted from Phil Moorefield's code.

'''

import csv, shapefile, glob
from os import path

__credits__ = "Gina Li, Colleen Reid, Melissa Maestas, Ellen Considine"
__email__ = "gina.li@colorado.edu"


processed_data = 'H:\MODIS_AOD\processed_data\shapefiles\\'
output_location = 'H:\MODIS_AOD\processed_data\shapefiles_merged_by_date\\'

for file in sorted(glob.glob(processed_data + "\\*.shp")):
