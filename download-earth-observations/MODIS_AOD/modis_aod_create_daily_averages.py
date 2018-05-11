# This script creates merges .shp files with the same date, performs Point to Raster with a 12km cell size resolution,
# and then clips the grids to the study area (buffered by 25 km).

'''
Prerequisites:
1) Download MODIS AOD data from NASA via FTP (MOD04_L2 and MYD04_L2)
2) Create .csv files containing lat/long and AOD values (by running modis_aod_create_csv_file.py script)
3) Convert .csv files to .shp files (by running modis_aod_convert_csv_to_shapefile.py script)
4) Reproject .shp files from WGS 84 to USA Contiguous Albers Equal Area Conic (by running modis_aod_project_to_albers.py script)

Objective:
Combine all .shp files with the same observation date from 2008 to 2014 and then produce a raster for
merged .shp file at a 12km resolution. Then, the interpolated grids are clipped to the boundaries of our study area,
which are the 11 western US states buffered by 100 km.

To Run:
1) Adjust the processed_data, output_path, env.workspace, and sa_shape to reflect your local directories
2) Run script with the correct Python interpreter that arcpy is installed on (the Python 2.7 that comes with ArcGIS)

Output:
One grid per day (for each date between 2008 and 2014) clipped to the study area extent containing the average daily
AOD value within each 12km cell.

'''

import glob, os, arcpy
import concurrent.futures
from arcpy import env

arcpy.env.overwriteOutput = 1
arcpy.CheckOutExtension("Spatial")

# processed_data refers to the path where the USA Contiguous Albers Equal Area Conic projected .shp files are located
processed_data = 'C:\\Users\\User\Documents\hard_drive_backup\MODIS_AOD\shapefiles_102003\\'
# output_path refers to the path where the daily merged .shp files will be output
output_path = 'C:\\Users\\User\Documents\hard_drive_backup\MODIS_AOD\shapefiles_by_day\\'
# env.workspace sets the workspace for the arcpy environment
env.workspace = 'C:\Users\User\Documents\hard_drive_backup\MODIS_AOD\\'
# sa_shape is the study area boundary .shp file (western states + 25km buffer)
sa_shape = 'C:\Users\User\Documents\hard_drive_backup\MODIS_AOD\western_us_boundary\western_states_merge_25km_buffer.shp'


# Create a list of unique days
days = []

# for each of the daily merged .shp files
for rawfile in sorted(glob.glob(processed_data + '\\*.shp')):
    # get the date from the file name (i.e. "2008001")
    day = os.path.basename(rawfile)[0:7]
    # add to days array if not already present
    if day not in days:
        days.append(day)


# Cycle through list of days, convert to raster and clip to study area
for day in days:
    fclist = []
    for rawfile in sorted(glob.glob(processed_data + '\\' + day + '*.shp')):
        fclist.append(rawfile)
        print(rawfile)


    # Merge all of the days together
    outshp = str(os.path.join(output_path, 'merge_' + day + '.shp'))
    #print(fclist)
    arcpy.Merge_management(fclist, outshp)

    # Convert points to raster
    outgrd = str(os.path.join('C:\Users\User\Documents\hard_drive_backup\MODIS_AOD\grids_by_day\\', 'g' + day))
    print(outshp)
    print(outgrd)
    arcpy.PointToRaster_conversion(outshp, "aod", outgrd, "MEAN", "", 12000)

    # Clip raster to study area boundary with buffer
    outgrdfin = str(
        os.path.join('C:\Users\User\Documents\hard_drive_backup\MODIS_AOD\\final_grids_by_day\\', 'g' + day + '_fin.tif'))
    arcpy.gp.ExtractByMask_sa(outgrd, sa_shape, outgrdfin)

    #arcpy.Delete_management(outshp)
    #arcpy.Delete_management(outgrd)
