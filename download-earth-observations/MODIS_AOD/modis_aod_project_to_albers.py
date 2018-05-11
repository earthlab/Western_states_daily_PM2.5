# This script converts each .shp file from WGS 84 (EPSG:4326) to USA Contiguous Abers Equal Area Conic (ESRI:102003)

'''
Prerequisites:
1) Download MODIS AOD data from NASA via FTP (MOD04_L2 and MYD04_L2)
2) Create .csv files containing lat/long and AOD values (by running modis_aod_create_csv_file.py script)
3) Convert .csv files to .shp files (by running modis_aod_convert_csv_to_shapefile.py script)

Objective:
Read in each .shp file (which has WGS 84 projection) and reproject to USA Contiguous Albers Equal Area Conic projection (ESRI:102003).
One output .shp file for every input .shp file. Use multiprocessing to utilize all cores and make for faster processing.

To Run:
1) Adjust the processed_data and output_path to reflect your local directories
2) Run script with the correct Python interpreter that arcpy is installed on (the Python 2.7 that comes with ArcGIS)

Output:
A reprojected .shp file for each input .shp file

Additional Notes/Resources:
1) Understand multiprocess implemetation:
https://medium.com/@ageitgey/quick-tip-speed-up-your-python-data-processing-scripts-with-process-pools-cf275350163a
'''

from utilities import getWKT_PRJ
import arcpy
import concurrent.futures

# processed_data refers to the path where the WGS84 .shp files are located
processed_data = 'C:\\Users\\User\Documents\hard_drive_backup\MODIS_AOD\\shapefiles_4326\\'
# output_path refers to the path where the reprojected USA Contiguous Albers Equal Area Conic .shp files will be output
output_path = 'C:\\Users\\User\Documents\hard_drive_backup\MODIS_AOD\shapefiles_102003\\'

prj_info = getWKT_PRJ("esri", "102003")

arcpy.env.workspace = processed_data

# function that reprojects an ESRI feature class (.shp file, in this case)
# returns the projected feature class (i.e. "2007001_1745t_prj.shp")
def projectShapefiles(fc):
    fcprj_path = output_path
    fcprj = fcprj_path + fc[:-4] + '_prj.shp'
    arcpy.Project_management(fc, fcprj, prj_info)
    return(fcprj)

if __name__ == '__main__':
    with concurrent.futures.ProcessPoolExecutor() as executor:
        # Get list of ESRI feature classes (.shp files) to process
        filelist = arcpy.ListFeatureClasses()

        # Process list of .shp files, but split the work across the process pool to use all CPUs
        for fc in zip(filelist, executor.map(projectShapefiles, filelist)):
            print("a shapefile was projected")

