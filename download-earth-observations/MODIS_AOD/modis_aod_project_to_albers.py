# This script converts shapefiles to US Abers Equal Area Conic Version

'''


'''

from utilities import getWKT_PRJ
import arcpy
import concurrent.futures


processed_data = 'C:\\Users\\User\Documents\hard_drive_backup\MODIS_AOD\\shapefiles_4326\\'
output_data = 'C:\\Users\\User\Documents\hard_drive_backup\MODIS_AOD\shapefiles_102003\\'

#prj_info = getWKT_PRJ("esri", "102003")
prj_info = "PROJCS['USA_Contiguous_Albers_Equal_Area_Conic',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-96.0],PARAMETER['Standard_Parallel_1',29.5],PARAMETER['Standard_Parallel_2',45.5],PARAMETER['Latitude_Of_Origin',37.5],UNIT['Meter',1.0]]"

arcpy.env.workspace = processed_data

def projectShapefiles(fc):
    #fcprj_path = main_path + 'shapefiles_102003\\'
    fcprj_path = output_data
    fcprj = fcprj_path + fc[:-4] + '_prj.shp'
    arcpy.Project_management(fc, fcprj, prj_info)
    return(fcprj)

if __name__ == '__main__':
    with concurrent.futures.ProcessPoolExecutor() as executor:
        # Get list of .csv files to process
        filelist = arcpy.ListFeatureClasses()

        # Process list of .csv files, but split the work across the process pool to use all CPUs
        for fc in zip(filelist, executor.map(projectShapefiles, filelist)):
            print("a shapefile was projected")

