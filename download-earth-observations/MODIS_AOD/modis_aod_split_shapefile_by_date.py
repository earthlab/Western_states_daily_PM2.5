# This script takes the AOD shapefile and creates multiple shapefiles split by unique date

'''


'''

# TODO: get access to AGOL to use ArcGIS Pro SplitByAttribute function

import arcpy

in_feature_class = 'H:\MODIS_AOD\processed_data\aod_all\aod_all.shp'
target_workspace = 'H:\MODIS_AOD\processed_data\aod_by_date'
fields = ['ts_local']

arcpy.Split