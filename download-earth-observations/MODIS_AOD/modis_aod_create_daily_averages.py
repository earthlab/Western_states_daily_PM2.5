# This script takes the AOD shapefile and creates multiple shapefiles split by unique date

'''


'''

import glob, os, arcpy
from arcpy import env

arcpy.CheckOutExtension("Spatial")

processed_location = 'C:\\Users\\User\Documents\hard_drive_backup\MODIS_AOD\shapefiles_102003\\'
output_data = 'C:\\Users\\User\Documents\hard_drive_backup\MODIS_AOD\shapefiles_by_day\\'
env.workspace = 'C:\Users\User\Documents\hard_drive_backup\MODIS_AOD\\'
sa_shape = 'C:\Users\User\Documents\hard_drive_backup\MODIS_AOD\western_us_boundary\western_states_merge.shp'

# Create a list of unique days
days = []

for rawfile in sorted(glob.glob(processed_location + '\\*.shp')):
    day = os.path.basename(rawfile)[0:7]
    if day not in days:
        days.append(day)


# Cycle through list of days, convert to raster and clip to study area
for day in days:
    fclist = []
    for rawfile in sorted(glob.glob(processed_location + '\\' + day + '*.shp')):
        fclist.append(rawfile)
        print(rawfile)


    # Merge all of the days together
    outshp = str(os.path.join(output_data, 'merge_' + day + '.shp'))
    print(fclist)
    arcpy.Merge_management(fclist, outshp)

# Convert points to raster
#path = "C:\Users\User\Documents\hard_drive_backup\MODIS_AOD\shapefiles_by_day\\" + '*.shp'

#for outshp in sorted(glob.glob(path)):
    #outgrd = str(os.path.join(processed_location, 'shapefiles_by_day', 'g' + day))
    outgrd = str(os.path.join('C:\Users\User\Documents\hard_drive_backup\MODIS_AOD\grids_by_day\\', 'g' + day))
    print(outshp)
    print(outgrd)
    arcpy.PointToRaster_conversion(outshp, "aod", outgrd, "MEAN", "NONE", "20000")

    # Clip raster to study area boundary with buffer
    outgrdfin = str(os.path.join('C:\Users\User\Documents\hard_drive_backup\MODIS_AOD\\', 'MODIS_full.gdb\\' + 'g' + day + '_fin'))
    arcpy.gp.ExtractByMask_sa(outgrd, sa_shape, outgrdfin)

    #arcpy.Delete_management(outshp)
    #arcpy.Delete_management(outgrd)

