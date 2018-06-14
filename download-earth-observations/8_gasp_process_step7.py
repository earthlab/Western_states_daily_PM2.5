# -----------------------------------------------------------------
# Author: Hollie
# Date: 6/23/15
# Purpose: Cycle through final folder of GASP rasters
# (each raster is average AOD values for 1 day) and
# extract AOD values to monitors.
# -----------------------------------------------------------------


import sys, re, os, glob, csv, time, datetime
import arcpy
from arcpy import env
from arcpy.sa import *
arcpy.env.overwriteOutput = 1


start = datetime.datetime.now()
print start
print '--------------------------'


arcpy.CheckOutExtension('Spatial')


inpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_6'
outpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\Output'


monitors = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\gis\\correct_monitors.shp'


env.workspace = inpath
rasters = arcpy.ListRasters("*", "GRID")



for raster in sorted(rasters):
    print raster

    day = str(re.search(r'\d+', raster).group())
    outPts = outpath + '\\day' + day + '_aod.shp'

    try:
        ExtractValuesToPoints(monitors, raster, outPts, '', 'VALUE_ONLY')
        arcpy.AddField_management(outPts, 'day', 'LONG')
        arcpy.CalculateField_management(outPts, 'day', day)


    except Exception, e:
        print str(e)

    print '--------------------------'


print 'Calculations complete'









