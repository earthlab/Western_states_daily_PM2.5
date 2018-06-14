# -----------------------------------------------------------------
# Author: Hollie
# Date: 6/16/15
# Purpose: Calculate the average AOD values for each day using
# cell statistics in spatial analyst toolbox
# -----------------------------------------------------------------


import sys, string, os, glob, csv, time, datetime
import arcpy
#from collections import OrderedDict
from arcpy import env
from arcpy.sa import *
arcpy.env.overwriteOutput = 1


start = datetime.datetime.now()
print start
print '--------------------------'


arcpy.CheckOutExtension('GeoStats')
arcpy.CheckOutExtension('Spatial')
arcpy.env.SnapRaster = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\snapraster.gdb\\snapraster'
arcpy.env.cellSize = '4000'
arcpy.env.extent = 'MAXOF'


#inpath = 'D:\\junk\\000_gasp_error_check\\data\\step_6_hollie'
#outpath = 'D:\\junk\\000_gasp_error_check\\data\\step_7_hollie'

inpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_5'
outpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_6'


##outgdb = outpath + '\\day_avgaod.gdb'
##
##if not os.path.exists(outgdb):
##    arcpy.CreateFileGDB_management(outpath,  'day_avgaod.gdb')
##

folderlist = sorted(os.listdir(inpath))

for folder in folderlist:
    currfolder = inpath + "\\" + folder

    print currfolder

    env.workspace = currfolder

    rasterList = []
    rasterExp = '"'
    rasters = arcpy.ListRasters("*", "GRID")
    #rasters = arcpy.ListRasters("*", "TIF")

    for raster in rasters:
        rasterList.append(raster)
        rasterExp = rasterExp + currfolder + '\\' + raster + '; '

    rasterExp = rasterExp[:-2] + '"'
    #print rasterExp

    try:
        #outRaster = arcpy.sa.CellStatistics(rasterExp, 'MEAN', 'DATA')
        #outRaster.save(outpath + "\\g" + folder + "_mean")

        arcpy.gp.CellStatistics_sa(rasterExp, outpath + "\\g" + folder + "_mean", 'MEAN', 'DATA')
        #arcpy.gp.CellStatistics_sa(rasterExp, outgdb + "\\g" + folder + "_mean", 'MEAN', 'DATA')

        now = datetime.datetime.now()
        elapsed = now - start
        print 'Cell statistics calculated: ' + str(elapsed)

    except Exception, e:
        print str(e)

    print '--------------------------'


print 'Calculations complete'









