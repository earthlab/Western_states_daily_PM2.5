#Author: Ellen
#Date: 7/17/18
#Purpose: Average the TIFs for each day to create an average AOD raster (this uses nearly identical code to the GASP step 6)

import os, datetime
import arcpy
from arcpy import env

arcpy.CheckOutExtension('GeoStats')
arcpy.CheckOutExtension('Spatial')
arcpy.env.SnapRaster = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\snapraster.gdb\\snapraster'
arcpy.env.cellSize = '4000'
arcpy.env.extent = 'MAXOF'

origpath = 'C:\Users\elco2649\Documents\MAIAC\output_TIFs\\'
outpath = 'C:\\Users\\elco2649\\Documents\\MAIAC\\avg_rasters\\'

start = datetime.datetime.now()

folderlist = sorted(os.listdir(origpath))
for folder in folderlist:
    currfolder = origpath + folder + '\\'
    print currfolder
    env.workspace = currfolder

    rasterList = []
    rasterExp = '"'
    rasters = arcpy.ListRasters("*", "GRID")
    rasters = arcpy.ListRasters("*", "TIF")

    for raster in rasters:
        rasterList.append(raster)
        rasterExp = rasterExp + currfolder + '\\' + raster + '; '

    rasterExp = rasterExp[:-2] + '"'
    # print rasterExp

    try:
        #Calculate mean AOD value
        arcpy.gp.CellStatistics_sa(rasterExp, outpath + "\\g" + folder + "_mean", 'MEAN', 'DATA')

        now = datetime.datetime.now()
        elapsed = now - start
        print 'Cell statistics calculated: ' + str(elapsed)

    except Exception, e:
        print str(e)