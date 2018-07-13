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
import shutil
from arcpy.sa import *
import subprocess

import boto.s3.connection
from boto.s3.key import Key

# Setting up AWS S3 Connection
access_key = 'AKIAJQKU7FLSOKSHUQ4A'
secret_key = '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'

conn = boto.connect_s3(
    aws_access_key_id=access_key,
    aws_secret_access_key=secret_key
)

s3Bucketname = "earthlab-reid-group"


arcpy.CheckOutExtension('GeoStats')
arcpy.CheckOutExtension('Spatial')
arcpy.env.SnapRaster = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\snapraster.gdb\\snapraster'
arcpy.env.cellSize = '4000'
arcpy.env.extent = 'MAXOF'

def upload_to_AWS(subdir, file):
    bucket = conn.get_bucket(s3Bucketname)
    k = Key(bucket)
    k.key = subdir + os.path.basename(file)
    k.set_contents_from_filename(file)  # rewind = True if from file


def six(folder, origpath, outpath, subdir, start):
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
        # outRaster = arcpy.sa.CellStatistics(rasterExp, 'MEAN', 'DATA')
        # outRaster.save(outpath + "\\g" + folder + "_mean")

        arcpy.gp.CellStatistics_sa(rasterExp, outpath + "\\g" + folder + "_mean", 'MEAN', 'DATA')
        # arcpy.gp.CellStatistics_sa(rasterExp, outgdb + "\\g" + folder + "_mean", 'MEAN', 'DATA')

        now = datetime.datetime.now()
        elapsed = now - start
        print 'Cell statistics calculated: ' + str(elapsed)

    except Exception, e:
        print str(e)

    # #Not working -- do this manually later
    # destpath = 's3://earthlab-reid-group/' + subdir + folder + '//'
    # subprocess(['aws', 's3', 'mv', currfolder, destpath, '--recursive'])

    print '--------------------------'
    #shutil.move(folder, "D:\\Western_US\\GASP\\step_5\\")

# if __name__ == '__main__':
#
#     start = datetime.datetime.now()
#     print start
#     print '--------------------------'
#
#     pool = multiprocessing.Pool()
#     folderlist = sorted(os.listdir(inpath))
#     for folder in folderlist:
#         pool.apply_async(six, [folder])
#
#     pool.close()
#     pool.join()
#
#     print('Calculations complete')










