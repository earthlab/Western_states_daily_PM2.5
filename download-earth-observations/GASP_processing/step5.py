# -----------------------------------------------------------------
# Author: Hollie
# Date: 6/16/15
# Purpose: For each projected set of points see if the extent is
# the same or greater than California. If it is create a geostatistical
# layer using the model parameters from Phil (GASP_RBF.xml). Lastly
# clip the new geostat layer to CA boundary
# -----------------------------------------------------------------


import sys, string, os, glob, csv, time, datetime
import arcpy
from collections import OrderedDict
from arcpy import env
import shutil
from arcpy.sa import *
import re
import multiprocessing
arcpy.env.overwriteOutput = 1

arcpy.CheckOutExtension('GeoStats')
arcpy.CheckOutExtension('Spatial')
arcpy.env.SnapRaster = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\snapraster.gdb\\snapraster'

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

sabuf = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\gis\\study-area-buffer_25km.shp'
xmlfile = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\gis\\GASP_RBF.xml'

def upload_to_AWS(subdir, file):
    bucket = conn.get_bucket(s3Bucketname)
    k = Key(bucket)
    k.key = subdir + os.path.basename(file)
    k.set_contents_from_filename(file)  # rewind = True if from file


def five(origpath, outpath, fc, day, start, subdir):

    fcname = os.path.basename(fc)
    timesliceA = str(fcname)
    # re.split(r"[\[\]]", "I need to [go out] to lunch")
    timesliceB = re.split(r"[_]", timesliceA)
    #timeslice = int(timesliceB[2])
    # print(timeslice)
    year = timesliceA[2:4]
    print 'year: ' + year


    #env.workspace = fc

    # Calculate total number of points in fc
    totPts = int(arcpy.GetCount_management(fc).getOutput(0))

    #Create layer
    arcpy.MakeFeatureLayer_management(fc, 'lyr')
    arcpy.SelectLayerByLocation_management('lyr', 'intersect', sabuf)
    print ( int(arcpy.GetCount_management('lyr').getOutput(0)) )

    #Save outside memory
    tempPts = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\gis\\junk\\temp_' + fcname
    arcpy.CopyFeatures_management('lyr', tempPts)
    arcpy.Delete_management('lyr')

    # Set the final variables
    gadir = outpath + year + "_" + day
    ganame = 'g' + str(fcname)[2:-4]
    galyr = ganame #'lyr_' +
    gafin = ganame[:-4]  # + '_ext'
    print(gafin)

    try:
        os.mkdir(gadir)
        print "makedir"

    except Exception:
        pass

    try:
        # arcpy.GACreateGeostatisticalLayer_ga(xmlfile, str(fc) + " AOD", galyr)
        arcpy.GACreateGeostatisticalLayer_ga(xmlfile, tempPts + ' AOD', galyr)

        now = datetime.datetime.now()
        elapsed = now - start
        print 'Creation of galayer: ' + str(elapsed)

        arcpy.GALayerToGrid_ga(galyr, gadir + '\\' + gafin, '4000', '', '')
        now = datetime.datetime.now()
        elapsed = now - start
        print 'Saving of galayer: ' + str(elapsed) + '\n --------------------------'

        upload_to_AWS(subdir, fc)
        os.remove(fc)
        # arcpy.Delete_management(tempPts)

    except Exception, e:
        print str(e)



