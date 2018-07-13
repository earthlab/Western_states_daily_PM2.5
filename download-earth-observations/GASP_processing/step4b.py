# -----------------------------------------------------------------
# Author: Hollie
# Date: 4/23/15
# Purpose: Project each of the shps in step 4a to
# USA_Contiguous_Albers_Equal_Area_Conic projection.
# -----------------------------------------------------------------


import arcpy as AP
import os, glob
from os import path
from arcpy import env
import shutil
import multiprocessing
AP.env.overwriteOutput = 1
AP.CheckOutExtension("Spatial")

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


#epsg = 'PROJCS["NAD_1983_California_Teale_Albers",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Albers"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",-4000000.0],PARAMETER["Central_Meridian",-120.0],PARAMETER["Standard_Parallel_1",34.0],PARAMETER["Standard_Parallel_2",40.5],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0],AUTHORITY["EPSG",3310]]'
epsg = 'PROJCS["USA_Contiguous_Albers_Equal_Area_Conic",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Albers"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["central_meridian",-96],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["latitude_of_origin",37.5],UNIT["Meter",1],AUTHORITY["EPSG","102003"]]'


def upload_to_AWS(subdir, file):
    bucket = conn.get_bucket(s3Bucketname)
    k = Key(bucket)
    k.key = subdir + os.path.basename(file)
    k.set_contents_from_filename(file)  # rewind = True if from file


def fourB(origpath, outpath, fc):
    print(path.basename(fc))
    fcprj = path.join(outpath, path.basename(fc).replace('.shp', '_prj.shp'))
    AP.Project_management(fc, fcprj, epsg)
    os.remove(fc)
    #shutil.move(fc, "D:\\Western_US\\GASP\\step_4a\\")



