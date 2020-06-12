#Author: Ellen Considine
#Date: 8/7/18
#Purpose: convert an average-value shapefile (for each day) to a raster

#import packages
import geopandas as gpd
#from osgeo import gdal
import subprocess
import os, glob
import multiprocessing
import datetime

import boto.s3.connection
from boto.s3.key import Key

#Copy from AWS, via the command line

# aws s3 cp s3://earthlab-reid-group/GASP_processed/2008/step4a/avg/ /home/jovyan/GASP_SHP/ --recursive
# aws s3 cp s3://earthlab-reid-group/GASP_processed/2009/step4a/avg/ /home/jovyan/GASP_SHP/ --recursive
# aws s3 cp s3://earthlab-reid-group/GASP_processed/NewBatch2/step4a/avg/ /home/jovyan/GASP_SHP/ --recursive

# Setting up AWS S3 Connection
access_key = ' '
secret_key = ' '

conn = boto.connect_s3(
    aws_access_key_id=access_key,
    aws_secret_access_key=secret_key
)

s3Bucketname = "earthlab-reid-group"


def upload_to_AWS(subdir, file):
    bucket = conn.get_bucket(s3Bucketname)
    k = Key(bucket)
    k.key = subdir + os.path.basename(file)
    k.set_contents_from_filename(file)  # rewind = True if from file

#Settings
origpath = '/home/jovyan/GASP_SHP/'
outpath = '/home/jovyan/GASP_tif/'
subdir = 'GASP_processed/projected_shp/'

def rasterize(item): #item will be a shapefile
    #read in shapefile
    SHP = gpd.read_file(item)
    basename = os.path.basename(item)
    print(basename)
    #reproject shapefile
    new_SHP = SHP.to_crs({'init':'esri:102003'})
    proj_str= 'PROJCS["USA_Contiguous_Albers_Equal_Area_Conic",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Albers"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["central_meridian",-96],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["latitude_of_origin",37.5],UNIT["Meter",1]]'
    #write reprojected shapefile
    intermediate = outpath + "new_" + basename
    new_SHP.to_file(intermediate, driver = 'ESRI Shapefile', crs_wkt = proj_str)
    #write raster
    outfile = outpath + basename[:-4] + ".tif"
    subprocess.call(['gdal_grid', '-zfield', 'aod', '-a', 'linear', '-txe', '-2380056.81286844', '-480056.81286844425', '-tye', '-638166.9912686478', '1581833.0087313522', '-outsize', '555', '475', '-of', 'GTiff', '-ot', 'Float64', intermediate, outfile])
    
    dbf = intermediate[:-4] + ".dbf"
    cpg = intermediate[:-4] + ".cpg"
    prj = intermediate[:-4] + ".prj"
    shx = intermediate[:-4] + ".shx"
    
    upload_to_AWS(subdir, intermediate)
    upload_to_AWS(subdir, dbf)
    upload_to_AWS(subdir, cpg)
    upload_to_AWS(subdir, prj)
    upload_to_AWS(subdir, shx)
    
    os.remove(intermediate)
    os.remove(dbf)
    os.remove(cpg)
    os.remove(prj)
    os.remove(shx)
    #print("Rasterized")
    
    
def main():
    
    #print(datetime.datetime.now())
    
    #implement multiprocessing:  
    pool = multiprocessing.Pool()

    for file in sorted(glob.glob(origpath + '*.shp')):
        pool.apply_async(rasterize, [file])

    pool.close()
    pool.join()
    
    #print(datetime.datetime.now())


if __name__ == "__main__":
    main()

    
#Then move tifs to AWS, via the command line:
#aws s3 mv /home/jovyan/GASP_tif/ s3://earthlab-reid-group/GASP_processed/rasters/ --recursive --exclude "*" --include "*.tif"

#Then move reprojected shps to AWS, via the command line:
#aws s3 mv /home/jovyan/GASP_tif/ s3://earthlab-reid-group/GASP_processed/projected_shp/ --recursive 

