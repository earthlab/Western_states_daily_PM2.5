import os
import argparse
from osgeo import gdal
import glob

parser = argparse.ArgumentParser(description='set up script with important file name variables')
parser.add_argument('--vrt_filename', type=str, required=True, help='name of VRT file to create')
parser.add_argument('--directory', type=str, required=True, help='directory with NED tiles')
parser.add_argument('--tif_filename', type=str, required=True, help='name of TIF file to create')
args = parser.parse_args()                           

# built gdal vrt
path = args.directory + '*.img'
gdal.BuildVRT(args.vrt_filename, glob.glob(path))

# convert vrt to tif
translate_str = ' '.join(['gdal_translate', args.vrt_filename, args.tif_filename])
print(translate_str)
os.system(translate_str)
