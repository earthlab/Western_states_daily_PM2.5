import argparse
import ulmo
import os

parser = argparse.ArgumentParser(description='Set up script with imiportant variables')
parser.add_argument('--outpath', type=str, required=True, help='name of VRT file to create')
args = parser.parse_args() 

logf = open(os.path.join(args.outpath, "ned_download_errors.log"), "w")

try:
    ulmo.usgs.ned.get_raster('1 arc-second', bbox=[-129.226343, 31.042818, -99.589304, 49.710378], path=args.outpath, mosaic=True)
except Exception as e:
    logf.write(str(e) + "\n")
