import argparse
import ulmo
import os

parser = argparse.ArgumentParser(description='Set up script with imiportant variables')
parser.add_argument('--outpath', type=str, required=True, help='name of VRT file to create')
args = parser.parse_args() 

logf = open(os.path.join(args.outpath, "ned_download_errors.log"), "w")

try:
    ulmo.usgs.ned.get_raster('1/3 arc-second', bbox=[-101.236910, 34.110220, -99.199683, 35.847546], path=args.outpath, mosaic=True)
except Exception as e:
    logf.write(str(e) + "\n")