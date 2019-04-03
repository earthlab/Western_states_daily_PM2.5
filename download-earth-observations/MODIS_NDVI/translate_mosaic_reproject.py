# This script translates HDF files to TIF files, mosaics the TIF files from the same day, and finally reprojects the TIF files from sinusoidal to albers equal area conic

import argparse
import glob
from osgeo import gdal
import os
from datetime import timedelta, date

def _setup():
    parser = argparse.ArgumentParser(description='Pass in arguments for mosaic same day script')
    parser.add_argument('--hdf_directory', type=str, required=True, help='directory with original hdf files')
    args = parser.parse_args()
    return args

def _daterange(start_date, end_date):
    for n in range(int ((end_date - start_date).days)):
        yield start_date + timedelta(n)

if __name__ == "__main__":
    
    args = _setup()
    
    
    # Translate
    for filename in glob.glob(args.hdf_directory + "*.hdf"):
        dataset = gdal.Open(filename)
        # get the first subdataset, which is the 1km monthly NDVI SDS
        ndvi_SDS = dataset.GetSubDatasets()[0][0]
        print(ndvi_SDS)
        ndvi_tif = ".".join(ndvi_SDS.split('"')[1].split(".")[:-1]) + ".tif.tif"
        # translate it from HDF to TIF
        gdal_translate_str = 'gdal_translate -of GTiff "' + ndvi_SDS + '" ' + ndvi_tif
        print(gdal_translate_str)
        os.system(gdal_translate_str)
    
    
    # Mosaic
    start_date = date(2018, 9, 1)
    end_date = date(2018, 12, 31)
    for single_date in _daterange(start_date, end_date):
        print("searching for files on " + single_date.strftime("%Y%j"))
        same_dates = []
        for filename in glob.glob(args.hdf_directory + "*.tif"):
            file_date = filename.split(".")[1][1:]
            if file_date == single_date.strftime("%Y%j"):
                same_dates.append(filename)
        if len(same_dates) > 0:
            # mosaic
            print(same_dates)
            # built gdal vrt
            vrt_filename = ".".join(same_dates[0].split(".")[:-5]) + ".mosaic.vrt.vrt"
            gdal.BuildVRT(vrt_filename, same_dates)

            # convert vrt to tif
            tif_mosaic_filename = ".".join(vrt_filename.split(".")[:-3]) + ".mosaic.tif.tif"
            translate_str = ' '.join(['gdal_translate', vrt_filename, tif_mosaic_filename])
            print(translate_str)
            os.system(translate_str)
    

    # Reproject
    for filename in glob.glob(args.hdf_directory + "*.mosaic.tif*"):
        reproj_tif_mosaic_filename = ".".join(filename.split(".")[:-2]) + ".reproj.tif.tif"
        gdal_warp_str = 'gdalwarp -s_srs "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" -t_srs EPSG:102003 ' + filename + ' ' + reproj_tif_mosaic_filename
        print(gdal_warp_str)
        os.system(gdal_warp_str)


    
