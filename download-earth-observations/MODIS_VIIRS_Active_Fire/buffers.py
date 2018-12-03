import geopandas as gpd
import argparse
import shapefile
import pandas as pd

def _setup():
    parser = argparse.ArgumentParser(description='Pass in arguments for buffer script')
    parser.add_argument('--buffer_shp', type=str, required=True, help='buffer shp file')
    parser.add_argument('--fire_shp', type=str, required=True, help='fire shp file')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    args = _setup()

    # load in buffer layer
    buffer_sf = shapefile.Reader(args.buffer_shp)
    # load in fire layer
    fire_sf = shapefile.Reader(args.fire_shp)

    buff_df = gpd.read_file(args.buffer_shp)
    fire_df = gpd.read_file(fire_sf)

    import IPython
    IPython.embed()

    # perform a spatial join on buffer (target) and fire (source) layer only on rows where date are equal
