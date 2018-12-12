'''
This script computes zonal statistics between a buffer shp file and an classified raster tif (in our
use case, a reclassified NLCD raster) The output is percent area of developed high density land cover in each 
buffer. The output is another csv, which is the input csv with an an extra column denoting the data.

Run this script with Python 3. You will need to install the rasterstats library first.
'''

import argparse
import pandas as pd
from rasterstats import zonal_stats

def _setup():
    parser = argparse.ArgumentParser(description='Pass in arguments for extracting NED values script')
    parser.add_argument('--buffer_shp', type=str, required=True, help='input csv file with station coordinates')
    parser.add_argument('--nlcd_raster_tif', type=str, required=True, help='input NLCD raster file')
    parser.add_argument('--input_csv_file', type=str, required=True, help='input csv file with PM25 station locations and dates')
    parser.add_argument('--output_csv_file', type=str, required=True, help='name of ouput csv file to create, which will look like the input file but with the data appended to it')
    args = parser.parse_args()
    return args


if __name__ == "__main__":
    args = _setup()

    # for each buffer point, determine percentage of dense urban pixels
    cmap = {1: 'developed high intensity', 2: 'vegetation', 3: 'agricultural', 4: 'other'}
    nlcd_stats = zonal_stats(args.buffer_shp, args.nlcd_raster_tif, categorical=True, category_map=cmap)
    perc_urban_list = []

    # set values to 0 where they don't exist
    # this step is necessary b/c rasterstats omits the kv pair instead of reporting 0,
    # which is an issue later when calculating percent urban area
    for x in nlcd_stats:
        try:
            x['developed high intensity'] = x['developed high intensity']
        except:
            x['developed high intensity'] = 0
    for x in nlcd_stats:
        try:
            x['vegetation'] = x['vegetation']
        except:
            x['vegetation'] = 0
    for x in nlcd_stats:
        try:
            x['agricultural'] = x['agricultural']
        except:
            x['agricultural'] = 0
    for x in nlcd_stats:
        try:
            x['other'] = x['other']
        except:
            x['other'] = 0


    for x in nlcd_stats:
        x['sum'] = x['developed high intensity'] + x['vegetation'] + x['agricultural'] + x['other']
        x['perc_urban'] = x['developed high intensity']/x['sum']
        perc_urban_list.append(x['perc_urban'])

    
    perc_urban_list_round = [round(x, 4) for x in perc_urban_list]
    
    df = pd.read_csv(args.input_csv_file)
    df['percent_urban_buffer'] = perc_urban_list_round
    df.to_csv(args.output_csv_file, index=False)



