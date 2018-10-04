# Add a date column for each shp file, and adjsut UTC to local time zone

import glob
import geopandas as gpd
import os
import argparse
import pytz
import datetime

def setup_args():
    parser = argparse.ArgumentParser(description='arguments for processing date for HMS Smoke data')
    parser.add_argument('--shp_directory', type=str, required=True,
                        help='directory containing daily shp files')
    parser.add_argument('--timezone_json_file', type=str, required=True,
                        help='Timezone geojson file')
    parser.add_argument('--output_filepath', type=str, required=True, 
                        help='full path of output')
    args = parser.parse_args()
    return args

def spatial_join(point_shp, poly_json_file):
    orig_shp_gdf = gpd.read_file(point_shp)
    orig_shp_gdf.crs = {'init': 'esri:102003'}

    # Read polygons from json file to geopandas df
    poly_gdf = gpd.read_file(poly_json_file)
    poly_gdf.crs = {'init': 'esri:102003'}
    combined_gdf = gpd.tools.sjoin(orig_shp_gdf, poly_gdf)
    return combined_gdf

def adjust_datetime(dt, timezone_str):
        timezone = pytz.timezone(timezone_str)
        adjusted_dt = pytz.timezone('UTC').localize(datetime.datetime(
            dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second)).astimezone(timezone)
        return adjusted_dt

def main():
    args = setup_args()
    # add date as a column to each shp file

    for shp_file in glob.glob(args.shp_directory + "/*.shp"):
        print(shp_file)
        combined_gdf = spatial_join(shp_file, args.timezone_json_file)
        
        if combined_gdf.shape[0] > 0:
            
            adj_dt_list = []
            for index, row in combined_gdf.iterrows():
                combined_gdf.is_copy = False
                dt_str = os.path.basename(shp_file)[9:].split(".")[0]
                start = row['Start']
                if len(row['Start']) > 4:
                    start = row['Start'][-4:]
                dt_str = dt_str + start
                dt = datetime.datetime.strptime(dt_str, '%Y%m%d%H%M')
                adj_dt = adjust_datetime(dt, row['tzid'])
                adj_dt_list.append(adj_dt.strftime('%Y-%m-%d %H%M'))
                
            combined_gdf['adj_time'] = adj_dt_list
            if combined_gdf.shape[0] == 1 and 'prev_gdf' not in locals():
                prev_gdf = combined_gdf
            elif 'prev_gdf' in locals():
                prev_gdf = prev_gdf.append(combined_gdf)

    prev_gdf.to_file(args.output_filepath, driver='ESRI Shapefile')
            
            
            

    '''    
    adj_dt_list = []
    for index, row in subset.iterrows():
        dt_str = row['ACQ_DATE'] + row['ACQ_TIME']
        dt = datetime.datetime.strptime(dt_str, '%Y-%m-%d%H%M')
        adj_dt = adjust_datetime(dt, row['tzid'])
        adj_dt_list.append(adj_dt.strftime('%Y-%m-%d %H%M'))
    subset.is_copy = False
    subset['adj_time'] = adj_dt_list
    subset.to_file(args.output_filepath, driver='ESRI Shapefile')
    '''

if __name__ == "__main__":
    main()