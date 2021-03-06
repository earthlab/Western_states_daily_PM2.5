### Author: Gina Li
### Edited by: Ellen Considine

import argparse, re
import geopandas as gpd
from datetime import datetime
import pandas as pd
import time
import IPython

def _setup():
    parser = argparse.ArgumentParser(description='Pass in arguments for buffer script')
    parser.add_argument('--buffer_shp', type=str, required=True, help='buffer shp file')
    parser.add_argument('--buffer_csv', type=str, required=True, help='buffer csv file')
    parser.add_argument('--fire_shp', type=str, required=True, help='fire shp file')
    parser.add_argument('--output_csv_file', type=str, required=True, help='name of ouput csv file to create, which will look like the input file but with the data appended to it')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    args = _setup()   
    buffer_gdf = gpd.read_file(args.buffer_shp)
    idx = range(0, len(buffer_gdf))
    buffer_gdf['idx'] = idx
    buffer_csv = pd.read_csv(args.buffer_csv)
    buffer_csv['idx'] = idx
    
    print("read in buffer shp file and buffer csv into geopandas df")
    fire_gdf = gpd.read_file(args.fire_shp)
    print("read in fire shp file into geopandas df")

    lats = []
    lons = []
    dates = []
    fire_count = []
    
    counter = 0
    
    for index, buf in buffer_gdf.iterrows():
        print("processing buffer " + str(index))

        # clip the fire points by the buffer
        fire_pts = fire_gdf[fire_gdf.geometry.intersects(buf.geometry)]

         # do a list intersection to find all shared dates

         ## To process the observation locations (as opposed to the prediction locations), change this line:
         # date_list = buffer_csv['Date'][index].split(',')

        date_list = pd.date_range("2008-01-01", "2018-12-31")
        datetimes = [datetime.strptime(str(d), '%Y-%m-%d %H:%M:%S') for d in date_list]

        buffer_dates = [datetime.strftime(dt, '%Y-%m-%d') for dt in datetimes]
        fire_dates = [re.split(" ", str(d))[0] for d in fire_pts['adj_time'].values]
        fire_pts = fire_pts.assign(adj_date = fire_dates)

        # now we have two lists (buffer_dates and fire_dates) and we want to find
        # the set intersection of those two lists efficiently
        shared_dates = set(buffer_dates).intersection(fire_dates)

        # then use those dates to further subset the fire points
        fire_pts_in_buffer_and_on_relevant_dates = fire_pts[fire_pts['adj_date'].isin(shared_dates)]

        # get counts of fire by date by grouping df by date 

        grouped_counts_by_date = fire_pts_in_buffer_and_on_relevant_dates.groupby('adj_date').size().reset_index(name='counts')

        # add the buffer latitude and longitude n times (n being the number of rows in the grouped df)
        lats += len(grouped_counts_by_date) * [buf.Lat]
        lons += len(grouped_counts_by_date) * [buf.Lon]
        # append to dates list
        dates.extend(list(grouped_counts_by_date['adj_date']))
        # append to fire counts list
        fire_count.extend(list(grouped_counts_by_date['counts']))

    df = pd.DataFrame(
    {'Lat': lats,
     'Lon': lons,
     'Date': dates,
     'fire_count': fire_count
    })     

    df.to_csv(args.output_csv_file, index=False)  
    



