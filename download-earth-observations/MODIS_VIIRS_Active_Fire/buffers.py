import argparse
import geopandas as gpd
from rasterstats import zonal_stats
from datetime import datetime
import pandas as pd
import time

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

    for index, buf in buffer_gdf.iterrows():
        print("processing buffer " + str(index))
        start = time.time()
        count = 0
        for date in buffer_csv['dates'][index].split(','):
            count += 1
            print("processing date " + str(count) + " out of " + str(len(buffer_csv['dates'][index].split(','))) + " for buffer " + str(index))
            buf_datetime_obj = datetime.strptime(date, '%Y-%m-%d')
            date_str = buf_datetime_obj.strftime('%m/%d/%Y')
            lats.append(buf['lat'])
            lons.append(buf['lon'])
            dates.append(date)

            fires_in_buf = 0
            poly = buf.geometry
            subset_fire_gdf = fire_gdf[fire_gdf.geometry.intersects(poly)]
        
            for index2, fire in subset_fire_gdf.iterrows():
                if fire['adj_date'] == date_str:
                    #len(fire['geometry']['coordinates'])
                    fires_in_buf+=1
                    print("buffer " + str(index) + " has matching fire date " + fire['adj_date'])
            fire_count.append(fires_in_buf)
        end = time.time()
        print(end-start)
            
        

    df = pd.DataFrame(
    {'lat': lats,
     'lon': lons,
     'date': dates,
     'fire_count': fire_count
    })     

    df.to_csv(args.output_csv_file, index=False)  
    



