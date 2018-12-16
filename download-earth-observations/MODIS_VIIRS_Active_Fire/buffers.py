import argparse
import geopandas as gpd
from rasterstats import zonal_stats
from datetime import datetime

def _setup():
    parser = argparse.ArgumentParser(description='Pass in arguments for buffer script')
    parser.add_argument('--buffer_shp', type=str, required=True, help='buffer shp file')
    parser.add_argument('--fire_shp', type=str, required=True, help='fire shp file')
    parser.add_argument('--output_csv_file', type=str, required=True, help='name of ouput csv file to create, which will look like the input file but with the data appended to it')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    args = _setup()   
    buffer_gdf = gpd.read_file(args.buffer_shp)
    print("read in buffer shp file into geopandas df")
    fire_gdf = gpd.read_file(args.fire_shp)
    print("read in fire shp file into geopandas df")

    lats = []
    lons = []
    dates = []
    fire_count = []

    for index, buf in buffer_gdf.iterrows():
        for date in buf['dates'].split(','):
            buf_datetime_obj = datetime.strptime(date, '%Y-%m-%d')
            date_str = buf_datetime_obj.strftime('%m/%d/%Y')
            print("buf date is " + date_str)
            lats.append(buf['lat'])
            lons.append(buf['lon'])
            dates.append(date)
            
            fires_in_buf = 0
            for index2, fire in fire_gdf.iterrows():
                if buf['geometry'].intersects(fire['geometry']) and fire['adj_date'] == date_str:
                    #len(fire['geometry']['coordinates'])
                    fires_in_buf+=1
                    print("matching fire date is " + fire['adj_date'])
            fire_count.append(fires_in_buf)
            
            print(str(fires_in_buf) + " fires in buffer " + str(index) + " on " + date_str)

    df = pd.DataFrame(
    {'lat': lats,
     'lon': lons,
     'date': dates,
     'fire_count': fire_count
    })     

    df.to_csv(args.output_csv_file, index=False)  
    



