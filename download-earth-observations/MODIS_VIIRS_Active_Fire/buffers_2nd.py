import argparse
import geopandas as gpd
from datetime import datetime
import pandas as pd
import time
from collections import defaultdict
import multiprocessing
import os, glob

def _setup():
    parser = argparse.ArgumentParser(description='Pass in arguments for buffer script')
    parser.add_argument('--buffer_shp', type=str, required=True, help='buffer shp file')
    parser.add_argument('--buffer_csv', type=str, required=True, help='buffer csv file')
    parser.add_argument('--fire_shp', type=str, required=True, help='fire shp file')
    parser.add_argument('--output_csv_file', type=str, required=True, help='name of ouput csv file to create, which will look like the input file but with the data appended to it')
    args = parser.parse_args()
    return args

#Command:
'''
python C:\\Users\\elco2649\\Documents\\estimate-pm25\\download-earth-observations\\MODIS_VIIRS_Active_Fire\\buffers.py --buffer_shp "C:\\Users\\elco2649\\Documents\\Random processing\\25km_geodesic_buffer_Loc_e.shp" --buffer_csv "C:\\Users\\elco2649\\Documents\\Random processing\\PM25_Step3_part_e_Locations_Dates_NAD83.csv"  --fire_shp "C:\\Users\\elco2649\\Documents\\Random processing\\fire_archive_M6_adjusted_time2.shp" --output_csv_file "C:\\Users\\elco2649\\Documents\\Active_Fires\\test_25km.csv"
'''

def process(index, buf, Buffer_info, fire_gdf, csv_name):
    print("processing buffer " + str(index))

    # clip the fire points by the buffer
    fire_pts = fire_gdf[fire_gdf.geometry.intersects(buf.geometry)]

    # do a list intersection to find all shared dates
    date_list = Buffer_info['Dates'][index].split(' ')
    datetimes = [datetime.strptime(d, '%Y-%m-%d') for d in date_list]
    buffer_dates = [datetime.strftime(dt, '%m/%d/%Y') for dt in datetimes]
    fire_dates = fire_pts['adj_date'].values

    # now we have two lists (buffer_dates and fire_dates) and we want to find
    # the set intersection of those two lists efficiently
    shared_dates = set(buffer_dates).intersection(fire_dates)
    # print(shared_dates)

    # then use those dates to further subset the fire points
    fire_pts_in_buffer_and_on_relevant_dates = fire_pts[fire_pts['adj_date'].isin(shared_dates)]

    # get counts of fire by date by grouping df by date
    grouped_counts_by_date = fire_pts_in_buffer_and_on_relevant_dates.groupby('adj_date').size().reset_index(
        name='counts')

    results = [len(grouped_counts_by_date) * [buf.Lat], len(grouped_counts_by_date) * [buf.Lon], list(grouped_counts_by_date['adj_date']), list(grouped_counts_by_date['counts'])]

    d = {'Lat': results[0], 'Lon': results[1], 'Date': results[2], 'Fire_Count': results[3]}
    DF = pd.DataFrame(d)
    # print(DF.head())

    DF.to_csv(csv_name, index=False)
    print("Wrote " + csv_name)



if __name__ == "__main__":
    args = _setup()
    buffer_gdf = gpd.read_file(args.buffer_shp)
    idx = range(0, len(buffer_gdf))
    buffer_gdf['idx'] = idx
    buffer_csv = pd.read_csv(args.buffer_csv)
    # print(buffer_gdf.head())

    #Code based on: https://stackoverflow.com/questions/52705423/match-a-value-in-the-column-and-return-another-column-in-pandas-python
    # use set for O(1) lookup
    scope_set = set(list(zip([round(b,5) for b in buffer_gdf.Lon], [round(b,5) for b in buffer_gdf.Lat])))
    # print(scope_set)

    # initialise defualtdict of lists
    dd = defaultdict(list)

    # iterate and create dictionary mapping numbers to keys
    for row in buffer_csv.itertuples(index=False):
        if (round(row.Lon,5), round(row.Lat,5)) in scope_set:
            dd[(round(row.Lon,5), round(row.Lat,5))].append(row.Date)

    # construct dataframe from defaultdict
    df = pd.DataFrame({'LatLons': list(dd), 'Dates': list(map(' '.join, dd.values()))})

    # reindex to include blanks
    df = df.set_index('LatLons').reindex(sorted(scope_set)).reset_index()
    df['Lon'] = [t[0] for t in df['LatLons']]
    df['Lat'] = [t[1] for t in df['LatLons']]

    # print(buffer_gdf.head())
    # print(df.head())

    Buffer_info = pd.merge(buffer_gdf, df, how = 'left', on = ["Lon", "Lat"])
    # print(Buffer_info.head())

    # buffer_csv['idx'] = idx
    print("read in buffer shp file and buffer csv into geopandas df")
    fire_gdf = gpd.read_file(args.fire_shp)
    print("read in fire shp file into geopandas df")


    pool = multiprocessing.Pool()
    for index, buf in Buffer_info.iterrows():
        name = args.output_csv_file
        csv_name = os.path.dirname(name) + "/" + str(index) + "_" + os.path.basename(name)
        if not os.path.isfile(csv_name):
            if not isinstance(Buffer_info['Dates'][index], float):
                pool.apply_async(process, [index, buf, Buffer_info, fire_gdf, csv_name])
            else:
                pass
    pool.close()
    pool.join()
    
    
    #Merge all data at end:
    origpath = str(os.path.dirname(args.output_csv_file))
    all_filenames = [i for i in sorted(glob.glob(origpath + "*.csv"))]
    # combine all files in the list
    combined_csv = pd.concat([pd.read_csv(f) for f in all_filenames])
    # export to csv
    combined_csv.to_csv(args.output_csv_file, index=False, encoding='utf-8-sig')

    print("Combined CSVs")
