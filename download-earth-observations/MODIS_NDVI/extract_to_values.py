import argparse
import os, re
import pandas as pd
import rasterio
import glob
import datetime
import calendar
import numpy as np
import IPython

def _setup():
    parser = argparse.ArgumentParser(description='Pass in arguments for NDVI extraction script')
    parser.add_argument('--NDVI_directory', type=str, required=True, help='directory with original hdf files')
    parser.add_argument('--input_csv_file', type=str, required=True, help='input csv file with station coordinates')
    parser.add_argument('--output_csv_file', type=str, required=True, help='ouput file for NDVI values')
    args = parser.parse_args()
    return args


# get NDVI value at point
def get_NDVI_value_at_point(filename, station_coords):
    with rasterio.open(filename) as src:
        vals = src.sample([station_coords])
        for val in vals:
           return val[0]

if __name__ == "__main__":
    args = _setup()

    # read csv file into pandas dataframe
    df = pd.read_csv(args.input_csv_file)
    
#     #Extra work for batch g
#     d = pd.DataFrame({'Date': pd.date_range("2008-01-01", "2018-12-31")})
#     D = pd.DataFrame(np.repeat(d.values,len(df),axis=0))
#     D.columns = d.columns
#     df_repeated = pd.concat([df]*len(d), ignore_index=True)
#     final_df = pd.concat([D, df_repeated], axis=1, ignore_index=True)
#     final_df.columns = D.columns.append(df.columns)

    station_locations = []
    julian_dates = []
    ndvi_values = []
    
    dates = pd.date_range("2008-01-01", "2018-12-31")
    
    with open('/home/jovyan/part_g_logfile.csv', mode='a+') as logfile:
        logfile.write(['Easting', 'Northing', 'Date', 'NDVI']
        for index, row in df.iterrows(): #used to just be df
            for d in dates:
                date_str = re.split(" ", str(d))[0]
                lon = round(row['Easting'], 6)
                lat = round(row['Northing'], 6)
        #         date_str = row['Date']
                print(lon,lat)
                print(date_str)
                date_obj = datetime.datetime.strptime(date_str, "%Y-%m-%d").date()
                julian_date_str = date_obj.strftime("%Y%j")
                print(julian_date_str)
                year = date_obj.year
                if calendar.isleap(year):
                        month_start_list = [1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336]
                else:
                    month_start_list = [1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335]

                mosaic_file_julian_day = str(year) + str(month_start_list[date_obj.month-1]).zfill(3)
                mosaic_file = args.NDVI_directory + '*' + mosaic_file_julian_day + '.mosaic.reproj.tif.tif'

                for fn in glob.glob(mosaic_file):
                    print("reading from this file: " + fn)
                    val = get_NDVI_value_at_point(fn, (lon, lat))
                    ndvi_values.append(val)
                    print("added this NDVI value from the file: " + str(get_NDVI_value_at_point(fn, (lon, lat))))
                    logfile.write([lon, lat, date_str, val*0.0001])

    ndvi_values = np.asarray(ndvi_values)*0.0001
    ndvi_values.round(decimals=4)
    
#     IPython.embed() #check dimensions of df and ndvi_values
    
#     df["ndvi"] = ndvi_values
#     print(df["ndvi"])

#     # turn df into csv
#     df.to_csv(args.output_csv_file, index=False)

    df_repeated = pd.DataFrame(np.repeat(df.values,len(dates),axis=0))
    df_repeated.columns = df.columns
    df_repeated["ndvi"] = ndvi_values
    print(df_repeated["ndvi"])

    # turn df into csv
    df_repeated.to_csv(args.output_csv_file, index=False)



