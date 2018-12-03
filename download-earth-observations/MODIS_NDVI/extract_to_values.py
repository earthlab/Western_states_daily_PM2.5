import argparse
import os
import pandas as pd
import rasterio
import glob
import datetime

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

    station_locations = []
    julian_dates = []
    ndvi_values = []

    for index, row in df.iterrows():
        lon = round(row['Lon'], 6)
        lat = round(row['Lat'], 6)
        date_str = row['Date']
        print(date_str)
        date_obj = datetime.datetime.strptime(date_str, "%Y-%m-%d").date()
        julian_date_str = date_obj.strftime("%Y%j")
        print(julian_date_str)
        #julian_dates.append(julian_date_str)
        #station_locations.append((lon, lat))

        for fn in glob.glob(args.NDVI_directory + '*' + julian_date_str + '.mosaic.tif.tif'):
            print("reading from this file: " + fn)
            ndvi_values.append(get_NDVI_value_at_point(fn, (lon, lat)))
            print("added this NDVI value from the file: " + get_NDVI_value_at_point(fn, (lon, lat)))

    
    '''
    for julian_date in julian_dates:
        for fn in glob.glob(args.NDVI_directory + '*' + julian_date_str + '.mosaic.tif.tif'):
            print("got the one file: " + fn)
            for station_location in station_locations:
                ndvi_values.append(get_NDVI_value_at_point(fn, station_location))
                print("got this NDVI point from the file: " + get_NDVI_value_at_point(fn, station_location))
    '''

    ndvi_values = np.asarray(ndvi_values)*0.0001

    df["ndvi"] = ndvi_values
    print(df["ndvi"])

    # turn df into csv
    df.to_csv(args.output_csv_file, index=False)
