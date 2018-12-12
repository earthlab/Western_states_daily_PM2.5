import argparse
import pandas as pd
from rasterstats import zonal_stats

def _setup():
    parser = argparse.ArgumentParser(description='Pass in arguments for buffer script')
    parser.add_argument('--buffer_shp', type=str, required=True, help='buffer shp file')
    parser.add_argument('--fire_shp', type=str, required=True, help='fire shp file')
    parser.add_argument('--input_csv_file', type=str, required=True, help='input csv file with PM25 station locations and dates')
    parser.add_argument('--output_csv_file', type=str, required=True, help='name of ouput csv file to create, which will look like the input file but with the data appended to it')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    args = _setup()    


