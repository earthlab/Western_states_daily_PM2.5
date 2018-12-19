import pandas as pd
import argparse
from datetime import datetime

def _setup():
    parser = argparse.ArgumentParser(description='Pass in arguments for buffer script')
    parser.add_argument('--orig_csv', type=str, required=True, help='original csv')
    parser.add_argument('--merge_csv', type=str, required=True, help='csv to merge')
    parser.add_argument('--output_csv_file', type=str, required=True, help='name of ouput csv file to create, which will look like the input file but with the data appended to it')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    args = _setup()
    orig_df = pd.read_csv(args.orig_csv)
    merge_df = pd.read_csv(args.merge_csv)
    merge_df['Date'] = pd.to_datetime(merge_df['Date'], format="%m/%d/%Y").dt.strftime('%m/%d/%Y').apply(str)
    orig_df['Date'] = pd.to_datetime(orig_df['Date'], format="%Y-%m-%d").dt.strftime('%m/%d/%Y').apply(str)
    merged_df = orig_df.merge(merge_df, how='left', on=['Lat', 'Lon', 'Date'])

    merged_df['fire_count'].fillna(0, inplace=True)

    merged_df.to_csv(args.output_csv_file, index=False)  
