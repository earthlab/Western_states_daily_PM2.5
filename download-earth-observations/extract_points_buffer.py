import argparse

def _setup():
    parser = argparse.ArgumentParser(description='Pass in arguments for buffer creation script')
    parser.add_argument('--input_csv_file', type=str, required=True, help='csv file with monitoring locations')
    parser.add_argument('--buffer_radius', type=int, required=True, help='buffer radius in km')
    args = parser.parse_args()
    return args

def buffer(lat_lon_tuple, buffer_radius):
    # make buffer of defined radius

    # get number of observations within buffer

    # return the number of observations
    pass

if __name__ == "__main__":
    args = _setup()
    # read in monitoring locationg
    df = pd.read_csv(args.input_csv_file)
    station_locations = []
    num_fires = []

    # for each row, make buffer around lat/lon, get number of observations, and append to list
    for index, row in df.iterrows():
        lon = round(row['Lon'], 6)
        lat = round(row['Lat'], 6)
        station_locations.append((lon, lat))

        # make buffer around monitoring location, get number of obs within buffer
        num_obs = buffer((lat, lon), args.buffer_radius)

        # append number to num_fires
