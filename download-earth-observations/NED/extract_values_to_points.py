import pandas as pd
import ulmo
import rasterio
import numpy as np
import argparse

def _setup():
    parser = argparse.ArgumentParser(description='Pass in arguments for extracting NED values script')
    parser.add_argument('--NED_directory', type=str, required=True, help='directory with NED tif files')
    parser.add_argument('--input_csv_file', type=str, required=True, help='input csv file with station coordinates')
    parser.add_argument('--output_csv_file', type=str, required=True, help='ouput file with station coordinates')
    args = parser.parse_args()
    return args

# generate a tiny bounding box around the point
def generate_bounding_box(lat, lon):
    ul = (lat+0.0001, lon-0.0001)
    br = (lat-0.0001, lon+0.0001)

    # returns list in order min long, min lat, max long, max lat
    return [ul[1], br[0], br[1], ul[0]]

# download that tile
def download_tile():
    pass

# get elevation value at point
def get_elevation_value_at_point(tilename, station_coords):
    with rasterio.open(tilename) as src:
        vals = src.sample(station_coords)
        for val in vals:
           return val[0]

if __name__ == "__main__":
    args = _setup()
    # read csv file into pandas dataframe
    df = pd.read_csv(args.input_csv_file)

    bounding_boxes = []
    station_locations = []
    # for each row, make a little bounding box around the lat/lon and append to bounding_boxes
    for index, row in df.iterrows():
        station_locations.append((row['Lon'], row['Lat']))
        bounding_boxes.append(generate_bounding_box(row['Lat'], row['Lon']))
    
    # for each bounding box, download the necessary NED tiles with no repeated downloads

    tilenames = []
    elevation_values = []
    # for each bounding box, get the corresponding tile name
    for i in range(len(bounding_boxes)):
        bbox_metadata = ulmo.usgs.ned.get_raster_availability('1 arc-second', bounding_boxes[i])
        tilename = bbox_metadata['features'][0]['properties']['download url'].split('/')[-1].split('.')[-2]+'.img'
        # next line not necessary
        tilenames.append(tilename)

        # get the elevation value from the tile based on the lat/lon
        if tilename.startswith("n"):
            tilename = 'img' + tilename.split(".")[0] + '_1.img'
        elevation_values.append(get_elevation_value_at_point(args.NED_directory + tilename, [station_locations[i]]))
        
    
    elevation_values = np.asarray(elevation_values)
    
    df["elevation"] = elevation_values

    # turn df into csv
    df.to_csv(args.output_csv_file, index=False)
    
    #result = get_elevation_value_at_point('C:\\Users\\ginal\\Documents\\EarthLab\\NED_factcheck\\img_files\\USGS_NED_1_n40w106_IMG.img', [(-105.525733, 39.586730)])

    




