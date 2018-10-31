import pandas as pd
import ulmo
import rasterio
import numpy as np
import argparse
import sys
import time
import os.path

def _setup():
    parser = argparse.ArgumentParser(description='Pass in arguments for extracting NED values script')
    parser.add_argument('--NED_directory', type=str, required=True, help='directory with NED tif files')
    parser.add_argument('--input_csv_file', type=str, required=True, help='input csv file with station coordinates')
    parser.add_argument('--output_csv_file', type=str, required=True, help='ouput file with station coordinates')
    args = parser.parse_args()
    return args

# generate a tiny bounding box around the point
def generate_bounding_box(lat, lon):
    ul = (round(lat+0.0001, 6), round(lon-0.0001, 6))
    br = (round(lat-0.0001, 6), round(lon+0.0001, 6))

    # returns list in order min long, min lat, max long, max lat
    return [ul[1], br[0], br[1], ul[0]]

# download that tile
def download_tile(bbox, directory):
    ulmo.usgs.ned.get_raster('1 arc-second', bbox, directory)

# get elevation value at point
def get_elevation_value_at_point(tilename, station_coords):
    print("tilename: " + tilename)
    for station_coord in station_coords:
        print(station_coord)
    with rasterio.open(tilename) as src:
        vals = src.sample(station_coords)
        for val in vals:
           return val[0]

def get_raster_availability_retry(tries=0):
    try:
        bbox_metadata = ulmo.usgs.ned.get_raster_availability('1 arc-second', bounding_boxes[i])
        return bbox_metadata
    except:
        if tries < sys.getrecursionlimit():
            return get_raster_availability_retry(tries+1)

if __name__ == "__main__":
    args = _setup()
    # read csv file into pandas dataframe
    df = pd.read_csv(args.input_csv_file)

    bounding_boxes = []
    station_locations = []
    # for each row, make a little bounding box around the lat/lon and append to bounding_boxes
    for index, row in df.iterrows():
        lon = round(row['Lon'], 6)
        lat = round(row['Lat'], 6)
        station_locations.append((lon, lat))
        bounding_boxes.append(generate_bounding_box(lat, lon))
    # for each bounding box, download the necessary NED tiles with no repeated downloads

    tilenames = []
    elevation_values = []
    # for each bounding box, get the corresponding tile name
    for i in range(len(bounding_boxes)):
        try:
            bbox_metadata = ulmo.usgs.ned.get_raster_availability('1 arc-second', bounding_boxes[i])
        except:
            try:
                print("sleeping for 5")
                time.sleep(5)
                bbox_metadata = ulmo.usgs.ned.get_raster_availability('1 arc-second', bounding_boxes[i])
            except:
                try: 
                    print("sleeping for 300")
                    time.sleep(300)
                    bbox_metadata = ulmo.usgs.ned.get_raster_availability('1 arc-second', bounding_boxes[i])
                except:
                    import IPython
                    IPython.embed()
        #bbox_metadata = get_raster_availability_retry()
        tilename = bbox_metadata['features'][0]['properties']['download url'].split('/')[-1].split('.')[-2]+'.img'
        # next line not necessary
        tilenames.append(tilename)

        # get the elevation value from the tile based on the lat/lon
        if tilename.startswith("n"):
            tilename = 'img' + tilename.split(".")[0] + '_1.img'

        if os.path.isfile(args.NED_directory + tilename) == False:
            download_tile(bounding_boxes[i], args.NED_directory)

        try:
            elevation_values.append(get_elevation_value_at_point(args.NED_directory + tilename, [station_locations[i]]))
        except:
            if tilename.startswith("img"):
                n = int(tilename[4:6])
                w = int(tilename[7:10])
            else:
                # tilename starts with USGS
                n = int(tilename[12:14])
                w = int(tilename[15:18])
                
                
                try:
                    tilename_new = tilename[0:4] + str(n+1) + tilename[6:]
                    if os.path.isfile(args.NED_directory + tilename_new) == False:
                        raise Exception('File not found')
                    elevation_values.append(get_elevation_value_at_point(args.NED_directory + tilename_new, [station_locations[i]]))
                except:
                    try:
                        tilename_new = tilename[0:4] + str(n-1) + tilename[6:]
                        if os.path.isfile(args.NED_directory + tilename_new) == False:
                            raise Exception('File not found')
                        elevation_values.append(get_elevation_value_at_point(args.NED_directory + tilename_new, [station_locations[i]]))
                    except:
                        try:
                            tilename_new = tilename[:7] + str(w+1) + tilename[10:]
                            if os.path.isfile(args.NED_directory + tilename_new) == False:
                                raise Exception('File not found')
                            elevation_values.append(get_elevation_value_at_point(args.NED_directory + tilename_new, [station_locations[i]]))
                        except:
                            try:
                                tilename_new = tilename[:7] + str(w-1) + tilename[10:]
                                if os.path.isfile(args.NED_directory + tilename_new) == False:
                                    raise Exception('File not found')
                                elevation_values.append(get_elevation_value_at_point(args.NED_directory + tilename_new, [station_locations[i]]))
                            except:
                                print(tilename)
                                print(station_locations[i])
                                raise Exception('tile was never added')
                                print("tile extraction issue for tile " + tilename + " at lat/long " + str(station_locations[i]))

    
    elevation_values = np.asarray(elevation_values)
    
    import IPython
    IPython.embed()

    df["elevation"] = elevation_values

    # turn df into csv
    df.to_csv(args.output_csv_file, index=False)
    
    #result = get_elevation_value_at_point('C:\\Users\\ginal\\Documents\\EarthLab\\NED_factcheck\\img_files\\USGS_NED_1_n40w106_IMG.img', [(-105.525733, 39.586730)])

    




