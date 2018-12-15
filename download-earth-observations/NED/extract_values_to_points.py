import pandas as pd
import ulmo
import rasterio
import numpy as np
import argparse
import time
import os.path
import glob

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
                    raise RuntimeError('Could not get raster availability. Likely timed out.')

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
            if get_elevation_value_at_point(args.NED_directory + tilename, [station_locations[i]]) < -3000:
                print("Hit NoData value because lat/lon on tile edge, trying neighbor tiles")
                raise ValueError('Hit NoData value')
            else:
                elevation_values.append(get_elevation_value_at_point(args.NED_directory + tilename, [station_locations[i]]))
                print(get_elevation_value_at_point(args.NED_directory + tilename, [station_locations[i]]))
        except:
            
            if tilename.startswith("img"):
                n = int(tilename[4:6])
                w = int(tilename[7:10])
            if tilename.startswith("USGS"):
                n = int(tilename[12:14])
                w = int(tilename[15:18])
            
                 
            try:
                print("Trying tile above")
                
                tilename_new = [tilename_new for tilename_new in glob.glob(args.NED_directory + '*n' + str(n+1) + 'w' + str(w) + "*.img")][0]
                print(get_elevation_value_at_point(tilename_new, [station_locations[i]]))
                elevation_values.append(get_elevation_value_at_point(tilename_new, [station_locations[i]]))

            except:
                try:
                    print("Trying tile below")
                    tilename_new = [tilename_new for tilename_new in glob.glob(args.NED_directory + '*n' + str(n-1) + 'w' + str(w) + "*.img")][0]
                    if get_elevation_value_at_point(tilename_new, [station_locations[i]]) < -3000:
                        print("Hit NoData value because lat/lon on tile edge, trying neighbor tiles")
                        raise ValueError('Hit NoData value')
                    else:
                        print(get_elevation_value_at_point(tilename_new, [station_locations[i]]))
                        elevation_values.append(get_elevation_value_at_point(tilename_new, [station_locations[i]]))
                except:
                    try:
                        print("Trying tile to the left")
                        tilename_new = [tilename_new for tilename_new in glob.glob(args.NED_directory + '*n' + str(n) + 'w' + str(w+1) + "*.img")][0]
                        if get_elevation_value_at_point(tilename_new, [station_locations[i]]) < -3000:
                            print("Hit NoData value because lat/lon on tile edge, trying neighbor tiles")
                            raise ValueError('Hit NoData value')
                        else:
                            print(get_elevation_value_at_point(tilename_new, [station_locations[i]]))
                            elevation_values.append(get_elevation_value_at_point(tilename_new, [station_locations[i]]))
                    except:
                        try:
                            print("Trying tile to the right")
                            tilename_new = [tilename_new for tilename_new in glob.glob(args.NED_directory + '*n' + str(n) + 'w' + str(w-1) + "*.img")][0]
                            if get_elevation_value_at_point(tilename_new, [station_locations[i]]) < -3000:
                                print("Hit NoData value because lat/lon on tile edge, trying neighbor tiles")
                                raise ValueError('Hit NoData value')
                            else:
                                print(get_elevation_value_at_point(tilename_new, [station_locations[i]]))
                                elevation_values.append(get_elevation_value_at_point(tilename_new, [station_locations[i]]))
                        except:
                            print("Tile extraction issue for tile " + tilename + " at lat/long " + str(station_locations[i]))
                            import IPython
                            IPython.embed()
                            raise ValueError('No value sampled')
        if len(elevation_values) != i + 1:
            print("elevation values do not match index, most likely failed to add an elevation value for a station location")
            raise ValueError("Elevation values do not match index, most likely failed to add an elecation value for a station location")
            import IPython
            IPython.embed()
    
    elevation_values = np.asarray(elevation_values)

    df["elevation"] = elevation_values

    # turn df into csv
    df.to_csv(args.output_csv_file, index=False)

    




