import pytz
import datetime
from dateutil import parser
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point, Polygon

def spatial_join(point_df, poly_json_file):
    print("Performing spatial join between points and polygons...")
    geometry = [Point(xy) for xy in zip(point_df.lon, point_df.lat)]
    #point_df = point_df.drop(['Longitude', 'Latitude'], axis=1)
    crs = {'init': 'esri:102003'}
    point_gdf = gpd.GeoDataFrame(point_df, crs=crs, geometry=geometry)

    # Read polygons from json file to geopandas df
    poly_gdf = gpd.read_file(poly_json_file)
    poly_gdf.crs = {'init': 'esri:102003'}
    combined_gdf = gpd.tools.sjoin(point_gdf, poly_gdf)

    print('Spatial join complete')
    # return spatially joined geopandas df

    return combined_gdf

def create_lookup_table(poly_json_file, output_csv):
    lon_df = pd.read_csv('lon.txt', sep="/n", names=['lon'], engine='python')
    lat_df = pd.read_csv('lat.txt', sep="/n", names=['lat'], engine='python')
    lon_lat_df = pd.concat([lon_df, lat_df], axis=1)
    print(lon_lat_df)

    # perform spatial join between observation points and tz regions
    combined_gdf = spatial_join(lon_lat_df, poly_json_file)
    print(combined_gdf)
    import IPython
    IPython.embed()
    fields = ['lon', 'lat', 'tzid']
    combined_gdf.to_csv(output_csv, columns=fields)

if __name__ == "__main__":
    create_lookup_table('timezones_western_us.json', 'lat_lon_tzid_lookup.csv')