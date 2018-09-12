import csv
import json
from geojson import Feature, FeatureCollection, Point
import datetime
from dateutil import parser
from shapely.geometry import Point, Polygon
import geopandas as gpd
import pandas as pd
import pytz
import os
import argparse

def create_geojson_points(gdf):
    print('creating new data...')
    features = []
    for index, row in gdf.iterrows():
        Latitude = row['Latitude']
        Longitude = row['Longitude']
        Date = row['Date']
        tzid = row['tzid']
        #Latitude, Longitude = map(float, (Latitude, Longitude))
        start_dt_local = parser.parse(Date)
        end_dt_local = start_dt_local + datetime.timedelta(0, 86399)
        features.append(
            Feature(
                geometry=Point((Longitude, Latitude)),
                properties={
                    'date': Date,
                    'latitude': Latitude,
                    'longitude': Longitude,
                    'utc_start': adjust_time(start_dt_local, tzid),
                    'utc_end': adjust_time(end_dt_local, tzid)
                }
            )
        )
    collection = FeatureCollection(features)

    with open("Locations_Dates_of_PM25_Obs.json", "w") as f:
        f.write('%s' % collection)

    print('new dataset created')
    return collection


def spatial_join(csv_file, poly_json_file):
    print('Executing spatial join between points and polygons...')
    # Read points from csv file to geopandas df
    fields = ["Latitude", "Longitude", "Projection", "Date"]
    point_df = pd.read_csv(csv_file, usecols=fields)
    geometry = [Point(xy) for xy in zip(point_df.Longitude, point_df.Latitude)]
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


def adjust_time(dt, timezone_str):
    timezone = pytz.timezone(timezone_str)
    adjusted_dt = timezone.localize(datetime.datetime(
        dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second)).astimezone(pytz.utc)
    fmt = '%Y-%m-%d %H:%M:%S %Z%z'
    return adjusted_dt.strftime(fmt)

#TODO: Complete this function to work properly
def json_to_csv(feature_collection, output_filename):
    import IPython
    IPython.embed()
    feature_collection_str = str(feature_collection)
    json.loads(feature_collection)
    df = pd.DataFrame.from_dict(json, orient='columns')
    print(df)
    df.to_csv(output_filename)

if __name__ == "__main__":
    combined_gdf = spatial_join(
        'Locations_Dates_of_PM25_Obs.csv', 'timezones_western_us.json')
    feature_collection = create_geojson_points(combined_gdf)
    #json_to_csv(feature_collection, 'Locations_Dates_of_PM25_Obj_new.csv')
