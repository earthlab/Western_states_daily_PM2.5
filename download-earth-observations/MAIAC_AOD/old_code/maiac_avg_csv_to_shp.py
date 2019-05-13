import geopandas as gpd
from shapely.geometry import Point
import matplotlib.pyplot as plt
import os, glob, re, csv
import multiprocessing
import os.path

def SHP(origpath, outpath, item):
    name = os.path.basename(item)[:-4]
    long = []
    lat = []
    aod = []
    with open(origpath + item, 'r') as csvfile:
        r = csv.reader(csvfile, delimiter=',')

        for i, row in enumerate(r):
            if i > 0: #skip header
                #print(row)
                long.append(float(row[1]))
                lat.append(float(row[2]))
                aod.append(float(row[3]))

    # Create an empty geodataframe
    newdata = gpd.GeoDataFrame(columns=['geometry', 'aod'])
    i = 1
    for j in range(0, len(aod)):
        coords = (float(long[j]), float(lat[j]))
        newdata.loc[i, 'geometry'] = Point(coords)
        newdata.loc[i, 'aod'] = aod[j]
        i += 1
    #print(newdata.head())

    # declare geodataframe coordinates
    newdata.crs = {'init': 'esri:4326'} #these data are in WGS84
    #     fig, ax = plt.subplots(figsize = (6, 6))
    #     newdata.plot(ax = ax, column = "aod", marker = "*");

    # reproject shapefile
    new_SHP = newdata.to_crs({'init': 'esri:102003'})
    #     fig, ax = plt.subplots(figsize = (6, 6))
    #     new_SHP.plot(ax = ax, column = "aod", marker = "*");

    # write reprojected shapefile
    proj_str = 'PROJCS["USA_Contiguous_Albers_Equal_Area_Conic",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Albers"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["central_meridian",-96],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["latitude_of_origin",37.5],UNIT["Meter",1]]'
    outfile = outpath + name + '_avg.shp'
    new_SHP.to_file(outfile, driver='ESRI Shapefile', crs_wkt=proj_str)
