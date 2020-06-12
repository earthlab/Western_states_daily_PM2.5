import geopandas as gpd
from shapely.geometry import Point
import matplotlib.pyplot as plt
import os, glob, re, csv
import multiprocessing

origpath = "/home/jovyan/MODIS-AOD/CSV/"  # these are the non-averaged shapefiles, downloaded from s3
outpath = "/home/jovyan/MODIS-AOD/avg_shp/"  # upload to s3 when finished

def process(day):
    print("Day: " + day)
    vals = dict()
    long = []
    lat = []
    aod = []
    for file in sorted(glob.glob(origpath + '*%s.*.csv' % (day))):
        with open(origpath + item, 'r') as csvfile: 
            r = csv.reader(csvfile, delimiter=',')

            for i, row in enumerate(r):
                if i > 0: #skip header
                    #print(row)
                    long.append(float(row[1]))
                    lat.append(float(row[2]))
                    aod.append(float(row[3]))


    for i in range(0, len(long)):
        if (long[i] >= -126) and (long[i] <= -101) and (lat[i] >= 25) and (lat[i] <= 50):
            coords = (float(long[i]), float(lat[i]))

            # print(numbers)
            if (coords in vals.keys()):
                vals[coords].append(aod[i])
            else:
                vals[coords] = [aod[i]]
    #             #If you need to save space:
    #             os.remove(file)
    #print(vals)

    # Create an empty geodataframe
    newdata = gpd.GeoDataFrame(columns=['geometry', 'aod'])
    i = 1
    for coords, a in vals.items():
        if (len(a) > 1):  # for some reason, it would otherwise take the first value twice
            a = a[1:]
            avg = sum(a) / float(len(a))
        else:
            avg = float(a[0])
        newdata.loc[i, 'geometry'] = Point(coords)
        newdata.loc[i, 'aod'] = avg
        i += 1
    #print(newdata.head())

    # declare geodataframe coordinates
    newdata.crs = {'init': 'esri:4269'}
    #     fig, ax = plt.subplots(figsize = (6, 6))
    #     newdata.plot(ax = ax, column = "aod", marker = "*");

    # reproject shapefile
    new_SHP = newdata.to_crs({'init': 'esri:102003'})
    #     fig, ax = plt.subplots(figsize = (6, 6))
    #     new_SHP.plot(ax = ax, column = "aod", marker = "*");

    # write reprojected shapefile
    proj_str = 'PROJCS["USA_Contiguous_Albers_Equal_Area_Conic",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Albers"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["central_meridian",-96],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["latitude_of_origin",37.5],UNIT["Meter",1]]'
    outfile = outpath + day + '_avg.shp'
    new_SHP.to_file(outfile, driver='ESRI Shapefile', crs_wkt=proj_str)

    
def main():
    # create a list of all the days
    days = []
    for item in os.listdir(origpath):
        # print(item)
        this_day = item[10:17]
        # print(this_day)
        if (this_day in days):
            pass
        else:
            days.append(this_day)

    # calculate average values at each location, on each day
    pool = multiprocessing.Pool()
    for day in days:
        pool.apply_async(process, [day])
    pool.close()
    pool.join()
    
    
if __name__ == "__main__":
    main()
