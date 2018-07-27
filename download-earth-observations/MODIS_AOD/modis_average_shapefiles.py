import geopandas as gpd
from shapely.geometry import Point
import matplotlib.pyplot as plt
import os, glob, re

origpath = "/home/jovyan/MODIS-AOD/Shapefiles/" #these are the non-averaged shapefiles, downloaded from s3
outpath = "/home/jovyan/MODIS-AOD/avg_shp/" #upload to s3 when finished 

#create a list of all the days
days = []
for item in os.listdir(origpath):
    #print(item)
    this_day = item[10:17]
    #print(this_day)
    if(this_day in days):
        pass
    else:
        days.append(this_day)
        
#calculate average values at each location, on each day       
for day in days:
    print("Day: " + day)
    vals = dict()
    for file in sorted(glob.glob('*%s.*.shp' %(day))):
        infile = gpd.read_file(file)
        #print(infile.head())
        
        geom = infile['geometry']
        #print(geom.head())
        aod = infile['aod']
        #print(aod.head())
        coordMatrix = np.empty((len(geom),2))
        for index, g in enumerate(geom):
            coords = re.sub('POINT ', "", str(g) )
            coords = coords.strip("(")
            coords = coords.strip(")")
            XY = re.split(" ", coords)
            numbers = (float(XY[0]), float(XY[1]))
            #print(numbers)
            if(numbers in vals.keys()):
                vals[numbers].append(aod[index])
            else:
                vals[numbers] = [aod[index]]
#             #If you need to save space:
#             os.remove(file)
    
        
    #Create an empty geodataframe 
    newdata = gpd.GeoDataFrame(columns=['geometry', 'aod'])
    i = 1
    for coords, a in vals.items():
        if(len(a) > 1): #for some reason, it would otherwise take the first value twice
            a = a[1:]
            avg = sum(a)/float(len(a))
        else:
            avg = float(a[0])
        newdata.loc[i, 'geometry'] = Point(coords)
        newdata.loc[i, 'aod'] = avg
        i += 1
    #print(newdata.head())
    
    #declare geodataframe coordinates
    newdata.crs = {'init':'esri:4269'}
#     fig, ax = plt.subplots(figsize = (6, 6))
#     newdata.plot(ax = ax, column = "aod", marker = "*");

    #reproject shapefile
    new_SHP = newdata.to_crs({'init':'esri:102003'})
#     fig, ax = plt.subplots(figsize = (6, 6))
#     new_SHP.plot(ax = ax, column = "aod", marker = "*");
    
    #write reprojected shapefile
    proj_str= 'PROJCS["USA_Contiguous_Albers_Equal_Area_Conic",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Albers"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["central_meridian",-96],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["latitude_of_origin",37.5],UNIT["Meter",1]]'
    outfile = outpath + day + '_avg.shp'
    new_SHP.to_file(outfile, driver = 'ESRI Shapefile', crs_wkt = proj_str)
    
    