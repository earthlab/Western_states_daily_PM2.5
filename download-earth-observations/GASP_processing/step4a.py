# -----------------------------------------------------------------
# Author: Hollie
# Date: 4/23/15
# Purpose: This script is in lieu of Phil's script (client_data\
# 20141217_gasp_modis_scripts\GASP_scripts\gasp_process_step4.py
# AND gasp_process_step5.py)
# Instead of using ArcGIS to create the shapefiles we're using the
# Python shapefile module. In Phil's script he uses the now
# deprecated 'CreateFeaturesFromTextFile' tool. Cycle through
# each of the text files and create a new shapefile. NOTE: this
# script combines 2 scripts. The tool he was using, 'CreateFeatures
# FromTextFile', would only output lat/long values, no data, so he
# had to subsequently add the aod values.
# -----------------------------------------------------------------

import glob, csv, os
from os import path
import shapefile as shp #this appears as pyshp in the available packages list
import shutil
import multiprocessing


# #epsg = 'GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433],AUTHORITY["EPSG",4326]]'
# epsg = 'GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295],AUTHORITY["EPSG", 4269]]'
# #removed the rest of this string from http://spatialreference.org/ref/esri/102003/esriwkt/:
# # ',PROJECTION["Albers"],PARAMETER["False_Easting",0],PARAMETER["False_Northing",0],PARAMETER["central_meridian",-96],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["latitude_of_origin",37.5],UNIT["Meter",1]]'

#filelist = sorted(glob.glob(origpath + '*.txt'))

def four(origpath, outpath, epsg, item):
    try:
        print(item)

        outfile = path.join(outpath, path.basename(item).replace('txt', 'shp'))
        #print(outfile)

        # Set up blank lists for data
        long, lat, id_no, aod = [],[],[],[]

        # Read data from csv file and store in lists
        with open(origpath + item, 'r') as csvfile: #had to change to 'r' from 'rb'
            r = csv.reader(csvfile, delimiter=',')

            for i, row in enumerate(r):
                if i > 0: #skip header
                    #print(row)
                    long.append(float(row[1]))
                    lat.append(float(row[2]))
                    id_no.append(row[0])
                    aod.append(float(row[3]))


        # If there are no values
        # if not id_no:
        #     print(path.basename(item[:-4]) + ' -- no values')
        #     f = open(data_no_vals, 'a')
        #     writer = csv.writer(f, delimiter=',')
        #     writer.writerow([path.basename(item[:-4])])
        #     f.close()

            # Set up shapefile writer and create empty fields
            w = shp.Writer(shp.POINT)
            w.autoBalance = 1 #ensures gemoetry and attributes match
            #check out http://pygis.blogspot.com/2012/10/pyshp-attribute-types-and-point-files.html
            w.field('long','F',10, 8) #F for float
            w.field('lat','F',10, 8)
            w.field('id_no','N') #N for double precision integer
            w.field('aod','F', 10, 8)


            # Loop through the data and write the shapefile
            for j,k in enumerate(long):
                w.point(k,lat[j]) # Write the geometry
                w.record(k,lat[j], id_no[j], aod[j]) # Write the attributes

            # Save shapefile
            w.save(outfile)
            print("saved")

            # Create the PRJ file
            prj = open("%s.prj" % outfile[:-4], "w")
            prj.write(epsg)
            prj.close()

            return(outfile)

    except Exception as e:
        print(str(e))
    #os.remove(origpath + item)
    #shutil.move(origpath + item, "D:\\Western_US\\GASP\\step_3\\")
