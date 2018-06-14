# -----------------------------------------------------------------
# Author: Hollie
# Date: 6/16/15
# Purpose: For each projected set of points see if the extent is
# the same or greater than California. If it is create a geostatistical
# layer using the model parameters from Phil (GASP_RBF.xml). Lastly
# clip the new geostat layer to CA boundary
# -----------------------------------------------------------------


import sys, string, os, glob, csv, time, datetime
import arcpy
from collections import OrderedDict
from arcpy import env
from arcpy.sa import *
import re
arcpy.env.overwriteOutput = 1


start = datetime.datetime.now()
print start
print '--------------------------'


arcpy.CheckOutExtension('GeoStats')
arcpy.CheckOutExtension('Spatial')
arcpy.env.SnapRaster = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\snapraster.gdb\\snapraster'


xmlfile = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\gis\\GASP_RBF.xml'

inpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_4b\\'
outpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_5\\'

sa = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\gis\\western_102003.shp'
sabuf = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\gis\\study-area-buffer_25km.shp'


env.workspace = inpath

# Grab the extent of California -- this will help
# qualify whether we include day/time slices
desc = arcpy.Describe(sa)
extent = desc.extent

sa_xmin = extent.XMin
sa_xmax = extent.XMax
sa_ymin = extent.YMin
sa_ymax = extent.YMax



# First create a list of days from which calculate
# frequency of days. We only want to create geostat
# layers for days that have 12+ time slices
fcs = sorted(glob.glob(inpath + '*.shp'))

daylist = []

for fc in fcs:
    fcname = os.path.basename(fc)
    dayslice1 = str(fcname)
    #re.split(r"[\[\]]", "I need to [go out] to lunch")
    dayslice2 = re.split(r"[_]", dayslice1)
    dayslice3 = dayslice2[1]
    #print(dayslice3)
    daylist.append(dayslice3)


dayFreq = {x:daylist.count(x) for x in daylist}
# for f in dayFreq:
#     print (f)

x = OrderedDict(sorted(dayFreq.items(), key=lambda t: t[0], reverse=True))
#dayUnique, dayCount = x.keys(), x.values()
dayUnique, dayCount = x.keys(), x.values() #x.keys()[52:], x.values()[52:]

# print(dayUnique)
# print(dayCount)

# Cycle through list of days and move forward if 12+
# timeslices per day.
with open('C:\\Users\\elco2649\\Documents\\GASP_AOD\\gis\\GASP_gaPts.csv', 'wb') as csvfile:
    spamwriter = csv.writer(csvfile, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
    spamwriter.writerow(['inName', 'outName', 'totPts', 'saPts', 'day', 'time', 'countTime'])

    for day, count in zip(dayUnique, dayCount):
        print 'day: ' + day + ', timeslices: ' + str(count)

        if count >= 3:
            fcs = sorted(glob.glob(inpath + '*_' + day + '_*.shp')) #not sure if this is good syntax?

            for index, fc in enumerate(fcs):
                start = datetime.datetime.now()
                fcname = os.path.basename(fc)
                timesliceA = str(fcname)
                # re.split(r"[\[\]]", "I need to [go out] to lunch")
                timesliceB = re.split(r"[_]", timesliceA)
                timeslice = int(timesliceB[2])
                # print(timeslice)
                year = timesliceA[:4]
                #print 'year: ' + year
                print start
                print str(index + 1) + '-' + fcname

                env.workspace = fc

                # Calculate total number of points in fc
                totPts = int(arcpy.GetCount_management(fc).getOutput(0))

                # Calculate the extent of the pts
                descPts = arcpy.Describe(fc)
                extentPts = descPts.extent

                pt_xmin = extentPts.XMin
                pt_xmax = extentPts.XMax
                pt_ymin = extentPts.YMin
                pt_ymax = extentPts.YMax


               # If the pts extend to the study area extent (or beyond) move forward
                if ((pt_xmin <= sa_xmin) & (pt_xmax >= sa_xmax) & (pt_ymin <= sa_ymin) & (pt_ymax >= sa_ymax)):

                    # Calculate how many points in the study area + buffer
                    arcpy.MakeFeatureLayer_management(fc, 'lyr')
                    arcpy.SelectLayerByLocation_management('lyr', 'intersect', sabuf)
                    saPts = int(arcpy.GetCount_management('lyr').getOutput(0))

                    tempPts = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\gis\\junk\\temp_' + fcname
                    arcpy.CopyFeatures_management('lyr', tempPts)
                    arcpy.Delete_management('lyr')


                    # Set the final variables
                    gadir = outpath + year + "_" + day
                    ganame = 'g' + str(fcname)[:-4]
                    galyr = 'lyr_' + ganame
                    gafin = ganame[:-4] # + '_ext'


                    try:
                        os.mkdir(gadir)

                    except Exception:
                        pass

                    try:

                        #arcpy.GACreateGeostatisticalLayer_ga(xmlfile, str(fc) + " AOD", galyr)
                        arcpy.GACreateGeostatisticalLayer_ga(xmlfile, tempPts + ' AOD', galyr)

                        now = datetime.datetime.now()
                        elapsed = now - start
                        print 'Creation of galayer: ' + str(elapsed)

                        arcpy.GALayerToGrid_ga(galyr, gadir + '\\' + gafin, '4000', '', '')
                        now = datetime.datetime.now()
                        elapsed = now - start
                        print 'Saving of galayer: ' + str(elapsed) + '\n --------------------------'

                        #arcpy.Delete_management(tempPts)

                    except Exception, e:
                        print str(e)

                else:
                    saPts =-99999
                    gafin =-99999

                    print fcname + ': extent not big enough' + '\n --------------------------'

            spamwriter.writerow([fcname, gafin, totPts, saPts, day, timeslice, year, count])


        else:
            print 'Skip, not enough timeslices' + '\n --------------------------'
            spamwriter.writerow(['-99999', '-99999', -99999, -99999, day, -99999, count])


print 'Calculations complete'








