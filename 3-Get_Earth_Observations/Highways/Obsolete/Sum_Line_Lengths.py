#---------------------------------------------------------------------------------
# Author:   Ellen
# Created:  5/31/2018
# Purpose:  NHPN processing Step 2
# Buffer stations and sum road lengths in each buffer
#----------------------------------------------------------------------------------

#Had to add '/usr/share/qgis/python/plugins' to settings interpreter paths
#https://docs.qgis.org/testing/en/docs/pyqgis_developer_cookbook/intro.html#python-plugins

import sys
import qgis.core as qc
from PyQt4.QtGui import *
#from PyQt4.QtCore import QSize

qc.QgsApplication.setPrefixPath("C:/Program Files/QGIS 2.18", True)# Adjust prefix path according to your installation (see note below)
qgs = qc.QgsApplication([], True)
qgs.initQgis()

sys.path.append('C:/Program Files/QGIS 2.18/apps/qgis-ltr/python/plugins/') # Folder where Processing is located
from processing.core.Processing import Processing
Processing.initialize()
import processing


#Load in station points:

CSV = "Unique_monitors.csv"
Xfield = "Longitude"
Yfield = "Latitude"
targetCRS = "ESRI: 4326" #make sure to use monitors in WGS84 -- coordinates should not matter between NAD83 and this one???
pointShpFile = "monitors.shp"
processing.runalg("qgis:pointslayerfromtable", CSV, Xfield, Yfield, targetCRS, pointShpFile)

pointLayer = qc.QgsVectorLayer(pointShpFile, "monitors", "ogr")

#Check for validity:
if not pointLayer.isValid():
    print "Point layer failed to load."
else:
    print "Point layer was loaded successfully!"


#Load in arterial roads:

ArterialFile = "Arterial_roads.shp"
AroadsLayer = qc.QgsVectorLayer(ArterialFile, "arterial_roads", 'ogr')

#Check for validity
if not AroadsLayer.isValid():
    print "Arterial roads layer failed to load."
else:
    print "Arterial roads layer was loaded successfully!"

#Load in collector roads:

ArterialFile = "Collector_roads.shp"
CroadsLayer = qc.QgsVectorLayer(ArterialFile, "collector_roads", 'ogr')

#Check for validity
if not CroadsLayer.isValid():
    print "Collector roads layer failed to load."
else:
    print "Collector roads layer was loaded successfully!"



#Make buffers:
distances = [0.001, 0.0025, 0.005, 0.01] #instead of [100, 250, 500, 1000]
segments = 5 #change this from the default?
dissolve = "False"

for d in distances:
    filename = "Buffer_" + str(int(d*100000)) + ".shp"
    processing.runalg("qgis:fixeddistancebuffer", pointLayer, d, segments, dissolve, filename)
    #processing.runalg("qgis:fixeddistancebuffer", pointLayer, d, segments, dissolve, CSV[:-4] + "_buffer_" + str(d * 100000) + ".shp")

    #Sum line lengths:
    processing.runalg("qgis:sumlinelengths", AroadsLayer, filename, "Road Length", "Road Count", "Arterial Road Sum " + str(int(d*100000)) + ".csv")
    processing.runalg("qgis:sumlinelengths", CroadsLayer, filename, "Road Length", "Road Count", "Collector Road Sum " + str(int(d*100000)) + ".csv")

#For sum line lengths:
# https://docs.qgis.org/2.8/en/docs/user_manual/processing_algs/qgis/vector_analysis_tools/sumlinelengths.html

qgs.exitQgis()
