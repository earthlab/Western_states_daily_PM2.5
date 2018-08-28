#---------------------------------------------------------------------------------
# Author:   Ellen
# Created:  5/31/2018
# Purpose:  NHPN processing Step 1
# Select only the roads in western and border states, of the appropriate classes
#----------------------------------------------------------------------------------


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


RoadFile = "/home/ellen/OneDrive/MyDocs/PycharmProjects/NHPN/Highways/NHPNLine.shp"
ALLRoads = qc.QgsVectorLayer(RoadFile, "A1_roads", 'ogr')

if ALLRoads.isValid():
    print "Layer is fine"

#Specifying the road functionality classes
#F_list = [1, 2, 6, 7, 8, 9, 11, 12, 14, 16, 17, 19]
A_list = (1, 2, 6, 11, 12, 14, 16) #arterial roads
C_list = (7, 8, 17) #collector roads

#Specifying the western and border states
state_list = (4, 6, 8, 16, 20, 30, 31, 32, 35, 38, 40, 41, 46, 48, 49, 53, 56) #western states and border states

#Get arterial roads:
selected_ids = []
STidx = ALLRoads.fieldNameIndex('STFIPS')
Fidx = ALLRoads.fieldNameIndex('FCLASS')

for feature in ALLRoads.getFeatures():
    attrs = feature.attributes()
    if (attrs[STidx] in state_list) & (attrs[Fidx] in A_list):
        selected_ids.append(feature.id())

ALLRoads.setSelectedFeatures(selected_ids)

#Write to shape file:
#NOTE: bool onlySelected = True
error = qc.QgsVectorFileWriter.writeAsVectorFormat(ALLRoads, "Arterial_roads.shp", "CP1250", None, "ESRI Shapefile", True)

if error == qc.QgsVectorFileWriter.NoError:
    print("Arterial roads: success!")

#Reprojection:
#processing.runalg("qgis:reprojectlayer", r"D:\testing\cities.shp", "epsg:3856", r"D:\testing\cities_prj.shp")
processing.runalg("qgis:reprojectlayer", "Arterial_roads.shp", "epsg:4269", "Arterial_roads_proj.shp")

# crsSrc = QgsCoordinateReferenceSystem(4326)
# crsDest = QgsCoordinateReferenceSystem(4269)
#
# xform = QgsCoordinateTransform(crsSrc, crsDest)
#
# Art_layer = QgsVectorLayer("Arterial_roads.shp", "Art_roads", 'ogr')
#
# if Art_layer.isValid():
#     print "Art_Layer is fine"
#
# #https://gis.stackexchange.com/questions/109235/how-to-make-crs-transformation-of-a-qgsvectorlayer
#
# feats = []
# features = Art_layer.getFeatures()
# for f in features:
#     try:
#         #print(f)
#         g = f.geometry()
#         g.transform(xform)
#         f.setGeometry(g)
#         feats.append(f)
#     except:
#         print("this feature is corrupt")
#
# #uri2 = "linestring?crs=epsg:32633&field=id:integer"
# #scratchLayer2 = QgsVectorLayer(uri2, "Scratch point layer2",  "memory")
# uri= "linestring?crs=epsg:4269&field=id:integer"
# Art_layer2 = QgsVectorLayer(uri, "art_2", "ogr")
#
# Art_layer2.dataProvider().addFeatures(feats)
#
# proj_error = QgsVectorFileWriter.writeAsVectorFormat(Art_layer2, "Arterial_roads_proj.shp", "CP1250", None, "ESRI Shapefile", True)
#
# if proj_error == QgsVectorFileWriter.NoError:
#     print("Arterial roads reprojection: success!")


ALLRoads.removeSelection()

#Get collector roads:
selected_ids2 = []

for feature in ALLRoads.getFeatures():
    attrs = feature.attributes()
    if (attrs[STidx] in state_list) & (attrs[Fidx] in C_list):
        selected_ids2.append(feature.id())

ALLRoads.setSelectedFeatures(selected_ids2)

#Write to shape file:
#NOTE: bool onlySelected = True
error = qc.QgsVectorFileWriter.writeAsVectorFormat(ALLRoads, "Collector_roads.shp", "CP1250", None, "ESRI Shapefile", True)

if error == qc.QgsVectorFileWriter.NoError:
    print("Collector roads: success!")

processing.runalg("qgis:reprojectlayer", "Collector_roads.shp", "epsg:4269", "Collector_roads_proj.shp")

# Col_layer = QgsVectorLayer("Collector_roads.shp", "Col_roads", 'ogr')
#
# if Col_layer.isValid():
#     print "Col_Layer is fine"
#
# feats = []
# for f in Col_layer.getFeatures():
#     try:
#         #print(f)
#         g = f.geometry()
#         g.transform(xform)
#         f.setGeometry(g)
#         feats.append(f)
#     except:
#         print("this feature is corrupt")
#
# uri= "linestring?crs=epsg:4269&field=id:integer"
# Col_layer2 = QgsVectorLayer(uri, "col_2", "ogr")
#
# Col_layer2.dataProvider().addFeatures(feats)
#
# proj_error = QgsVectorFileWriter.writeAsVectorFormat(Col_layer2, "Collector_roads_proj.shp", "CP1250", None, "ESRI Shapefile", True)
#
# if proj_error == QgsVectorFileWriter.NoError:
#     print("Collector roads reprojection: success!")
#

#Documentation: https://gis.stackexchange.com/questions/201751/projection-conversion-of-coordinate-pairs-in-python/201922
