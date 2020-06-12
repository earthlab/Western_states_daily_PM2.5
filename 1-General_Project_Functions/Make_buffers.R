### Author: Ellen Considine

library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(dismo)

##NLCD:
my_rast<- raster("~/Data/nlcd_reclass2.tif")
# my_proj<- proj4string(my_rast)

#Read in monitors:
all_monts<- read.csv("Locations.csv")
mont_csv<- unique(all_monts[,c("Lat", "Lon")])
pos<- which((mont_csv[, "Lat"] <= 50) & (mont_csv[, "Lat"] >= 25) & 
              (mont_csv[, "Lon"] <= -101) & (mont_csv[, "Lon"] >= -126)) #bounding box
mont_csv<- mont_csv[pos,]
mont_geom<- mont_csv
coordinates(mont_geom)<- c("Lon", "Lat")
proj4string(mont_geom)<- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")

monitors<- SpatialPointsDataFrame(mont_geom, mont_csv)
Monitors<- spTransform(monitors, CRS(proj4string(my_rast)))

Buffers<- c(1000, 5000, 10000)

for(b in 1:length(Buffers)){
  gbuffer<- gBuffer(Monitors, width = Buffers[b], byid= TRUE)
  
  writeOGR(obj = gbuffer, dsn = "~/Data/", 
           driver = "ESRI Shapefile", 
           layer=paste0("NLCD_buffer_", Buffers[b]/1000, "km"))
}


##Active Fires:
all_monts<- read.csv("Locations.csv")
mont_csv<- unique(all_monts[,c("Lat", "Lon")])
pos<- which((mont_csv[, "Lat"] <= 50) & (mont_csv[, "Lat"] >= 25) & 
              (mont_csv[, "Lon"] <= -101) & (mont_csv[, "Lon"] >= -126)) #bounding box
mont_csv<- mont_csv[pos,]
mont_geom<- mont_csv
coordinates(mont_geom)<- c("Lon", "Lat")
proj4string(mont_geom)<- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
monitors<- SpatialPointsDataFrame(mont_geom, mont_csv)
Monitors<- spTransform(monitors, CRS(proj4string(my_rast)))

Buffers<- c(25000, 50000, 100000, 500000)

for(b in 1:length(Buffers)){
  gbuffer<- gBuffer(Monitors, width = Buffers[b], byid= TRUE)
  
  shp_ready<- spTransform(gbuffer, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
  
  writeOGR(obj = shp_ready, dsn = "~/Data/", 
           driver = "ESRI Shapefile", 
           layer=paste0("AF_buffer_", Buffers[b]/1000, "km"))
}







