library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(dismo)

##NLCD:
my_rast<- raster("~/Data/nlcd_reclass2.tif")
# my_proj<- proj4string(my_rast)

#Read in monitors:
all_monts<- read.csv("Final_missing_stationary.csv")
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
           layer=paste0("NLCD_part-h_buffer_", Buffers[b]/1000, "km"))
}


##Active Fires:
all_monts<- read.csv("Final_missing_stationary.csv")
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
           layer=paste0("AF_part-h_buffer_", Buffers[b]/1000, "km"))
}


##Reproject for NDVI:
Stat<- read.csv("Final_missing_stationary.csv")

for_proj<- Stat

coordinates(for_proj)<- c("Lon", "Lat")
proj4string(for_proj)<- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
albers<- spTransform(for_proj, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
Albers<- coordinates(albers)
colnames(Albers)<- c("Easting", "Northing")
Final<- cbind(Stat, Albers)
row.names(Final)<- c()

write.csv(Final, "Reprojected_missing_points.csv", row.names=FALSE)






