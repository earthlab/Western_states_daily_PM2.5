library(raster)
library(rgdal)
library(sp)
library(rgeos)

setwd("C:/Users/elco2649/Documents/NHPN/")

#Read in roads:
roads<- shapefile("C:/Users/elco2649/Documents/NHPN/NHPNLine.shp")
# roads<- shapefile("C:/Users/ellen/OneDrive/MyDocs/Pycharm Projects/NHPN/qgis_output/Collector_roads.shp")

#Note: we have to transform so the units are in meters
Roads<- spTransform(roads, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "))

#Select Western and border states:
Roads<- Roads[Roads$STATE_CODE %in% c(4, 6, 8, 16, 20, 30, 31, 32, 35, 38, 40, 41, 46, 48, 49, 53, 56),]

#Select arterial and collector roads:
A_roads<- Roads[Roads$FCLASS %in% c(1, 2, 6, 11, 12, 14, 16),]
C_roads<- Roads[Roads$FCLASS %in% c(7, 8, 17),]


#Read in monitors:
all_monts<- read.csv("C:/Users/elco2649/Documents/NHPN/Projected_locations_part_a.csv")
mont_csv<- unique(all_monts[,c("Lat", "Lon")])
#mont_csv<- read.csv("Unique_monitors.csv")
pos<- which((mont_csv[, "Lat"] <= 50) & (mont_csv[, "Lat"] >= 25) & 
              (mont_csv[, "Lon"] <= -101) & (mont_csv[, "Lon"] >= -126)) #bounding box
mont_csv<- mont_csv[pos,]
mont_geom<- mont_csv
coordinates(mont_geom)<- c("Lon", "Lat")
proj4string(mont_geom)<- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")

monitors<- SpatialPointsDataFrame(mont_geom, mont_csv)
Monitors<- spTransform(monitors, CRS( proj4string(A_roads)))

#Make buffers 
#Note: this projection is in meters
Buffers<- c(100, 250, 500, 1000)

for(b in 1:length(Buffers)){
  gbuffer<- gBuffer(Monitors, width = Buffers[b], byid= TRUE)
  
  #ARTERIAL
  #Sum road lengths which intersect
  AspInt<- over(gbuffer, A_roads[,c("MILES", "KM")], fn= sum)
  #check: sum(!is.na(AspInt[,1]))
  
  Aoutput<- cbind(mont_csv, AspInt)
  Afilename <- paste("C:/Users/elco2649/Documents/NHPN/R_output/Arterial_road_sum_", Buffers[b], ".csv", sep = "")
  write.csv(Aoutput, Afilename)
  
  #COLLECTOR
  #Sum road lengths which intersect
  CspInt<- over(gbuffer, C_roads[,c("MILES", "KM")], fn= sum)
  #check: sum(!is.na(CspInt[,1]))
  
  Coutput<- cbind(mont_csv, CspInt)
  Cfilename<- paste0("C:/Users/elco2649/Documents/NHPN/R_output/Collector_road_sum_", Buffers[b], ".csv", sep = "")
  write.csv(Coutput, Cfilename)
  
  #BOTH types of roads: 
  Broads<- Reduce(function(x,y){x[is.na(x)] <- 0; y[is.na(y)] <- 0; x + y}, list(AspInt, CspInt))
  Boutput<- cbind(mont_csv, Broads)
  Bfilename<- paste0("C:/Users/elco2649/Documents/NHPN/R_output/Both_types_road_sum_", Buffers[b], ".csv", sep = "")
  write.csv(Boutput, Bfilename)
  
  print(paste0("Buffer = ", Buffers[b], sep = ""))
}
