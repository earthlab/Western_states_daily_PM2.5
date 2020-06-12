### Author: Ellen Considine

library(raster)
library(rgdal)
library(sp)
library(rgeos)

# #Read in roads:
# roads<- shapefile("NHPNLine.shp")
# 
# #Note: we have to transform (project) so the units are in meters
# Roads<- spTransform(roads, CRS("+proj=aea +Lat_1=29.5 +Lat_2=45.5 +Lat_0=37.5 +Lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "))
# 
# #Select Western and border states:
# Roads<- Roads[Roads$STATE_CODE %in% c(4, 6, 8, 16, 20, 30, 31, 32, 35, 38, 40, 41, 46, 48, 49, 53, 56),]
# 
# #Select arterial and collector roads:
# A_roads<- Roads[Roads$FCLASS %in% c(1, 2, 6, 11, 12, 14, 16),]
# C_roads<- Roads[Roads$FCLASS %in% c(7, 8, 17),]
# 
# writeOGR(obj= A_roads, layer= "Arterial roads", driver="ESRI Shapefile")
# writeOGR(obj= C_roads, layer= "Collector roads", driver="ESRI Shapefile")

A_roads<- shapefile("Arterial roads.shp")
C_roads<- shapefile("Collector roads.shp")

#Read in monitor locations:
all_monts<- read.csv("Locations.csv") 
mont_csv<- unique(all_monts[,c("Lat", "Lon")])

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
  Afilename <- paste("Arterial_road_sum_", Buffers[b], ".csv", sep = "")
  write.csv(Aoutput, Afilename)
  
  #COLLECTOR
  #Sum road lengths which intersect
  CspInt<- over(gbuffer, C_roads[,c("MILES", "KM")], fn= sum)
  #check: sum(!is.na(CspInt[,1]))
  
  Coutput<- cbind(mont_csv, CspInt)
  Cfilename<- paste0("Collector_road_sum_", Buffers[b], ".csv", sep = "")
  write.csv(Coutput, Cfilename)
  
  print(paste0("Buffer = ", Buffers[b], sep = ""))
}



#Pulling both types together:
first_A<- read.csv("Arterial_road_sum_100.csv")
second_A<- read.csv("Arterial_road_sum_250.csv")
third_A<- read.csv("Arterial_road_sum_500.csv")
fourth_A<- read.csv("Arterial_road_sum_1000.csv")
first_C<- read.csv("Collector_road_sum_100.csv")
second_C<- read.csv("Collector_road_sum_250.csv")
third_C<- read.csv("Collector_road_sum_500.csv")
fourth_C<- read.csv("Collector_road_sum_1000.csv")

# output<- data.frame("Lon", "Lat", "Date", "A_100", "C_100", "Both_100", "A_250", "C_250", "Both_250", "A_500", "C_500", "Both_500", "A_1000", "C_1000", "Both_1000")
Lon<- as.numeric(first_A$Lon)
Lat<- as.numeric(first_A$Lat)
A_100<- replace(first_A$KM, which(is.na(first_A$KM)), 0)
C_100<- replace(first_C$KM, which(is.na(first_C$KM)), 0)
Both_100<- A_100 + C_100
A_250<- replace(second_A$KM, which(is.na(second_A$KM)), 0)
C_250<- replace(second_C$KM, which(is.na(second_C$KM)), 0)
Both_250<- A_250 + C_250
A_500<- replace(third_A$KM, which(is.na(third_A$KM)), 0)
C_500<- replace(third_C$KM, which(is.na(third_C$KM)), 0)
Both_500<- A_500 + C_500
A_1000<- replace(fourth_A$KM, which(is.na(fourth_A$KM)), 0)
C_1000<- replace(fourth_C$KM, which(is.na(fourth_C$KM)), 0)
Both_1000<- A_1000 + C_1000

library(dplyr)

dates_data<- read.csv("Locations_Dates.csv")
dates_data<- as.data.frame(dates_data)
dates_data$Lat<- as.numeric(dates_data$Lat)
dates_data$Lon<- as.numeric(dates_data$Lon)
All<- cbind(Lat, Lon, A_100, C_100, Both_100, A_250, C_250, Both_250, A_500, C_500, Both_500, A_1000, C_1000, Both_1000)
All<- as.data.frame(All)
All$Lon<- as.numeric(All$Lon)
All$Lat<- as.numeric(All$Lat)

write_out<- left_join(dates_data, All)
join(dates_data, All, by = c("Lat", "Lon"), type = "left", match = "all")

write.csv(write_out, "Highways.csv")


