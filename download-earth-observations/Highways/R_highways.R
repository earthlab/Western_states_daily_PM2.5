library(raster)
library(rgdal)
library(sp)
library(rgeos)

setwd("C:/Users/elco2649/Documents/NHPN/")

# #Read in roads:
# roads<- shapefile("C:/Users/elco2649/Documents/NHPN/NHPNLine.shp")
# # roads<- shapefile("C:/Users/ellen/OneDrive/MyDocs/Pycharm Projects/NHPN/qgis_output/Collector_roads.shp")
# 
# #Note: we have to transform so the units are in meters
# Roads<- spTransform(roads, CRS("+proj=aea +Latitude_1=29.5 +Latitude_2=45.5 +Latitude_0=37.5 +Longitude_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "))
# 
# #Select Western and border states:
# Roads<- Roads[Roads$STATE_CODE %in% c(4, 6, 8, 16, 20, 30, 31, 32, 35, 38, 40, 41, 46, 48, 49, 53, 56),]
# 
# #Select arterial and collector roads:
# A_roads<- Roads[Roads$FCLASS %in% c(1, 2, 6, 11, 12, 14, 16),]
# C_roads<- Roads[Roads$FCLASS %in% c(7, 8, 17),]
# 
# writeOGR(obj= A_roads, layer= "Arterial roads", "C:/Users/elco2649/Documents/NHPN/R_output/A_roads", driver="ESRI Shapefile")
# writeOGR(obj= C_roads, layer= "Collector roads", "C:/Users/elco2649/Documents/NHPN/R_output/C_roads", driver="ESRI Shapefile")

A_roads<- shapefile("C:/Users/elco2649/Documents/NHPN/R_output/A_roads/Arterial roads.shp")
C_roads<- shapefile("C:/Users/elco2649/Documents/NHPN/R_output/C_roads/Collector roads.shp")

#Read in monitors:
all_monts<- read.csv("C:/Users/elco2649/Documents/locations_part_c.csv") #change back to Projected... for part b
mont_csv<- unique(all_monts[,c("Latitude", "Longitude")]) #change to Lat and Lon for part b
#mont_csv<- read.csv("Unique_monitors.csv")
pos<- which((mont_csv[, "Latitude"] <= 50) & (mont_csv[, "Latitude"] >= 25) & 
              (mont_csv[, "Longitude"] <= -101) & (mont_csv[, "Longitude"] >= -126)) #bounding box
mont_csv<- mont_csv[pos,]
mont_geom<- mont_csv
coordinates(mont_geom)<- c("Longitude", "Latitude")
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
  
  # #BOTH types of roads: 
  # Broads<- Reduce(function(x,y){x[is.na(x)] <- 0; y[is.na(y)] <- 0; x + y}, list(AspInt, CspInt))
  # Boutput<- cbind(mont_csv, Broads)
  # Bfilename<- paste0("C:/Users/elco2649/Documents/NHPN/R_output/Both_types_road_sum_", Buffers[b], ".csv", sep = "")
  # write.csv(Boutput, Bfilename)
  
  print(paste0("Buffer = ", Buffers[b], sep = ""))
}



#Pulling both types together:
first_A<- read.csv("C:/Users/elco2649/Documents/NHPN/R_output/Arterial_road_sum_100.csv")
second_A<- read.csv("C:/Users/elco2649/Documents/NHPN/R_output/Arterial_road_sum_250.csv")
third_A<- read.csv("C:/Users/elco2649/Documents/NHPN/R_output/Arterial_road_sum_500.csv")
fourth_A<- read.csv("C:/Users/elco2649/Documents/NHPN/R_output/Arterial_road_sum_1000.csv")
first_C<- read.csv("C:/Users/elco2649/Documents/NHPN/R_output/Collector_road_sum_100.csv")
second_C<- read.csv("C:/Users/elco2649/Documents/NHPN/R_output/Collector_road_sum_250.csv")
third_C<- read.csv("C:/Users/elco2649/Documents/NHPN/R_output/Collector_road_sum_500.csv")
fourth_C<- read.csv("C:/Users/elco2649/Documents/NHPN/R_output/Collector_road_sum_1000.csv")

# output<- data.frame("Lon", "Lat", "Date", "A_100", "C_100", "Both_100", "A_250", "C_250", "Both_250", "A_500", "C_500", "Both_500", "A_1000", "C_1000", "Both_1000")
Longitude<- as.numeric(first_A$Longitude)
Latitude<- as.numeric(first_A$Latitude)
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

library(plyr)
#dates_data<- read.csv("C:\\Users\\elco2649\\Documents\\PM25_Step3_part_b_Locations_Dates_Projected.csv")
dates_data<- read.csv("C:\\Users\\elco2649\\Documents\\locations_dates_part_c.csv")
dates_data<- as.data.frame(dates_data)
dates_data$Latitude<- as.numeric(dates_data$Latitude)
dates_data$Longitude<- as.numeric(dates_data$Longitude)
All<- cbind(Latitude, Longitude, A_100, C_100, Both_100, A_250, C_250, Both_250, A_500, C_500, Both_500, A_1000, C_1000, Both_1000)
All<- as.data.frame(All)
All$Longitude<- as.numeric(All$Longitude)
All$Latitude<- as.numeric(All$Latitude)

write_out<- left_join(dates_data, All)
join(dates_data, All, by = c("Latitude", "Longitude"), type = "left", match = "all")

write.csv(write_out, "C:\\Users\\elco2649\\Documents\\Highways_part_c.csv")


