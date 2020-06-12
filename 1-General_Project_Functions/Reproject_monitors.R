### Author: Ellen Considine

library(dismo)
library(rgdal)
library(raster)

##Reproject when they're all from the same datum (NAD83):
Stat<- read.csv("Locations.csv")

for_proj<- Stat

coordinates(for_proj)<- c("Lon", "Lat")
proj4string(for_proj)<- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
albers<- spTransform(for_proj, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
Albers<- coordinates(albers)
colnames(Albers)<- c("Easting", "Northing")
Final<- cbind(Stat, Albers)
row.names(Final)<- c()

write.csv(Final, "Reprojected_locations.csv", row.names=FALSE)

####################################################

##Reproject all monitor locations when they're coming from different datums:
monitors<- read.csv("Locations.csv", stringsAsFactors = FALSE)
df84<- monitors[monitors$Datum == "WGS84",]
df27<- monitors[monitors$Datum == "NAD27",]
df83<- monitors[monitors$Datum == "NAD83",]

coordinates(df84) <- c("Longitude", "Latitude")
proj4string(df84) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
DF84 <- spTransform(df84, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
summary(DF84)

coordinates(df27) <- c("Longitude", "Latitude")
proj4string(df27) <- CRS("+proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs")
DF27 <- spTransform(df27, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
summary(DF27)

DF83<- df83[,1:2]

table_84<- cbind(coordinates(df84), df84$Datum, coordinates(DF84))
colnames(table_84)<- c("old_lon", "old_lat", "Datum", "Lon", "Lat")
table_83<- cbind(df83,DF83)
table_83<- table_83[,c(2, 1, 3, 5, 4)]
colnames(table_83)<- c("old_lon", "old_lat", "Datum", "Lon", "Lat")
table_27<- cbind(coordinates(df27), df27$Datum, coordinates(DF27))
colnames(table_27)<- c("old_lon", "old_lat", "Datum", "Lon", "Lat")

All<- rbind(table_84, table_83, table_27, stringsAsFactors = FALSE) #1 = WGS84, 2 = NAD83, 3 = NAD27
for(i in c(1,2,4,5)){
  All[,i]<- as.numeric(All[,i])
}
all_nad83<- All[,c("Lon", "Lat")]
coordinates(all_nad83)<- c("Lon", "Lat")
proj4string(all_nad83)<- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
albers<- spTransform(all_nad83, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
Albers<- coordinates(albers)
colnames(Albers)<- c("Easting", "Northing")
Final<- cbind(All, Albers)
row.names(Final)<- c()

write.csv(Final, "Reprojected_locations.csv")

#################################################
##Update all location/date pairs

loc_date<- read.csv("Locations_Dates.csv", stringsAsFactors = FALSE)
loc_date$Lat<- Final[match(loc_date$Latitude, Final$old_lat), 'Lat']
loc_date$Lon<- Final[match(loc_date$Longitude, Final$old_lon), 'Lon']
loc_date$Northing<- Final[match(loc_date$Latitude, Final$old_lat), 'Northing']
loc_date$Easting<- Final[match(loc_date$Longitude, Final$old_lon), 'Easting']

write.csv(loc_date, "Reprojected_locations_dates.csv")



