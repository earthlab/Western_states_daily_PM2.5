#Data from https://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt

data<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\2010_county_pop_centroids.txt")
states<- c(4, 6, 8, 16, 30, 32, 35, 41, 49, 53, 56) 
#check: unique(data[,c('STATEFP','STNAME')])
output<- data[data$STATEFP %in% states,]
write.csv(output, "C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\2010_western_county_pop_centroids.csv")

#Plot:
library(raster)
library(rgdal)
library(sp)
library(rgeos)

coordinates(output)<- c("LONGITUDE", "LATITUDE")
proj4string(output)<- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
plot(output)
