library(dismo)
library(rgdal)
library(raster)

#LINEAR

#Read in shapefile and raster
shp<- readOGR(dsn = "C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\test_data", layer = "linear_2008_092_0715") 
raster<- raster("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\test_data\\test_linear.tif")

#Visuals
hist(raster)
breaks<- quantile(raster)
plot(raster, breaks = breaks, col = terrain.colors(4))
plot(raster)

#They're already in the same coordinate system, but if not:
projection(shp)<- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
shp.rs<- spTransform(shp, CRS(projection(raster)))

#Extract values
#head(shp)
data<- data.frame(coordinates(shp.rs), shp.rs$aod, extract(raster, shp.rs))
names(data)<- c("x", "y", "shp_aod", "raster_aod")
#check:
sum(!is.na(data[,4])) #there are 238187 non-NA values

differences<- data[,3] - data[,4]

mean(differences, na.rm = TRUE) #-0.0153513
sd(differences, na.rm = TRUE) #1.222049
hist(differences, main = "Differences for Linear", ylim = c(0, 90000))


#NEAREST

#Read in other raster
raster2<- raster("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\test_data\\test_nearest.tif")

#Visuals
hist(raster2)
breaks<- quantile(raster2)
plot(raster2, breaks = breaks, col = terrain.colors(4))
plot(raster2)

#They're already in the same coordinate system, but if not:
projection(shp)<- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
shp.rs<- spTransform(shp, CRS(projection(raster2)))

#Extract values
#head(shp)
data<- data.frame(coordinates(shp.rs), shp.rs$aod, extract(raster2, shp.rs))
names(data)<- c("x", "y", "shp_aod", "raster_aod")
#check:
sum(!is.na(data[,4])) #there are 238187 non-NA values

differences<- data[,3] - data[,4]

mean(differences, na.rm = TRUE) #-0.02136704
sd(differences, na.rm = TRUE) #1.23744
hist(differences, main = "Differences for Nearest Neighbor", ylim = c(0, 90000))


#INVERSE DISTANCE

#Read in other raster
raster3<- raster("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\test_data\\test_invdist.tif")

#Visuals
hist(raster3)
breaks<- quantile(raster3)
plot(raster3, breaks = breaks, col = terrain.colors(4))
plot(raster3)

#They're already in the same coordinate system, but if not:
projection(shp)<- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
shp.rs<- spTransform(shp, CRS(projection(raster3)))

#Extract values
#head(shp)
data<- data.frame(coordinates(shp.rs), shp.rs$aod, extract(raster3, shp.rs))
names(data)<- c("x", "y", "shp_aod", "raster_aod")
#check:
sum(!is.na(data[,4])) #there are 238187 non-NA values

differences<- data[,3] - data[,4]

mean(differences, na.rm = TRUE) #-0.005777424
sd(differences, na.rm = TRUE) # 1.072843
hist(differences, main = "Differences for Inverse Distance" , ylim = c(0, 90000))



