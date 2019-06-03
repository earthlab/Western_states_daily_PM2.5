library(sp)
library(raster)

#Read in data
shp<- shapefile("C:\\Users\\elco2649\\Documents\\Census_data\\US_2010_census_tracts_NAD83.shp") #Got this from ArcMap
MLdata<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_no-GASP_5-21-2019.csv")
PD<- read.csv("C:\\Users\\elco2649\\Documents\\Census_data\\Census_2010_pop_density.csv")

#Prepare points and polygons
coords<- as.data.frame(MLdata[,c("Longitude", "Latitude")])
pointsSP<- SpatialPoints(coords, proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

#Overlay -- this takes a while
indices<- over(pointsSP, shp)

#Merge data
DATA<- cbind(MLdata, Pop_density = indices[,"POP_SQMI"])
DATA$Log_PM2.5_Obs<- log(DATA$PM2.5_Obs)

#Write to file
write.csv(DATA, "C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_5-30-2019.csv")

