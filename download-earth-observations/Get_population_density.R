library(sp)
library(raster)

#Read in data
shp<- shapefile("C:\\Users\\elco2649\\Documents\\Census_data\\US_2010_census_tracts_NAD83.shp")
MLdata<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_no-GASP_5-21-2019.csv")
PD<- read.csv("C:\\Users\\elco2649\\Documents\\Census_data\\Census_2010_pop_density.csv")

#Prepare points and polygons
coords<- as.data.frame(MLdata[,c("Longitude", "Latitude")])
pointsSP<- SpatialPoints(coords, proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

#Merge open source PD data with shp
SHP<- merge(shp, PD, by.x = "FIPS", by.y = "Geo_FIPS")

#Overlay -- this takes a while
indices<- over(pointsSP, SHP)

#Merge data
DATA<- cbind(MLdata, Pop_density = indices[,"SE_T002_001"])
# DATA$Log_PM2.5_Obs<- log(DATA$PM2.5_Obs)

#Write to file
write.csv(DATA, "C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_6-10-2019.csv")

