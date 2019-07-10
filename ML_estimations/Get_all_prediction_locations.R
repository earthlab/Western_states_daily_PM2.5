library(raster)
library(plyr)

county<- shapefile("C:\\Users\\elco2649\\Documents\\Census_data\\tl_2010_us_county10.shp")
zips<- shapefile("C:\\Users\\elco2649\\Documents\\Census_data\\tl_2010_us_zcta510.shp")

states<- c(4,6,8,16,30,35,32,41,49,53,56)

county$STATEFP10<- as.numeric(county$STATEFP10)
west_county<- county[which(county$STATEFP10 %in% states), c("INTPTLAT10", "INTPTLON10", "STATEFP10", "COUNTYFP10")]
names(west_county)<- c("Latitude", "Longitude", "State_FIPS", "County_FIPS")

zips$INTPTLAT10<- as.numeric(zips$INTPTLAT10)
zips$INTPTLON10<- as.numeric(zips$INTPTLON10)
west_zips<- zips[which((zips$INTPTLAT10 < 50) & (zips$INTPTLAT10 > 30)
                  & (zips$INTPTLON10 > -126) & (zips$INTPTLON10 < -100)), c("ZCTA5CE10", "INTPTLAT10", "INTPTLON10")]
names(west_zips)<- c("ZCTA5_code", "Latitude", "Longitude")


#Census Tracts are a bit trickier (they don't come in one national file)

#Having trouble with the FTP link, so I downloaded the state CT files manually...
CT<- shapefile("C:\\Users\\elco2649\\Documents\\Census_data\\State_Census_tracts\\tl_2010_04_tract10.shp")
CT$STATEFP10<- as.numeric(CT$STATEFP10)
west_CT<- CT[which(CT$STATEFP10 %in% states), c("INTPTLAT10", "INTPTLON10", "STATEFP10", "COUNTYFP10", "TRACTCE10")]

for(s in states[-1]){
  if(s < 10){
    filename<- paste0("C:\\Users\\elco2649\\Documents\\Census_data\\State_Census_tracts\\tl_2010_0", s, "_tract10.shp")
  }else{
    filename<- paste0("C:\\Users\\elco2649\\Documents\\Census_data\\State_Census_tracts\\tl_2010_", s, "_tract10.shp")
  }
  shp<- shapefile(filename)
  shp$STATEFP10<- as.numeric(shp$STATEFP10)
  west_CT<- rbind(west_CT, shp[which(shp$STATEFP10 %in% states), 
                               c("INTPTLAT10", "INTPTLON10", "STATEFP10", "COUNTYFP10", "TRACTCE10")])
}

names(west_CT)<- c("Latitude", "Longitude", "State_FIPS", "County_FIPS", "Tract_code")


west_pred_locs<- rbind.fill(sapply(list(west_county, west_CT, west_zips), as.data.frame))

write.csv(west_pred_locs, "C:\\Users\\elco2649\\Documents\\Census_data\\West_prediction_locations.csv",
          row.names = FALSE)

Date<- seq.Date(as.Date("2008-01-01"), as.Date("2018-12-31"), by = "day")

west_pred_loc_dates<- cbind(sort(rep(Date, dim(west_pred_locs)[1])), west_pred_locs)

# write.csv(west_pred_loc_dates, "C:\\Users\\elco2649\\Documents\\Census_data\\West_prediction_locations_dates.csv",
          # row.names = FALSE)

for(s in c("county", "zips")){
  data<- get(paste0("west_", s))
  write.csv(cbind(Date = sort(rep(Date, dim(data)[1])), as.data.frame(data)), paste0("C:\\Users\\elco2649\\Documents\\Census_data\\West_", s, "_prediction_locations_dates.csv"),
            row.names = FALSE)
}

data<- west_CT
county_LDs<- cbind(Date = sort(rep(Date, dim(data)[1])), as.data.frame(data))
N<- dim(county_LDs)[1]
n<- round(N/3)

for(i in 1:3){
  write.csv(county_LDs[((i-1)*n + 1):(i*n), ], paste0("C:\\Users\\elco2649\\Documents\\Census_data\\West_CT_subset", i, "_prediction_locations_dates.csv"),
            row.names = FALSE)
}

###############################
# #Merge only lat,lon
# CO<- read.csv("C:\\Users\\elco2649\\Documents\\Census_data\\West_county_prediction_locations_dates.csv")
# CT1<- read.csv("C:\\Users\\elco2649\\Documents\\Census_data\\West_CT_subset1_prediction_locations_dates.csv")
# CT2<- read.csv("C:\\Users\\elco2649\\Documents\\Census_data\\West_CT_subset2_prediction_locations_dates.csv")
# CT3<- read.csv("C:\\Users\\elco2649\\Documents\\Census_data\\West_CT_subset3_prediction_locations_dates.csv")
# ZP<- read.csv("C:\\Users\\elco2649\\Documents\\Census_data\\West_zips_prediction_locations_dates.csv")
# 
# ALL<- rbind(CO[,c("Longitude", "Latitude")], CT1[,c("Longitude", "Latitude")], CT2[,c("Longitude", "Latitude")],
#             CT3[,c("Longitude", "Latitude")], ZP[,c("Longitude", "Latitude")])
# 
# write.csv(ALL, "C:\\Users\\elco2649\\Documents\\Census_data\\Final_batch_g\\West_batch_g_locations_dates.csv")

##############################
library(sp)

#Transform
locs<- read.csv("C:\\Users\\elco2649\\Documents\\Census_data\\West_prediction_locations.csv")
coordinates(locs)<- c("Longitude", "Latitude")

monitors<- SpatialPoints(coordinates(locs), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
Monitors<- spTransform(monitors, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

locs<- read.csv("C:\\Users\\elco2649\\Documents\\Census_data\\West_prediction_locations.csv")
NM<- names(locs)
Locs<- cbind(locs, as.data.frame(Monitors))
names(Locs)<- c(NM, "Easting", "Northing")

write.csv(Locs, "C:\\Users\\elco2649\\Documents\\Census_data\\West_prediction_locations_with_projections.csv", row.names = FALSE)
