### Author: Ellen Considine

library(sp)
library(raster)
library(stringr)

#Read in data
shp<- shapefile("~/Data/US_2010_census_tracts_NAD83.shp") #FIPS have leading zeros
Loc_data<- read.csv("Locations.csv") 
PD<- read.csv("~/Data/Census_2010_pop_density.csv")

#Merge open source PD data with shp
PD$Geo_FIPS<- sapply(PD$Geo_FIPS, as.character)
PD$Geo_FIPS<- sapply(PD$Geo_FIPS, function(y){if(nchar(y) == 10){paste0("0", y)}else{y}})

SHP<- merge(shp, PD, by.x = "FIPS", by.y = "Geo_FIPS")

fine<- SHP[-which(is.na(SHP$SE_T002_001)),]

#Investigate

pop_stat$TRACT<- str_pad(as.character(pop_stat$Tract_code), width= 6, side="left", pad="0")
pop_stat$STATE_FIPS<- str_pad(as.character(pop_stat$State_FIPS), width= 2, side="left", pad="0")
pop_stat$CNTY_FIPS<- str_pad(as.character(pop_stat$County_FIPS), width= 3, side="left", pad="0")

no_match<- anti_join(as.data.frame(fine[,c("STATE_FIPS", "CNTY_FIPS", "TRACT", "SE_T002_001")]), pop_stat[,c("STATE_FIPS", "CNTY_FIPS", "TRACT")], by = c("STATE_FIPS", "CNTY_FIPS", "TRACT"))

west_no_match<- no_match[which(no_match$STATE_FIPS %in% c("04", "06", "08", "16",
                                                          "30", "32", "35", "41",
                                                          "49", "53", "56")),]
no_pop_locs$TRACT<- str_pad(as.character(no_pop_locs$Tract_code), width= 6, side="left", pad="0")
no_pop_locs$STATE_FIPS<- str_pad(as.character(no_pop_locs$State_FIPS), width= 2, side="left", pad="0")
no_pop_locs$CNTY_FIPS<- str_pad(as.character(no_pop_locs$County_FIPS), width= 3, side="left", pad="0")

lost_ones<- anti_join(no_pop_locs, west_no_match, by = c("STATE_FIPS", "CNTY_FIPS", "TRACT"))

got_these<- inner_join(no_pop_locs, west_no_match, by = c("STATE_FIPS", "CNTY_FIPS", "TRACT"))

#Prepare points
coords<- as.data.frame(lost_ones[,c("Lon", "Lat")]) #c("Longitude", "Latitude") 
pointsSP<- SpatialPoints(coords, proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

#Overlay -- this takes a while
indices<- over(pointsSP, fine)
DATA<- cbind(lost_ones, Pop_density = indices[,"SE_T002_002"])

#Prepare data

DATA2<- DATA[-which(is.na(DATA$Pop_density)),]
names(got_these)[10]<- "Pop_density"
new_PD<- rbind(got_these, DATA2)

write.csv(new_PD[,c("Lon", "Lat", "State_FIPS", "County_FIPS", "Tract_code", "Pop_density")],
          "Extra_PD.csv", row.names = FALSE)

Lost_ones<- anti_join(lost_ones, DATA2, by = c("Lon","Lat"))
write.csv(Lost_ones, "New_lost_PD.csv", row.names = FALSE)
