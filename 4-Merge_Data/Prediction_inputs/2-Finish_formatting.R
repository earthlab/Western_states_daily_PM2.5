##Libraries:
library(dplyr)
library(multidplyr)
library(lubridate)
library(stringr)
library(future.apply)
library(parallel)
library(stringr)

source("Ellen_merging_functions.R")

##Data sets:
# pred_locs<- read.csv("~/Data/West_prediction_locations.csv")
# pred_locs$Lon<- round(pred_locs$Lon, 5)
# pred_locs$Lat<- round(pred_locs$Lat, 5)
# Locs<- unique(pred_locs[,c("Lon", "Lat")])
# 
# Stat<- read.csv("Stationary_variables.csv")
# Stat$State<- latlong2state(as.data.frame(Stat[,c("Lon", "Lat")]))
# all_stat<- inner_join(pred_locs, Stat, by = c("Lon", "Lat"))
# stat_vars<- c("Lon", "Lat", "State_FIPS", "County_FIPS", "Tract_code", "ZCTA5_code",
#               "Pop_density", "NLCD_1km", "NLCD_5km", "NLCD_10km", "Both_100", 
#               "Both_250", "Both_500", "Both_1000", "elevation", "State" )
# write.csv(all_stat[,stat_vars], "All_stationary.csv", row.names = FALSE)
Stat<- read.csv("All_stationary.csv")


##Time-varying variables:

load("MAIAC.RData")
write_state(MAIAC, "MAIAC")

load("NDVI_final.RData")
write_state(NDVI_final, "NDVI")

load("AF_final.RData")
AF_final$Lon<- round(AF_final$Lon, 4)
AF_final$Lat<- round(AF_final$Lat, 4)
AF_final$Date<- as.Date(AF_final$Date)
#Here, need to adjust the write_state function
write_state(AF_final, "AF") 

load("NAM.RData")
write_state(all_NAM, "NAM")

##CMAQ: 2008-2012

load("~/Data/CMAQ_08_12.RData")

plan(multiprocess, workers = 11)
options(future.globals.maxSize= 2000*1024^2)
future_sapply(west, function(x) knn_per_state(x, X1))

future_sapply(west, function(x) get_CMAQ_08_12(x))


##Second CMAQ set: 2013-2016

CM_2013 <- read.csv("Data/ds.input.cmaq.pm25.2013.csv")
CM_2014 <- read.csv("Data/ds.input.cmaq.pm25.2014.csv")
CM_2015 <- read.csv("Data/ds.input.cmaq.pm25.2015.csv")
CM_2016 <- read.csv("Data/ds.input.cmaq.pm25.2016.csv")

# CM_locs<- unique(CM_2013[,c("Lon", "Lat")]) #DF in knn_per_stat2
# write.csv(CM_locs, "CMAQ_13-16_locs.csv", row.names=FALSE)
CM_locs<- read.csv("CMAQ_13-16_locs.csv")

plan(multiprocess, workers = 11)
options(future.globals.maxSize= 5000*1024^2)
future_sapply(west, function(x) knn_per_state2(x, CM_locs))

future_sapply(west, function(x) get_CMAQ_13_16(x, CM_locs, CM_2013, "2013"))
future_sapply(west, function(x) get_CMAQ_13_16(x, CM_locs, CM_2014, "2014"))
future_sapply(west, function(x) get_CMAQ_13_16(x, CM_locs, CM_2015, "2015"))
future_sapply(west, function(x) get_CMAQ_13_16(x, CM_locs, CM_2016, "2016"))

#Get CMAQ 2013-2016 locations crossover files: first merge in terminal
for(s in State){
  print(s)
  locs<- read.csv(paste0("~/CMAQ_data/CMAQ_Locs_",s,".csv"))
  locs<- locs[which(locs$Lon != "Lon"),]
  locs$Lon<- as.numeric(as.character(locs$Lon))
  locs$Lat<- as.numeric(as.character(locs$Lat))
  locs$CM_row<- as.numeric(as.character(locs$CM_row))
  
  locs$CM_lon<- CM_locs[locs$CM_row,"Lon"]
  locs$CM_lat<- CM_locs[locs$CM_row,"Lat"]
  
  cmaq<- read.csv(paste0("~/FINAL_state_sets/CMAQ_FINAL_", s, ".csv"))
  cmaq<- cmaq[which(cmaq$Lon != "Lon"),]
  names(cmaq)[4]<- "all_CMAQ"
  cmaq$Date<- as.Date(as.character(cmaq$Date))
  cmaq[,c("Lon", "Lat")]<- apply(cmaq[,c("Lon", "Lat")],
                                 MARGIN = 2, function(x) as.numeric(as.character(x)) )
  Lon_pos<- match(cmaq$Lon, locs$CM_lon)
  Lat_pos<- match(cmaq$Lat, locs$CM_lat)
  
  cmaq$Lon[which(!is.na(Lon_pos))]<- locs$Lon[Lon_pos[which(!is.na(Lon_pos))]]
  cmaq$Lat[which(!is.na(Lat_pos))]<- locs$Lat[Lat_pos[which(!is.na(Lat_pos))]]
  write.csv(cmaq, paste0("~/FINAL_state_sets/CMAQ_FINAL2_", s, ".csv"), row.names=FALSE)
}

