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

# Stat<- read.csv("Stationary_variables.csv")
# Stat$State<- latlong2state(as.data.frame(Stat[,c("Lon", "Lat")]))
# all_stat<- inner_join(pred_locs, Stat, by = c("Lon", "Lat"))
# write.csv(all_stat, "All_stationary.csv", row.names = FALSE)
# Stat<- read.csv("All_stationary.csv") #Need both of these for second CMAQ batch

# Stat<- read.csv("Stationary_variables2.csv")
# Stat$State<- latlong2state(as.data.frame(Stat[,c("Lon", "Lat")]))
# # all_stat<- inner_join(pred_locs, Stat, by = c("Lon", "Lat"))
# write.csv(Stat, "All_stationary2.csv", row.names = FALSE)
# Stat<- read.csv("All_stationary2.csv")

# Stat<- read.csv("Stationary_variables3.csv")
# Stat$State<- latlong2state(as.data.frame(Stat[,c("Lon", "Lat")]))
# # all_stat<- inner_join(pred_locs, Stat, by = c("Lon", "Lat"))
# write.csv(Stat, "All_stationary3.csv", row.names = FALSE)
# Stat<- read.csv("All_stationary3.csv")

Stat1<- read.csv("All_stationary.csv")
Stat2<- read.csv("All_stationary2.csv")
Stat3<- read.csv("All_stationary3.csv")
stat_vars<- c("Lon", "Lat", "State_FIPS", "County_FIPS", "Tract_code", "ZCTA5_code",
              "Pop_density", "NLCD_1km", "NLCD_5km", "NLCD_10km", "Both_100", 
              "Both_250", "Both_500", "Both_1000", "elevation", "State" )
Stat<- rbind(Stat1[,stat_vars],
             Stat2[,stat_vars],
             Stat3[,stat_vars])
rm(list=c("Stat1", "Stat2", "Stat3"))


#Time-varying

load("MAIAC.RData")
# rm(list=setdiff(ls(), "MAIAC"))

# start<- Sys.time()
# DF<- inner_join(Stat, MAIAC, by = "Lon", "Lat")
# end<- Sys.time()
# end-start

write_state(MAIAC, "MAIAC2")

MAIAC<- read.csv("~/Data/MAIAC_part_h.csv")[-113,] #Another header...
MAIAC$Lon<- round(as.numeric(as.character(MAIAC$Lon)),5)
MAIAC$Lat<- round(as.numeric(as.character(MAIAC$Lat)),5)
MAIAC$MAIAC_AOD<- round(as.numeric(as.character(MAIAC$MAIAC_AOD)),5)
write_state(MAIAC, "MAIAC_h")


load("NDVI_final.RData")
write_state(NDVI_final, "NDVI2")

#For part h:
NDVI<- read.csv("~/Data/ndvi_mod13a3_part_h_extract.csv")
NDVI$Date<- rep(dates, 112)
write_state(NDVI, "NDVI_h")

load("AF_final.RData")
AF_final$Lon<- round(AF_final$Lon, 4)
AF_final$Lat<- round(AF_final$Lat, 4)
AF_final$Date<- as.Date(AF_final$Date)

write_state(AF_final, "AF")

#Get AF from first script...
# AF_final<- AF
# write_state(AF_final, "AF_h")

days_2008<- seq.Date(as.Date("2008-01-02"), as.Date("2008-12-31"), by = "day")
# # NAM<- read.csv("~/Data/NAM_data/NAM.csv") #Too BIG
for(st in State[-11]){
  print(st)
  nam<- read.csv(paste0("~/NAM_2008/nam_",st, "_2008-01-01.csv"))
  for(d in 1:length(days_2008)){
    n<- read.csv(paste0("~/NAM_2008/nam_",st,"_",days_2008[d],".csv"))
    nam<- rbind(nam, n)
  }
  # nam<- read.csv(paste0("~/NAM_2008/nam_",s, "_08.csv"))
  nam<- nam[,c(1:6,10:15,17)] #[,c(1:2,7:10, 14:19, 21)]
  nam<- nam[which(nam$Lon != "Lon"),]
  names(nam)<- c("Lat", "Lon", "HPBL_surface", "TMP_2m", "RH_2m",
                 "DPT_2m",  "Ugrd_10m", "Vgrd_10m", "PRMSL_mean_sea_level",
                 "PRES_surface", "DZDT_850_mb", "DZDT_700_mb", "Date")
  nam$Lon<- round(as.numeric(as.character(nam$Lon)), 5)
  nam$Lat<- round(as.numeric(as.character(nam$Lat)), 5)
  
  # NAM<- inner_join(nam, Stat, by = c("Lon", "Lat"))
  write.csv(nam, paste0("~/NAM_2008/NAM_",st,"_08.csv"), row.names=FALSE)
}


all_NAM<- NAM

for(y in 2009:2018){
  nam<- read.csv(paste0("~/Data/NAM_data/NAM_", y, ".csv"))
  nam<- nam[,c(1:2,7:10, 14:19, 21)]
  names(nam)<- c("Lat", "Lon", "HPBL_surface", "TMP_2m", "RH_2m",
                 "DPT_2m",  "Ugrd_10m", "Vgrd_10m", "PRMSL_mean_sea_level",
                 "PRES_surface", "DZDT_850_mb", "DZDT_700_mb", "Date")
  nam<- nam[which(nam$Lon != "Longitude"),]
  nam$Lon<- round(as.numeric(as.character(nam$Lon)), 5)
  nam$Lat<- round(as.numeric(as.character(nam$Lat)), 5)

  NAM<- inner_join(nam, Stat, by = c("Lon", "Lat"))
  all_NAM<- rbind(all_NAM, NAM)
}

save.image("extra_NAM.RData")
load("extra_NAM.RData")

write_state(all_NAM, "NAM2")

#Later NAM (for missing points):
nam<- read.csv(paste0("~/NAM_processing/NAM_processed/nam_", "2008-01-01", ".csv"))
nam<- nam[,c(1:2,4:7, 10:15, 17:18)]
names(nam)<- c("Lat", "Lon", "HPBL_surface", "TMP_2m", "RH_2m",
               "DPT_2m",  "Ugrd_10m", "Vgrd_10m", "PRMSL_mean_sea_level",
               "PRES_surface", "DZDT_850_mb", "DZDT_700_mb", "Date", "State")
NAM<- nam

for(i in 1:length(dates[-1])){
  if(file.exists(paste0("~/NAM_processing/NAM_processed/nam_", dates[i+1], ".csv"))){
    nam<- read.csv(paste0("~/NAM_processing/NAM_processed/nam_", dates[i+1], ".csv"))
    nam<- nam[,c(1:2,4:7, 10:15, 17:18)]
    names(nam)<- c("Lat", "Lon", "HPBL_surface", "TMP_2m", "RH_2m",
                   "DPT_2m",  "Ugrd_10m", "Vgrd_10m", "PRMSL_mean_sea_level",
                   "PRES_surface", "DZDT_850_mb", "DZDT_700_mb", "Date", "State")
    NAM<- rbind(NAM, nam)
  }
}

write_state(NAM, "NAM_h")

##Merge everything by state...

## Add other variables: Region, Timespan, Interactions, aggregate AF, CMAQ...

##Get part h:
# Stat<- read.csv("Final_missing_stationary.csv")
# Stat$State<- latlong2state(as.data.frame(Stat[,c("Lon", "Lat")]))
# write.csv(Stat, "Locations_part_h.csv", row.names=FALSE)

Stat<- read.csv("Locations_part_h.csv")

##CMAQ:

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
  
  # write.csv(locs, paste0("~/CMAQ_data/CMAQ_crossover-13-16_",s,".csv"), row.names=FALSE)
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

