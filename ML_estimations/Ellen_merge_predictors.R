library(dplyr)
library(lubridate)
library(stringr)
library(future.apply)

#Active Fires
AF1a<- read.csv("fire_modis_part_f_wLags_25km_extract_FINAL.csv")
AF1b<- read.csv("fire_modis_part_f_wLags_50km_extract_FINAL.csv")
AF1c<- read.csv("fire_modis_part_f_wLags_100km_extract_FINAL.csv")
AF1d<- read.csv("fire_modis_part_f_wLags_500km_extract_FINAL.csv")

af1a<- data.frame(Lon = AF1a$Lon, Lat = AF1a$Lat, Date = AF1a$Date, Fires_25km = AF1a$Fire_Count)
af1b<- data.frame(Lon = AF1b$Lon, Lat = AF1b$Lat, Date = AF1b$Date, Fires_50km = AF1b$Fire_Count)
af1c<- data.frame(Lon = AF1c$Lon, Lat = AF1c$Lat, Date = AF1c$Date, Fires_100km = AF1c$Fire_Count)
af1d<- data.frame(Lon = AF1d$Lon, Lat = AF1d$Lat, Date = AF1d$Date, Fires_500km = AF1d$Fire_Count)

AF<- Reduce(function(x,y) merge(x = x, y = y, by = c("Lon", "Lat", "Date"), all = TRUE), list(af1a, af1b, af1c, af1d))

AF[is.na(AF)]<- 0

AF$Date<- as.Date(AF$Date)
AF$Lon<- round(AF$Lon, 4)
AF$Lat<- round(AF$Lat, 4)

AF_agg<- aggregate(. ~ Lon + Lat + Date, AF, mean)
AF<- AF_agg

AF_unique_locs<- unique(AF[,c("Lon", "Lat")])

ncores = detectCores() - 1
uniq_seq<- seq(1,dim(AF_unique_locs)[1], length.out = ncores+1)
AF_lags_template<- data.frame(Lon = numeric(), Lat = numeric(), Date = as.Date(character()),
                              Fires_lag0_25km = numeric(), Fires_lag0_50km = numeric(),
                              Fires_lag0_100km = numeric(), Fires_lag0_500km = numeric(),
                              Fires_lag1_25km = numeric(), Fires_lag1_50km = numeric(),
                              Fires_lag1_100km = numeric(), Fires_lag1_500km = numeric(),
                              Fires_lag2_25km = numeric(), Fires_lag2_50km = numeric(),
                              Fires_lag2_100km = numeric(), Fires_lag2_500km = numeric(),
                              Fires_lag3_25km = numeric(), Fires_lag3_50km = numeric(),
                              Fires_lag3_100km = numeric(), Fires_lag3_500km = numeric(),
                              Fires_lag4_25km = numeric(), Fires_lag4_50km = numeric(),
                              Fires_lag4_100km = numeric(), Fires_lag4_500km = numeric(),
                              Fires_lag5_25km = numeric(), Fires_lag5_50km = numeric(),
                              Fires_lag5_100km = numeric(), Fires_lag5_500km = numeric(),
                              Fires_lag6_25km = numeric(), Fires_lag6_50km = numeric(),
                              Fires_lag6_100km = numeric(), Fires_lag6_500km = numeric(),
                              Fires_lag7_25km = numeric(), Fires_lag7_50km = numeric(),
                              Fires_lag7_100km = numeric(), Fires_lag7_500km = numeric())

merge_AF_lags<- function(uniq_locs, j){
  these_AF_lags<- AF_lags_template
  Uniq_locs<- AF_unique_locs[uniq_locs,]
  p<-1
  for(l in 1:dim(Uniq_locs)[1]){
    # start<-Sys.time()
    data<- AF[which((AF$Lon == Uniq_locs[l,1])&(AF$Lat == Uniq_locs[l,2])),]
    data$Date<- as.Date(data$Date)
    data_sorted<- data[order(data$Date),]
    these_dates<- data.frame(Date = seq.Date(as.Date(data_sorted$Date[1]), as.Date(data_sorted$Date[length(data$Date)]), by= "day"),One = 1)
    merged<- merge(these_dates, data_sorted, by = "Date", all.x = TRUE)
    merged<- as.matrix(merged)
    merged[which(is.na(merged))]<- 0
    
    for(d in 1:length(data_sorted$Date)){
      if(d>7){
        k<- 0:7
        lags<- merged[d-k,5:8]
      }else{
        k<- 0:(d-1)
        lags<- rep(0,32)
        lags[(33-4*d):32]<- merged[d-k,5:8]
      }
      
      these_AF_lags[p,1:2]<- Uniq_locs[l,]
      these_AF_lags[p,3]<- data_sorted$Date[d]
      these_AF_lags[p, 4:35]<- as.vector(t(lags))
      print(p)
      p<- p+1
      # end<- Sys.time()
    }#Time difference of 0.08949041 secs
  }
  write.csv(these_AF_lags, paste0("AF_lags4_",j,".csv"), row.names = FALSE)
  return(these_AF_lags)
}

loc_list<- c()

for(j in 1:ncores){
  uniq_locs<- round(uniq_seq[j]):round(uniq_seq[j+1]-1)
  loc_list<- append(loc_list, list(uniq_locs))
}

plan(multiprocess, workers = ncores) ## Parallelize using 15 cores
this_list<- future_lapply(1:length(loc_list), function(j){merge_AF_lags(loc_list[[j]],j)})
save.image("With_AF_lags.RData")
##THEN, IN TERMINAL: cat AF_lags_4_* > AF_lags4.csv

#NAM
files<- list.files("Processed_Data/NAM_data/NAM_Step5/NAM_Step5_batch2019-07-10/", pattern="csv", full.names = TRUE)

NAM_vars<- c("Lon", "Lat", "Date", "HPBL.surface", "TMP.2.m.above.ground",
             "RH.2.m.above.ground", "DPT.2.m.above.ground", "APCP.surface",
             "UGRD.10.m.above.ground", "VGRD.10.m.above.ground",
             "PRMSL.mean.sea.level", "PRES.surface", "DZDT.850.mb", "DZDT.700.mb")

NAM<- matrix(,nrow=0, ncol = length(NAM_vars))
colnames(NAM)<- NAM_vars

for(f in files){
 
  data<- read.csv(f)
 
  #For now, only grab the training set data:
  train<- merge(Part_f, data, by.y = c("Longitude", "Latitude", "Local.Date"), by.x = c("Lon", "Lat", "Date"))
  NAM<- append(NAM, train[, NAM_vars])
}

NAM_unlisted<- unlist(NAM) #this is big
NAM_day_pos<- which(names(NAM_unlisted) == "Lon1") #4015 of these

#Start with last in sequence (counterintuitively)
obs = (length(NAM_unlisted)+1 - NAM_day_pos[length(NAM_day_pos)])/14
seq_ref<- seq(NAM_day_pos[length(NAM_day_pos)], length(NAM_unlisted)+1, obs)
Lon<- NAM_unlisted[seq(seq_ref[1], seq_ref[2]-1)]
Lat<- NAM_unlisted[seq(seq_ref[2], seq_ref[3]-1)]
Date<- NAM_unlisted[seq(seq_ref[3], seq_ref[4]-1)]
HPBL_surface<- NAM_unlisted[seq(seq_ref[4], seq_ref[5]-1)]
TMP_2m<- NAM_unlisted[seq(seq_ref[5], seq_ref[6]-1)]
RH_2m<- NAM_unlisted[seq(seq_ref[6], seq_ref[7]-1)]
DPT_2m<- NAM_unlisted[seq(seq_ref[7], seq_ref[8]-1)]
APCP_surface<- NAM_unlisted[seq(seq_ref[8], seq_ref[9]-1)]
Ugrd_10m<- NAM_unlisted[seq(seq_ref[9], seq_ref[10]-1)]
Vgrd_10m<- NAM_unlisted[seq(seq_ref[10], seq_ref[11]-1)]
PRMSL_mean_sea_level<- NAM_unlisted[seq(seq_ref[11], seq_ref[12]-1)]
PRES_surface<- NAM_unlisted[seq(seq_ref[12], seq_ref[13]-1)]
DZDT_850_mb<- NAM_unlisted[seq(seq_ref[13], seq_ref[14]-1)]
DZDT_700_mb<- NAM_unlisted[seq(seq_ref[14], seq_ref[15]-1)]

final_NAM<- cbind(Lon, Lat, Date, HPBL_surface, TMP_2m, RH_2m, DPT_2m, APCP_surface,
                  Ugrd_10m, Vgrd_10m, PRMSL_mean_sea_level, PRES_surface,
                  DZDT_850_mb, DZDT_700_mb)

for(d in 2:length(NAM_day_pos)){
  obs = (NAM_day_pos[d] - NAM_day_pos[d-1])/14
  seq_ref<- seq(NAM_day_pos[d-1], NAM_day_pos[d], obs)
  Lon<- NAM_unlisted[seq(seq_ref[1], seq_ref[2]-1)]
  Lat<- NAM_unlisted[seq(seq_ref[2], seq_ref[3]-1)]
  Date<- NAM_unlisted[seq(seq_ref[3], seq_ref[4]-1)]
  HPBL_surface<- NAM_unlisted[seq(seq_ref[4], seq_ref[5]-1)]
  TMP_2m<- NAM_unlisted[seq(seq_ref[5], seq_ref[6]-1)]
  RH_2m<- NAM_unlisted[seq(seq_ref[6], seq_ref[7]-1)]
  DPT_2m<- NAM_unlisted[seq(seq_ref[7], seq_ref[8]-1)]
  APCP_surface<- NAM_unlisted[seq(seq_ref[8], seq_ref[9]-1)]
  Ugrd_10m<- NAM_unlisted[seq(seq_ref[9], seq_ref[10]-1)]
  Vgrd_10m<- NAM_unlisted[seq(seq_ref[10], seq_ref[11]-1)]
  PRMSL_mean_sea_level<- NAM_unlisted[seq(seq_ref[11], seq_ref[12]-1)]
  PRES_surface<- NAM_unlisted[seq(seq_ref[12], seq_ref[13]-1)]
  DZDT_850_mb<- NAM_unlisted[seq(seq_ref[13], seq_ref[14]-1)]
  DZDT_700_mb<- NAM_unlisted[seq(seq_ref[14], seq_ref[15]-1)]
 
  additional<- cbind(Lon, Lat, Date, HPBL_surface, TMP_2m, RH_2m, DPT_2m, APCP_surface,
                     Ugrd_10m, Vgrd_10m, PRMSL_mean_sea_level, PRES_surface,
                     DZDT_850_mb, DZDT_700_mb)
   
  final_NAM<- rbind(final_NAM, additional)
}

#Fix final_NAM$Date
Days<- seq.Date(as.Date("2008-01-01"), as.Date("2018-12-31"), by = "day")
final_NAM<- data.frame(final_NAM)
final_NAM[,"Date"]<- Days[final_NAM[,"Date"]]

#MAIAC
MAIAC1<- read.csv("Processed_Data/PredictorVariablesExtractedToDatesLocations/MAIAC_extracted_part_b.csv", stringsAsFactors = FALSE)
MAIAC2<- read.csv("Processed_Data/PredictorVariablesExtractedToDatesLocations/MAIAC_extracted_part_e_not_in_b.csv", stringsAsFactors = FALSE)
MAIAC3<- read.csv("Processed_Data/PredictorVariablesExtractedToDatesLocations/MAIAC_extracted_part_e_2014_JD-1-through-278.csv", stringsAsFactors = FALSE)
MAIAC4<- read.csv("Processed_Data/PredictorVariablesExtractedToDatesLocations/MAIAC_extracted_part_f_minus_e.csv", stringsAsFactors = FALSE)

# MAIAC_vars<- c("Lon", "Lat", "Date", "MAIAC_AOD")

MAIAC<- rbind(MAIAC1, MAIAC2, MAIAC3, MAIAC4)

#NDVI
NDVI1<- read.csv("Processed_Data/PredictorVariablesExtractedToDatesLocations/ndvi_mod13a3_part_e_extract.csv")
NDVI2<- read.csv("Processed_Data/PredictorVariablesExtractedToDatesLocations/ndvi_mod13a3_part_f_minus_e_extract.csv")

NDVI_vars<- c("Lon", "Lat", "Date", "ndvi")
NDVI<- rbind(NDVI1[,NDVI_vars], NDVI2[,NDVI_vars])

## START MERGING:
MAIAC<- unique(MAIAC)
NDVI<- unique(NDVI)
AF_lags<- read.csv("AF_lags4.csv")
AF_lags$Lon<- as.numeric(as.character(AF_lags$Lon))
AF_lags$Lat<- as.numeric(as.character(AF_lags$Lat))
AF_lags<- AF_lags[-which(is.na(AF_lags$Lon)),]
AF_lags$Date<- as.Date(as.character(AF_lags$Date))
AF_lags[,4:35]<- apply(AF_lags[,4:35], MARGIN = 2, function(y){as.numeric(as.character(y))})

#NLCD
NLCD1a<- read.csv("nlcd_1km_part_bc_extract.csv")
NLCD1b<- read.csv("nlcd_5km_part_bc_extract.csv")
NLCD1c<- read.csv("nlcd_10km_part_bc_extract.csv")
NLCD2a<- read.csv("nlcd_part_e_not_b_1km_extract.csv")
NLCD2b<- read.csv("nlcd_part_e_not_b_5km_extract.csv")
NLCD2c<- read.csv("nlcd_part_e_not_b_10km_extract.csv")
NLCD3a<- read.csv("nlcd_part_f_minus_e_1km_extract.csv")
NLCD3b<- read.csv("nlcd_part_f_minus_e_5km_extract.csv")
NLCD3c<- read.csv("nlcd_part_f_minus_e_10km_extract.csv")

NLCD1a_agg<- aggregate(. ~ Lon + Lat, data = NLCD1a, FUN = mean)
NLCD1b_agg<- aggregate(. ~ Lon + Lat, data = NLCD1b, FUN = mean)
NLCD1c_agg<- aggregate(. ~ Lon + Lat, data = NLCD1c, FUN = mean)

NLCD1<- Reduce(function(x,y) unique(inner_join(x, y, by = c("Lon", "Lat"))), list(NLCD1a_agg, NLCD1b_agg, NLCD1c_agg))
NLCD2<- Reduce(function(x,y) unique(inner_join(x = x, y = y, by = c("Lon", "Lat"))), list(NLCD2a, NLCD2b, NLCD2c))
NLCD3<- Reduce(function(x,y) unique(inner_join(x = x, y = y, by = c("Lon", "Lat"))), list(NLCD3a, NLCD3b, NLCD3c))

NLCD1$Lon<- round(as.numeric(as.character(NLCD1$Lon)),5)
NLCD1$Lat<- round(as.numeric(as.character(NLCD1$Lat)),5)
NLCD2$Lon<- round(as.numeric(as.character(NLCD2$Lon)),5)
NLCD2$Lat<- round(as.numeric(as.character(NLCD2$Lat)),5)
NLCD3$Lon<- round(as.numeric(as.character(NLCD3$Lon)),5)
NLCD3$Lat<- round(as.numeric(as.character(NLCD3$Lat)),5)

NLCD1_agg<- aggregate(. ~ Lon + Lat, NLCD1, mean)
NLCD2_agg<- aggregate(. ~ Lon + Lat, NLCD2, mean)
NLCD3_agg<- aggregate(. ~ Lon + Lat, NLCD3, mean)

NLCD_vars<- c("Lon", "Lat", "percent_urban_buffer.x", "percent_urban_buffer.y",
              "percent_urban_buffer") #.x = a, .y = b, plain = c
NLCD<- rbind(NLCD1_agg[,NLCD_vars], NLCD2_agg[,NLCD_vars], NLCD3_agg[,NLCD_vars])
names(NLCD)<- c("Lon", "Lat", "NLCD_1km", "NLCD_5km",
                "NLCD_10km")
NLCD_agg<- aggregate(. ~ Lon + Lat, NLCD, mean)

#Population Density
pop<- read.csv("Pop_density_part_f.csv")
pop$Lon<- round(pop$Lon, 5)
pop$Lat<- round(pop$Lat, 5)
pop_agg<- aggregate(. ~ Lon + Lat, pop, mean)

#Highways
HW1<- read.csv("Highways_part_e.csv")
HW2<- read.csv("Highways_part_f_minus_e.csv")

HW_vars<- c("Lon", "Lat", "A_100", "C_100", "Both_100", "A_250", "C_250", "Both_250",
            "A_500", "C_500", "Both_500", "A_1000", "C_1000", "Both_1000")
HW<- rbind(HW1[,HW_vars], HW2[,HW_vars])
HW$Lon<- round(HW$Lon, 5)
HW$Lat<- round(HW$Lat, 5)
HW_agg<- aggregate(. ~ Lon + Lat, HW, mean)

#Elevation
elev1<- read.csv("ned_part_bc_extract.csv")
elev2<- read.csv("ned_part_e_not_in_b_extract.csv")
elev3<- read.csv("ned_part_f_minus_e_extract.csv")

elev_vars<- c("Lon", "Lat", "elevation")
elev<- rbind(elev1[,elev_vars], elev2[,elev_vars], elev3[,elev_vars])
elev$Lon<- round(elev$Lon, 5)
elev$Lat<- round(elev$Lat, 5)
elev_agg<- aggregate(. ~ Lon + Lat, elev, mean)

######Merge everything
##Stationary
stat<- unique(Reduce(function(x,y){inner_join(x,y, by = c("Lon", "Lat"))},
                     list(pop_agg, NLCD_agg, HW_agg, elev_agg)))
dont_need<- which(names(stat) %in% c("X", "Datum"))
Stat<- stat[,-dont_need]

##Time-varying: 
round_LL<- function(DF, rn){
  new_Lon<- round(DF$Lon, rn)
  new_Lat<- round(DF$Lat, rn)
  if(rn ==4){
    DF$Lon4<- new_Lon
    DF$Lat4<- new_Lat
  }else{
    DF$Lon<- new_Lon
    DF$Lat- new_Lat
  }
  
  return(DF)
}

maiac<- round_LL(MAIAC,4)
ndvi<- round_LL(NDVI,4)
nam<- round_LL(final_NAM,5)
af_lags<- unique(round_LL(AF_lags,4))
part_f<- round_LL(Part_f_LLDate,4)
Part_f_5<- round_LL(Part_f_LLDate,5)

ndvi_merged<- inner_join(part_f[,-3], ndvi, by = c("Lon4", "Lat4", "Date"))
ndvi_5<- ndvi_merged[,c("Lon.x", "Lat.x", "Date", "ndvi")]
names(ndvi_5)<- c("Lon", "Lat", "Date", "ndvi")

maiac_merged<- inner_join(part_f[,-3], maiac, by = c("Lon4", "Lat4", "Date"))
maiac_5<- maiac_merged[,c("Lon.x", "Lat.x", "Date", "MAIAC_AOD")]
names(maiac_5)<- c("Lon", "Lat", "Date", "MAIAC_AOD")

af_merged<- left_join(part_f[,-3], af_lags, by = c("Lon4", "Lat4", "Date"))
for(j in 11:42){
  af_merged[which(is.na(af_merged[,j])),j]<- 0
}
not_include<- c("Lon.y", "Lat.y", "PM2.5_Obs", "Year", "Month", "Lon4", "Lat4")
af_lags_5<- af_merged[,-which(names(af_merged) %in% not_include)]
names(af_lags_5)[1:2]<- c("Lon", "Lat")

maiac_5$MAIAC_AOD<- as.numeric(maiac_5$MAIAC_AOD)
ndvi_5$ndvi<- as.numeric(ndvi_5$ndvi)

maiac_5_agg<- aggregate(MAIAC_AOD ~ Lon + Lat + Date, maiac_5, mean)
ndvi_5_agg<- aggregate(ndvi ~ Lon + Lat + Date, ndvi_5, mean)
# nam_agg<- aggregate(. ~ Lon + Lat + Date, nam, mean) #same as nam

Time_var<- unique(Reduce(function(x,y){inner_join(x,y, by = c("Lon", "Lat", "Date"))},
                         list(Part_f_5[,-3], maiac_5_agg, nam, ndvi_5_agg,
                              af_lags_5))) 
ALL_unique<- unique(inner_join(Time_var, Stat, by = c("Lon", "Lat")))

dim(ALL_unique[,c("Lon", "Lat", "Date")]) #1591592 unique lat-lon-date

rm(list=setdiff(ls(), "ALL_unique"))
save.image("Done_for_now.RData")

library(sp)
library(maps)
library(maptools)

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  
  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- over(pointsSP, states_sp)
  
  NApos<- which(is.na(indices))
  
  for(p in NApos){
    if(pointsDF[p,2]<= 41.9952){
      if(pointsDF[p,1] <= -114.1315){
        indices[p]<- which(stateNames == "california")
      }else if(pointsDF[p,1] <= -109.0475){
        indices[p]<- which(stateNames == "arizona")
      }else{
        indices[p]<- which(stateNames == "new mexico")
      }
    }else if((pointsDF[p,2] > 41.9952)& (pointsDF[p,2]<= 46.2891) ){
      indices[p]<- which(stateNames == "oregon")
    }else if(pointsDF[p,2] > 46.2891){
      if(pointsDF[p,1] <= -117.2372){
        indices[p]<- which(stateNames == "washington")
      }else if(pointsDF[p,1] <= -116.0458){
        indices[p]<- which(stateNames == "idaho")
      }else{
        indices[p]<- which(stateNames == "montana")
      }
    }
  }
  
  # Return the state names of the Polygons object containing each point
  stateNames[indices]
}

ALL_unique$State<- latlong2state(as.data.frame(ALL_unique[,c("Lon", "Lat")]))

ALL_unique$Day<- sapply(ALL_unique$Date, function(x){as.numeric(strsplit(as.character(x), split = "-")[[1]][3])})
ALL_unique$DayOfWeek<- sapply(ALL_unique$Date, function(x){wday(as.POSIXct(x))})
ALL_unique$Season<- sapply(ALL_unique$Month, function(m){ifelse(m %in% c(3:5), "spring", ifelse(m %in% c(6:8), "summer",
                                                                                                ifelse(m %in% c(9:11), "fall", "winter")))})

non_leap<- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
leap<- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

DOY<- function(YMD){ # YMD is a vector containing year, month, day
  
  #Figure out if it is a leap year or not
  if(YMD[1] %in% c(2008, 2012, 2016)){
    nums<- leap
  }else{
    nums<- non_leap
  }
  
  if(YMD[2] == 1){
    return(YMD[3])
  }else{
    return(sum(nums[1:(YMD[2]-1)]) + YMD[3])
  }
}

my_DOY<- apply(ALL_unique[,c("Year", "Month", "Day")], MARGIN = 1, FUN = DOY)

ALL_unique$CosDOW<- cos(2*pi*ALL_unique$DayOfWeek/7)
ALL_unique$CosDOY<- cos(2*pi*my_DOY/365)
ALL_unique$CosMonth<- cos(2*pi*ALL_unique$Month/12)

ALL_unique[,sapply(names(ALL_unique), FUN = function(x){str_detect(x, "Fires_lag")})]<- apply(ALL_unique[,sapply(names(ALL_unique), FUN = function(x){str_detect(x, "Fires_lag")})], MARGIN = 2, as.numeric)

ALL_unique$Binary_fire<- rowSums(ALL_unique[,sapply(names(ALL_unique), FUN = function(x){str_detect(x, "Fires_lag")})]) > 0

write.csv(ALL_unique, "Part_f_merged4.csv", row.names = FALSE)

rm(list=setdiff(ls(), "ALL_unique"))
save.image("Done4.RData")

                              
                            


