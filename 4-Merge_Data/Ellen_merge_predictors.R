library(dplyr)
library(multidplyr)
library(lubridate)
library(stringr)
library(future.apply)
library(parallel)
library(stringr)

pred_locs<- read.csv("~/Data/West_prediction_locations.csv")
pred_locs$Lon<- round(pred_locs$Lon, 5)
pred_locs$Lat<- round(pred_locs$Lat, 5)
Locs<- unique(pred_locs[,c("Lon", "Lat")])
#Need to round to 4 digits for AF

# #Active Fires
# AF1a<- read.csv("~/Data/fire_modis_part_g_25km_extract_final.csv")
# AF1b<- read.csv("~/Data/fire_modis_part_g_50km_extract_final.csv")
# AF1c<- read.csv("~/Data/fire_modis_part_g_100km_extract_final.csv")
# AF1d<- read.csv("~/Data/fire_modis_part_g_500km_extract_final.csv")

AF1a<- read.csv("~/Data/fire_modis_part_h_25km_extract_final.csv")
AF1b<- read.csv("~/Data/fire_modis_part_h_50km_extract_final.csv")
AF1c<- read.csv("~/Data/fire_modis_part_h_100km_extract_final.csv")
AF1d<- read.csv("~/Data/fire_modis_part_h_500km_extract_final.csv")

af1a<- data.frame(Lon = AF1a$Lon, Lat = AF1a$Lat, Date = AF1a$Date, Fires_25km = AF1a$fire_count)
af1b<- data.frame(Lon = AF1b$Lon, Lat = AF1b$Lat, Date = AF1b$Date, Fires_50km = AF1b$fire_count)
af1c<- data.frame(Lon = AF1c$Lon, Lat = AF1c$Lat, Date = AF1c$Date, Fires_100km = AF1c$fire_count)
af1d<- data.frame(Lon = AF1d$Lon, Lat = AF1d$Lat, Date = AF1d$Date, Fires_500km = AF1d$fire_count)

AF<- Reduce(function(x,y) merge(x = x, y = y, by = c("Lon", "Lat", "Date"), all = TRUE), list(af1a, af1b, af1c, af1d))

AF[is.na(AF)]<- 0

AF$Date<- as.Date(AF$Date)
AF$Lon<- round(AF$Lon, 4)
AF$Lat<- round(AF$Lat, 4)

AF_agg<- aggregate(. ~ Lon + Lat + Date, AF, mean)
AF<- AF_agg
AF_unique_locs<- unique(AF_agg[,c("Lon", "Lat")])
# 
# load("~/With_AF_lags.RData")
# 
rm(list=c("AF", "AF1a", "AF1b", "AF1c", "AF1d", "af1a", "af1b", "af1c", "af1d"))

ncores = detectCores() - 6
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

#Second try:
dates<- seq.Date(as.Date("2008-01-01"), as.Date("2018-12-31"), by = "day")
Date<- sort(rep(dates, dim(AF_unique_locs)[1]))
Lon<- rep(AF_unique_locs$Lon, length(dates))
Lat<- rep(AF_unique_locs$Lat, length(dates))
all_LLD<- data.frame(Lon, Lat, Date)

ready_AF<- left_join(all_LLD, AF_agg, by = c("Lon", "Lat", "Date"))
num_Locs<- dim(AF_unique_locs)[1]

merge_AF_lags<- function(these, j){
  these_AF_lags<- AF_lags_template
  p<-1
  for(l in these){
    for(d in 1:length(dates)){
      lags<- 7
      if(dates[d] < "2008-01-08"){
        lags<- as.numeric(dates[d] - as.Date("2008-01-01") )
      }
      these_AF_lags[p,1]<- ready_AF[l,"Lon"]
      these_AF_lags[p,2]<- ready_AF[l,"Lat"]
      these_AF_lags[p,3]<- dates[d]
      my_vec<- c()
      for(i in 0:lags){
        new<- ready_AF[(d-1-i)*num_Locs + l, 4:7]
        new[is.na(new)]<- 0
        my_vec<- append(my_vec, new)
      }
      my_vec<- unlist(my_vec)
      my_vec<- append(my_vec, rep(0, 32-length(my_vec)))
      these_AF_lags[p,4:35]<- my_vec
      p<- p+1
    }
  }
  write.csv(these_AF_lags, paste0("~/Data/AF/AF_lags4_",j,".csv"), row.names = FALSE)
  return(these_AF_lags)
}

# start<- Sys.time()
# merge_AF_lags(1:10, 1)
# end<- Sys.time()

my_seq<- seq(1, num_Locs, length.out = 20)

loc_list<- c()
Still_need<- 1:(length(my_seq)-1)
# Still_need<- Still_need[-c(1, 150, 151, 152, 153, 154, 175, 2, 200, 201, 202, 203, 204, 3, 4, 5, 51)]

for(j in Still_need){
  uniq_locs<- round(my_seq[j]):round(my_seq[j+1]-1)
  loc_list<- append(loc_list, list(uniq_locs))
}

loc_list[[length(loc_list)]]<- append(loc_list[[length(loc_list)]], num_Locs)

# round2<- unlist(loc_list)
# 
# my_seq2<- seq(1, length(round2), length.out = 2000)
# 
# loc_list<- c()
# 
# for(j in 1:(length(my_seq2)-1)){
#   uniq_locs<- round2[round(my_seq2[j]):round(my_seq2[j+1]-1)]
#   loc_list<- append(loc_list, list(uniq_locs))
# }
# 
# loc_list[[length(loc_list)]]<- append(loc_list[[length(loc_list)]], round2[length(round2)])

options(future.globals.maxSize= 5000*1024^2)

plan(multiprocess, workers = 8) ## Parallelize
this_list<- future_lapply(1:length(loc_list), function(j){merge_AF_lags(loc_list[[j]],j)})
# save.image("With_AF_lags.RData")
##THEN, IN TERMINAL: cat _____* > ______

# rm(list=setdiff(ls(), "this_list"))
# save.image("~/Merged/AF.RData")
# merged<- as.matrix(merged)
# merged[which(is.na(merged))]<- 0
# 
##Second round:
# AF_intermediate<- read.csv("~/Data/AF/AF_lags4_part_g.csv")
# 
# done_locs<- unique(AF_intermediate[,c("Lon", "Lat")])
# Done_locs<- apply(done_locs, MARGIN=2, function(y){as.numeric(as.character(y))})
# NA_pos<- which(is.na(Done_locs[,1]))
# Done_locs<- Done_locs[-NA_pos,]
# 
# Done_locs<- as.data.frame(Done_locs)
# AF_unique_locs<- as.data.frame(AF_unique_locs)
# AF_unique_locs$Index<- 1:dim(AF_unique_locs)[1]
# 
# need_locs<- anti_join(AF_unique_locs, Done_locs, by = c("Lon", "Lat"))
# 
# my_seq2<- seq(1, dim(need_locs)[1], length.out = 200)
# 
# loc_list<- c()
# 
# for(j in 1:(length(my_seq2)-1)){
#   uniq_locs<- need_locs[round(my_seq2[j]):round(my_seq2[j+1]-1), "Index"]
#   loc_list<- append(loc_list, list(uniq_locs))
# }
# 
# loc_list[[length(loc_list)]]<- append(loc_list[[length(loc_list)]], need_locs[length(need_locs$Index), "Index"])
# 
# rm(list=c("AF_agg", "all_LLD", "AF_intermediate"))
# 
# save.image("AF_intermediate.RData")
# 
# load("AF_intermediate.RData")
# 
# options(future.globals.maxSize= 7000*1024^2)
# 
# plan(multiprocess, workers = 8) ## Parallelize
# this_list<- future_lapply(1:length(loc_list), function(j){merge_AF_lags(loc_list[[j]],j)})
# 

# rm(list=setdiff(ls(), c("AF_unique_locs", "ready_AF", "dates", "Lat", "Lon", 
#                         "num_Locs"))) 
# 
# ##Get unique:
# AF_intermediate<- read.csv("~/Data/AF/AF_lags4_part_g.csv")
# # AF_intermediate$Index<- 1:dim(AF_intermediate)[1]
# 
# #Remove text:
# text_pos<- which((AF_intermediate$Lon == "Lon")&(AF_intermediate$Lat == "Lat"))
# AF_intermediate<- AF_intermediate[-text_pos,]
# 
# # plan(multiprocess, workers = 8)
# # AF_intermediate<- future_apply(AF_intermediate, MARGIN = 2, function(y){as.numeric(as.character(y))})
# for(j in 1:(dim(AF_intermediate)[2]-1)){
#   if(j != 3){
#     AF_intermediate[,j]<- as.numeric(as.character(AF_intermediate[,j]))
#   }else{
#     AF_intermediate[,j]<- as.Date(AF_intermediate[,j])
#   }
# }
# 
# AF_final<- AF_intermediate
# rm(list=c("AF_intermediate", "AF_unique_locs", "cluster", "ready_AF", 
#           "j", "text_pos"))
# save.image("AF_final.RData")

##Prepare for others:
# load("AF_final.RData")

AF<- read.csv("~/Data/AF/AF_lags_h.csv")
text_pos<- which((AF$Lon == "Lon")&(AF$Lat == "Lat"))
AF<- AF[-text_pos,]

AF$Lon<- round(as.numeric(as.character(AF$Lon)), 4)
AF$Lat<- round(as.numeric(as.character(AF$Lat)), 4)
AF$Date<- as.Date(as.character(AF$Date))

#MAIAC
# MAIAC<- read.csv("~/Data/MAIAC_extracted_part_g.csv")
# maiac_locs<- unique(MAIAC[,c("Lon", "Lat")])
# maiac_locs<- apply(maiac_locs, MARGIN = 2, function(y){as.numeric(as.character(y))})
# maiac_locs<- maiac_locs[-1,]
# maiac_locs<- data.frame(Lon = maiac_locs[,1], Lat = maiac_locs[,2])
# maiac_locs$Lon<- round(maiac_locs$Lon, 5)
# maiac_locs$Lat<- round(maiac_locs$Lat, 5)
# 
# MAIAC<- MAIAC[-1,]
# MAIAC<- MAIAC[-1,]
# MAIAC$Lon<- round(as.numeric(as.character(MAIAC$Lon)),5)
# MAIAC$Lat<- round(as.numeric(as.character(MAIAC$Lat)),5)
# 
# Locs_with_maiac<- data.frame(Lon = Locs$Lon, Lat = Locs$Lat, 
#                              M_lon = maiac_locs$Lon, M_lat = maiac_locs$Lat )
# #Note: Locs and maiac_locs are the same, but don't exactly match... rounding error somewhere along the line
# 
# row.names(MAIAC)<- 1:dim(MAIAC)[1]
# 
# MAIAC$Lon<- Locs_with_maiac[match(MAIAC$Lon, Locs_with_maiac$M_lon), "Lon"]
# MAIAC$Lat<- Locs_with_maiac[match(MAIAC$Lat, Locs_with_maiac$M_lat), "Lat"]
# 
# save.image("MAIAC.RData")

load("MAIAC.RData")
MAIAC$Lon<- round(MAIAC$Lon, 4)
MAIAC$Lat<- round(MAIAC$Lat, 4)
MAIAC$Date<- as.Date(MAIAC$Date)

#NAM
files<- list.files("~/NAM_2008/", pattern="Step5*", full.names = TRUE)

for(s in State[-11]){
  for(f in files){ #f is the filename
    data<- read.csv(f)
    date_str<- strsplit(f, "Step5_")[[1]][2] #Used to be 2
    date<- strsplit(date_str, "_batch")[[1]][1]
    names(data)[1:2]<- c("Lat", "Lon")
    names(data)[length(names(data))]<- "Date"
    data[,1:2]<- apply(data[,1:2], MARGIN = 2, 
                       function(y) round(as.numeric(as.character(y)),5))
    nam<- inner_join(data[,c(1:2, 7:21)], Stat[which(Stat$State == s),c("Lon", "Lat", "State")], by = c("Lon", "Lat"))
    write.csv(nam, paste0("~/NAM_2008/nam_",s,"_", date, ".csv"), row.names = FALSE)
  }
  print(s)
}

#Then merge in terminal, read in, and remove header rows


##NDVI
# NDVI1<- read.csv("~/Data/ndvi_mod13a3_part_g_subset1_latlon.csv")
# NDVI2<- read.csv("~/Data/ndvi_mod13a3_part_g_subset2_latlon.csv")
# NDVI3<- read.csv("~/Data/ndvi_mod13a3_part_g_subset3_latlon.csv")
# NDVI3<- NDVI3[,-1]
# NDVI<- rbind(NDVI1, NDVI2, NDVI3) #Actually have all the data!
# rm(list=c("NDVI1", "NDVI2", "NDVI3"))
# 
# NDVI_final<- NDVI[,c("Longitude", "Latitude", "Date", "NDVI")]
# names(NDVI_final)[1:2]<- c("Lon", "Lat")
# NDVI_final[,c("Lon", "Lat", "NDVI")]<- apply(NDVI_final[,c("Lon", "Lat", "NDVI")], 
#                                           MARGIN = 2, 
#                                           function(y) round(as.numeric(y),5))
# NDVI_final$Date<- as.Date(NDVI_final$Date)
# rm(list=setdiff(ls(), "NDVI_final"))
save.image("NDVI_final.RData")

##Stationary!

#NLCD
# NLCD1<- read.csv("~/Data/nlcd_part_g_1km_extract.csv")
# NLCD2<- read.csv("~/Data/nlcd_part_g_5km_extract.csv")
# NLCD3<- read.csv("~/Data/nlcd_part_g_10km_extract.csv")

NLCD1<- read.csv("~/Data/nlcd_part_h_1km_extract.csv")
NLCD2<- read.csv("~/Data/nlcd_part_h_5km_extract.csv")
NLCD3<- read.csv("~/Data/nlcd_part_h_10km_extract.csv")

NLCD1_agg<- aggregate(percent_urban_buffer ~ Lon + Lat, data = NLCD1, FUN = mean)
NLCD2_agg<- aggregate(percent_urban_buffer ~ Lon + Lat, data = NLCD2, FUN = mean)
NLCD3_agg<- aggregate(percent_urban_buffer ~ Lon + Lat, data = NLCD3, FUN = mean)

NLCD<- Reduce(function(x,y) unique(inner_join(x, y, by = c("Lon", "Lat"))), list(NLCD1_agg, NLCD2_agg, NLCD3_agg))

NLCD$Lon<- round(as.numeric(as.character(NLCD$Lon)),5)
NLCD$Lat<- round(as.numeric(as.character(NLCD$Lat)),5)

NLCD_vars<- c("Lon", "Lat", "percent_urban_buffer.x", "percent_urban_buffer.y",
              "percent_urban_buffer") #.x = a, .y = b, plain = c
names(NLCD)<- c("Lon", "Lat", "NLCD_1km", "NLCD_5km",
                "NLCD_10km")
# NLCD_agg<- aggregate(. ~ Lon + Lat, NLCD, mean)

#Population Density
# pop<- read.csv("~/Data/Pop_density_part_g.csv")
# pop$Lon<- round(pop$Lon, 5)
# pop$Lat<- round(pop$Lat, 5)
# pop_agg<- aggregate(. ~ Lon + Lat, pop[,c(2:3, 8)], mean)
# 
# pop_stat<- inner_join(pop_agg, Stat[,c(1:6,24)], by = c("Lon", "Lat"))
# no_pop<- anti_join(Locs, pop_stat, by = c("Lon", "Lat"))
# no_pop_locs<- inner_join(no_pop, pred_locs, by = c("Lon", "Lat"))
# write.csv(no_pop_locs, "No_pop_density.csv", row.names=FALSE)
# 
# extra_pop<- read.csv("Extra_PD.csv")
# extra_pop2<- inner_join(extra_pop, pred_locs)
# 
# no_pop<- read.csv("New_lost_PD.csv")
# no_pop$Pop_density<- 0
# 
# Extra_pop<- rbind(extra_pop2[, c("Lon", "Lat", "State_FIPS", "County_FIPS", "Tract_code",
#                                  "ZCTA5_code", "Pop_density")],
#                   no_pop[41:59,c("Lon", "Lat", "State_FIPS", "County_FIPS", "Tract_code",
#                                  "ZCTA5_code", "Pop_density")])
# Extra_pop$Pop_density<- round(Extra_pop$Pop_density, 2)
# write.csv(Extra_pop, "Final_extra_PD.csv", row.names=FALSE)
extra_pop<- read.csv("Final_extra_PD.csv")
extra_pop$Lon<- round(extra_pop$Lon, 5)
extra_pop$Lat<- round(extra_pop$Lat, 5)

#Highways
# HW_vars<- c("Lon", "Lat", "A_100", "C_100", "Both_100", "A_250", "C_250", "Both_250",
#             "A_500", "C_500", "Both_500", "A_1000", "C_1000", "Both_1000")

# HW<- read.csv("~/Data/Highways_part_g.csv")
HW<- read.csv("~/Data/Highways_part_h.csv")
HW$Lon<- round(HW$Lon, 5)
HW$Lat<- round(HW$Lat, 5)
# HW_agg<- aggregate(. ~ Lon + Lat, HW[,c(2:3,8:19)], mean)
HW_agg<- aggregate(. ~ Lon + Lat, HW, mean)

#Elevation
# elev<- read.csv("~/Data/ned_part_g_extract.csv")
elev<- read.csv("~/Data/ned_part_h_extract.csv")
elev$Lon<- round(elev$Lon, 5)
elev$Lat<- round(elev$Lat, 5)
# elev_agg<- aggregate(. ~ Lon + Lat, elev[,c(1:8)], mean)

#####Merge everything
#Stationary

Stat<- unique(Reduce(function(x,y){inner_join(x,y, by = c("Lon", "Lat"))},
                     list(NLCD, HW_agg, elev))) #list(pop_agg, NLCD, HW_agg, elev_agg)

# write.csv(Stat, "Stationary_variables.csv", row.names = FALSE)
write.csv(Stat, "Stationary_variables3.csv", row.names=FALSE)

missing<- anti_join(extra_pop, Stat, by = names(extra_pop)[1:6])
write.csv(missing, "Final_missing_stationary.csv", row.names=FALSE) #Would need to go back and get all of these from all the data sets

sum(!is.na(missing$ZCTA5_code))

sum(is.na(missing$ZCTA5_code) & !is.na(missing$Tract_code))

# ##Time-varying: 
# 
# round_LL<- function(DF, rn){
#   new_Lon<- round(DF$Lon, rn)
#   new_Lat<- round(DF$Lat, rn)
#   if(rn ==4){
#     DF$Lon4<- new_Lon
#     DF$Lat4<- new_Lat
#   }else{
#     DF$Lon<- new_Lon
#     DF$Lat- new_Lat
#   }
#   
#   return(DF)
# }
# 
# maiac<- round_LL(MAIAC,4)
# ndvi<- round_LL(NDVI,4)
# nam<- round_LL(final_NAM,5)
# af_lags<- unique(round_LL(AF_lags,4))
# part_f<- round_LL(Part_f_LLDate,4)
# Part_f_5<- round_LL(Part_f_LLDate,5)
# 
# ndvi_merged<- inner_join(part_f[,-3], ndvi, by = c("Lon4", "Lat4", "Date"))
# ndvi_5<- ndvi_merged[,c("Lon.x", "Lat.x", "Date", "ndvi")]
# names(ndvi_5)<- c("Lon", "Lat", "Date", "ndvi")
# 
# maiac_merged<- inner_join(part_f[,-3], maiac, by = c("Lon4", "Lat4", "Date"))
# maiac_5<- maiac_merged[,c("Lon.x", "Lat.x", "Date", "MAIAC_AOD")]
# names(maiac_5)<- c("Lon", "Lat", "Date", "MAIAC_AOD")
# 
# af_merged<- left_join(part_f[,-3], af_lags, by = c("Lon4", "Lat4", "Date"))
# for(j in 11:42){
#   af_merged[which(is.na(af_merged[,j])),j]<- 0
# }
# not_include<- c("Lon.y", "Lat.y", "PM2.5_Obs", "Year", "Month", "Lon4", "Lat4")
# af_lags_5<- af_merged[,-which(names(af_merged) %in% not_include)]
# names(af_lags_5)[1:2]<- c("Lon", "Lat")
# 
# maiac_5$MAIAC_AOD<- as.numeric(maiac_5$MAIAC_AOD)
# ndvi_5$ndvi<- as.numeric(ndvi_5$ndvi)
# 
# maiac_5_agg<- aggregate(MAIAC_AOD ~ Lon + Lat + Date, maiac_5, mean)
# ndvi_5_agg<- aggregate(ndvi ~ Lon + Lat + Date, ndvi_5, mean)
# # nam_agg<- aggregate(. ~ Lon + Lat + Date, nam, mean) #same as nam
# 
# Time_var<- unique(Reduce(function(x,y){inner_join(x,y, by = c("Lon", "Lat", "Date"))},
#                          list(Part_f_5[,-3], maiac_5_agg, nam, ndvi_5_agg,
#                               af_lags_5))) 
# ALL_unique<- unique(inner_join(Time_var, Stat, by = c("Lon", "Lat")))
# 
# dim(ALL_unique[,c("Lon", "Lat", "Date")]) #1591592 unique lat-lon-date
# 
# rm(list=setdiff(ls(), "ALL_unique"))
# save.image("Done_for_now.RData")
# 
# library(sp)
# library(maps)
# library(maptools)
# 
# latlong2state <- function(pointsDF) {
#   # Prepare SpatialPolygons object with one SpatialPolygon
#   # per state (plus DC, minus HI & AK)
#   states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
#   IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
#   states_sp <- map2SpatialPolygons(states, IDs=IDs,
#                                    proj4string=CRS("+proj=longlat +datum=WGS84"))
#   
#   stateNames <- sapply(states_sp@polygons, function(x) x@ID)
#   
#   # Convert pointsDF to a SpatialPoints object
#   pointsSP <- SpatialPoints(pointsDF,
#                             proj4string=CRS("+proj=longlat +datum=WGS84"))
#   
#   # Use 'over' to get _indices_ of the Polygons object containing each point
#   indices <- over(pointsSP, states_sp)
#   
#   NApos<- which(is.na(indices))
#   
#   for(p in NApos){
#     if(pointsDF[p,2]<= 41.9952){
#       if(pointsDF[p,1] <= -114.1315){
#         indices[p]<- which(stateNames == "california")
#       }else if(pointsDF[p,1] <= -109.0475){
#         indices[p]<- which(stateNames == "arizona")
#       }else{
#         indices[p]<- which(stateNames == "new mexico")
#       }
#     }else if((pointsDF[p,2] > 41.9952)& (pointsDF[p,2]<= 46.2891) ){
#       indices[p]<- which(stateNames == "oregon")
#     }else if(pointsDF[p,2] > 46.2891){
#       if(pointsDF[p,1] <= -117.2372){
#         indices[p]<- which(stateNames == "washington")
#       }else if(pointsDF[p,1] <= -116.0458){
#         indices[p]<- which(stateNames == "idaho")
#       }else{
#         indices[p]<- which(stateNames == "montana")
#       }
#     }
#   }
#   
#   # Return the state names of the Polygons object containing each point
#   stateNames[indices]
# }
# 
# ALL_unique$State<- latlong2state(as.data.frame(ALL_unique[,c("Lon", "Lat")]))
# 
# ALL_unique$Day<- sapply(ALL_unique$Date, function(x){as.numeric(strsplit(as.character(x), split = "-")[[1]][3])})
# ALL_unique$DayOfWeek<- sapply(ALL_unique$Date, function(x){wday(as.POSIXct(x))})
# ALL_unique$Season<- sapply(ALL_unique$Month, function(m){ifelse(m %in% c(3:5), "spring", ifelse(m %in% c(6:8), "summer",
#                                                                                                 ifelse(m %in% c(9:11), "fall", "winter")))})
# 
# non_leap<- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
# leap<- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
# 
# DOY<- function(YMD){ # YMD is a vector containing year, month, day
#   
#   #Figure out if it is a leap year or not
#   if(YMD[1] %in% c(2008, 2012, 2016)){
#     nums<- leap
#   }else{
#     nums<- non_leap
#   }
#   
#   if(YMD[2] == 1){
#     return(YMD[3])
#   }else{
#     return(sum(nums[1:(YMD[2]-1)]) + YMD[3])
#   }
# }
# 
# my_DOY<- apply(ALL_unique[,c("Year", "Month", "Day")], MARGIN = 1, FUN = DOY)
# 
# ALL_unique$CosDOW<- cos(2*pi*ALL_unique$DayOfWeek/7)
# ALL_unique$CosDOY<- cos(2*pi*my_DOY/365)
# ALL_unique$CosMonth<- cos(2*pi*ALL_unique$Month/12)
# 
# ALL_unique[,sapply(names(ALL_unique), FUN = function(x){str_detect(x, "Fires_lag")})]<- apply(ALL_unique[,sapply(names(ALL_unique), FUN = function(x){str_detect(x, "Fires_lag")})], MARGIN = 2, as.numeric)
# 
# ALL_unique$Binary_fire<- rowSums(ALL_unique[,sapply(names(ALL_unique), FUN = function(x){str_detect(x, "Fires_lag")})]) > 0
# 
# write.csv(ALL_unique, "Part_g_merged4.csv", row.names = FALSE)
# 
# rm(list=setdiff(ls(), "ALL_unique"))
# save.image("Done4.RData")
# 
# ##Get additional variables from ML scripts...
# 
# ##Merge with CMAQ
# 
