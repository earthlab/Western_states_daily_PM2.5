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

# ##Active Fires
# 
# AF1a<- read.csv("~/Data/fire_modis_25km_extract_final.csv")
# AF1b<- read.csv("~/Data/fire_modis_50km_extract_final.csv")
# AF1c<- read.csv("~/Data/fire_modis_100km_extract_final.csv")
# AF1d<- read.csv("~/Data/fire_modis_500km_extract_final.csv")
# 
# af1a<- data.frame(Lon = AF1a$Lon, Lat = AF1a$Lat, Date = AF1a$Date, Fires_25km = AF1a$fire_count)
# af1b<- data.frame(Lon = AF1b$Lon, Lat = AF1b$Lat, Date = AF1b$Date, Fires_50km = AF1b$fire_count)
# af1c<- data.frame(Lon = AF1c$Lon, Lat = AF1c$Lat, Date = AF1c$Date, Fires_100km = AF1c$fire_count)
# af1d<- data.frame(Lon = AF1d$Lon, Lat = AF1d$Lat, Date = AF1d$Date, Fires_500km = AF1d$fire_count)
# 
# AF<- Reduce(function(x,y) merge(x = x, y = y, by = c("Lon", "Lat", "Date"), all = TRUE), list(af1a, af1b, af1c, af1d))
# 
# AF[is.na(AF)]<- 0
# 
# AF$Date<- as.Date(AF$Date)
# AF$Lon<- round(AF$Lon, 4)
# AF$Lat<- round(AF$Lat, 4)
# 
# AF_agg<- aggregate(. ~ Lon + Lat + Date, AF, mean)
# AF<- AF_agg
# AF_unique_locs<- unique(AF_agg[,c("Lon", "Lat")])
# 
# rm(list=c("AF", "AF1a", "AF1b", "AF1c", "AF1d", "af1a", "af1b", "af1c", "af1d"))
# 
# ncores = detectCores() - 6
# AF_lags_template<- data.frame(Lon = numeric(), Lat = numeric(), Date = as.Date(character()),
#                               Fires_lag0_25km = numeric(), Fires_lag0_50km = numeric(),
#                               Fires_lag0_100km = numeric(), Fires_lag0_500km = numeric(),
#                               Fires_lag1_25km = numeric(), Fires_lag1_50km = numeric(),
#                               Fires_lag1_100km = numeric(), Fires_lag1_500km = numeric(),
#                               Fires_lag2_25km = numeric(), Fires_lag2_50km = numeric(),
#                               Fires_lag2_100km = numeric(), Fires_lag2_500km = numeric(),
#                               Fires_lag3_25km = numeric(), Fires_lag3_50km = numeric(),
#                               Fires_lag3_100km = numeric(), Fires_lag3_500km = numeric(),
#                               Fires_lag4_25km = numeric(), Fires_lag4_50km = numeric(),
#                               Fires_lag4_100km = numeric(), Fires_lag4_500km = numeric(),
#                               Fires_lag5_25km = numeric(), Fires_lag5_50km = numeric(),
#                               Fires_lag5_100km = numeric(), Fires_lag5_500km = numeric(),
#                               Fires_lag6_25km = numeric(), Fires_lag6_50km = numeric(),
#                               Fires_lag6_100km = numeric(), Fires_lag6_500km = numeric(),
#                               Fires_lag7_25km = numeric(), Fires_lag7_50km = numeric(),
#                               Fires_lag7_100km = numeric(), Fires_lag7_500km = numeric())
# 
# #Second try:
# dates<- seq.Date(as.Date("2008-01-01"), as.Date("2018-12-31"), by = "day")
# Date<- sort(rep(dates, dim(AF_unique_locs)[1]))
# Lon<- rep(AF_unique_locs$Lon, length(dates))
# Lat<- rep(AF_unique_locs$Lat, length(dates))
# all_LLD<- data.frame(Lon, Lat, Date)
# 
# ready_AF<- left_join(all_LLD, AF_agg, by = c("Lon", "Lat", "Date"))
# num_Locs<- dim(AF_unique_locs)[1]
# 
# merge_AF_lags<- function(these, j){
#   these_AF_lags<- AF_lags_template
#   p<-1
#   for(l in these){
#     for(d in 1:length(dates)){
#       lags<- 7
#       if(dates[d] < "2008-01-08"){
#         lags<- as.numeric(dates[d] - as.Date("2008-01-01") )
#       }
#       these_AF_lags[p,1]<- ready_AF[l,"Lon"]
#       these_AF_lags[p,2]<- ready_AF[l,"Lat"]
#       these_AF_lags[p,3]<- dates[d]
#       my_vec<- c()
#       for(i in 0:lags){
#         new<- ready_AF[(d-1-i)*num_Locs + l, 4:7]
#         new[is.na(new)]<- 0
#         my_vec<- append(my_vec, new)
#       }
#       my_vec<- unlist(my_vec)
#       my_vec<- append(my_vec, rep(0, 32-length(my_vec)))
#       these_AF_lags[p,4:35]<- my_vec
#       p<- p+1
#     }
#   }
#   write.csv(these_AF_lags, paste0("~/Data/AF/AF_lags4_",j,".csv"), row.names = FALSE)
#   return(these_AF_lags)
# }
# 
# my_seq<- seq(1, num_Locs, length.out = 300)
# 
# loc_list<- c()
# 
# for(j in my_seq){
#   uniq_locs<- round(my_seq[j]):round(my_seq[j+1]-1)
#   loc_list<- append(loc_list, list(uniq_locs))
# }
# 
# loc_list[[length(loc_list)]]<- append(loc_list[[length(loc_list)]], num_Locs)
# 
# 
# options(future.globals.maxSize= 5000*1024^2)
# 
# plan(multiprocess, workers = 8) ## Parallelize
# this_list<- future_lapply(1:length(loc_list), function(j){merge_AF_lags(loc_list[[j]],j)})
# # save.image("With_AF_lags.RData")
# ##THEN, IN TERMINAL: cat _____* > ______
# 
# ##Get unique:
# AF_intermediate<- read.csv("~/Data/AF/AF_lags4.csv")
# 
# #Remove text:
# text_pos<- which((AF_intermediate$Lon == "Lon")&(AF_intermediate$Lat == "Lat"))
# AF_intermediate<- AF_intermediate[-text_pos,]
# 
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


#MAIAC
# MAIAC<- read.csv("~/Data/MAIAC_extracted.csv")
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

# ##NAM
# files<- list.files("~/NAM/", pattern="Step5*", full.names = TRUE)
# 
# for(s in State[-11]){
#   for(f in files){ #f is the filename
#     data<- read.csv(f)
#     date_str<- strsplit(f, "Step5_")[[1]][2] #Used to be 2
#     date<- strsplit(date_str, "_batch")[[1]][1]
#     names(data)[1:2]<- c("Lat", "Lon")
#     names(data)[length(names(data))]<- "Date"
#     data[,1:2]<- apply(data[,1:2], MARGIN = 2, 
#                        function(y) round(as.numeric(as.character(y)),5))
#     nam<- inner_join(data[,c(1:2, 7:21)], Stat[which(Stat$State == s),c("Lon", "Lat", "State")], by = c("Lon", "Lat"))
#     write.csv(nam, paste0("~/NAM/nam_",s,"_", date, ".csv"), row.names = FALSE)
#   }
#   print(s)
# }
##Then merge in terminal, read in, and remove header rows

# for(y in 2008:2018){
#   nam<- read.csv(paste0("~/Data/NAM_data/NAM_", y, ".csv"))
#   nam<- nam[,c(1:2,7:10, 14:19, 21)]
#   names(nam)<- c("Lat", "Lon", "HPBL_surface", "TMP_2m", "RH_2m",
#                  "DPT_2m",  "Ugrd_10m", "Vgrd_10m", "PRMSL_mean_sea_level",
#                  "PRES_surface", "DZDT_850_mb", "DZDT_700_mb", "Date")
#   nam<- nam[which(nam$Lon != "Longitude"),]
#   nam$Lon<- round(as.numeric(as.character(nam$Lon)), 5)
#   nam$Lat<- round(as.numeric(as.character(nam$Lat)), 5)
# 
#   NAM<- inner_join(nam, Stat, by = c("Lon", "Lat"))
#   all_NAM<- rbind(all_NAM, NAM)
# }
# save.image("NAM.RData")


##NDVI
# NDVI1<- read.csv("~/Data/ndvi_mod13a3_subset1_latlon.csv")
# NDVI2<- read.csv("~/Data/ndvi_mod13a3_subset2_latlon.csv")
# NDVI3<- read.csv("~/Data/ndvi_mod13a3_subset3_latlon.csv")
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
# save.image("NDVI_final.RData")

##Stationary variables:

#NLCD
NLCD1<- read.csv("~/Data/nlcd_1km_extract.csv")
NLCD2<- read.csv("~/Data/nlcd_5km_extract.csv")
NLCD3<- read.csv("~/Data/nlcd_10km_extract.csv")

NLCD1_agg<- aggregate(percent_urban_buffer ~ Lon + Lat, data = NLCD1, FUN = mean)
NLCD2_agg<- aggregate(percent_urban_buffer ~ Lon + Lat, data = NLCD2, FUN = mean)
NLCD3_agg<- aggregate(percent_urban_buffer ~ Lon + Lat, data = NLCD3, FUN = mean)

NLCD<- Reduce(function(x,y) unique(inner_join(x, y, by = c("Lon", "Lat"))), list(NLCD1_agg, NLCD2_agg, NLCD3_agg))

NLCD$Lon<- round(as.numeric(as.character(NLCD$Lon)),5)
NLCD$Lat<- round(as.numeric(as.character(NLCD$Lat)),5)

names(NLCD)<- c("Lon", "Lat", "NLCD_1km", "NLCD_5km",
                "NLCD_10km")

##Population Density
pop<- read.csv("~/Data/Pop_density.csv")
pop$Lon<- round(pop$Lon, 5)
pop$Lat<- round(pop$Lat, 5)
pop_agg<- aggregate(. ~ Lon + Lat, pop[,c(2:3, 8)], mean)


##Highways
HW<- read.csv("~/Data/Highways.csv")
HW$Lon<- round(HW$Lon, 5)
HW$Lat<- round(HW$Lat, 5)
HW_vars<- c("Lon", "Lat", "A_100", "C_100", "Both_100", "A_250", "C_250", "Both_250",
            "A_500", "C_500", "Both_500", "A_1000", "C_1000", "Both_1000")
HW_agg<- aggregate(. ~ Lon + Lat, HW[,HW_vars], mean)

##Elevation
elev<- read.csv("~/Data/ned_extracted.csv")
elev$Lon<- round(elev$Lon, 5)
elev$Lat<- round(elev$Lat, 5)
elev_agg<- aggregate(. ~ Lon + Lat, elev[,c(1:8)], mean)

##Merge stationary variables:

Stat<- unique(Reduce(function(x,y){inner_join(x,y, by = c("Lon", "Lat"))},
                     list(pop_agg, NLCD, HW_agg, elev_agg)))

write.csv(Stat, "Stationary_variables.csv", row.names = FALSE)
