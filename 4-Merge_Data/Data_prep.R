# ##Read in data:
# data<- read.csv("~/Data/Processed_Data/ML_input_files/Part_f_merged4.csv")
# 
# #Don't use these columns:
# remove_cols<- which(names(data) %in% c("Month", "Day", "APCP_surface", "C_100"))
# DATA<- data[,-remove_cols]
# 
# DATA$Date<- as.Date(DATA$Date)
# non_numeric_vars<- c("Date", "State", "Season", "Binary_fire")
# DATA[,-which(names(DATA) %in% non_numeric_vars)]<- apply(DATA[,-which(names(DATA) %in% non_numeric_vars)],
#                                                          MARGIN = 2, function(y){as.numeric(as.character(y))})
# 
# #Look at high PM2.5 values:
# highs<- DATA[which(DATA$PM2.5_Obs > 3000),] #3000 >> 18 obs
# 
# DATA<- DATA[-which(DATA$PM2.5_Obs > 850),]
# 
# rm(list=setdiff(ls(), "DATA"))
# # save.image("ML_DATA.RData")
# # 
# # load("ML_DATA.RData")
# 
# #Additional indicators:
# DATA$Mid_Study<- (DATA$Year > 2012)&(DATA$Year <= 2016)
# DATA$Late_Study<- DATA$Year > 2016
# DATA$Year<- as.factor(DATA$Year)
# 
# region<- c()
# for(i in 1:dim(DATA)[1]){
#   if(DATA[i, "State"] %in% c("california", "nevada")){
#     region<- append(region, "Southwest")
#   }else if(DATA[i, "State"] %in% c("colorado", "utah", "new mexico",
#                                    "arizona")){
#     region<- append(region, "Four Corners")
#   }else if(DATA[i, "State"] %in% c("washington", "oregon")){
#     region<- append(region, "Northwest")
#   }else if(DATA[i, "State"] %in% c("idaho", "montana", "wyoming")){
#     region<- append(region, "Northern Mountain States")
#   }else{
#     region<- append(region, "Texas")
#   }
# }
# DATA$Region<- region
# 
# #Reduce active fire variables:
# 
# agg_AF<- function(i){
#   lag<- DATA[,sapply(c("25km", "50km", "100km", "500km"), function(x) paste0("Fires_lag",i, "_", x))]
#   lag[,1]<- lag[,1]*(1/25)
#   lag[,2]<- (lag[,2] - lag[,1])*(1/50)
#   lag[,3]<- (lag[,3] - lag[,2])*(1/100)
#   lag[,4]<- (lag[,4] - lag[,3])*(1/500)
#   return(as.vector(rowSums(lag)))
# }
# 
# DATA$Lag0<- agg_AF(0)
# DATA$Lag1<- agg_AF(1)
# DATA$Lag2<- agg_AF(2)
# DATA$Lag3<- agg_AF(3)
# DATA$Lag4<- agg_AF(4)
# DATA$Lag5<- agg_AF(5)
# DATA$Lag6<- agg_AF(6)
# DATA$Lag7<- agg_AF(7)
# 
# #Keep only new AF variables and "Both" highway variables:
# data<- DATA[,c(1:17,50:53, 55, 58, 61, 64:83)]
# 
# save.image("~/Data/Final_data.RData")
# load("~/Data/Final_data.RData")
# 
# early_CMAQ<- read.csv("~/Data/Data_with_CMAQ_08-12.csv")
# late_CMAQ<- read.csv("~/Data/Data_with_CMAQ_13-16.csv")
# 
# vars<- names(data)
# CMAQ_data<- rbind(early_CMAQ[,c(vars, "all_CMAQ")], late_CMAQ[,c(vars, "all_CMAQ")])
# 
# rm(list=setdiff(ls(), c("DATA", "data", "CMAQ_data", "vars")))
# 
# #Make interactions
# data$Region_Mid_Study<- interaction(data$Region, data$Mid_Study)
# data$Region_Late_Study<- interaction(data$Region, data$Late_Study)
# 
# CMAQ_data$Region_Mid_Study<- interaction(CMAQ_data$Region, CMAQ_data$Mid_Study)
# CMAQ_data$Region_Late_Study<- interaction(CMAQ_data$Region, CMAQ_data$Late_Study)
# 
# save.image("FINAL_ML_data.RData")

# load("FINAL_ML_data.RData")
#
# library(parallel)
# 
# #Make sure everything is numeric!!
# categorical1<- which(names(data) %in% c("State", "Season", "Region",
#                                         "Date", "Binary_fire",
#                                         "Mid_Study", "Late_Study",
#                                         "Region_Mid_Study", "Region_Late_Study"))
# data[,-categorical1]<- apply(data[,-categorical1], MARGIN = 2, as.numeric)
# data$Date<- as.Date(data$Date)
# 
# set.seed(321)
# tr_pos<- sample(1:(dim(data)[1]),round(dim(data)[1]*0.9), replace = FALSE)
# train<- data[tr_pos,]
# test<- data[-tr_pos,]
# lev<- 15
# 
# high_train<- train[which(train$PM2.5_Obs >= lev),]
# low_train<- train[which(train$PM2.5_Obs < lev),]
# high_test<- test[which(test$PM2.5_Obs >= lev),]
# low_test<- test[which(test$PM2.5_Obs < lev),]
# 
# #CMAQ data:
# early_data<- data[which(data$Year %in% 2008:2012),]
# ncores<- detectCores()-1
# uniq_seq<- seq(1,dim(early_data)[1], length.out = ncores + 1)
# loc_list<- c()
# for(j in 1:ncores){
#   uniq_locs<- round(uniq_seq[j]):round(uniq_seq[j+1]-1)
#   loc_list<- append(loc_list, list(uniq_locs))
# }
# early_pos<- c(unlist(loc_list), dim(early_data)[1])
# 
# data_2013<- data[which(data$Year %in% 2013),]
# uniq_seq2013<- seq(1,dim(data_2013)[1], length.out = ncores + 1)
# loc_list2013<- c()
# for(j in 1:ncores){
#   uniq_locs2013<- round(uniq_seq2013[j]):round(uniq_seq2013[j+1]-1)
#   loc_list2013<- append(loc_list2013, list(uniq_locs2013))
# }
# pos_2013<- c(unlist(loc_list2013), dim(data_2013)[1])
# 
# data_2014<- data[which(data$Year %in% 2014),]
# uniq_seq2014<- seq(1,dim(data_2014)[1], length.out = ncores + 1)
# loc_list2014<- c()
# for(j in 1:ncores){
#   uniq_locs2014<- round(uniq_seq2014[j]):round(uniq_seq2014[j+1]-1)
#   loc_list2014<- append(loc_list2014, list(uniq_locs2014))
# }
# pos_2014<- c(unlist(loc_list2014), dim(data_2014)[1])
# 
# data_2015<- data[which(data$Year %in% 2015),]
# uniq_seq2015<- seq(1,dim(data_2015)[1], length.out = ncores + 1)
# loc_list2015<- c()
# for(j in 1:ncores){
#   uniq_locs2015<- round(uniq_seq2015[j]):round(uniq_seq2015[j+1]-1)
#   loc_list2015<- append(loc_list2015, list(uniq_locs2015))
# }
# pos_2015<- c(unlist(loc_list2015), dim(data_2015)[1])
# 
# data_2016<- data[which(data$Year %in% 2016),]
# uniq_seq2016<- seq(1,dim(data_2016)[1], length.out = ncores + 1)
# loc_list2016<- c()
# for(j in 1:ncores){
#   uniq_locs2016<- round(uniq_seq2016[j]):round(uniq_seq2016[j+1]-1)
#   loc_list2016<- append(loc_list2016, list(uniq_locs2016))
# }
# pos_2016<- c(unlist(loc_list2016), dim(data_2016)[1])
# 
# CMAQ_pos<- c(row.names(early_data)[early_pos], row.names(data_2013)[pos_2013],
#              row.names(data_2014)[pos_2014], row.names(data_2015)[pos_2015],
#              row.names(data_2016)[pos_2016])
# row.names(CMAQ_data)<- CMAQ_pos
# train_names<- intersect(row.names(train),CMAQ_pos)
# test_names<- intersect(row.names(test),CMAQ_pos)
# 
# #Make sure everything is numeric!!
# categorical2<- which(names(CMAQ_data) %in% c("State", "Season", "Region",
#                                              "Date", "Binary_fire",
#                                              "Mid_Study", "Late_Study",
#                                              "Region_Mid_Study", "Region_Late_Study"))
# 
# CMAQ_data[,-categorical2]<- apply(CMAQ_data[,-categorical2], MARGIN = 2, as.numeric)
# CMAQ_data$Date<- as.Date(CMAQ_data$Date)
# 
# CMAQ_train<- CMAQ_data[match(train_names, row.names(CMAQ_data)),]
# CMAQ_test<- CMAQ_data[match(test_names, row.names(CMAQ_data)),]
# 
# CMAQ_high_train<- CMAQ_train[which(CMAQ_train$PM2.5_Obs >= lev),]
# CMAQ_low_train<- CMAQ_train[which(CMAQ_train$PM2.5_Obs < lev),]
# CMAQ_high_test<- CMAQ_test[which(CMAQ_test$PM2.5_Obs >= lev),]
# CMAQ_low_test<- CMAQ_test[which(CMAQ_test$PM2.5_Obs < lev),]
# 
# rm(list=c("early_pos", "loc_list", "loc_list2013", "loc_list2014", "loc_list2015", "loc_list2016",
#           "pos_2013", "pos_2014", "pos_2015", "pos_2016", "uniq_locs",
#           "uniq_locs2013", "uniq_locs2014", "uniq_locs2015", "uniq_locs2016",
#           "uniq_seq", "uniq_seq2013", "uniq_seq2014", "uniq_seq2015", "uniq_seq2016"))
# rm(list=c("data_2013","data_2014", "data_2015", "data_2016",
#           "early_data", "test", "train", "CMAQ_pos", "test_names", "train_names",
#           "tr_pos"))
# gc()
# 
# save.image("FINAL_ML_data2.RData")
#
#
##Add back in AF variables:
# load("FINAL_ML_data2.RData")
# 
# #Make interactions:
# DATA$Region_Mid_Study<- interaction(DATA$Region, DATA$Mid_Study)
# DATA$Region_Late_Study<- interaction(DATA$Region, DATA$Late_Study)
# 
# with_AF<- DATA[,c(1:53, 55, 58, 61, 64:75, 84:85)]
# 
# save.image("Final_ML_data3.RData")

load("Final_ML_data3.RData")

train<- rbind(high_train, low_train)
test<- rbind(high_test, low_test)

train<- train[which(train$State != "texas"),]
test<- test[which(test$State != "texas"),]

CMAQ_train<- CMAQ_train[which(CMAQ_train$State != "texas"),]
CMAQ_test<- CMAQ_test[which(CMAQ_test$State != "texas"),]

Both<- rbind(train, test)
CMAQ_Both<- rbind(CMAQ_train, CMAQ_test)

####### Make spatial subsets:
unique_locs<- unique(Both[,c("Lon", "Lat")])
set.seed(321)
spatial_tr_pos<- sample(1:(dim(unique_locs)[1]),round(dim(unique_locs)[1]*0.9), replace = FALSE)
spatial_train<- Both[which(do.call(paste0, Both[,c("Lon", "Lat")]) %in% do.call(paste0, unique_locs[spatial_tr_pos,])),]
spatial_test<- Both[which(do.call(paste0, Both[,c("Lon", "Lat")]) %in% do.call(paste0, unique_locs[-spatial_tr_pos,])),]

##Get CMAQ:
train_names<- intersect(row.names(spatial_train),row.names(CMAQ_data))
test_names<- intersect(row.names(spatial_test),row.names(CMAQ_data))

CMAQ_spatial_train<- CMAQ_data[match(train_names, row.names(CMAQ_data)),]
CMAQ_spatial_test<- CMAQ_data[match(test_names, row.names(CMAQ_data)),]

# head(unique(CMAQ_spatial_test[,c("Lon", "Lat")]))
# head(unique(spatial_test[,c("Lon", "Lat")]))



