library("missRanger")

State<- c("nevada", "colorado", "utah", "new mexico", "arizona",
          "washington", "oregon", "idaho", "montana", "wyoming", "california")

###Note: have to split up the sets to impute the data from California.
  ## None of the other states crash when you use all their data.

for(s in State){
  print(s)
  
  load(paste0("~/FINAL_by_state/Pred_withNA_", s, ".RData"))
  
  exclude_vars<- which(names(To_save) %in% c("State_FIPS", "County_FIPS", "Tract_code",
                                             "ZCTA5_code", "State", "Region", "Region_Mid_Study", "Region_Late_Study"))
  
  factors<- c("Date", "Season", "Mid_Study", "Late_Study", "Binary_fire")
  
  data<- To_save[,-exclude_vars]
  
  ####
  data1<- data[which(data$Year %in% 2008:2009),]
  
  data1[,which(! names(data) %in% factors)[1:18]]<- apply(data1[,which(! names(data) %in% factors)[1:18]], 
                                                          MARGIN = 2, function(y) as.numeric(as.character(y)))
  data1[,which(! names(data) %in% factors)[19:36]]<- apply(data1[,which(! names(data) %in% factors)[19:36]], 
                                                           MARGIN = 2, function(y) as.numeric(as.character(y)))
  
  data1[,factors]<- apply(data1[,factors], MARGIN = 2, function(y) as.character(y))
  
  
  my_int1<- missRanger(data1, pmm.k = 3, splitrule = "extratrees", 
                       #Data here if testing accuracy
                       num.trees = 50, sample.fraction = 0.6, maxiter = 1)
  
  DF1<- cbind(To_save[which(To_save$Year %in% 2008:2009),exclude_vars], my_int1)[,-1]
  
  DF1$Missing_NDVI<- is.na(data1$ndvi)
  DF1$Missing_NAM<- is.na(data1$TMP_2m)
  DF1$Missing_MAIAC<- is.na(data1$MAIAC_AOD)
  DF1$Missing_vars<- DF1$Missing_MAIAC | DF1$Missing_NDVI | DF1$Missing_NAM
  
  save(DF1, file = paste0("~/FINAL_by_state/Imputed_", s, "_2008-2009.RData"),
       compress = TRUE)
  
  rm(list=c("data1", "DF1", "my_int1"))
  
  ####
  
  data2<- data[which(data$Year %in% 2010:2011),]
  
  data2[,which(! names(data) %in% factors)[1:18]]<- apply(data2[,which(! names(data) %in% factors)[1:18]], 
                                                          MARGIN = 2, function(y) as.numeric(as.character(y)))
  data2[,which(! names(data) %in% factors)[19:36]]<- apply(data2[,which(! names(data) %in% factors)[19:36]], 
                                                           MARGIN = 2, function(y) as.numeric(as.character(y)))
  
  data2[,factors]<- apply(data2[,factors], MARGIN = 2, function(y) as.character(y))
  
  my_int2<- missRanger(data2, pmm.k = 3, splitrule = "extratrees", 
                       #Data here if testing accuracy
                       num.trees = 50, sample.fraction = 0.6, maxiter = 1)
  
  DF2<- cbind(To_save[which(To_save$Year %in% 2010:2011),exclude_vars], my_int2)[,-1]
  
  DF2$Missing_NDVI<- is.na(data2$ndvi)
  DF2$Missing_NAM<- is.na(data2$TMP_2m)
  DF2$Missing_MAIAC<- is.na(data2$MAIAC_AOD)
  DF2$Missing_vars<- DF2$Missing_MAIAC | DF2$Missing_NDVI | DF2$Missing_NAM
  
  save(DF2, file = paste0("~/FINAL_by_state/Imputed_", s, "_2010-2011.RData"),
       compress = TRUE)
  
  rm(list=c("data2", "DF2", "my_int2"))
  
  ####
  
  data3<- data[which(data$Year %in% 2012:2013),]
  
  data3[,which(! names(data) %in% factors)[1:18]]<- apply(data3[,which(! names(data) %in% factors)[1:18]], 
                                                          MARGIN = 2, function(y) as.numeric(as.character(y)))
  data3[,which(! names(data) %in% factors)[19:36]]<- apply(data3[,which(! names(data) %in% factors)[19:36]], 
                                                           MARGIN = 2, function(y) as.numeric(as.character(y)))
  
  data3[,factors]<- apply(data3[,factors], MARGIN = 2, function(y) as.character(y))
  
  my_int3<- missRanger(data3, pmm.k = 3, splitrule = "extratrees", 
                       #Data here if testing accuracy
                       num.trees = 50, sample.fraction = 0.6, maxiter = 1)
  
  DF3<- cbind(To_save[which(To_save$Year %in% 2012:2013),exclude_vars], my_int3)[,-1]
  
  DF3$Missing_NDVI<- is.na(data3$ndvi)
  DF3$Missing_NAM<- is.na(data3$TMP_2m)
  DF3$Missing_MAIAC<- is.na(data3$MAIAC_AOD)
  DF3$Missing_vars<- DF3$Missing_MAIAC | DF3$Missing_NDVI | DF3$Missing_NAM
  
  save(DF3, file = paste0("~/FINAL_by_state/Imputed_", s, "_2012-2013.RData"),
       compress = TRUE)
  
  rm(list=c("data3", "DF3", "my_int3"))
  
  ####
  
  data4<- data[which(data$Year %in% 2014:2015),]
  
  data4[,which(! names(data) %in% factors)[1:18]]<- apply(data4[,which(! names(data) %in% factors)[1:18]], 
                                                          MARGIN = 2, function(y) as.numeric(as.character(y)))
  data4[,which(! names(data) %in% factors)[19:36]]<- apply(data4[,which(! names(data) %in% factors)[19:36]], 
                                                           MARGIN = 2, function(y) as.numeric(as.character(y)))
  
  data4[,factors]<- apply(data4[,factors], MARGIN = 2, function(y) as.character(y))
  
  my_int4<- missRanger(data4, pmm.k = 3, splitrule = "extratrees", 
                       #Data here if testing accuracy
                       num.trees = 50, sample.fraction = 0.6, maxiter = 1)
  
  DF4<- cbind(To_save[which(To_save$Year %in% 2014:2015),exclude_vars], my_int4)[,-1]
  
  DF4$Missing_NDVI<- is.na(data4$ndvi)
  DF4$Missing_NAM<- is.na(data4$TMP_2m)
  DF4$Missing_MAIAC<- is.na(data4$MAIAC_AOD)
  DF4$Missing_vars<- DF4$Missing_MAIAC | DF4$Missing_NDVI | DF4$Missing_NAM
  
  save(DF4, file = paste0("~/FINAL_by_state/Imputed_", s, "_2014-2015.RData"),
       compress = TRUE)
  
  rm(list=c("data4", "DF4", "my_int4"))
  
  ####
  
  data4<- data[which(data$Year %in% 2016:2017),]
  
  data4[,which(! names(data) %in% factors)[1:18]]<- apply(data4[,which(! names(data) %in% factors)[1:18]], 
                                                          MARGIN = 2, function(y) as.numeric(as.character(y)))
  data4[,which(! names(data) %in% factors)[19:36]]<- apply(data4[,which(! names(data) %in% factors)[19:36]], 
                                                           MARGIN = 2, function(y) as.numeric(as.character(y)))
  
  data4[,factors]<- apply(data4[,factors], MARGIN = 2, function(y) as.character(y))
  
  my_int4<- missRanger(data4, pmm.k = 3, splitrule = "extratrees", 
                       #Data here if testing accuracy
                       num.trees = 50, sample.fraction = 0.6, maxiter = 1)
  
  DF4<- cbind(To_save[which(To_save$Year %in% 2016:2017),exclude_vars], my_int4)[,-1]
  
  DF4$Missing_NDVI<- is.na(data4$ndvi)
  DF4$Missing_NAM<- is.na(data4$TMP_2m)
  DF4$Missing_MAIAC<- is.na(data4$MAIAC_AOD)
  DF4$Missing_vars<- DF4$Missing_MAIAC | DF4$Missing_NDVI | DF4$Missing_NAM
  
  save(DF4, file = paste0("~/FINAL_by_state/Imputed_", s, "_2016-2017.RData"),
       compress = TRUE)
  
  rm(list=c("data4", "DF4", "my_int4"))
  
  ####
  
  data4<- data[which(data$Year == 2018),]
  
  data4[,which(! names(data) %in% factors)[1:18]]<- apply(data4[,which(! names(data) %in% factors)[1:18]], 
                                                          MARGIN = 2, function(y) as.numeric(as.character(y)))
  data4[,which(! names(data) %in% factors)[19:36]]<- apply(data4[,which(! names(data) %in% factors)[19:36]], 
                                                           MARGIN = 2, function(y) as.numeric(as.character(y)))
  
  data4[,factors]<- apply(data4[,factors], MARGIN = 2, function(y) as.character(y))
  
  my_int4<- missRanger(data4, pmm.k = 3, splitrule = "extratrees", 
                       #Data here if testing accuracy
                       num.trees = 50, sample.fraction = 0.6, maxiter = 1)
  
  DF4<- cbind(To_save[which(To_save$Year == 2018),exclude_vars], my_int4)[,-1]
  
  DF4$Missing_NDVI<- is.na(data4$ndvi)
  DF4$Missing_NAM<- is.na(data4$TMP_2m)
  DF4$Missing_MAIAC<- is.na(data4$MAIAC_AOD)
  DF4$Missing_vars<- DF4$Missing_MAIAC | DF4$Missing_NDVI | DF4$Missing_NAM
  
  save(DF4, file = paste0("~/FINAL_by_state/Imputed_", s, "_2018.RData"),
       compress = TRUE)
  
  rm(list=c("data4", "DF4", "my_int4"))
  
}


##Add in CMAQ:
library(dplyr)

for(s in State[-8]){
  print(s)
  load(paste0("~/FINAL_by_state/Imputed_", s, ".RData"))
  load(paste0("~/FINAL_by_state/Pred_withNA_CMAQ_", s, ".RData"))
  
  DF$Date<- as.Date(DF$Date)
  
  uniq_cmaq<- distinct(cmaq)
  rm(cmaq)
  
  # cm_lon<- round(uniq_cmaq$Lon, 4)
  # match_lon<- match(cm_lon, DF$Lon)
  # 
  # cm_lat<- round(uniq_cmaq$Lat, 5)
  # match_lat<- match(cm_lat, DF$Lat)
  # 
  uniq_cmaq$Lon<- round(uniq_cmaq$Lon, 4)
  uniq_cmaq$Lat<- round(uniq_cmaq$Lat, 5)
  
  data<- left_join(DF, uniq_cmaq, by = c("Lon", "Lat", "Date"))
  
  # ##Testing accuracy:
  # n<- round(dim(data)[1]*0.01)
  # test_pos<- sample(1:(dim(data)[1]),n, replace = FALSE)
  # 
  # Data<- data
  # Data[test_pos,"all_CMAQ"]<- NA
  # 
  # start<- Sys.time()
  # 
  # my_int<- missRanger(Data[which(data$Year<2017),c(6:48, 53)], pmm.k = 3, splitrule = "extratrees", 
  #                     #Data here if testing accuracy
  #                     num.trees = 50, sample.fraction = 0.6, maxiter = 1)
  # 
  # end<- Sys.time()
  # print(end - start)
  # 
  # ##More accuracy stuff:
  # test<- cbind(Orig=data[test_pos,"all_CMAQ"], Imp=my_int[test_pos,"all_CMAQ"])
  # test<- test[which(!is.na(test[,1])),]
  # print("CMAQ:")
  # print(paste("RMSE/SD =",round(sqrt(mean((test[,1] - test[,2])^2))/sd(test[,1]),4)))
  # print(paste("R2 =", round((cor(test[,1], test[,2]))^2,4)))
  
  data1<- data[1:20000000,]
  data2<- data[20000001:(dim(data)[1]),]
  
  ###Here, one should avoid imputing the County and Census Tract FIPS!
    ## I did not, hence code at the end of the Predictions.R script to fix this
  
  my_int1<- missRanger(data1[which(data1$Year<2017),], pmm.k = 3, splitrule = "extratrees", 
                       #Data here if testing accuracy
                       num.trees = 50, sample.fraction = 0.6, maxiter = 1)
  
  my_int2<- missRanger(data2[which(data2$Year<2017),], pmm.k = 3, splitrule = "extratrees", 
                       #Data here if testing accuracy
                       num.trees = 50, sample.fraction = 0.6, maxiter = 1)
  
  Data<- rbind(my_int1, my_int2, data[which(data$Year > 2016),])
  Data$Missing_CMAQ<- c(is.na(data[which(data$Year<2017),"all_CMAQ"]),
                        rep(FALSE, length(which(data$Year > 2016))))
  
  save(Data, file = paste0("~/FINAL_by_state/Prediction_inputs_", s, ".RData"),
       compress = TRUE)
}

  