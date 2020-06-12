##Libraries:
library(dplyr)
library(stringr)
library(caret)

dates<- seq.Date(as.Date(paste0("2008-01-01")), as.Date(paste0("2018-12-31")), by = "day")

# dates<- c(seq.Date(as.Date(paste0("2016-02-17")), as.Date(paste0("2016-02-25")), by = "day"),
          # seq.Date(as.Date(paste0("2011-03-03")), as.Date(paste0("2011-03-05")), by = "day"))
n_days<- length(dates)

Stat<- read.csv("FINAL_Stat.csv")

temporal<- read.csv("Temporal_variables.csv")
temporal$Date<- as.Date(as.character(temporal$Date))

regions<- read.csv("Regions.csv")

## Space-time interactions:
# Region<- rep(unique(regions$Region),3)
# Mid_Study<- c(rep(FALSE,4),rep(TRUE,4),rep(FALSE,4))
# Late_Study<- c(rep(FALSE,4),rep(FALSE,4),rep(TRUE,4))
# stdf<- data.frame(Region, Mid_Study, Late_Study,
#   Region_Mid_Study=interaction(Region, Mid_Study), 
#   Region_Late_Study= interaction(Region, Late_Study))
# write.csv(stdf, "Spacetime_interactions.csv", row.names=FALSE)
stdf<- read.csv("Spacetime_interactions.csv")

#All basic variables:
time_interactions<- inner_join(temporal, stdf, by=c("Mid_Study", "Late_Study"))
ti_state<- inner_join(time_interactions, regions, by="Region")

##Put it all together:
State<- c("nevada", "colorado", "utah", "new mexico", "arizona",
          "washington", "oregon", "idaho", "montana", "wyoming", "california")
#Need to round to four decimals for AF
Lon<- round(Stat$Lon, 4)
Lat<- round(Stat$Lat, 4)
stat<- data.frame(Lon, Lat, Stat[,3:6], State=Stat$State)
af_ref<- cbind(stat, Date = sort(rep(as.Date(dates), dim(stat)[1])))

##Checking for missing locations / days:
Locs<- unique(Stat[,c("Lon", "Lat", "State")])
Ref<- data.frame(Locs, Date = sort(rep(as.Date(dates), dim(Locs)[1])))

##Previously-missing NDVI:
ndvi_miss<- read.csv("~/NDVI_part_i.csv", header = FALSE)
ndvi_miss<- ndvi_miss[-dim(ndvi_miss)[1],]
names(ndvi_miss)<- c("Lon", "Lat", "Date", "ndvi")
ndvi_miss[,c(1,2,4)]<- apply(ndvi_miss[,c(1,2,4)], MARGIN = 2, 
                             function(y) as.numeric(as.character(y)))
ndvi_miss$Date<- as.Date(ndvi_miss$Date)

sink("Missing_individual_data_sets8.txt")
for(s in State){
  print(s)
  ref<- Ref[which(Ref$State == s),]
  # locs<- Locs[which(Locs$State == s),]
  
  #Active Fire:
  af<- read.csv(paste0("~/FINAL_state_sets/AF_FINAL_", s, ".csv"))
  ind<- which(names(af) == "X")
  af<- af[which(af$Lon != "Lon"),-ind]
  af$Date<- as.Date(as.character(af$Date))
  af[,which(names(af) != "Date")]<- apply(af[,which(names(af) != "Date")],
                                                        MARGIN = 2, function(x) round(as.numeric(as.character(x)),4) )
  af$Binary_fire<- rowSums(af[,sapply(names(af), FUN = function(x){str_detect(x, "Lag")})]) > 0

  # print("Active Fires:")
  # print(dim(af))
  # print(dim(unique(af[,c("Lon", "Lat")]))[1])
  # print(length(unique(af[,"Date"])))
  # print(dim(unique(af[,c("Lon", "Lat", "Date")]))[1])

  #Be sure to do a left_join with AF, so zeros are taken into account!
  S1<- left_join(af_ref[which(af_ref$State == s),], af, by = c("Lon", "Lat", "Date"))

  S1[which(is.na(S1$Binary_fire)),sapply(0:7, function(x) paste0("Lag",x))]<- 0
  S1[which(is.na(S1$Binary_fire)),"Binary_fire"]<- FALSE

  rm("af")
  print("Done with active fire")

  #MAIAC AOD:
  maiac<- read.csv(paste0("~/FINAL_state_sets/MAIAC_FINAL_", s, ".csv"))[,c(1:6,8,9)]
  maiac<- maiac[which(maiac$Lon != "Lon"),]
  maiac$Date<- as.Date(as.character(maiac$Date))
  maiac[,c("State_FIPS", "County_FIPS", "Tract_code", "ZCTA5_code",
           "Lon", "Lat")]<- apply(maiac[,c("State_FIPS", "County_FIPS", "Tract_code", "ZCTA5_code", "Lon", "Lat")],
                                                                           MARGIN = 2, function(x) as.numeric(as.character(x)) )
  maiac$Lon<- round(maiac$Lon, 5)
  maiac$Lat<- round(maiac$Lat, 5)

  switched<- which(maiac$Lat < 0)
  temp<- maiac[switched, "Lat"]
  maiac[switched, "Lat"]<- maiac[switched, "Lon"]
  maiac[switched, "Lon"]<- temp

  # print("MAIAC:")
  # print(dim(locs)[1] - dim(unique(maiac[,c("Lon", "Lat")]))[1])
  # print(n_days - length(unique(maiac[,"Date"])))
  # print(dim(ref)[1] - dim(unique(maiac[,c("Lon", "Lat", "Date")]))[1])

  S2<- left_join(S1[,c(3:(dim(S1)[2]))], maiac, by = c("State_FIPS","County_FIPS",
                                                        "Tract_code","ZCTA5_code",
                                                        "Date"))
  rm("maiac", "S1")

  # print(dim(locs)[1] - dim(unique(S2[,c("Lon", "Lat")]))[1])
  # print(n_days - length(unique(S2[,"Date"])))
  # print(dim(ref)[1] - dim(unique(S2[,c("Lon", "Lat", "Date")]))[1])
  print("Done with MAIAC AOD")


  #NAM:
  nam<- read.csv(paste0("~/FINAL_state_sets/NAM_FINAL_", s, ".csv"))[,c(1:13)]
  nam<- nam[which(nam$Lon != "Lon"),]
  nam$Date<- as.Date(as.character(nam$Date))
  # nam$Date<- as.Date(format(nam$Date, "%Y-%m-%d"))
  nam[,c("Lon", "Lat")]<- apply(nam[,c("Lon", "Lat")],
                                          MARGIN = 2, function(x) round(as.numeric(as.character(x)),5) )

  nam08<- read.csv(paste0("~/NAM_2008/NAM_",s,"_08.csv"))[,c(1:13)]
  nam08$Date<- as.Date(as.character(nam08$Date))

  NAM<- rbind(nam08, nam)

  # print("NAM:")
  # print(dim(locs)[1] - dim(unique(NAM[,c("Lon", "Lat")]))[1])
  # print(n_days - length(unique(NAM[,"Date"])))
  # print(dim(ref)[1] - dim(unique(NAM[,c("Lon", "Lat", "Date")]))[1])

  S3<- left_join(S2, NAM, by = c("Lon", "Lat", "Date"))
  # save(S3, file = paste0("~/FINAL_by_state/AF_MAIAC_NAM-not-unique_", s, ".RData"),
       # compress = TRUE)
  # S3<- distinct(S3)
  rm("nam", "nam08", "NAM", "S2")

  # print(dim(locs)[1] - dim(unique(S3[,c("Lon", "Lat")]))[1])
  # print(n_days - length(unique(S3[,"Date"])))
  # print(dim(ref)[1] - dim(unique(S3[,c("Lon", "Lat", "Date")]))[1])
  print("Done with NAM")

  #NDVI:
  ndvi<- read.csv(paste0("~/FINAL_state_sets/NDVI_FINAL_", s, ".csv"))[,c("Lon", "Lat",
                                                                          "Date", "ndvi")]
  ndvi<- ndvi[which(ndvi$Lon != "Lon"),]
  ndvi$Date<- as.Date(as.character(ndvi$Date))
  ndvi[,c("Lon", "Lat", "ndvi")]<- apply(ndvi[,c("Lon", "Lat", "ndvi")],
                                         MARGIN = 2, function(x) round(as.numeric(as.character(x)),5) )

  # print("NDVI:")
  # print(dim(locs)[1] - dim(distinct(ndvi[,c("Lon", "Lat")]))[1])
  # print(n_days - length(unique(ndvi[,"Date"])))
  # print(dim(ref)[1] - dim(distinct(ndvi[,c("Lon", "Lat", "Date")]))[1])

  ndvi<- rbind(ndvi, ndvi_miss)

  # ndvi_missing<- anti_join(ref[,-3], ndvi, by = c("Lon", "Lat", "Date"))
  # write.csv(ndvi_missing, paste0("~/FINAL_state_sets/NDVI_missing_",s,".csv"), row.names = FALSE)

  S4<- left_join(S3, ndvi, by = c("Lon", "Lat", "Date"))
  rm("ndvi", "S3")

  # print(dim(locs)[1] - dim(unique(S4[,c("Lon", "Lat")]))[1])
  # print(n_days - length(unique(S4[,"Date"])))
  # print(dim(ref)[1] - dim(unique(S4[,c("Lon", "Lat", "Date")]))[1])
  print("Done with NDVI")

  #Add in spacetime variables
  all<- inner_join(S4, regions, by = "State")
  rm("S4")
  all2<- inner_join(all, temporal, by = "Date")
  rm("all")
  all3<- inner_join(all2, stdf, by = c("Region", "Mid_Study", "Late_Study"))
  rm("all2")

  to_save<- left_join(all3, Stat[which(Stat$State == s),])

  # save(to_save, file = paste0("~/FINAL_by_state/Pred_withNA_not-unique_", s, ".RData"),
  #      # compress = TRUE)
  rm(list=setdiff(ls(), "to_save"))
       
  To_save<- distinct(to_save) #to_save

  save(To_save, file = paste0("~/FINAL_by_state/Pred_withNA_", s, ".RData"),
       compress = TRUE)

  rm("all3", "to_save")
  print("Saved without CMAQ")
  
  # #Find all missing
  # 
  # uniq_both<- distinct(to_save[,c("Lon", "Lat", "Date")])
  # uniq_locs<- distinct(to_save[,c("Lon", "Lat")])
  # uniq_days<- as.Date(unique(to_save$Date))
  # 
  # print("Without CMAQ:")
  # print("Number of Locations Missing:")
  # print(dim(locs)[1] - dim(uniq_locs)[1])
  # print("Number of Dates Missing:")
  # print(n_days - length(uniq_days))
  # print("Total Missing Location-Days")
  # print(dim(ref)[1] - dim(uniq_both)[1])
  # # 
  # #CMAQ:
  # cmaq<- read.csv(paste0("~/FINAL_state_sets/CMAQ_FINAL2_", s, ".csv"))
  # # cmaq<- cmaq[which(cmaq$Lon != "Lon"),]
  # names(cmaq)[4]<- "CMAQ_pm"
  # 
  # cmaq2<- read.csv(paste0("~/CMAQ_data/CMAQ_i_", s, ".csv"))
  # cmaq3<- read.csv(paste0("~/CMAQ_data/CMAQ_i2_", s, "_2013.csv"))
  # cmaq4<- read.csv(paste0("~/CMAQ_data/CMAQ_i2_", s, "_2014.csv"))
  # cmaq5<- read.csv(paste0("~/CMAQ_data/CMAQ_i2_", s, "_2015.csv"))
  # cmaq6<- read.csv(paste0("~/CMAQ_data/CMAQ_i2_", s, "_2016.csv"))
  # 
  # cmaq<- rbind(cmaq, cmaq2, cmaq3, cmaq4, cmaq5, cmaq6)
  # names(cmaq)[4]<- "all_CMAQ"
  # 
  # cmaq$Date<- as.Date(as.character(cmaq$Date))
  # cmaq[,c("Lon", "Lat")]<- apply(cmaq[,c("Lon", "Lat")],
  #                                MARGIN = 2, function(x) as.numeric(as.character(x)) )
  # 
  # 
  # cmaq_missing<- anti_join(ref[which(ref$Date < as.Date("2017-01-01")),], cmaq, 
  #                          by = c("Lon", "Lat", "Date"))
  # print(paste("CMAQ Missing:", dim(cmaq_missing)[1]))
  # 
  # # write.csv(cmaq_missing, paste0("~/FINAL_state_sets/CMAQ_missing_",s,".csv"), row.names = FALSE)
  # 
  # save(cmaq, file = paste0("~/FINAL_by_state/Pred_withNA_CMAQ_", s, ".RData"),
  #            compress = TRUE)
  # 
  # print("CMAQ:")
  # print(dim(cmaq))
  # print(dim(distinct(cmaq[,c("Lon", "Lat")]))[1])
  # print(length(distinct(cmaq[,"Date"])))
  # print(dim(distinct(cmaq[,c("Lon", "Lat", "Date")]))[1])
  #
  # all4<- left_join(To_save[which(To_save$Date < as.Date("2017-01-01")),], cmaq, by = c("Lon", "Lat", "Date"))
  # # all4<- inner_join(cmaq, all3[,-xx], by = c("Lon", "Lat", "Date"))
  # rm("cmaq", "To_save")
  # 
  # print("Done with CMAQ")
  # #
  # ##For now:
  # # write.csv(all4, paste0("~/FINAL_by_state/Pred_data_CMAQ_", s, ".csv"), row.names=FALSE)
  # save(all4, file = paste0("~/FINAL_by_state/Pred_withNA_CMAQ_", s, ".RData"),
  #      compress = TRUE)
  # # 
  # # uniq_both<- distinct(all4[,c("Lon", "Lat", "Date")])
  # # uniq_locs<- distinct(all4[,c("Lon", "Lat")])
  # # uniq_days<- as.Date(unique(all4$Date))
  # # 
  # # print("With CMAQ:")
  # # print("Number of Locations Missing:")
  # # print(dim(locs)[1] - dim(uniq_locs)[1])
  # # print("Number of Dates Missing:")
  # # print(3288 - length(uniq_days))
  # # print("Total Missing Location-Days")
  # # print((dim(ref)[1]/4018)*(3288) - dim(uniq_both)[1])
  # # 
  # # rm("all4")
  # print("Saved with CMAQ")
}

sink()

# missing_ndvi<- read.csv("~/FINAL_state_sets/NDVI_missing_all.csv")
# mls<- distinct(missing_ndvi[,c("Lon", "Lat")]) #2055 missing locations ~ 9%
# mls<- mls[which(mls$Lon != "Lon"),]
# mls$Lon<- as.numeric(as.character(mls$Lon))
# mls$Lat<- as.numeric(as.character(mls$Lat))
# 
# library("maps")
# map("state")
# points(Stat$Lon, Stat$Lat, pch = 16, cex = 0.4)
# points(mls$Lon, mls$Lat, pch = 16, cex = 0.4)
# 
# mls2<- mls[which(mls$Lon < -103.0023),] #Now only 1871 missing locations
# map("state")
# points(mls2$Lon, mls2$Lat, pch = 16, cex = 0.4)

##Get number of locs by state:
for(s in State){
  print(s)
  print(dim(Locs[which(Locs$State == s),])[1])
}

##Get number of missing from each data set:
sink("Sum_NAs_percentage3.txt")
for(s in State[-1]){
  print(s)
  
  load(paste0("~/FINAL_by_state/Pred_withNA_", s, ".RData"))
  n<- dim(To_save)[1]
  print("Total:")
  print(n)
  
  print("NDVI:")
  print(sum(is.na(To_save$ndvi))/n)
  print("NAM:")
  print(sum(is.na(To_save$TMP_2m))/n)
  print("MAIAC")
  print(sum(is.na(To_save$MAIAC_AOD))/n)
  
  # load(paste0("~/FINAL_by_state/Pred_withNA_CMAQ_", s, ".RData"))
  # n2<- dim(all4)[1]
  # print("CMAQ total:")
  # print(n2)
  # print("CMAQ:")
  # print(sum(is.na(all4$all_CMAQ))/n2)
}

sink()


##Investigating interpolation
#load a file -- idaho is small

##For ranger:
my_control<- trainControl(method = "none", savePredictions = "final")
tgrid<- expand.grid( .mtry = 5, .splitrule = "variance", .min.node.size = 6 )

sink("Interpolate_RF.txt")
exclude<- c(5, 9)
for(s in State){
  print(s)
  load(paste0("~/FINAL_by_state/Pred_withNA_", s, ".RData"))
  
  # my_ndvi<- lm(ndvi ~ Lon + Lat + Date + CosMonth + Pop_density 
  #              + NLCD_1km + NLCD_5km + NLCD_10km + elevation, 
  #              data = To_save, na.action = na.omit)
  # 
  # print(summary(my_ndvi)$adj.r.squared)
  
  
  my_ndvi<- train(ndvi ~ Lon + Lat + Date + CosMonth + Pop_density 
                  + NLCD_1km + NLCD_5km + NLCD_10km + elevation,
                  data = To_save,   na.action = na.omit,
                  method = "ranger", tuneGrid = tgrid, num.trees = 150, 
                  trControl = my_control)
  
  print(my_ndvi$finalModel$r.squared)
  
  
  To_save$MAIAC_AOD<- as.numeric(as.character(To_save$MAIAC_AOD))
  
  nam_ars<- c()
  for(i in 19:28){
    # my_nam<- lm(To_save[,i] ~ Lon + Lat + Date + CosMonth + CosDOY 
    #             + Pop_density + elevation + MAIAC_AOD, data = To_save, 
    #             na.action = na.omit)
    # nam_ars<- append(nam_ars, summary(my_nam)$adj.r.squared)
    
    To_save[,i]<- as.numeric(as.character(To_save[,i]))
  }
    
  my_nam<- train(HPBL_surface ~ Lon + Lat + Date + CosMonth + CosDOY 
                               + Pop_density + elevation + MAIAC_AOD,
                    data = To_save,   na.action = na.omit,
                    method = "ranger", tuneGrid = tgrid, num.trees = 150, 
                    trControl = my_control) #Need to have it be the name of the var
  
  nam_ars<- append(nam_ars, my_nam$finalModel$r.squared)
 
  my_nam<- train(TMP_2m ~ Lon + Lat + Date + CosMonth + CosDOY 
                 + Pop_density + elevation + MAIAC_AOD,
                 data = To_save,   na.action = na.omit,
                 method = "ranger", tuneGrid = tgrid, num.trees = 150, 
                 trControl = my_control) #Need to have it be the name of the var
  
  nam_ars<- append(nam_ars, my_nam$finalModel$r.squared)
  
  my_nam<- train(RH_2m ~ Lon + Lat + Date + CosMonth + CosDOY 
                 + Pop_density + elevation + MAIAC_AOD,
                 data = To_save,   na.action = na.omit,
                 method = "ranger", tuneGrid = tgrid, num.trees = 150, 
                 trControl = my_control) #Need to have it be the name of the var
  
  nam_ars<- append(nam_ars, my_nam$finalModel$r.squared)
  
  my_nam<- train(DPT_2m ~ Lon + Lat + Date + CosMonth + CosDOY 
                 + Pop_density + elevation + MAIAC_AOD,
                 data = To_save,   na.action = na.omit,
                 method = "ranger", tuneGrid = tgrid, num.trees = 150, 
                 trControl = my_control) #Need to have it be the name of the var
  
  nam_ars<- append(nam_ars, my_nam$finalModel$r.squared)
  
  my_nam<- train(Ugrd_10m ~ Lon + Lat + Date + CosMonth + CosDOY 
                 + Pop_density + elevation + MAIAC_AOD,
                 data = To_save,   na.action = na.omit,
                 method = "ranger", tuneGrid = tgrid, num.trees = 150, 
                 trControl = my_control) #Need to have it be the name of the var
  
  nam_ars<- append(nam_ars, my_nam$finalModel$r.squared)
  
  my_nam<- train(Vgrd_10m ~ Lon + Lat + Date + CosMonth + CosDOY 
                 + Pop_density + elevation + MAIAC_AOD,
                 data = To_save,   na.action = na.omit,
                 method = "ranger", tuneGrid = tgrid, num.trees = 150, 
                 trControl = my_control) #Need to have it be the name of the var
  
  nam_ars<- append(nam_ars, my_nam$finalModel$r.squared)
  
  my_nam<- train(PRMSL_mean_sea_level ~ Lon + Lat + Date + CosMonth + CosDOY 
                 + Pop_density + elevation + MAIAC_AOD,
                 data = To_save,   na.action = na.omit,
                 method = "ranger", tuneGrid = tgrid, num.trees = 150, 
                 trControl = my_control) #Need to have it be the name of the var
  
  nam_ars<- append(nam_ars, my_nam$finalModel$r.squared)
  
  my_nam<- train(PRES_surface ~ Lon + Lat + Date + CosMonth + CosDOY 
                 + Pop_density + elevation + MAIAC_AOD,
                 data = To_save,   na.action = na.omit,
                 method = "ranger", tuneGrid = tgrid, num.trees = 150, 
                 trControl = my_control) #Need to have it be the name of the var
  
  nam_ars<- append(nam_ars, my_nam$finalModel$r.squared)
  
  my_nam<- train(DZDT_850_mb ~ Lon + Lat + Date + CosMonth + CosDOY 
                 + Pop_density + elevation + MAIAC_AOD,
                 data = To_save,   na.action = na.omit,
                 method = "ranger", tuneGrid = tgrid, num.trees = 150, 
                 trControl = my_control) #Need to have it be the name of the var
  
  nam_ars<- append(nam_ars, my_nam$finalModel$r.squared)
  
  my_nam<- train(DZDT_700_mb ~ Lon + Lat + Date + CosMonth + CosDOY 
                 + Pop_density + elevation + MAIAC_AOD,
                 data = To_save,   na.action = na.omit,
                 method = "ranger", tuneGrid = tgrid, num.trees = 150, 
                 trControl = my_control) #Need to have it be the name of the var
  
  nam_ars<- append(nam_ars, my_nam$finalModel$r.squared)
  
  
  nam_df<- data.frame(NAM_var = names(To_save[,19:28]), AdjR2 = nam_ars)
  
  print(nam_df)

}

sink()




