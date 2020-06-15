##Investigating imputation strategies

State<- c("nevada", "colorado", "utah", "new mexico", "arizona",
          "washington", "oregon", "idaho", "montana", "wyoming", "california")

##For ranger:
my_control<- trainControl(method = "none", savePredictions = "final")
tgrid<- expand.grid( .mtry = 5, .splitrule = "variance", .min.node.size = 6 )

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