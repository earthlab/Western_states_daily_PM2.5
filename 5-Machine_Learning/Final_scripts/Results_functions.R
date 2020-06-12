library(plyr)
library(dplyr)

subset_results<- function(df, df_ref, subset_var, subset, type, tr){
  if(tr){ #training set
    rows<- which(df_ref[,subset_var] == subset) 
    this_data<- df[which(df$rowIndex %in% rows),]
    if(type == "RMSE"){
      Results<- ddply(this_data, "Resample", function(x) sqrt(mean((x$pred - x$obs)^2)))
      return(round(mean(Results$V1),4))
    }else{
      Results<- ddply(this_data, "Resample", function(x) (cor(x$pred, x$obs))^2)
      return(round(mean(Results$V1),4))
    }
  }else{ #testing set
    this_data<- df[which(df_ref[,subset_var] == subset),]
    if(type == "RMSE"){
      return(round(sqrt(mean((this_data$Preds - this_data$Obs)^2)),4))
    }else{
      return(round((cor(this_data$Preds, this_data$Obs))^2,4))
    }
  }
}

space_time<- function(df, df_ref, subset_var, type, tr, Kloog = FALSE){
  vals<- sort(unique(df_ref[,subset_var])) #alphabetical if characters; increasing if numeric
  vec<- rep(0, length(vals)) 
  for(v in 1:length(vals)){
    if(Kloog == FALSE){
      vec[v]<- subset_results(df, df_ref, subset_var, vals[v], type, tr)
    }else if (Kloog == "spatial"){
      vec[v]<- Kloog_spatial(df, df_ref, subset_var, vals[v], type, tr)
    }else if (Kloog == "temporal"){
      vec[v]<- Kloog_temporal(df, df_ref, subset_var, vals[v], type, tr)
    }
  }
  return(vec)
}

Kloog_spatial<- function(df, df_ref, subset_var, subset, type, tr){
  if(tr){
    rows<- which(df_ref[,subset_var] == subset) 
    this_data<- df[which(df$rowIndex %in% rows),]
    this_data$Lon<- df_ref[this_data$rowIndex,"Lon"]
    this_data$Lat<- df_ref[this_data$rowIndex, "Lat"]
    this_data$Year<- df_ref[this_data$rowIndex, "Year"]
    agg<- ddply(this_data, c("Resample", "Year", "Lon", "Lat"), 
                function(x) c(mean(x$pred), mean(x$obs)))
    if(type == "RMSE"){
      agg2<- ddply(agg, "Resample", function(x) sqrt(mean((x$V1 - x$V2)^2)))
      return(round(mean(agg2$V1, na.rm = TRUE),4))
    }else{
      agg2<- ddply(agg, "Resample", function(x) (cor(x$V1, x$V2))^2)
      return(round(mean(agg2$V1, na.rm = TRUE),4))
    }
    
  }else{
    this_data<- df[which(df_ref[,subset_var] == subset),]
    this_data$Lon<- df_ref[which(df_ref[,subset_var] == subset),"Lon"]
    this_data$Lat<- df_ref[which(df_ref[,subset_var] == subset), "Lat"]
    this_data$Year<- df_ref[which(df_ref[,subset_var] == subset), "Year"]
    agg<- ddply(this_data, c("Year", "Lon", "Lat"), 
                function(x) c(mean(x$Preds), mean(x$Obs)))
    if(type == "RMSE"){
      return(round(sqrt(mean((agg[,"V1"] - agg[,"V2"])^2)),4))
    }else{
      return(round((cor(agg[,"V1"], agg[,"V2"]))^2,4))
    }
  }
}
  
  
Kloog_temporal<- function(df, df_ref, subset_var, subset, type, tr){
  if(tr){
    rows<- which(df_ref[,subset_var] == subset) 
    this_data<- df[which(df$rowIndex %in% rows),]
    this_data$Lon<- df_ref[this_data$rowIndex,"Lon"]
    this_data$Lat<- df_ref[this_data$rowIndex, "Lat"]
    this_data$Year<- df_ref[this_data$rowIndex, "Year"]
    agg<- ddply(this_data, c("Resample", "Year", "Lon", "Lat"), 
                function(x) c(mean(x$pred), mean(x$obs)))
    merged<- inner_join(this_data, agg, by = c("Resample", "Year", "Lon", "Lat"))
    merged$delta_pred<- merged$pred - merged$V1
    merged$delta_obs<- merged$obs - merged$V2
    
    if(type == "RMSE"){
      agg2<- ddply(merged, "Resample", function(x) sqrt(mean((x$delta_pred - x$delta_obs)^2)))
      return(round(mean(agg2$V1, na.rm = TRUE),4))
    }else{
      agg2<- ddply(merged, "Resample", function(x) (cor(x$delta_pred, x$delta_obs))^2)
      return(round(mean(agg2$V1, na.rm = TRUE),4))
    }
    
  }else{
    this_data<- df[which(df_ref[,subset_var] == subset),]
    this_data$Lon<- df_ref[as.numeric(row.names(this_data)),"Lon"]
    this_data$Lat<- df_ref[as.numeric(row.names(this_data)), "Lat"]
    this_data$Year<- df_ref[as.numeric(row.names(this_data)), "Year"]
    agg<- ddply(this_data, c("Year", "Lon", "Lat"), 
                function(x) c(mean(x$Preds), mean(x$Obs)))
    merged<- inner_join(this_data, agg, by = c("Year", "Lon", "Lat"))
    merged$delta_pred<- merged$Preds - merged$V1
    merged$delta_obs<- merged$Obs - merged$V2
    if(type == "RMSE"){
      return(round(sqrt(mean((merged[,"delta_pred"] - merged[,"delta_obs"])^2)),4))
    }else{
      return(round((cor(merged[,"delta_pred"], merged[,"delta_obs"]))^2,4))
    }
  }
}

level_results<- function(df, df_ref, old_lev, new_lev, type, tr){
  if(tr){ #training set
    rows<- which((df_ref$PM2.5_Obs < new_lev)&(df_ref$PM2.5_Obs >= old_lev))
    this_data<- df[which(df$rowIndex %in% rows),]
    if(type == "RMSE"){
      Results<- ddply(this_data, "Resample", function(x) sqrt(mean((x$pred - x$obs)^2)))
      return(round(mean(Results$V1),4))
    }else{
      Results<- ddply(this_data, "Resample", function(x) (cor(x$pred, x$obs))^2)
      return(round(mean(Results$V1),4))
    }
    
  }else{
    this_data<- df[which((df_ref$PM2.5_Obs < new_lev)&(df_ref$PM2.5_Obs >= old_lev)),]
    if(type == "RMSE"){
      return(round(sqrt(mean((this_data$Preds - this_data$Obs)^2)),4))
    }else{
      return(round((cor(this_data$Preds, this_data$Obs))^2,4))
    }
  }
}

PM_levels<- function(df, df_ref, levs, type, tr, Kloog = FALSE){
  vec<- rep(0, length(levs))
  for(v in 2:length(levs)){
    if(Kloog == FALSE){
      vec[v]<- level_results(df, df_ref, old = levs[v-1], new = levs[v], type, tr)
    }else if(Kloog == "spatial"){
      vec[v]<- levels_Kloog_spatial(df, df_ref, old = levs[v-1], new = levs[v], type, tr)
    }else if(Kloog == "temporal"){
      vec[v]<- levels_Kloog_temporal(df, df_ref, old = levs[v-1], new = levs[v], type, tr)
    }
  }
  return(vec)
}


levels_Kloog_spatial<- function(df, df_ref, old_lev, new_lev, type, tr){
  if(tr){
    rows<- which((df_ref$PM2.5_Obs < new_lev)&(df_ref$PM2.5_Obs >= old_lev))
    this_data<- df[which(df$rowIndex %in% rows),]
    this_data$Lon<- df_ref[this_data$rowIndex,"Lon"]
    this_data$Lat<- df_ref[this_data$rowIndex, "Lat"]
    this_data$Year<- df_ref[this_data$rowIndex, "Year"]
    agg<- ddply(this_data, c("Resample", "Year", "Lon", "Lat"), 
                function(x) c(mean(x$pred), mean(x$obs)))
    if(type == "RMSE"){
      agg2<- ddply(agg, "Resample", function(x) sqrt(mean((x$V1 - x$V2)^2)))
      return(round(mean(agg2$V1),4))
    }else{
      agg2<- ddply(agg, "Resample", function(x) (cor(x$V1, x$V2))^2)
      return(round(mean(agg2$V1),4))
    }
    
  }else{
    this_data<- df[which((df_ref$PM2.5_Obs < new_lev)&(df_ref$PM2.5_Obs >= old_lev)),]
    this_data$Lon<- df_ref[which((df_ref$PM2.5_Obs < new_lev)&(df_ref$PM2.5_Obs >= old_lev)),"Lon"]
    this_data$Lat<- df_ref[which((df_ref$PM2.5_Obs < new_lev)&(df_ref$PM2.5_Obs >= old_lev)), "Lat"]
    this_data$Year<- df_ref[which((df_ref$PM2.5_Obs < new_lev)&(df_ref$PM2.5_Obs >= old_lev)), "Year"]
    agg<- ddply(this_data, c("Year", "Lon", "Lat"), 
                function(x) c(mean(x$Preds), mean(x$Obs)))
    if(type == "RMSE"){
      return(round(sqrt(mean((agg[,"V1"] - agg[,"V2"])^2)),4))
    }else{
      return(round((cor(agg[,"V1"], agg[,"V2"]))^2,4))
    }
  }
}


levels_Kloog_temporal<- function(df, df_ref, old_lev, new_lev, type, tr){
  if(tr){
    rows<- which((df_ref$PM2.5_Obs < new_lev)&(df_ref$PM2.5_Obs >= old_lev)) 
    this_data<- df[which(df$rowIndex %in% rows),]
    this_data$Lon<- df_ref[this_data$rowIndex,"Lon"]
    this_data$Lat<- df_ref[this_data$rowIndex, "Lat"]
    this_data$Year<- df_ref[this_data$rowIndex, "Year"]
    agg<- ddply(this_data, c("Resample", "Year", "Lon", "Lat"), 
                function(x) c(mean(x$pred), mean(x$obs)))
    merged<- inner_join(this_data, agg, by = c("Resample", "Year", "Lon", "Lat"))
    merged$delta_pred<- merged$pred - merged$V1
    merged$delta_obs<- merged$obs - merged$V2
    
    if(type == "RMSE"){
      agg2<- ddply(merged, "Resample", function(x) sqrt(mean((x$delta_pred - x$delta_obs)^2)))
      return(round(mean(agg2$V1),4))
    }else{
      agg2<- ddply(merged, "Resample", function(x) (cor(x$delta_pred, x$delta_obs))^2)
      return(round(mean(agg2$V1),4))
    }
    
  }else{
    this_data<- df[which((df_ref$PM2.5_Obs < new_lev)&(df_ref$PM2.5_Obs >= old_lev)),]
    this_data$Lon<- df_ref[as.numeric(row.names(this_data)),"Lon"]
    this_data$Lat<- df_ref[as.numeric(row.names(this_data)), "Lat"]
    this_data$Year<- df_ref[as.numeric(row.names(this_data)), "Year"]
    agg<- ddply(this_data, c("Year", "Lon", "Lat"), 
                function(x) c(mean(x$Preds), mean(x$Obs)))
    merged<- inner_join(this_data, agg, by = c("Year", "Lon", "Lat"))
    merged$delta_pred<- merged$Preds - merged$V1
    merged$delta_obs<- merged$Obs - merged$V2
    if(type == "RMSE"){
      return(round(sqrt(mean((merged[,"delta_pred"] - merged[,"delta_obs"])^2)),4))
    }else{
      return(round((cor(merged[,"delta_pred"], merged[,"delta_obs"]))^2,4))
    }
  }
}


