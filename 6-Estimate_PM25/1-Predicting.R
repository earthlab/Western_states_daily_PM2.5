library(caret)
library(caretEnsemble)

##Read in models:
ranger_model<- readRDS("~/Models/Full_ranger.rds")
xgbt_model<- readRDS("~/Models/Full_xgbt.rds")
CMAQ_ranger_model<- readRDS("~/Models/Full_CMAQ_ranger.rds")
CMAQ_xgbt_model<- readRDS("~/Models/Full_CMAQ_xgbt.rds")

##Predict on data from all states:
State<- c("nevada", "colorado", "utah", "new mexico", "arizona",
          "washington", "oregon", "idaho", "montana", "wyoming", "california")

for(s in State){
  print(s)
  load(file = paste0("~/FINAL_by_state/Prediction_inputs_", s, ".RData"))
  
  Data$Binary_fire<- as.logical(Data$Binary_fire)
  Data$Mid_Study<- as.logical(Data$Mid_Study)
  Data$Late_Study<- as.logical(Data$Late_Study)
  
  #Without CMAQ:
  ranger_preds<- c()
  xgbt_preds<- c()
  n<- 40
  my_seq<- round(seq(1,dim(Data)[1], length.out = n+1))
  for(i in 1:n){
    data<- Data[my_seq[i]:(my_seq[i+1]-1),]
    if(i==n){
      data<- rbind(data, Data[my_seq[i+1],])
    }
    ranger_preds<- append(ranger_preds, predict(ranger_model, data))
    xgbt_preds<- append(xgbt_preds, predict(xgbt_model, data))
  }
  
  preds_no_CMAQ<- data.frame(Data[,c("County_FIPS", "Tract_code", "ZCTA5_code",
                                     "Lon", "Lat", "Date")],
                             ranger_preds, xgbt_preds,
                             Missing_vars=Data$Missing_vars)
  
  ##Have to do extra work for California so the EC2 instance doesn't crash:
  # ranger_preds1<- predict(ranger_model, Data[1:5000000,])
  # ranger_preds2<- predict(ranger_model, Data[5000001:10000000,])
  # ranger_preds3<- predict(ranger_model, Data[10000001:15000000,])
  # ranger_preds4<- predict(ranger_model, Data[15000001:20000000,])
  # ranger_preds5<- predict(ranger_model, Data[20000001:25000000,])
  # ranger_preds6<- predict(ranger_model, Data[25000001:30000000,])
  # ranger_preds7<- predict(ranger_model, Data[30000001:35000000,])
  # ranger_preds8<- predict(ranger_model, Data[35000001:(dim(Data)[1]),])
  # xgbt_preds1<- predict(xgbt_model, Data[1:5000000,])
  # xgbt_preds2<- predict(xgbt_model, Data[5000001:10000000,])
  # xgbt_preds3<- predict(xgbt_model, Data[10000001:15000000,])
  # xgbt_preds4<- predict(xgbt_model, Data[15000001:20000000,])
  # xgbt_preds5<- predict(xgbt_model, Data[20000001:25000000,])
  # xgbt_preds6<- predict(xgbt_model, Data[25000001:30000000,])
  # xgbt_preds7<- predict(xgbt_model, Data[30000001:35000000,])
  # xgbt_preds8<- predict(xgbt_model, Data[35000001:(dim(Data)[1]),])
  # 
  # preds_no_CMAQ<- data.frame(Data[,c("County_FIPS", "Tract_code", "ZCTA5_code",
  #                                    "Lon", "Lat", "Date")],
  #                            ranger_preds=cbind(ranger_preds1, ranger_preds2,
  #                                               ranger_preds3, ranger_preds4,
  #                                               ranger_preds5, ranger_preds6,
  #                                               ranger_preds7, ranger_preds8), 
  #                            xgbt_preds=cbind(xgbt_preds1, xgbt_preds2,
  #                                             xgbt_preds3, xgbt_preds4,
  #                                             xgbt_preds5, xgbt_preds6,
  #                                             xgbt_preds7, xgbt_preds8), 
  #                            Missing_vars=Data$Missing_vars)
  
  save(preds_no_CMAQ, file = paste0("~/Predictions/Preds_no_CMAQ_", s, ".RData"),
       compress = TRUE)
  
  rm(list=c("preds_no_CMAQ", "ranger_preds", "xgbt_preds"))
  
  #With CMAQ:
  CMAQ_ranger_preds<- c()
  CMAQ_xgbt_preds<- c()
  n<- 40
  Data2<- Data[which(!is.na(Data$all_CMAQ)),]
  my_seq<- round(seq(1,dim(Data2)[1], length.out = n+1))
  for(i in 1:n){
    data<- Data2[my_seq[i]:(my_seq[i+1]-1),]
    if(i==n){
      data<- rbind(data, Data2[my_seq[i+1],])
    }
    CMAQ_ranger_preds<- append(CMAQ_ranger_preds, predict(CMAQ_ranger_model, data))
    CMAQ_xgbt_preds<- append(CMAQ_xgbt_preds, predict(CMAQ_xgbt_model, data))
  }
  
  preds_with_CMAQ<- data.frame(Data[which(!is.na(Data$all_CMAQ)),c("County_FIPS", "Tract_code", "ZCTA5_code",
                                       "Lon", "Lat", "Date")],
                               CMAQ_ranger_preds, CMAQ_xgbt_preds, 
                             Missing_vars=Data[which(!is.na(Data$all_CMAQ)),"Missing_vars"],
                             Missing_CMAQ=Data[which(!is.na(Data$all_CMAQ)),"Missing_CMAQ"])
  save(preds_with_CMAQ, file = paste0("~/Predictions/Preds_with_CMAQ_", s, ".RData"),
       compress = TRUE)
  
  rm(preds_with_CMAQ)
  
}


### Ensembles:

load("~/Final_ML_data3.RData")
rm(list=setdiff(ls(), c("data", "CMAQ_data")))
Both<- data[which(data$State != "texas"),]
CMAQ_Both<- CMAQ_data[which(CMAQ_data$State != "texas"),]
rm(list=c("data", "CMAQ_data"))

#Without CMAQ:
ranger_model<- readRDS("~/Models/Full_ranger.rds")
xgbt_model<- readRDS("~/Models/Full_xgbt.rds")
ranger_preds<- predict.train(ranger_model, Both)
xgbt_preds<- predict.train(xgbt_model, Both)
NC_training<- data.frame(Obs = Both$PM2.5_Obs, ranger_preds, xgbt_preds)

GLM<- train(Obs ~ ranger_preds + xgbt_preds, data = NC_training, method = "glm")

for(s in State){
  print(s)
  load(paste0("~/Predictions/Preds_no_CMAQ_", s, ".RData"))
  
  #For CA:
  Ens_pred<- c()
  n<- 40
  my_seq<- round(seq(1,dim(preds_no_CMAQ)[1], length.out = n+1))
  for(i in 1:n){
    data<- preds_no_CMAQ[my_seq[i]:(my_seq[i+1]-1),]
    if(i==n){
      data<- rbind(data, preds_no_CMAQ[my_seq[i+1],])
    }
    Ens_pred<- append(Ens_pred, predict(GLM, data))
  }
  
  # Ens_pred<- predict(GLM, preds_no_CMAQ)
  
  DF<- data.frame(preds_no_CMAQ, Ens_pred)
  save(DF, file = paste0("~/Predictions/Ensemble_preds_no_CMAQ_", s, ".RData"),
       compress = TRUE)
  
}

rm(list=c("Both", "data", "DF", "GLM", "NC_training", "preds_no_CMAQ", 
          "ranger_model", "xgbt_model", "Ens_pred", "ranger_preds", "xgbt_preds"))

#With CMAQ:
CMAQ_ranger_model<- readRDS("~/Models/Full_CMAQ_ranger.rds")
CMAQ_xgbt_model<- readRDS("~/Models/Full_CMAQ_xgbt.rds")
CMAQ_ranger_preds<- predict.train(CMAQ_ranger_model, CMAQ_Both)
CMAQ_xgbt_preds<- predict.train(CMAQ_xgbt_model, CMAQ_Both)
CMAQ_NC_training<- data.frame(Obs = CMAQ_Both$PM2.5_Obs, CMAQ_ranger_preds, CMAQ_xgbt_preds)

CMAQ_GLM<- train(Obs ~ CMAQ_ranger_preds + CMAQ_xgbt_preds, data = CMAQ_NC_training, method = "glm")

for(s in State){
  print(s)
  load(paste0("~/Predictions/Preds_with_CMAQ_", s, ".RData"))
  
  #For CA:
  Ens_pred<- c()
  n<- 40
  my_seq<- round(seq(1,dim(preds_with_CMAQ)[1], length.out = n+1))
  for(i in 1:n){
    data<- preds_with_CMAQ[my_seq[i]:(my_seq[i+1]-1),]
    if(i==n){
      data<- rbind(data, preds_with_CMAQ[my_seq[i+1],])
    }
    Ens_pred<- append(Ens_pred, predict(CMAQ_GLM, data))
  }
  
  DF<- data.frame(preds_with_CMAQ, Ens_pred)
  save(DF, file = paste0("~/Predictions/Ensemble_preds_with_CMAQ_", s, ".RData"),
       compress = TRUE)
  
}

##Need to go back and fix the FIPS codes after imputation during CMAQ step:
library(dplyr)

Stat<- read.csv("FINAL_Stat.csv")
Stat$Lon<- round(Stat$Lon, 4)
Stat$Lat<- round(Stat$Lat, 5)
# Ref<- data.frame(Stat[,c("State", "County_FIPS", "Tract_code","ZCTA5_code",
                         # "Lon", "Lat")], Date = sort(rep(as.Date(dates), dim(Stat)[1])))

for(s in State[-8]){
  print(s)
  
  print("Inputs")
  load(paste0("~/FINAL_by_state/Prediction_inputs_", s, ".RData"))
  uniq<- distinct(Data[,4:54])
  
  DATA<- inner_join(Stat[,c("State", "County_FIPS", "Tract_code","ZCTA5_code",
                                 "Lon", "Lat")], uniq, by = c("State", "Lon", "Lat"))
  save(DATA, file = paste0("~/FINAL_by_state/Prediction_inputs_", s, "_2.RData"),
       compress = TRUE)
  rm(list=c("Data", "DATA", "uniq"))
  
  print("Predictions")
  load(paste0("~/Predictions/Ensemble_preds_no_CMAQ_", s, ".RData"))
  uniq<- distinct(DF[,4:10])
  
  DF<- inner_join(Stat[,c("County_FIPS", "Tract_code","ZCTA5_code",
                            "Lon", "Lat")], uniq, by = c("Lon", "Lat"))
  save(DF, file = paste0("~/Predictions/Ensemble_preds_no_CMAQ_", s, "_2.RData"),
       compress = TRUE)
  rm(list=c("DF", "uniq"))
  
  load(paste0("~/Predictions/Ensemble_preds_with_CMAQ_", s, ".RData"))
  uniq<- distinct(DF[,4:11])
  
  DF<- inner_join(Stat[,c("County_FIPS", "Tract_code","ZCTA5_code",
                          "Lon", "Lat")], uniq, by = c("Lon", "Lat"))
  save(DF, file = paste0("~/Predictions/Ensemble_preds_with_CMAQ_", s, "_2.RData"),
       compress = TRUE)
  rm(list=c("DF", "uniq"))
}


