source("~/Scripts/Data_prep.R")
source("~/Scripts/Base_ML_functions.R")

train<- rbind(high_train, low_train)
test<- rbind(high_test, low_test)

seasons<- c("winter", "spring", "summer", "fall")

by_time<- function(model, test_set, CMAQ, ens){
  if(CMAQ){
    years<- 2008:2016
  }else{
    years<- 2008:2018
  }
  RMSE<- matrix(0,nrow = length(years)*4, ncol = 3)
  i<-1
  if(ens){
    for(y in years){
      for(s in seasons){
        Test<- test_set[which((test_set$Year == y)&(test_set$Season == s)),]
        model_list_preds <- data.frame(predict(model$models, Test))
        preds<- data.frame(predict(model$ens_model, model_list_preds))[,1]
        RMSE[i,1]<- y
        RMSE[i,2]<- s
        RMSE[i,3]<- round(sqrt(mean((preds - Test$PM2.5_Obs)^2)),4)
        i<- i+1
      }
    }
  }else{
    for(y in years){
      for(s in seasons){
        Test<- test_set[which((test_set$Year == y)&(test_set$Season == s)),]
        preds<- data.frame(predict(model, Test))[,1]
        RMSE[i,1]<- y
        RMSE[i,2]<- s
        RMSE[i,3]<- round(sqrt(mean((preds - Test$PM2.5_Obs)^2)),4)
        i<- i+1
      }
    }
  }
  
  RMSE<- as.data.frame(RMSE)
  names(RMSE)<- c("Year", "Season", "RMSE")
  RMSE$Year<- as.numeric(as.character(RMSE$Year))
  RMSE$RMSE<- as.numeric(as.character(RMSE$RMSE))
  return(RMSE)
}

splits<- 5
states<- unique(DATA$State)[1:11]

by_space<- function(model, test_set, ens){
  
  RMSE<- rep(0,length(states))
  
  for(s in 1:length(states)){
    Test<- test_set[which(test_set$State == states[s]),]
    if(ens){
      model_list_preds <- data.frame(predict(model$models, Test))
      preds<- data.frame(predict(model$ens_model, model_list_preds))[,1]
    }else{
      preds<- data.frame(predict(model, Test))[,1]
    }
    RMSE[s]<- round(sqrt(mean((preds - Test$PM2.5_Obs)^2)),4)
  }
  # lons<- seq(min(test_set$Lon), max(test_set$Lon), length.out = splits+1)
  # lats<- seq(min(test_set$Lat), max(test_set$Lat), length.out = splits+1)
  # 
  #   RMSE<- matrix(0, nrow = splits, ncol = splits)
  #   for(i in 1:splits){
  #     for(j in 1:splits){
  #       Test<- test_set[which((test_set$Lon >= lons[i])&(test_set$Lon < lons[i+1])
  #                             &(test_set$Lat >= lats[j])&(test_set$Lat < lats[j+1])),]
  #       if(ens){
  #         model_list_preds <- data.frame(predict(model$models, Test))
  #         preds<- data.frame(predict(model$ens_model, model_list_preds))[,1]
  #       }else{
  #         preds<- data.frame(predict(model, Test))[,1]
  #       }
  #       
  #       RMSE[i,j]<- round(sqrt(mean((preds - Test$PM2.5_Obs)^2)),4)
  #     }
  # }
  return(RMSE)
}

##2008-2018, no CMAQ:
ranger_model<- readRDS("~/Models/All_years_ranger.rds")
xgbt_model<- readRDS("~/Models/All_years_xgbt.rds")

RMSE_time_R<- by_time(ranger_model, test, CMAQ = FALSE, ens = FALSE)
plot(RMSE_time_R$Year, RMSE_time_R$RMSE, col = RMSE_time_R$Season, pch = 16,
     xlab = "Year", ylab = "RMSE", 
     main = paste(seasons, paste("=", c("black", "red", "green", "blue"))))

RMSE_time_X<- by_time(xgbt_model, test, CMAQ = FALSE, ens = FALSE)
plot(RMSE_time_X$Year, RMSE_time_X$RMSE, col = RMSE_time_X$Season, pch = 16,
     xlab = "Year", ylab = "RMSE", 
     main = paste(seasons, paste("=", c("black", "red", "green", "blue"))))

library(fields)

lons<- seq(min(test$Lon), max(test$Lon), length.out = splits+1)
lats<- seq(min(test$Lat), max(test$Lat), length.out = splits+1)

RMSE_space_R<- by_space(ranger_model, test)
image.plot(lons, lats, RMSE_space_R, xlab = "Longitude", ylab = "Latitude", 
           main = "Color = RMSE")

RMSE_space_X<- by_space(xgbt_model, test)
image.plot(lons, lats, RMSE_space_X, xlab = "Longitude", ylab = "Latitude", 
           main = "Color = RMSE")

##With CMAQ:
CMAQ_ranger_model<- readRDS("~/Models/All_years_CMAQ_ranger.rds")
CMAQ_xgbt_model<- readRDS("~/Models/All_years_CMAQ_xgbt.rds")

RMSE_time_R<- by_time(CMAQ_ranger_model, CMAQ_test, CMAQ = TRUE, ens = FALSE)
plot(RMSE_time_R$Year, RMSE_time_R$RMSE, col = RMSE_time_R$Season, pch = 16,
     xlab = "Year", ylab = "RMSE", 
     main = paste(seasons, paste("=", c("black", "red", "green", "blue"))))

RMSE_time_X<- by_time(CMAQ_xgbt_model, CMAQ_test, CMAQ = TRUE, ens = FALSE)
plot(RMSE_time_X$Year, RMSE_time_X$RMSE, col = RMSE_time_X$Season, pch = 16,
     xlab = "Year", ylab = "RMSE", 
     main = paste(seasons, paste("=", c("black", "red", "green", "blue"))))

library(fields)

CMAQ_lons<- seq(min(CMAQ_test$Lon), max(CMAQ_test$Lon), length.out = splits+1)
CMAQ_lats<- seq(min(CMAQ_test$Lat), max(CMAQ_test$Lat), length.out = splits+1)

RMSE_space_R<- by_space(CMAQ_ranger_model, CMAQ_test)
image.plot(CMAQ_lons, CMAQ_lats, RMSE_space_R, xlab = "Longitude", ylab = "Latitude", 
           main = "Color = RMSE")

RMSE_space_X<- by_space(CMAQ_xgbt_model, CMAQ_test)
image.plot(CMAQ_lons, CMAQ_lats, RMSE_space_X, xlab = "Longitude", ylab = "Latitude", 
           main = "Color = RMSE")


##For ensemble:
my_GLM<- readRDS("~/Models/Ens_GLM_no-split.rds")
CMAQ_my_GLM<- readRDS("~/Models/Ens_GLM_CMAQ_no-split.rds")


##No CMAQ:
RMSE_time_R<- by_time(my_GLM, test, CMAQ = FALSE, ens = TRUE)
plot(RMSE_time_R$Year, RMSE_time_R$RMSE, col = RMSE_time_R$Season, pch = 16,
     xlab = "Year", ylab = "RMSE", 
     main = paste(seasons, paste("=", c("black", "red", "green", "blue"))))

library(maps)
library(RColorBrewer)
library(classInt)

west<- match(map("state", plot = FALSE)$names,states)
west[60]<- 3 # "washington:main"

RMSE_space<- by_space(my_GLM, test, ens = TRUE)

# plotclr<- "YlOrRd"
nclr<- 8
breaks<- round(quantile(RMSE_space, seq(0, 1, 1/nclr), na.rm = TRUE), 4)
class<- classIntervals(RMSE_space, n = nclr, style = "fixed", fixedBreaks = breaks)
colcode <- findColours(class, brewer.pal(nclr, "YlOrRd"))

map("state", fill = T, col = colcode[west])
map('state', col="lightgray", add=T, myborder = 10)
legend("bottomright", # position
       legend = names(attr(colcode, "table")), 
       title = "Legend", #title = "Quantiles",
       fill = attr(colcode, "palette"),
       cex = 0.75,
       bty = "n")

# library(fields)
# 
# lons<- seq(min(test$Lon), max(test$Lon), length.out = splits+1)
# lats<- seq(min(test$Lat), max(test$Lat), length.out = splits+1)
# 
# RMSE_space<- by_space(my_GLM, test, ens = TRUE)
# image.plot(lons, lats, RMSE_space, xlab = "Longitude", ylab = "Latitude", 
#            main = "Color = RMSE")

##With CMAQ:
RMSE_time_R<- by_time(CMAQ_my_GLM, CMAQ_test, CMAQ = FALSE, ens = TRUE)
plot(RMSE_time_R$Year, RMSE_time_R$RMSE, col = RMSE_time_R$Season, pch = 16,
     xlab = "Year", ylab = "RMSE", 
     main = paste(seasons, paste("=", c("black", "red", "green", "blue"))))

CMAQ_RMSE_space<- by_space(CMAQ_my_GLM, CMAQ_test, ens = TRUE)

CMAQ_breaks<- round(quantile(CMAQ_RMSE_space, seq(0, 1, 1/nclr), na.rm = TRUE), 4)
CMAQ_class<- classIntervals(CMAQ_RMSE_space, n = nclr, style = "fixed", fixedBreaks = CMAQ_breaks)
CMAQ_colcode <- findColours(CMAQ_class, brewer.pal(nclr, "YlOrRd"))

map("state", fill = T, col = CMAQ_colcode[west])
map('state', col="lightgray", add=T, myborder = 10)
legend("bottomright", # position
       legend = names(attr(CMAQ_colcode, "table")), 
       title = "Legend", #title = "Quantiles",
       fill = attr(CMAQ_colcode, "palette"),
       cex = 0.75,
       bty = "n")


# library(fields)
# 
# CMAQ_lons<- seq(min(CMAQ_test$Lon), max(CMAQ_test$Lon), length.out = splits+1)
# CMAQ_lats<- seq(min(CMAQ_test$Lat), max(CMAQ_test$Lat), length.out = splits+1)
# 
# RMSE_space<- by_space(CMAQ_my_GLM, CMAQ_test, ens = TRUE)
# image.plot(lons, lats, RMSE_space, xlab = "Longitude", ylab = "Latitude", 
#            main = "Color = RMSE")
