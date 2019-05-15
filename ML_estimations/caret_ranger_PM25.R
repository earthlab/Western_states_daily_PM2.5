# Load the required libraries
library(caret)
library(ranger)
library(fields)
library(splines)
library(lubridate)

# data<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Machine Learning\\ML_input_test.csv")
data<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Machine Learning\\clean_ML_input_testing1.csv")

#Calculate day-of-year and cubic spline
julian<- yday(data$Date)
day_spline<- ns(julian, df=3)
data<- cbind(data, day_spline)

DATA_2<- data[1:4500, c("PM2.5_Obs", "AOD", "MAIAC_AOD", "elevation", "NLCD"
                        ,"TMP.2.m.above.ground", "RH.2.m.above.ground", "HPBL.surface"
                        ,"Longitude", "Latitude")]

DATA_3<- data[1:10000,c("PM2.5_Obs", "Latitude", "Longitude", "Both_500",
                  "Both_1000","AOD", "MAIAC_AOD","HPBL.surface",
                 "TMP.2.m.above.ground","RH.2.m.above.ground", "DPT.2.m.above.ground",
                 "APCP.surface", "WEASD.surface", "SNOWC.surface", "UGRD.10.m.above.ground",
                 "VGRD.10.m.above.ground", "PRMSL.mean.sea.level", "PRES.surface", "DZDT.850.mb",
                 "DZDT.700.mb", "elevation", "NLCD")]

#Run lm on DATA_3 and select variables for DATA_4 based off of significance codes...

DATA_4<- data[1:10000, c("PM2.5_Obs", "AOD", "MAIAC_AOD", "elevation", "NLCD"
                         ,"TMP.2.m.above.ground", "RH.2.m.above.ground", "HPBL.surface"
                         ,"Longitude", "Latitude",
                         "DPT.2.m.above.ground", "WEASD.surface", "SNOWC.surface",
                         "PRMSL.mean.sea.level", "PRES.surface",
                         "1", "2", "3" #spline
                         )]
#Use varImp on DATA_4 to remove SNOWC.surface, WEASD.surface, and NLCD
DATA_4.5<- data[1:10000, c("PM2.5_Obs", "AOD", "MAIAC_AOD", "elevation", #"NLCD",
                           "TMP.2.m.above.ground", "RH.2.m.above.ground", "HPBL.surface"
                           ,"Longitude", "Latitude",
                           "DPT.2.m.above.ground", #"WEASD.surface", "SNOWC.surface",
                           "PRMSL.mean.sea.level", "PRES.surface",
                           "1", "2", "3" #spline
                           )]

date_split<- strsplit(as.character(data$Date), split="-")
all<- unlist(date_split)

Year<- as.numeric(all[seq(1,length(all)-2, 3)])
Month<- as.numeric(all[seq(2,length(all)-1, 3)])
Day<- as.numeric(all[seq(3,length(all), 3)])

Summer<- data[Month %in% 6:9, c("PM2.5_Obs", "AOD", "MAIAC_AOD", "elevation", #"NLCD",
                  "TMP.2.m.above.ground", "RH.2.m.above.ground", "HPBL.surface"
                  ,"Longitude", "Latitude",
                  "DPT.2.m.above.ground", #"WEASD.surface", "SNOWC.surface",
                  "PRMSL.mean.sea.level", "PRES.surface")]
DATA_5<- Summer[1:10000,]

Lat_thresh<- mean(range(data$Latitude))
Lon_thresh<- mean(range(data$Longitude))

Summer_SW<- Summer[(Summer$Latitude < Lat_thresh) & (Summer$Longitude < Lon_thresh),]
DATA_6<- Summer_SW[1:10000,]

Summer_NW<- Summer[(Summer$Latitude > Lat_thresh) & (Summer$Longitude < Lon_thresh),]
DATA_7<- Summer_NW[1:10000,]

n<- round(dim(DATA_2)[1]*0.1)
test_pos<- sample(1:(dim(DATA_2)[1]),n, replace = FALSE)

test<- DATA_2[test_pos,]
train<- DATA_2[-test_pos,]

#caret R^2 calculation: doesn't seem to be working for ranger...
# ?R2
R2<- function(pred, obs, formula = "corr", na.rm = FALSE) {
  n <- sum(complete.cases(pred))
  switch(formula,
         corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
         traditional = 1 - (sum((obs-pred)^2, na.rm = na.rm)/((n-1)*var(obs, na.rm = na.rm))))
}

adjR2<- function(pred, obs, numPredictors){
  n<- sum(complete.cases(pred))
  k<- numPredictors
  return(1 - ((n-1)/(n-k-1))*(1 - R2(pred, obs) ) )
}


runRanger<- function(dataset, splitvar= NULL){
  # set.seed(123)
  n<- round(dim(dataset)[1]*0.1)
  test_pos<- sample(1:(dim(dataset)[1]),n, replace = FALSE)
  test<- dataset[test_pos,]
  train<- dataset[-test_pos,]

  tgrid<- expand.grid( .mtry = 3:6, .splitrule = "variance", .min.node.size = seq(5,9,2) )
  
  if(!is.null(splitvar)){
    grouped<- cbind(train, group = interaction(train[,splitvar]))
    IND<- groupKFold(grouped$group, k=10) #k = number of folds
  }
  else{
    IND<- createResample(train$PM2.5_Obs, 10)
  }
 

# myControl<- trainControl(method = "oob", number = 10, search = "grid", #method = "repeatedcv", repeats = 3,
#                          savePredictions = "final", index = createResample(train$PM2.5_Obs, 10), #insert groupKFold here, for index
#                          verboseIter = TRUE)
  myControl<- trainControl(number = 10, search = "grid", method = "repeatedcv", repeats = 3,
                           savePredictions = "final", index = IND, #insert groupKFold here, for index
                           verboseIter = FALSE)

  ranger_model <- train(PM2.5_Obs ~ ., data = train, method = "ranger", #change to train[,-(1:3)] with ST subset data
                       trControl = myControl, tuneGrid = tgrid, num.trees = 100,
                       importance = "permutation") #oob.error = TRUE?
   
  # #Trying out other algorithms:
  # ranger_model <- train(PM2.5_Obs ~ ., data = train, method = "gbm", #change to train[,-(1:3)] with ST subset data
  #                       trControl = myControl) #oob.error = TRUE?

  print(ranger_model)
  # print(varImp(ranger_model))
  
  train_preds <- data.frame(predict(ranger_model, train[,-(which(names(train)=="PM2.5_Obs"))]))
  res<- t(train_preds - train$PM2.5_Obs)
  # quilt.plot(x = train$Latitude, y = train$Longitude, z = t(res))
  
  print(paste("Training set R^2 =", round(R2(pred = train_preds, obs = train$PM2.5_Obs), digits = 4)))
  print(paste("Training set RMSE =", round(sqrt(mean(res^2)), digits = 4)))

  test_preds <- data.frame(predict(ranger_model, test[,-(which(names(test)=="PM2.5_Obs"))])) #sanity check
  compare<- cbind(test_preds, test$PM2.5_Obs)
  errors<- (compare[,1] - compare[,2])
  print(paste("Testing set R^2 =", round(R2(pred = compare[,1], obs = compare[,2]), digits = 4)))
  print(paste("Testing set RMSE =", round(sqrt(mean(errors^2)), digits = 4)))
  # quilt.plot(x = test$Latitude, y = test$Longitude, z = t(errors))
  
  fitted<- test_preds$predict.ranger_model..test.....which.names.test......PM2.5_Obs.....
  plot(fitted, errors, main = "Residuals vs Fitted Values")
}


#Regular folds:
runRanger(DATA_4.5)

runRanger(Sum08_SW)

runRanger(DATA_6)
runRanger(DATA_7)

#Specific folds:

#Monitors
runRanger(Sum08_SW, splitvar = c("Latitude", "Longitude"))

runRanger(DATA_6, splitvar = c("Latitude", "Longitude"))

#Months
runRanger(small_Sum_SW, splitvar = c("Month", "Year"))

runRanger(DATA_6, splitvar = c("Month", "Year"))



