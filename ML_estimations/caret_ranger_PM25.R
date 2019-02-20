# Load the required libraries
library(caret)
library(ranger)
library(fields)

# data<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Machine Learning\\ML_input_test.csv")
data<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Machine Learning\\clean_ML_input_testing1.csv")

DATA_2<- data[1:4500, c("PM2.5_Obs", "AOD", "MAIAC_AOD", "elevation", "NLCD"
                        ,"TMP.2.m.above.ground", "RH.2.m.above.ground", "HPBL.surface"
                        ,"Longitude", "Latitude"
)]

DATA_3<- data[1:10000,c("PM2.5_Obs", "Latitude", "Longitude", "Both_500",
                  "Both_1000","AOD", "MAIAC_AOD","HPBL.surface",
                 "TMP.2.m.above.ground","RH.2.m.above.ground", "DPT.2.m.above.ground",
                 "APCP.surface", "WEASD.surface", "SNOWC.surface", "UGRD.10.m.above.ground",
                 "VGRD.10.m.above.ground", "PRMSL.mean.sea.level", "PRES.surface", "DZDT.850.mb",
                 "DZDT.700.mb", "elevation", "NLCD")]

#Run lm on DATA_3 and select variables for DATA_4 based off of significance codes...

DATA_4<- data[1:50000, c("PM2.5_Obs", "AOD", "MAIAC_AOD", "elevation", "NLCD"
                         ,"TMP.2.m.above.ground", "RH.2.m.above.ground", "HPBL.surface"
                         ,"Longitude", "Latitude",
                         "DPT.2.m.above.ground", "WEASD.surface", "SNOWC.surface",
                         "PRMSL.mean.sea.level", "PRES.surface"
                         )]

#Use varImp on DATA_4 to remove SNOWC.surface, WEASD.surface, and NLCD
Summer<- data[Month %in% 6:9, c("PM2.5_Obs", "AOD", "MAIAC_AOD", "elevation", #"NLCD",
                  "TMP.2.m.above.ground", "RH.2.m.above.ground", "HPBL.surface"
                  ,"Longitude", "Latitude",
                  "DPT.2.m.above.ground", #"WEASD.surface", "SNOWC.surface",
                  "PRMSL.mean.sea.level", "PRES.surface")]
DATA_5<- Summer[1:10000,]

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
                           verboseIter = TRUE)

  ranger_model <- train(PM2.5_Obs ~ ., data = train, method = "ranger", #change to train[,-(1:3)] with ST subset data
                       trControl = myControl, tuneGrid = tgrid, num.trees = 100,
                       importance = "permutation") #oob.error = TRUE?

  print(ranger_model)
  print(varImp(ranger_model))
  
  train_preds <- data.frame(predict(ranger_model, train[,-(which(names(train)=="PM2.5_Obs"))]))
  res<- train_preds - train$PM2.5_Obs
  quilt.plot(x = train$Latitude, y = train$Longitude, z = t(res))
  
  print(paste("Training set R^2 =", round(R2(pred = ranger_model$pred$pred, obs = ranger_model$pred$obs), digits = 4)))
  print(paste("Training set RMSE =", round(sqrt(mean((ranger_model$pred$pred - ranger_model$pred$obs)^2)), digits = 4)))

  test_preds <- data.frame(predict(ranger_model, test[,-(which(names(test)=="PM2.5_Obs"))]))
  compare<- cbind(test_preds, test$PM2.5_Obs)
  print(paste("Testing set R^2 =", round(R2(pred = compare[,1], obs = compare[,2]), digits = 4)))
  print(paste("Testing set RMSE =", round(sqrt(mean((compare[,1] - compare[,2])^2)), digits = 4)))

  quilt.plot(x = test$Latitude, y = test$Longitude, z = t(compare[,1] - compare[,2]))
}

#Regular folds:
runRanger(Sum08_SW)

#Specific folds:

#Monitors
runRanger(Sum08_SW, splitvar = c("Latitude", "Longitude"))

#Months
runRanger(small_Sum_SW, splitvar = c("Month", "Year"))



