### Author: Ellen Considine

gc()

#Read in packages:
library(caret)
library(CAST)
library(parallel)
library(doParallel)
library(pryr)

#Define useful functions:
ML_to_CSV<- function(model, test_set){
  
  # #get training results
  train_RMSE<- round(model$results$RMSE, digits = 4)
  train_R2<- round(model$results$Rsquared, digits = 4)
  ##If standardizing variables:
  # train_preds<- model$pred$pred*stats$std["PM2.5_Obs"] + stats$mean["PM2.5_Obs"]
  # train_obs<- model$pred$obs*stats$std["PM2.5_Obs"] + stats$mean["PM2.5_Obs"]
  # train_RMSE<- round(sqrt(mean((train_preds - train_obs)^2)), digits = 4)
  # train_R2<- round(R2(pred = train_preds, obs = train_obs), digits = 4)
  
  #compute testing results
  test_preds <- data.frame(predict(model, test_set))
  compare<- cbind(test_preds, test_set[,"PM2.5_Obs"])
  
  ##If standardizing variables:
  # compare<- apply(compare, MARGIN = 2, 
  #                 function(y){y*stats$std["PM2.5_Obs"] + stats$mean["PM2.5_Obs"]})
  # 
  resids<- (compare[,1] - compare[,2])
  test_RMSE<- round(sqrt(mean(resids^2)), digits = 4)
  test_R2<- round(R2(pred = compare[,1], obs = compare[,2]), digits = 4)
  
  results<- c(train_RMSE, train_R2, test_RMSE, test_R2)
  # names(results)<- c("Training RMSE", "Training R2", "Testing RMSE", "Testing R2")
  return(results)
}

run_ranger<- function(this_train, folds = NULL){
  
  set.seed(321)
  # n<- round(dim(DATA)[1]*test_pct)
  # test_pos<- sample(1:(dim(DATA)[1]),n, replace = FALSE)
  # test<- DATA[test_pos,]
  # train<- DATA[-test_pos,]
  if(folds == "partition"){
    IND<- createFolds(this_train$PM2.5_Obs, 10, returnTrain = TRUE)
  }else if(folds == "spatial"){
    grouped<- as.numeric(as.factor(interaction(this_train[,c("Lon", "Lat")])))
    IND<- CreateSpacetimeFolds(cbind(this_train, group = grouped), spacevar = "group", k = 10)$index
  }else{
    IND<- createResample(this_train$PM2.5_Obs, 10)
  }
  
  myControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = "grid",
                            savePredictions = "final", index = IND,
                            verboseIter = FALSE, allowParallel = FALSE)
  tgrid<- expand.grid( .mtry = 15, .splitrule = "variance", .min.node.size = 6 ) #15, 6
  
  #Set up parallelization
  start<- Sys.time()
  # cluster <- makeCluster(3)
  # registerDoParallel(cluster)
  
  ranger_model <- train(PM2.5_Obs ~ ., data = this_train, method = "ranger",
                        trControl = myControl, tuneGrid = tgrid, num.trees = 150 )
  # , importance = "permutation")
  
  # stopCluster(cluster)
  
  end<- Sys.time()
  # PID<- Sys.getpid()
  # Mem_peak<- system(paste0('grep VmPeak /proc/', PID, '/status'), intern = TRUE)
  # 
  # print(Mem_peak)
  print(end-start)
  print(ranger_model$results)
  
  return(ranger_model)
}

run_xgbt<- function(this_train, folds = NULL){
  
  set.seed(321)
  # n<- round(dim(DATA)[1]*test_pct)
  # test_pos<- sample(1:(dim(DATA)[1]),n, replace = FALSE)
  # test<- DATA[test_pos,]
  # train<- DATA[-test_pos,]
  if(folds == "partition"){
    IND<- createFolds(this_train$PM2.5_Obs, 10, returnTrain = TRUE)
  }else if(folds == "spatial"){
    grouped<- as.numeric(as.factor(interaction(this_train[,c("Lon", "Lat")])))
    IND<- CreateSpacetimeFolds(cbind(this_train, group = grouped), spacevar = "group", k = 10)$index
  }else{
    IND<- createResample(this_train$PM2.5_Obs, 10)
  }
  
  myControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = "grid",
                            savePredictions = "final", index = IND,
                            verboseIter = FALSE, allowParallel = FALSE)
  tgrid<- expand.grid( .nrounds = 150, .max_depth = 3, .eta = 0.3,
                       .gamma = 0, .colsample_bytree = 2/3, .min_child_weight = 1, 
                       .subsample = 1)
  
  #Set up parallelization
  start<- Sys.time()
  # cluster <- makeCluster(3)
  # registerDoParallel(cluster)
  
  xgbt_model <- train(PM2.5_Obs ~ ., data = this_train, method = "xgbTree",
                      trControl = myControl, tuneGrid = tgrid)
  
  # stopCluster(cluster)
  
  end<- Sys.time()
  # PID<- Sys.getpid()
  # Mem_peak<- system(paste0('grep VmPeak /proc/', PID, '/status'), intern = TRUE)
  # 
  # print(Mem_peak)
  print(end-start)
  print(xgbt_model$results)
  
  return(xgbt_model)
}
