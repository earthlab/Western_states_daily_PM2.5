### Author: Ellen Considine

library(caret)
library(CAST)
library(caretEnsemble)

getResults<- function(model, this_test){
  print("Training: ")
  print(model$error)
  model_list_preds <- data.frame(predict(model$models, this_test))
  stack_test_preds<- data.frame(predict(model$ens_model, model_list_preds))
  compare<- cbind(stack_test_preds, this_test$PM2.5_Obs)
  print(paste("Testing set R^2 =", round(R2(pred = compare[,1], obs = compare[,2]), digits = 4)))
  print(paste("Testing set RMSE =", round(sqrt(mean((compare[,1] - compare[,2])^2)), digits = 4)))
}

run_ens<- function(this_train, my_list, folds = NULL){
  set.seed(321)
  if(is.null(folds)){
    ind<- createFolds(this_train$PM2.5_Obs, 10, returnTrain = TRUE)
    row_reference<- my_list[[1]]$pred[order(my_list[[1]]$pred$obs),]
    IND<- lapply(ind, function(x) match(x, row_reference$rowIndex))
  }else if(folds == "spatial"){
    grouped<- as.numeric(as.factor(interaction(this_train[,c("Lon", "Lat")])))
    ind<- CreateSpacetimeFolds(cbind(this_train, group = grouped), spacevar = "group", k = 10)$index
    row_reference<- my_list[[1]]$pred[order(my_list[[1]]$pred$obs),]
    IND<- lapply(ind, function(x) match(x, row_reference$rowIndex))
  }
  ##Ensemble:
  stackControl <- trainControl(method = "repeatedcv", number = 10,
                               savePredictions = "final", verboseIter = FALSE,
                               index = IND)
  GLM<- caretStack(my_list, method = "glm", metric = "RMSE", trControl = stackControl)
  return(GLM)
}

