# source("~/Scripts/Data_prep.R")

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


# ##2008-2018 no CMAQ:
# 
# #Read in models:
# high_ranger<- readRDS("~/Models/All_years_high_ranger.rds")
# low_ranger<- readRDS("~/Models/All_years_low_ranger.rds")
# high_xgbt<- readRDS("~/Models/All_years_high_xgbt.rds")
# low_xgbt<- readRDS("~/Models/All_years_low_xgbt.rds")
# 
# # #Create interactions of predictions and lat,lon:
# # high_ranger_preds<- data.frame(predict(high_ranger, high_train))[,1]
# # high_xgbt_preds<- data.frame(predict(high_xgbt, high_train))[,1]
# # high_obs<- high_train$PM2.5_Obs
# # 
# # high_ints<- train(high_obs ~ high_ranger_preds*high_xgbt_preds,
# #                     method = "ranger", num.trees = 150)
# # 
# # high_ranger_test_preds<- data.frame(predict(high_ranger, high_test))
# # high_xgbt_test_preds<- data.frame(predict(high_xgbt, high_test))
# 
# 
# #Create lists:
# high_list<- list(high_ranger, high_xgbt)
# class(high_list)<- "caretList"
# low_list<- list(low_ranger, low_xgbt)
# class(low_list)<- "caretList"
# 
# #Stacks:
# sink("~/Results/Ens_GLM.txt")
# GLM_high<- caretStack(high_list, method = "glm", metric = "RMSE", trControl = stackControl)
# print(getResults(GLM_high, high_test))
# saveRDS(GLM_high, "~/Models/Ens_GLM_high.rds")
# 
# GLM_low<- caretStack(low_list, method = "glm", metric = "RMSE", trControl = stackControl)
# print(getResults(GLM_low, low_test))
# saveRDS(GLM_low, "~/Models/Ens_GLM_low.rds")
# sink()
# 
# #Ranger as stacker:
# non_GLM_Control <- trainControl(method = "repeatedcv", number = 10, repeats = 1,
#                              savePredictions = TRUE, verboseIter = TRUE)
# 
# sink("~/Results/Ens_Ranger.txt")
# Ranger_high<- caretStack(high_list, method = "ranger", metric = "RMSE", trControl = non_GLM_Control)
# print(getResults(Ranger_high, high_test))
# saveRDS(Ranger_high, "~/Models/Ens_Ranger_high.rds")
# 
# Ranger_low<- caretStack(low_list, method = "ranger", metric = "RMSE", trControl = non_GLM_Control)
# print(getResults(Ranger_low, low_test))
# saveRDS(Ranger_low, "~/Models/Ens_Ranger_low.rds")
# 
# sink()
# 
# # rm(list = c("GLM_high", "GLM_low", "CMAQ_GLM_high", "CMAQ_GLM_low"))
# # rm("DATA", "data", "CMAQ_test", "CMAQ_train")
# 
# # library(gam)
# # 
# # GAM_high_stack<- caretStack(high_list_ints, method = "gam", metric = "RMSE", trControl = stackControl)
# 
# 
# ##2008-2016 with CMAQ:
# 
# #Read in models:
# CMAQ_high_ranger<- readRDS("~/Models/CMAQ_years_high_ranger.rds")
# CMAQ_low_ranger<- readRDS("~/Models/CMAQ_years_low_ranger.rds")
# CMAQ_high_xgbt<- readRDS("~/Models/CMAQ_years_high_xgbt.rds")
# CMAQ_low_xgbt<- readRDS("~/Models/CMAQ_years_low_xgbt.rds")
# 
# #Create lists:
# CMAQ_high_list<- list(CMAQ_high_ranger, CMAQ_high_xgbt)
# class(CMAQ_high_list)<- "caretList"
# CMAQ_low_list<- list(CMAQ_low_ranger, CMAQ_low_xgbt)
# class(CMAQ_low_list)<- "caretList"
# 
# #Stacks:
# sink("~/Results/Ens_GLM_CMAQ.txt")
# CMAQ_GLM_high<- caretStack(CMAQ_high_list, method = "glm", metric = "RMSE", trControl = stackControl)
# print(getResults(CMAQ_GLM_high, CMAQ_high_test))
# saveRDS(CMAQ_GLM_high, "~/Models/Ens_GLM_CMAQ_high.rds")
# 
# CMAQ_GLM_low<- caretStack(CMAQ_low_list, method = "glm", metric = "RMSE", trControl = stackControl)
# print(getResults(CMAQ_GLM_low, CMAQ_low_test))
# saveRDS(CMAQ_GLM_low, "~/Models/Ens_GLM_CMAQ_low.rds")
# sink()
# 
# ##Testing subsets of the data:
# GLM_high<- readRDS("~/Models/Ens_GLM_high.rds")
# GLM_low<- readRDS("~/Models/Ens_GLM_low.rds")
# CMAQ_GLM_high<- readRDS("~/Models/Ens_GLM_CMAQ_high.rds")
# CMAQ_GLM_low<- readRDS("~/Models/Ens_GLM_CMAQ_low.rds")
# 
# subset_Results<- function(model, years, test_set){
#   tab<- model$ens_model$pred[which(model$models[[1]]$trainingData$Year %in% years),]
#   Pred<- tab$pred
#   Obs<- tab$obs
#   Train_R2<- round(R2(pred = Pred, obs = Obs), digits = 4)
#   Train_RMSE<- round(sqrt(mean((Pred - Obs)^2)), digits = 4)
#   
#   testing<- test_set[which(test_set$Year %in% years),]
#   model_list_preds <- data.frame(predict(model$models, testing))
#   stack_test_preds<- data.frame(predict(model$ens_model, model_list_preds))
#   compare<- cbind(stack_test_preds, testing$PM2.5_Obs)
#   Test_R2<- round(R2(pred = compare[,1], obs = compare[,2]), digits = 4)
#   Test_RMSE<- round(sqrt(mean((compare[,1] - compare[,2])^2)), digits = 4)
#   
#   print(c(Train_RMSE, Train_R2, Test_RMSE, Test_R2))
# }
# 
# subset_Results(GLM_high, 2008:2012, high_test)
# subset_Results(CMAQ_GLM_high, 2008:2012, CMAQ_high_test)
# subset_Results(GLM_low, 2008:2012, low_test)
# subset_Results(CMAQ_GLM_low, 2008:2012, CMAQ_low_test)
# 
# subset_Results(GLM_high, 2013:2016, high_test)
# subset_Results(CMAQ_GLM_high, 2013:2016, CMAQ_high_test)
# subset_Results(GLM_low, 2013:2016, low_test)
# subset_Results(CMAQ_GLM_low, 2013:2016, CMAQ_low_test)
# 
# subset_Results(GLM_high, 2017:2018, high_test)
# subset_Results(GLM_low, 2017:2018, low_test)
# 
