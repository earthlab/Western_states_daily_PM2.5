source("~/Scripts/Data_prep.R")
source("~/Scripts/Base_ML_functions.R")
source("~/Scripts/Ensembles.R")

##2008-2018 no CMAQ:
# 
# sink("~/Results/March_All_years_individual.txt")
# 
# print("Ranger:")
# ranger_model<- run_ranger(train, folds = "partition")
# print(ML_to_CSV(ranger_model, test))
# 
# write.csv(ranger_model$pred[,c("rowIndex", "Resample", "pred", "obs")],
#           "~/Results/March_All_ranger_train_preds.csv", row.names = FALSE)
# 
# test_preds<- data.frame(predict(ranger_model, test))
# test_DF<- data.frame(Preds = test_preds[,1], Obs = test$PM2.5_Obs)
# write.csv(test_DF, "~/Results/March_All_ranger_test_preds.csv", row.names = FALSE)
# 
# saveRDS(ranger_model, "~/Models/March_All_years_ranger.rds")
# 
# print("XGBT:")
# xgbt_model<- run_xgbt(train, folds = "partition")
# print(ML_to_CSV(xgbt_model, test))
# 
# write.csv(xgbt_model$pred[,c("rowIndex", "Resample", "pred", "obs")],
#           "~/Results/March_All_xgbt_train_preds.csv", row.names = FALSE)
# 
# test_preds2<- data.frame(predict(xgbt_model, test))
# test_DF2<- data.frame(Preds = test_preds2[,1], Obs = test$PM2.5_Obs)
# write.csv(test_DF2, "~/Results/March_All_xgbt_test_preds.csv", row.names = FALSE)
# 
# saveRDS(xgbt_model, "~/Models/March_All_years_xgbt.rds")
# 
# sink()

##Ensemble without CMAQ:

ranger_model<- readRDS("~/Models/March_All_years_ranger.rds")
xgbt_model<- readRDS("~/Models/March_All_years_xgbt.rds")

my_list<- list(ranger_model, xgbt_model)
class(my_list)<- "caretList"

sink("~/Results/March_Ens_GLM_no-split.txt")

my_GLM<- run_ens(train, my_list)
print(getResults(my_GLM, test))

write.csv(my_GLM$ens_model$pred[,c("rowIndex", "Resample", "pred", "obs")],
          "~/Results/March_All_train_preds.csv", row.names = FALSE)

test_list_preds<- data.frame(predict(my_GLM$models, test))
test_stack_preds<- data.frame(predict(my_GLM$ens_model, test_list_preds))
test_DF<- data.frame(Preds = test_stack_preds[,1], Obs = test$PM2.5_Obs)
write.csv(test_DF, "~/Results/March_All_test_preds.csv", row.names = FALSE)

saveRDS(my_GLM, "~/Models/March_Ens_GLM_no-split.rds")
sink()

##Free up memory:
rm(list = c("ranger_model","xgbt_model", "my_GLM"))

##2008-2016 with CMAQ:
# 
# sink("~/Results/March_All_years_CMAQ_individual.txt")
# 
# print("Ranger:")
# CMAQ_ranger_model<- run_ranger(CMAQ_train, folds = "partition")
# print(ML_to_CSV(CMAQ_ranger_model, CMAQ_test))
# 
# write.csv(CMAQ_ranger_model$pred[,c("rowIndex", "Resample", "pred", "obs")],
#           "~/Results/March_CMAQ_ranger_train_preds.csv", row.names = FALSE)
# 
# CMAQ_test_preds<- data.frame(predict(CMAQ_ranger_model, CMAQ_test))
# CMAQ_test_DF<- data.frame(Preds = CMAQ_test_preds[,1], Obs = CMAQ_test$PM2.5_Obs)
# write.csv(CMAQ_test_DF, "~/Results/March_CMAQ_ranger_test_preds.csv", row.names = FALSE)
# 
# saveRDS(CMAQ_ranger_model, "~/Models/March_All_years_CMAQ_ranger.rds")
# 
# print("XGBT:")
# CMAQ_xgbt_model<- run_xgbt(CMAQ_train, folds = "partition")
# print(ML_to_CSV(CMAQ_xgbt_model, CMAQ_test))
# 
# write.csv(CMAQ_xgbt_model$pred[,c("rowIndex", "Resample", "pred", "obs")],
#           "~/Results/March_CMAQ_xgbt_train_preds.csv", row.names = FALSE)
# 
# CMAQ_test_preds2<- data.frame(predict(CMAQ_xgbt_model, CMAQ_test))
# CMAQ_test_DF2<- data.frame(Preds = CMAQ_test_preds2[,1], Obs = CMAQ_test$PM2.5_Obs)
# write.csv(CMAQ_test_DF2, "~/Results/March_CMAQ_xgbt_test_preds.csv", row.names = FALSE)
# 
# saveRDS(CMAQ_xgbt_model, "~/Models/March_All_years_CMAQ_xgbt.rds")
# 
# sink()

##Ensemble with CMAQ:

CMAQ_ranger_model<- readRDS("~/Models/March_All_years_CMAQ_ranger.rds")
CMAQ_xgbt_model<- readRDS("~/Models/March_All_years_CMAQ_xgbt.rds")

CMAQ_my_list<- list(CMAQ_ranger_model, CMAQ_xgbt_model)
class(CMAQ_my_list)<- "caretList"

sink("~/Results/March_Ens_GLM_CMAQ_no-split.txt")

CMAQ_my_GLM<- run_ens(CMAQ_train, CMAQ_my_list)
print(getResults(CMAQ_my_GLM, CMAQ_test))

write.csv(CMAQ_my_GLM$ens_model$pred[,c("rowIndex", "Resample", "pred", "obs")],
          "~/Results/March_CMAQ_train_preds.csv", row.names = FALSE)

CMAQ_test_list_preds<- data.frame(predict(CMAQ_my_GLM$models, CMAQ_test))
CMAQ_test_stack_preds<- data.frame(predict(CMAQ_my_GLM$ens_model, CMAQ_test_list_preds))
CMAQ_test_DF<- data.frame(Preds = CMAQ_test_stack_preds[,1], Obs = CMAQ_test$PM2.5_Obs)
write.csv(CMAQ_test_DF, "~/Results/March_CMAQ_test_preds.csv", row.names = FALSE)

saveRDS(CMAQ_my_GLM, "~/Models/March_Ens_GLM_CMAQ_no-split.rds")
sink()


##Make FVO plots:
# 
# plot(train$PM2.5_Obs, train_stack_preds[,1], xlab = "Observed", ylab = "Predicted",
#      main = "2008-2018, no CMAQ: Training Set", ylim = c(0,600))
# lines(1:max(train$PM2.5_Obs), 1:max(train$PM2.5_Obs))
# 
# plot(test$PM2.5_Obs, test_stack_preds[,1], xlab = "Observed", ylab = "Predicted",
#      main = "2008-2018, no CMAQ: Testing Set", ylim = c(0,600))
# lines(1:max(train$PM2.5_Obs), 1:max(train$PM2.5_Obs))
# 
# plot(test$PM2.5_Obs, test_stack_preds[,1], xlab = "Observed", ylab = "Predicted",
#      main = "2008-2018, no CMAQ: Truncated Testing Set", ylim = c(0,200), xlim = c(0,200))
# lines(1:max(train$PM2.5_Obs), 1:max(train$PM2.5_Obs))
# 
# ##With CMAQ:
# plot(CMAQ_train$PM2.5_Obs, CMAQ_train_stack_preds[,1], xlab = "Observed", ylab = "Predicted",
#      main = "2008-2016, with CMAQ: Training Set", ylim = c(0,600))
# lines(1:max(CMAQ_train$PM2.5_Obs), 1:max(CMAQ_train$PM2.5_Obs))
# 
# plot(CMAQ_test$PM2.5_Obs, CMAQ_test_stack_preds[,1], xlab = "Observed", ylab = "Predicted",
#      main = "2008-2016, with CMAQ: Testing Set", ylim = c(0,600), xlim = c(0,800))
# lines(1:max(CMAQ_train$PM2.5_Obs), 1:max(CMAQ_train$PM2.5_Obs))
# 
# plot(CMAQ_test$PM2.5_Obs, CMAQ_test_stack_preds[,1], xlab = "Observed", ylab = "Predicted",
#      main = "2008-2016, with CMAQ: Truncated Testing Set", ylim = c(0,200), xlim = c(0,200))
# lines(1:max(CMAQ_train$PM2.5_Obs), 1:max(CMAQ_train$PM2.5_Obs))
