# source("~/4-Merge_Data/Training_observations/Training_data_prep.R")
#
# ##2008-2018, no CMAQ:
# 
# Train<- rbind(high_train, low_train)
# Train$High_Low<- as.factor(as.numeric(Train$PM2.5_Obs > lev))
# Test<- rbind(high_test, low_test)
# Test$High_Low<- as.factor(as.numeric(Test$PM2.5_Obs > lev))
# 
# levels(Train$High_Low)<- c("Low", "High")
# levels(Test$High_Low)<- c("Low", "High")
# 
# class_model<- readRDS("~/Models/Classify_all-years.rds")
# 
# train_preds <- data.frame(predict(class_model, Train))[,1]
# 
# pred_high_tr<- Train[which(train_preds == "High"), -which(names(Train) == "High_Low")]
# pred_low_tr<- Train[which(train_preds == "Low"), -which(names(Train) == "High_Low")]
# 
# test_preds <- data.frame(predict(class_model, Test))[,1]
# 
# pred_high<- Test[which(test_preds == "High"),]
# pred_low<- Test[which(test_preds == "Low"),]
# 
# ## With CMAQ:
# CMAQ_Train<- rbind(CMAQ_high_train, CMAQ_low_train)
# CMAQ_Train$High_Low<- as.factor(as.numeric(CMAQ_Train$PM2.5_Obs > lev))
# CMAQ_Test<- rbind(CMAQ_high_test, CMAQ_low_test)
# CMAQ_Test$High_Low<- as.factor(as.numeric(CMAQ_Test$PM2.5_Obs > lev))
# 
# levels(CMAQ_Train$High_Low)<- c("Low", "High")
# levels(CMAQ_Test$High_Low)<- c("Low", "High")
# 
# CMAQ_class_model<- readRDS("~/Models/Classify_all-years_CMAQ.rds")
# 
# CMAQ_train_preds <- data.frame(predict(CMAQ_class_model, CMAQ_Train))[,1]
# 
# CMAQ_pred_high_tr<- CMAQ_Train[which(CMAQ_train_preds == "High"),-which(names(CMAQ_Train) == "High_Low")]
# CMAQ_pred_low_tr<- CMAQ_Train[which(CMAQ_train_preds == "Low"),-which(names(CMAQ_Train) == "High_Low")]
# 
# CMAQ_test_preds <- data.frame(predict(CMAQ_class_model, CMAQ_Test))[,1]
# 
# CMAQ_pred_high<- CMAQ_Test[which(CMAQ_test_preds == "High"),]
# CMAQ_pred_low<- CMAQ_Test[which(CMAQ_test_preds == "Low"),]
# 
# rm(list=setdiff(ls(), c("pred_high", "pred_low", "CMAQ_pred_high", "CMAQ_pred_low",
#                         "pred_high_tr", "pred_low_tr", "CMAQ_pred_high_tr", "CMAQ_pred_low_tr")))
# 
# save.image("Post-classification_data-sets-2.RData")

load("Post-classification_data-sets-2.RData")
source("~/Scripts/Base_ML_functions.R")

Train<- rbind(pred_high_tr, pred_low_tr)
Test<- rbind(pred_high, pred_low)

CMAQ_Train<- rbind(CMAQ_pred_high_tr, CMAQ_pred_low_tr)
CMAQ_Test<- rbind(CMAQ_pred_high, CMAQ_pred_low)

## Individual models:

sink("~/Results/All_years_individual_with-HL.txt")

print("Ranger:")
ranger_model<- run_ranger(Train)
saveRDS(ranger_model, "~/Models/All_years_ranger_with-HL.rds")
print(ML_to_CSV(ranger_model, Test))

print("XGBT:")
xgbt_model<- run_xgbt(Train)
saveRDS(xgbt_model, "~/Models/All_years_xgbt_with-HL.rds")
print(ML_to_CSV(xgbt_model, Test))

sink()


##2008-2016 with CMAQ:

sink("~/Results/All_years_CMAQ_individual_with-HL.txt")

print("Ranger:")
CMAQ_ranger_model<- run_ranger(CMAQ_Train)
saveRDS(CMAQ_ranger_model, "~/Models/All_years_CMAQ_ranger_with-HL.rds")
print(ML_to_CSV(CMAQ_ranger_model, CMAQ_Test))

print("XGBT:")
CMAQ_xgbt_model<- run_xgbt(CMAQ_Train)
saveRDS(CMAQ_xgbt_model, "~/Models/All_years_CMAQ_xgbt_with-HL.rds")
print(ML_to_CSV(CMAQ_xgbt_model, CMAQ_Test))

sink()


##Ensemble: get functions from Ensembles.R
# ranger_model<- readRDS("~/Models/All_years_ranger_with-HL.rds")
# xgbt_model<- readRDS("~/Models/All_years_xgbt_with-HL.rds")

my_list<- list(ranger_model, xgbt_model)
class(my_list)<- "caretList"

sink("~/Results/Ens_GLM_no-split_with-HL.txt")
my_GLM<- caretStack(my_list, method = "glm", metric = "RMSE", trControl = stackControl)
print(getResults(my_GLM, Test))
saveRDS(my_GLM, "~/Models/Ens_GLM_no-split_with-HL.rds")
sink()

#With CMAQ:

# CMAQ_ranger_model<- readRDS("~/Models/All_years_CMAQ_ranger_with-HL.rds")
# CMAQ_xgbt_model<- readRDS("~/Models/All_years_CMAQ_xgbt_with-HL.rds")

CMAQ_my_list<- list(CMAQ_ranger_model, CMAQ_xgbt_model)
class(CMAQ_my_list)<- "caretList"

sink("~/Results/Ens_GLM_CMAQ_no-split_with-HL.txt")
CMAQ_my_GLM<- caretStack(CMAQ_my_list, method = "glm", metric = "RMSE", trControl = stackControl)
print(getResults(CMAQ_my_GLM, CMAQ_Test))
saveRDS(CMAQ_my_GLM, "~/Models/Ens_GLM_CMAQ_no-split_with-HL.rds")
sink()




