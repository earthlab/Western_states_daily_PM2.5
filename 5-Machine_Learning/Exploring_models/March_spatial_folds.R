source("~/Scripts/Data_prep.R")
source("~/Scripts/Base_ML_functions.R")
source("~/Scripts/Ensembles.R")

##2008-2018 no CMAQ:

sink("~/Results/March_All_years_individual_spatial.txt")

print("Ranger:")
ranger_model<- run_ranger(spatial_train, folds = "spatial")
print(ML_to_CSV(ranger_model, spatial_test))

write.csv(ranger_model$pred[,c("rowIndex", "Resample", "pred", "obs")],
          "~/Results/March_All_ranger_train_preds_spatial.csv", row.names = FALSE)

test_preds<- data.frame(predict(ranger_model, spatial_test))
test_DF<- data.frame(Preds = test_preds[,1], Obs = spatial_test$PM2.5_Obs)
write.csv(test_DF, "~/Results/March_All_ranger_test_preds_spatial.csv", row.names = FALSE)

saveRDS(ranger_model, "~/Models/March_All_years_ranger_spatial.rds")

print("XGBT:")
xgbt_model<- run_xgbt(spatial_train, folds = "spatial")
print(ML_to_CSV(xgbt_model, spatial_test))

write.csv(xgbt_model$pred[,c("rowIndex", "Resample", "pred", "obs")],
          "~/Results/March_All_xgbt_train_preds_spatial.csv", row.names = FALSE)

test_preds2<- data.frame(predict(xgbt_model, spatial_test))
test_DF2<- data.frame(Preds = test_preds2[,1], Obs = spatial_test$PM2.5_Obs)
write.csv(test_DF2, "~/Results/March_All_xgbt_test_preds_spatial.csv", row.names = FALSE)

saveRDS(xgbt_model, "~/Models/March_All_years_xgbt_spatial.rds")

sink()

##Ensemble without CMAQ:

# ranger_model<- readRDS("~/Models/March_All_years_ranger_spatial.rds")
# xgbt_model<- readRDS("~/Models/March_All_years_xgbt_spatial.rds")

my_list<- list(ranger_model, xgbt_model)
class(my_list)<- "caretList"

sink("~/Results/March_Ens_GLM_no-split_spatial.txt")

my_GLM<- run_ens(spatial_train, my_list, folds = "spatial")
print(getResults(my_GLM, spatial_test))

write.csv(my_GLM$ens_model$pred[,c("rowIndex", "Resample", "pred", "obs")],
          "~/Results/March_All_train_preds_spatial.csv", row.names = FALSE)

test_list_preds<- data.frame(predict(my_GLM$models, spatial_test))
test_stack_preds<- data.frame(predict(my_GLM$ens_model, test_list_preds))
test_DF<- data.frame(Preds = test_stack_preds[,1], Obs = spatial_test$PM2.5_Obs)
write.csv(test_DF, "~/Results/March_All_test_preds_spatial.csv", row.names = FALSE)

saveRDS(my_GLM, "~/Models/March_Ens_GLM_no-split_spatial.rds")
sink()

##Free up memory:
rm(list = c("ranger_model","xgbt_model", "my_GLM", "my_list"))

##2008-2016 with CMAQ:

sink("~/Results/March_All_years_CMAQ_individual_spatial.txt")

print("Ranger:")
CMAQ_ranger_model<- run_ranger(CMAQ_spatial_train, folds = "spatial")
print(ML_to_CSV(CMAQ_ranger_model, CMAQ_spatial_test))

write.csv(CMAQ_ranger_model$pred[,c("rowIndex", "Resample", "pred", "obs")],
          "~/Results/March_CMAQ_ranger_train_preds_spatial.csv", row.names = FALSE)

CMAQ_test_preds<- data.frame(predict(CMAQ_ranger_model, CMAQ_spatial_test))
CMAQ_test_DF<- data.frame(Preds = CMAQ_test_preds[,1], Obs = CMAQ_spatial_test$PM2.5_Obs)
write.csv(CMAQ_test_DF, "~/Results/March_CMAQ_ranger_test_preds_spatial.csv", row.names = FALSE)

saveRDS(CMAQ_ranger_model, "~/Models/March_All_years_CMAQ_ranger_spatial.rds")

print("XGBT:")
CMAQ_xgbt_model<- run_xgbt(CMAQ_spatial_train, folds = "spatial")
print(ML_to_CSV(CMAQ_xgbt_model, CMAQ_spatial_test))

write.csv(CMAQ_xgbt_model$pred[,c("rowIndex", "Resample", "pred", "obs")],
          "~/Results/March_CMAQ_xgbt_train_preds_spatial.csv", row.names = FALSE)

CMAQ_test_preds2<- data.frame(predict(CMAQ_xgbt_model, CMAQ_spatial_test))
CMAQ_test_DF2<- data.frame(Preds = CMAQ_test_preds2[,1], Obs = CMAQ_spatial_test$PM2.5_Obs)
write.csv(CMAQ_test_DF2, "~/Results/March_CMAQ_xgbt_test_preds_spatial.csv", row.names = FALSE)

saveRDS(CMAQ_xgbt_model, "~/Models/March_All_years_CMAQ_xgbt_spatial.rds")

sink()

##Ensemble with CMAQ:

# CMAQ_ranger_model<- readRDS("~/Models/March_All_years_CMAQ_ranger_spatial.rds")
# CMAQ_xgbt_model<- readRDS("~/Models/March_All_years_CMAQ_xgbt_spatial.rds")

CMAQ_my_list<- list(CMAQ_ranger_model, CMAQ_xgbt_model)
class(CMAQ_my_list)<- "caretList"

sink("~/Results/March_Ens_GLM_CMAQ_no-split_spatial.txt")

CMAQ_my_GLM<- run_ens(CMAQ_spatial_train, CMAQ_my_list, folds = "spatial")
print(getResults(CMAQ_my_GLM, CMAQ_spatial_test))

write.csv(CMAQ_my_GLM$ens_model$pred[,c("rowIndex", "Resample", "pred", "obs")],
          "~/Results/March_CMAQ_train_preds_spatial.csv", row.names = FALSE)

CMAQ_test_list_preds<- data.frame(predict(CMAQ_my_GLM$models, CMAQ_spatial_test))
CMAQ_test_stack_preds<- data.frame(predict(CMAQ_my_GLM$ens_model, CMAQ_test_list_preds))
CMAQ_test_DF<- data.frame(Preds = CMAQ_test_stack_preds[,1], Obs = CMAQ_spatial_test$PM2.5_Obs)
write.csv(CMAQ_test_DF, "~/Results/March_CMAQ_test_preds_spatial.csv", row.names = FALSE)

saveRDS(CMAQ_my_GLM, "~/Models/March_Ens_GLM_CMAQ_no-split_spatial.rds")
sink()
