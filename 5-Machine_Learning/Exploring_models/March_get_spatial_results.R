source("~/Scripts/Data_prep.R")
source("~/Scripts/Results_functions.R")

# ranger_train<- read.csv("~/Results/March_All_ranger_train_preds_spatial.csv")
# ranger_test<- read.csv("~/Results/March_All_ranger_test_preds_spatial.csv")
# xgbt_train<- read.csv("~/Results/March_All_xgbt_train_preds_spatial.csv")
# xgbt_test<- read.csv("~/Results/March_All_xgbt_test_preds_spatial.csv")
# 
# GLM_train<- read.csv("~/Results/March_All_train_preds_spatial.csv")
# GLM_test<- read.csv("~/Results/March_All_test_preds_spatial.csv")
# 
# row_reference<- ranger_train[order(ranger_train$obs),]
# row.names(row_reference)<- 1:dim(row_reference)[1]
# GLM_train$rowIndex<- row_reference[GLM_train$rowIndex, "rowIndex"]

## Do these separately:

CMAQ_ranger_train<- read.csv("~/Results/March_CMAQ_ranger_train_preds_spatial.csv")
CMAQ_ranger_test<- read.csv("~/Results/March_CMAQ_ranger_test_preds_spatial.csv")
CMAQ_xgbt_train<- read.csv("~/Results/March_CMAQ_xgbt_train_preds_spatial.csv")
CMAQ_xgbt_test<- read.csv("~/Results/March_CMAQ_xgbt_test_preds_spatial.csv")

CMAQ_GLM_train<- read.csv("~/Results/March_CMAQ_train_preds_spatial.csv")
CMAQ_GLM_test<- read.csv("~/Results/March_CMAQ_test_preds_spatial.csv")

CMAQ_row_reference<- CMAQ_ranger_train[order(CMAQ_ranger_train$obs),]
row.names(CMAQ_row_reference)<- 1:dim(CMAQ_row_reference)[1]
CMAQ_GLM_train$rowIndex<- CMAQ_row_reference[CMAQ_GLM_train$rowIndex, "rowIndex"]

## RENAME all CMAQ ones to get the results:
GLM_train<- CMAQ_GLM_train
GLM_test<- CMAQ_GLM_test
ranger_train<- CMAQ_ranger_train
ranger_test<- CMAQ_ranger_test
xgbt_train<- CMAQ_xgbt_train
xgbt_test<- CMAQ_xgbt_test
spatial_train<- CMAQ_spatial_train
spatial_test<- CMAQ_spatial_test

##TABLES of results:

name<- "CMAQ_" #Change this each time!

my_levels<- c(0, 35, 60, 150, 300, 500, 1000)
RMSE_no_CMAQ<- cbind(PM_levels(GLM_train, spatial_train, my_levels, "RMSE", tr = TRUE),
                     PM_levels(GLM_test, spatial_test, my_levels, "RMSE", tr = FALSE),
                     PM_levels(GLM_train, spatial_train, my_levels, "RMSE", tr = TRUE, "spatial"),
                     PM_levels(GLM_test, spatial_test, my_levels, "RMSE", tr = FALSE, "spatial"),
                     PM_levels(GLM_train, spatial_train, my_levels, "RMSE", tr = TRUE, "temporal"),
                     PM_levels(GLM_test, spatial_test, my_levels, "RMSE", tr = FALSE, "temporal"),
                     PM_levels(ranger_train, spatial_train, my_levels, "RMSE", tr = TRUE),
                     PM_levels(ranger_test, spatial_test, my_levels, "RMSE", tr = FALSE),
                     PM_levels(xgbt_train, spatial_train, my_levels, "RMSE", tr = TRUE),
                     PM_levels(xgbt_test, spatial_test, my_levels, "RMSE", tr = FALSE))
RMSE_no_CMAQ<- as.data.frame(RMSE_no_CMAQ)  
row.names(RMSE_no_CMAQ)<- paste("Below", my_levels, "non-inclusive")
colnames(RMSE_no_CMAQ)<- c("Ensemble Training", "Ensemble Testing",
                           "Ensemble Training, Spatial", "Ensemble Testing, Spatial",
                           "Ensemble Training, Temporal", "Ensemble Testing, Temporal",
                           "Ranger Training", "Ranger Testing",
                           "XGBT Training", "XGBT Testing")
RMSE_no_CMAQ
write.csv(RMSE_no_CMAQ, paste0("~/Results/", name, "RMSE_PM25_non-inclusive-levels_spatial.csv"))

R2_no_CMAQ<- cbind(PM_levels(GLM_train, spatial_train, my_levels, "R2", tr = TRUE),
                   PM_levels(GLM_test, spatial_test, my_levels, "R2", tr = FALSE),
                   PM_levels(GLM_train, spatial_train, my_levels, "R2", tr = TRUE, "spatial"),
                   PM_levels(GLM_test, spatial_test, my_levels, "R2", tr = FALSE, "spatial"),
                   PM_levels(GLM_train, spatial_train, my_levels, "R2", tr = TRUE, "temporal"),
                   PM_levels(GLM_test, spatial_test, my_levels, "R2", tr = FALSE, "temporal"),
                   PM_levels(ranger_train, spatial_train, my_levels, "R2", tr = TRUE),
                   PM_levels(ranger_test, spatial_test, my_levels, "R2", tr = FALSE),
                   PM_levels(xgbt_train, spatial_train, my_levels, "R2", tr = TRUE),
                   PM_levels(xgbt_test, spatial_test, my_levels, "R2", tr = FALSE))
R2_no_CMAQ<- as.data.frame(R2_no_CMAQ)  
row.names(R2_no_CMAQ)<- paste("Below", my_levels, "non-inclusive")
colnames(R2_no_CMAQ)<- c("Ensemble Training", "Ensemble Testing",
                         "Ensemble Training, Spatial", "Ensemble Testing, Spatial",
                         "Ensemble Training, Temporal", "Ensemble Testing, Temporal",
                         "Ranger Training", "Ranger Testing",
                         "XGBT Training", "XGBT Testing")
R2_no_CMAQ
write.csv(R2_no_CMAQ, paste0("~/Results/", name, "R2_PM25_non-inclusive-levels_spatial.csv"))

###### Space-Time:
run_space_time<- function(subset_var, name){ # name = "No_CMAQ_" or "CMAQ_"
  RMSE<- cbind(space_time(GLM_train, spatial_train, subset_var, "RMSE", tr = TRUE),
               space_time(GLM_test, spatial_test, subset_var, "RMSE", tr = FALSE),
               space_time(GLM_train, spatial_train, subset_var, "RMSE", tr = TRUE, "spatial"),
               space_time(GLM_test, spatial_test, subset_var, "RMSE", tr = FALSE, "spatial"),
               space_time(GLM_train, spatial_train, subset_var, "RMSE", tr = TRUE, "temporal"),
               space_time(GLM_test, spatial_test, subset_var, "RMSE", tr = FALSE, "temporal"),
               space_time(ranger_train, spatial_train, subset_var, "RMSE", tr = TRUE),
               space_time(ranger_test, spatial_test, subset_var, "RMSE", tr = FALSE),
               space_time(xgbt_train, spatial_train, subset_var, "RMSE", tr = TRUE),
               space_time(xgbt_test, spatial_test, subset_var, "RMSE", tr = FALSE))
  RMSE<- as.data.frame(RMSE)  
  row.names(RMSE)<- sort(unique(spatial_train[,subset_var]))
  colnames(RMSE)<- c("Ensemble Training", "Ensemble Testing",
                     "Ensemble Training, Spatial", "Ensemble Testing, Spatial",
                     "Ensemble Training, Temporal", "Ensemble Testing, Temporal",
                     "Ranger Training", "Ranger Testing",
                     "XGBT Training", "XGBT Testing")
  write.csv(RMSE, paste0("~/Results/", name, subset_var, "_RMSE_spatial", ".csv"))
  print(paste0(name, subset_var, "_RMSE"))
  
  R2<- cbind(space_time(GLM_train, spatial_train, subset_var, "R2", tr = TRUE),
             space_time(GLM_test, spatial_test, subset_var, "R2", tr = FALSE),
             space_time(GLM_train, spatial_train, subset_var, "R2", tr = TRUE, "spatial"),
             space_time(GLM_test, spatial_test, subset_var, "R2", tr = FALSE, "spatial"),
             space_time(GLM_train, spatial_train, subset_var, "R2", tr = TRUE, "temporal"),
             space_time(GLM_test, spatial_test, subset_var, "R2", tr = FALSE, "temporal"),
             space_time(ranger_train, spatial_train, subset_var, "R2", tr = TRUE),
             space_time(ranger_test, spatial_test, subset_var, "R2", tr = FALSE),
             space_time(xgbt_train, spatial_train, subset_var, "R2", tr = TRUE),
             space_time(xgbt_test, spatial_test, subset_var, "R2", tr = FALSE))
  R2<- as.data.frame(R2)  
  row.names(R2)<- sort(unique(spatial_train[,subset_var]))
  colnames(R2)<- c("Ensemble Training", "Ensemble Testing",
                   "Ensemble Training, Spatial", "Ensemble Testing, Spatial",
                   "Ensemble Training, Temporal", "Ensemble Testing, Temporal",
                   "Ranger Training", "Ranger Testing",
                   "XGBT Training", "XGBT Testing")
  write.csv(R2, paste0("~/Results/", name, subset_var, "_R2_spatial", ".csv"))
  print(paste0(name, subset_var, "_R2"))
}

run_space_time("Year", "No_CMAQ_")
run_space_time("Season", "No_CMAQ_")
run_space_time("State", "No_CMAQ_")

## Read in CMAQ data and variable names, then:
# run_space_time("Year", "CMAQ_")
# run_space_time("Season", "CMAQ_")
# run_space_time("State", "CMAQ_")
