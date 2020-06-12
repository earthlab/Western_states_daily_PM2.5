#ML:
library(dplyr)
lev<- 15
n<- round(dim(Early_data)[1]*0.9)
set.seed(321)
tr_pos_CMAQ<- sample(1:(dim(Early_data)[1]),n, replace = FALSE)

high_train<- Early_data[tr_pos_CMAQ,][which(Early_data$PM2.5_Obs[tr_pos_CMAQ] >= lev),]
low_train<- Early_data[tr_pos_CMAQ,][which(Early_data$PM2.5_Obs[tr_pos_CMAQ] < lev),]
high_test<- Early_data[-tr_pos_CMAQ,][which(Early_data$PM2.5_Obs[-tr_pos_CMAQ] >= lev),]
low_test<- Early_data[-tr_pos_CMAQ,][which(Early_data$PM2.5_Obs[-tr_pos_CMAQ] < lev),]

sink("Cut_15_interactions_CMAQ_both.txt")

print("High:")
high_model<- run_ranger(high_train, "high")
high_model_xgbt<- run_xgbt(high_train)
saveRDS(high_model, "Cut_15_interactions-CMAQ-early_high_Ranger.rds")
# print(ML_to_CSV(high_model, high_test))
# print(varImp(high_model))

print("Low:")
low_model<- run_ranger(low_train, "low")
low_model_xgbt<- run_xgbt(low_train)
saveRDS(low_model, "Cut_15_interactions-CMAQ-early_low_Ranger.rds")
# print(ML_to_CSV(low_model_ranger, low_test))

sink()


#Exploring points:
high<- DATA[which(DATA$PM2.5_Obs > 300),]
high[,c("Date", "State", "PM2.5_Obs")]

########################################################

##Later years:

source("Base_ML_functions.R")

#Modeling:
DATA<- read.csv("/home/rstudio/Data_with_CMAQ_13-16.csv")

lev<- 15
n<- round(dim(DATA)[1]*0.9)
set.seed(321)
tr_pos_CMAQ<- sample(1:(dim(DATA)[1]),n, replace = FALSE)

high_train<- DATA[tr_pos_CMAQ,][which(DATA$PM2.5_Obs[tr_pos_CMAQ] >= lev),]
low_train<- DATA[tr_pos_CMAQ,][which(DATA$PM2.5_Obs[tr_pos_CMAQ] < lev),]
high_test<- DATA[-tr_pos_CMAQ,][which(DATA$PM2.5_Obs[-tr_pos_CMAQ] >= lev),]
low_test<- DATA[-tr_pos_CMAQ,][which(DATA$PM2.5_Obs[-tr_pos_CMAQ] < lev),]

sink("Cut_15_interactions_CMAQ_2013-2016.txt")

print("High:")
print("Ranger:")
high_model<- run_ranger(high_train, "high")
saveRDS(high_model, "Cut_15_interactions-CMAQ-late_high_Ranger.rds")
print(ML_to_CSV(high_model, high_test))
# print(varImp(high_model))
print("XGBT:")
high_model_xgbt<- run_xgbt(high_train)
saveRDS(high_model_xgbt, "Cut_15_interactions-CMAQ-late_high_XGBT.rds")
print(ML_to_CSV(high_model_xgbt, high_test))


print("Low:")
print("Ranger:")
low_model<- run_ranger(low_train, "low")
saveRDS(low_model, "Cut_15_interactions-CMAQ-late_low_Ranger.rds")
print(ML_to_CSV(low_model, low_test))
print("XGBT:")
low_model_xgbt<- run_xgbt(low_train)
saveRDS(low_model_xgbt, "Cut_15_interactions-CMAQ-late_low_XGBT.rds")
print(ML_to_CSV(low_model_xgbt, low_test))

sink()

##########
##Ensembles:

#Get packages and functions from Ensembles.R
high_ranger<- readRDS("Cut_15_interactions-CMAQ-late_high_Ranger.rds")
high_xgbt<- readRDS("Cut_15_interactions-CMAQ-late_high_XGBT.rds")
low_ranger<- readRDS("Cut_15_interactions-CMAQ-late_low_Ranger.rds")
low_xgbt<- readRDS("Cut_15_interactions-CMAQ-late_low_XGBT.rds")

glmControl<- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                          savePredictions = TRUE, verboseIter = TRUE)

rangerControl<- trainControl(method = "repeatedcv", number = 10, repeats = 1,
                             savePredictions = TRUE, verboseIter = TRUE)

high_list<- list(high_ranger, high_xgbt)
class(high_list)<- "caretList"

low_list<- list(low_ranger, low_xgbt)
class(low_list)<- "caretList"

##Models:

high_glm<- caretStack(high_list, method = "glm", metric = "RMSE", trControl = glmControl)
getResults(high_glm, high_test)

low_glm<- caretStack(low_list, method = "glm", metric = "RMSE", trControl = glmControl)
getResults(low_glm, low_test)

high_ranger<- caretStack(high_list, method = "ranger", metric = "RMSE", trControl = rangerControl)
getResults(high_ranger, high_test)

low_ranger<- caretStack(low_list, method = "ranger", metric = "RMSE", trControl = rangerControl)
getResults(low_ranger, low_test)

###################
## Smaller ranger models:
source("Base_ML_functions.R")

##2017-2018 only
load("Normalized_data2.RData")

Last<- DATA[which(DATA$Year %in% c(2017, 2018)),]
train<- DATA[as.numeric(row.names(train)),]
test<- DATA[as.numeric(row.names(test)),]
last_test<- test[which(test$Year %in% c(2017, 2018)),]
last_train<- train[which(train$Year %in% c(2017, 2018)),]

lev<- 15
high_train<- last_train[which(last_train$PM2.5_Obs >= lev),]
low_train<- last_train[which(last_train$PM2.5_Obs < lev),]
high_test<- last_test[which(last_test$PM2.5_Obs >= lev),]
low_test<- last_test[which(last_test$PM2.5_Obs < lev),]

sink("Cut_15_interactions_CMAQ_2017-2018.txt")

print("High:")
print("Ranger:")
high_model<- run_ranger(high_train, "high")
saveRDS(high_model, "Cut_15_interactions-2017-2018_high_Ranger.rds")
print(ML_to_CSV(high_model, high_test))
print(varImp(high_model))

print("Low:")
print("Ranger:")
low_model<- run_ranger(low_train, "low")
saveRDS(low_model, "Cut_15_interactions-2017-2018_low_Ranger.rds")
print(ML_to_CSV(low_model, low_test))

sink()

## Individual-year models:
DATA<- read.csv("/home/rstudio/Data_with_CMAQ_13-16.csv")

lev<- 15
n<- round(dim(DATA)[1]*0.9)
set.seed(321)
tr_pos_CMAQ<- sample(1:(dim(DATA)[1]),n, replace = FALSE)

high_train<- DATA[tr_pos_CMAQ,][which(DATA$PM2.5_Obs[tr_pos_CMAQ] >= lev),]
low_train<- DATA[tr_pos_CMAQ,][which(DATA$PM2.5_Obs[tr_pos_CMAQ] < lev),]
high_test<- DATA[-tr_pos_CMAQ,][which(DATA$PM2.5_Obs[-tr_pos_CMAQ] >= lev),]
low_test<- DATA[-tr_pos_CMAQ,][which(DATA$PM2.5_Obs[-tr_pos_CMAQ] < lev),]

sink("Cut_15_interactions_CMAQ_2017-2018.txt")

for(y in 2013:2016){
  this_h_train<- high_train[which(high_train$Year == y),]
  this_h_test<- high_test[which(high_test$Year == y),]
  this_l_train<- low_train[which(low_train$Year == y),]
  this_l_test<- low_test[which(low_test$Year == y),]
  
  print(y)
  print("High:")
  high_model<- run_ranger(this_h_train, "high")
  print(ML_to_CSV(high_model, this_h_test))
  print(varImp(high_model))
  
  print("Low:")
  low_model<- run_ranger(this_l_train, "low")
  print(ML_to_CSV(low_model, this_l_test))
}

sink()