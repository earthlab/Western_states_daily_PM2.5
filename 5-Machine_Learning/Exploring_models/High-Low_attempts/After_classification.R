##### Feed the results of the classification model into the regression models...

source("~/Scripts/Data_prep.R")
source("~/Scripts/Base_ML_functions.R")

##2008-2018, no CMAQ:

Train<- rbind(high_train, low_train)
Train$High_Low<- as.factor(as.numeric(Train$PM2.5_Obs > lev))
Test<- rbind(high_test, low_test)
Test$High_Low<- as.factor(as.numeric(Test$PM2.5_Obs > lev))

levels(Train$High_Low)<- c("Low", "High")
levels(Test$High_Low)<- c("Low", "High")

class_train<- Train[,-which(names(Train) %in% c("PM2.5_Obs"))]
class_test<- Test[,-which(names(Test) %in% c("PM2.5_Obs"))]

class_model<- readRDS("~/Models/Classify_all-years.rds")

test_preds <- data.frame(predict(class_model, class_test))[,1]

pred_high<- Test[which(test_preds == "High"),]
pred_low<- Test[which(test_preds == "Low"),]

wrong_high<- pred_high[which(pred_high$High_Low == "Low"),]
wrong_low<- pred_low[which(pred_low$High_Low == "High"),]

#Test on (ensemble) regressions:
Ens_high<- readRDS("~/Models/Ens_GLM_high.rds")
Ens_low<- readRDS("~/Models/Ens_GLM_low.rds")

#Get packages and functions from Ensembles.R
getResults(Ens_high, pred_high)
getResults(Ens_low, pred_low)

## With CMAQ:
CMAQ_Train<- rbind(CMAQ_high_train, CMAQ_low_train)
CMAQ_Train$High_Low<- as.factor(as.numeric(CMAQ_Train$PM2.5_Obs > lev))
CMAQ_Test<- rbind(CMAQ_high_test, CMAQ_low_test)
CMAQ_Test$High_Low<- as.factor(as.numeric(CMAQ_Test$PM2.5_Obs > lev))

levels(CMAQ_Train$High_Low)<- c("Low", "High")
levels(CMAQ_Test$High_Low)<- c("Low", "High")

CMAQ_class_train<- CMAQ_Train[,-which(names(CMAQ_Train) %in% c("PM2.5_Obs"))]
CMAQ_class_test<- CMAQ_Test[,-which(names(CMAQ_Test) %in% c("PM2.5_Obs"))]

CMAQ_class_model<- readRDS("~/Models/Classify_all-years_CMAQ.rds")

CMAQ_test_preds <- data.frame(predict(CMAQ_class_model, CMAQ_class_test))[,1]

CMAQ_pred_high<- CMAQ_Test[which(CMAQ_test_preds == "High"),]
CMAQ_pred_low<- CMAQ_Test[which(CMAQ_test_preds == "Low"),]

#Test on (ensemble) regressions:
CMAQ_Ens_high<- readRDS("~/Models/Ens_GLM_CMAQ_high.rds")
CMAQ_Ens_low<- readRDS("~/Models/Ens_GLM_CMAQ_low.rds")

#Get packages and functions from Ensembles.R
getResults(CMAQ_Ens_high, CMAQ_pred_high)
getResults(CMAQ_Ens_low, CMAQ_pred_low)

### Look at individual year subsets:
subset_Results(Ens_high, 2008:2012, pred_high)
subset_Results(CMAQ_Ens_high, 2008:2012, CMAQ_pred_high)
subset_Results(Ens_low, 2008:2012, pred_low)
subset_Results(CMAQ_Ens_low, 2008:2012, CMAQ_pred_low)

subset_Results(Ens_high, 2013:2016, pred_high)
subset_Results(CMAQ_Ens_high, 2013:2016, CMAQ_pred_high)
subset_Results(Ens_low, 2013:2016, pred_low)
subset_Results(CMAQ_Ens_low, 2013:2016, CMAQ_pred_low)

subset_Results(Ens_high, 2017:2018, high_test)
subset_Results(Ens_low, 2017:2018, low_test)

##Testing subsets of the data:
load("Post-classification_data-sets.RData")

GLM_high<- readRDS("~/Models/Ens_GLM_high.rds")
GLM_low<- readRDS("~/Models/Ens_GLM_low.rds")
CMAQ_GLM_high<- readRDS("~/Models/Ens_GLM_CMAQ_high.rds")
CMAQ_GLM_low<- readRDS("~/Models/Ens_GLM_CMAQ_low.rds")

##Make sure we're testing against the same group as the overall (no-split) model:

#2008-2018, no CMAQ:
high_preds <- data.frame(predict(GLM_high$models, pred_high))
stack_preds_H<- data.frame(predict(GLM_high$ens_model, high_preds))
compare_H<- cbind(stack_preds_H, pred_high$PM2.5_Obs)
names(compare_H)<- c("Preds", "Obs")

low_preds <- data.frame(predict(GLM_low$models, pred_low))
stack_preds_L<- data.frame(predict(GLM_low$ens_model, low_preds))
compare_L<- cbind(stack_preds_L, pred_low$PM2.5_Obs)
names(compare_L)<- c("Preds", "Obs")

compare<- rbind(compare_H, compare_L)

high_compare<- compare[which(compare$Obs >= 15),]
low_compare<- compare[which(compare$Obs < 15),]

print("High:")
print(paste0("RMSE: ", round(sqrt(mean((high_compare[,1]-high_compare[,2])^2)),4), ",",
             " R^2: ", round((cor(high_compare[,1], high_compare[,2]))^2,4)))

print("Low:")
print(paste0("RMSE: ", round(sqrt(mean((low_compare[,1]-low_compare[,2])^2)),4), ",",
             " R^2: ", round((cor(low_compare[,1], low_compare[,2]))^2,4)))


#2008-2016, with CMAQ:
CMAQ_high_preds <- data.frame(predict(CMAQ_GLM_high$models, CMAQ_pred_high))
CMAQ_stack_preds_H<- data.frame(predict(CMAQ_GLM_high$ens_model, CMAQ_high_preds))
CMAQ_compare_H<- cbind(CMAQ_stack_preds_H, CMAQ_pred_high$PM2.5_Obs)
names(CMAQ_compare_H)<- c("Preds", "Obs")

CMAQ_low_preds <- data.frame(predict(CMAQ_GLM_low$models, CMAQ_pred_low))
CMAQ_stack_preds_L<- data.frame(predict(CMAQ_GLM_low$ens_model, CMAQ_low_preds))
CMAQ_compare_L<- cbind(CMAQ_stack_preds_L, CMAQ_pred_low$PM2.5_Obs)
names(CMAQ_compare_L)<- c("Preds", "Obs")

CMAQ_compare<- rbind(CMAQ_compare_H, CMAQ_compare_L)

CMAQ_high_compare<- CMAQ_compare[which(CMAQ_compare$Obs >= 15),]
CMAQ_low_compare<- CMAQ_compare[which(CMAQ_compare$Obs < 15),]

print("High:")
print(paste0("RMSE: ", round(sqrt(mean((CMAQ_high_compare[,1]-CMAQ_high_compare[,2])^2)),4), ",",
             " R^2: ", round((cor(CMAQ_high_compare[,1], CMAQ_high_compare[,2]))^2,4)))

print("Low:")
print(paste0("RMSE: ", round(sqrt(mean((CMAQ_low_compare[,1]-CMAQ_low_compare[,2])^2)),4), ",",
             " R^2: ", round((cor(CMAQ_low_compare[,1], CMAQ_low_compare[,2]))^2,4)))

##Now for subsets like this...
years<- 2008:2016 #change out

#2008-2018, no CMAQ:
high_preds <- data.frame(predict(GLM_high$models, pred_high[which(pred_high$Year %in% years),]))
stack_preds_H<- data.frame(predict(GLM_high$ens_model, high_preds))
compare_H<- cbind(stack_preds_H, pred_high$PM2.5_Obs[which(pred_high$Year %in% years)])
names(compare_H)<- c("Preds", "Obs")

low_preds <- data.frame(predict(GLM_low$models, pred_low[which(pred_low$Year %in% years),]))
stack_preds_L<- data.frame(predict(GLM_low$ens_model, low_preds))
compare_L<- cbind(stack_preds_L, pred_low$PM2.5_Obs[which(pred_low$Year %in% years)])
names(compare_L)<- c("Preds", "Obs")

compare<- rbind(compare_H, compare_L)

high_compare<- compare[which(compare$Obs >= 15),]
low_compare<- compare[which(compare$Obs < 15),]

print("High:")
print(paste0("RMSE: ", round(sqrt(mean((high_compare[,1]-high_compare[,2])^2)),4), ",",
             " R^2: ", round((cor(high_compare[,1], high_compare[,2]))^2,4)))

print("Low:")
print(paste0("RMSE: ", round(sqrt(mean((low_compare[,1]-low_compare[,2])^2)),4), ",",
             " R^2: ", round((cor(low_compare[,1], low_compare[,2]))^2,4)))


#2008-2016, with CMAQ:
CMAQ_high_preds <- data.frame(predict(CMAQ_GLM_high$models, CMAQ_pred_high[which(CMAQ_pred_high$Year %in% years),]))
CMAQ_stack_preds_H<- data.frame(predict(CMAQ_GLM_high$ens_model, CMAQ_high_preds))
CMAQ_compare_H<- cbind(CMAQ_stack_preds_H, CMAQ_pred_high$PM2.5_Obs[which(CMAQ_pred_high$Year %in% years)])
names(CMAQ_compare_H)<- c("Preds", "Obs")

CMAQ_low_preds <- data.frame(predict(CMAQ_GLM_low$models, CMAQ_pred_low[which(CMAQ_pred_low$Year %in% years),]))
CMAQ_stack_preds_L<- data.frame(predict(CMAQ_GLM_low$ens_model, CMAQ_low_preds))
CMAQ_compare_L<- cbind(CMAQ_stack_preds_L, CMAQ_pred_low$PM2.5_Obs[which(CMAQ_pred_low$Year %in% years)])
names(CMAQ_compare_L)<- c("Preds", "Obs")

CMAQ_compare<- rbind(CMAQ_compare_H, CMAQ_compare_L)

CMAQ_high_compare<- CMAQ_compare[which(CMAQ_compare$Obs >= 15),]
CMAQ_low_compare<- CMAQ_compare[which(CMAQ_compare$Obs < 15),]

print("High:")
print(paste0("RMSE: ", round(sqrt(mean((CMAQ_high_compare[,1]-CMAQ_high_compare[,2])^2)),4), ",",
             " R^2: ", round((cor(CMAQ_high_compare[,1], CMAQ_high_compare[,2]))^2,4)))

print("Low:")
print(paste0("RMSE: ", round(sqrt(mean((CMAQ_low_compare[,1]-CMAQ_low_compare[,2])^2)),4), ",",
             " R^2: ", round((cor(CMAQ_low_compare[,1], CMAQ_low_compare[,2]))^2,4)))

#Overall:
years<- 2013:2016

#2008-2018, no CMAQ:
high_preds <- data.frame(predict(GLM_high$models, pred_high[which(pred_high$Year %in% years),]))
stack_preds_H<- data.frame(predict(GLM_high$ens_model, high_preds))
compare_H<- cbind(stack_preds_H, pred_high$PM2.5_Obs[which(pred_high$Year %in% years)])
names(compare_H)<- c("Preds", "Obs")

low_preds <- data.frame(predict(GLM_low$models, pred_low[which(pred_low$Year %in% years),]))
stack_preds_L<- data.frame(predict(GLM_low$ens_model, low_preds))
compare_L<- cbind(stack_preds_L, pred_low$PM2.5_Obs[which(pred_low$Year %in% years)])
names(compare_L)<- c("Preds", "Obs")

compare<- rbind(compare_H, compare_L)

print(paste0("RMSE: ", round(sqrt(mean((compare[,1]-compare[,2])^2)),4), ",",
             " R^2: ", round((cor(compare[,1], compare[,2]))^2,4)))

#2008-2016, with CMAQ:
CMAQ_high_preds <- data.frame(predict(CMAQ_GLM_high$models, CMAQ_pred_high[which(CMAQ_pred_high$Year %in% years),]))
CMAQ_stack_preds_H<- data.frame(predict(CMAQ_GLM_high$ens_model, CMAQ_high_preds))
CMAQ_compare_H<- cbind(CMAQ_stack_preds_H, CMAQ_pred_high$PM2.5_Obs[which(CMAQ_pred_high$Year %in% years)])
names(CMAQ_compare_H)<- c("Preds", "Obs")

CMAQ_low_preds <- data.frame(predict(CMAQ_GLM_low$models, CMAQ_pred_low[which(CMAQ_pred_low$Year %in% years),]))
CMAQ_stack_preds_L<- data.frame(predict(CMAQ_GLM_low$ens_model, CMAQ_low_preds))
CMAQ_compare_L<- cbind(CMAQ_stack_preds_L, CMAQ_pred_low$PM2.5_Obs[which(CMAQ_pred_low$Year %in% years)])
names(CMAQ_compare_L)<- c("Preds", "Obs")

CMAQ_compare<- rbind(CMAQ_compare_H, CMAQ_compare_L)

print(paste0("RMSE: ", round(sqrt(mean((CMAQ_compare[,1]-CMAQ_compare[,2])^2)),4), ",",
             " R^2: ", round((cor(CMAQ_compare[,1], CMAQ_compare[,2]))^2,4)))
