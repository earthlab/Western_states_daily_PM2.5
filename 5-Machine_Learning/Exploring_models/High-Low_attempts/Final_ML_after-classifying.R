source("~/Scripts/Data_prep.R")
source("~/Scripts/Base_ML_functions.R")

##2008-2018, no CMAQ:

Train<- rbind(high_train, low_train)
Train$High_Low<- as.factor(as.numeric(Train$PM2.5_Obs > lev))
Test<- rbind(high_test, low_test)
Test$High_Low<- as.factor(as.numeric(Test$PM2.5_Obs > lev))

levels(Train$High_Low)<- c("Low", "High")
levels(Test$High_Low)<- c("Low", "High")

class_model<- readRDS("~/Models/Classify_all-years.rds")

train_preds <- data.frame(predict(class_model, Train))[,1]

pred_high_tr<- Train[which(train_preds == "High"), -which(names(Train) == "High_Low")]
pred_low_tr<- Train[which(train_preds == "Low"), -which(names(Train) == "High_Low")]

test_preds <- data.frame(predict(class_model, Test))[,1]

pred_high<- Test[which(test_preds == "High"),]
pred_low<- Test[which(test_preds == "Low"),]

## With CMAQ:
CMAQ_Train<- rbind(CMAQ_high_train, CMAQ_low_train)
CMAQ_Train$High_Low<- as.factor(as.numeric(CMAQ_Train$PM2.5_Obs > lev))
CMAQ_Test<- rbind(CMAQ_high_test, CMAQ_low_test)
CMAQ_Test$High_Low<- as.factor(as.numeric(CMAQ_Test$PM2.5_Obs > lev))

levels(CMAQ_Train$High_Low)<- c("Low", "High")
levels(CMAQ_Test$High_Low)<- c("Low", "High")

CMAQ_class_model<- readRDS("~/Models/Classify_all-years_CMAQ.rds")

CMAQ_train_preds <- data.frame(predict(CMAQ_class_model, CMAQ_Train))[,1]

CMAQ_pred_high_tr<- CMAQ_Train[which(CMAQ_train_preds == "High"),-which(names(CMAQ_Train) == "High_Low")]
CMAQ_pred_low_tr<- CMAQ_Train[which(CMAQ_train_preds == "Low"),-which(names(CMAQ_Train) == "High_Low")]

CMAQ_test_preds <- data.frame(predict(CMAQ_class_model, CMAQ_Test))[,1]

CMAQ_pred_high<- CMAQ_Test[which(CMAQ_test_preds == "High"),]
CMAQ_pred_low<- CMAQ_Test[which(CMAQ_test_preds == "Low"),]

rm(list=setdiff(ls(), c("pred_high", "pred_low", "CMAQ_pred_high", "CMAQ_pred_low")))
save.image("Post-classification_data-sets.RData")

#Run regressions:
sink("~/Results/All_years_individual_HL.txt")

print("High:")
print("Ranger:")
high_ranger<- run_ranger(pred_high_tr)
saveRDS(high_ranger, "~/Models/All_years_H_ranger.rds")
print(ML_to_CSV(high_ranger, pred_high))
print("XGBT:")
high_xgbt<- run_xgbt(pred_high_tr)
saveRDS(high_xgbt, "~/Models/All_years_H_xgbt.rds")
print(ML_to_CSV(high_xgbt, pred_high))

print("Low:")
print("Ranger:")
low_ranger<- run_ranger(pred_low_tr)
saveRDS(low_ranger, "~/Models/All_years_L_ranger.rds")
print(ML_to_CSV(low_ranger, pred_low))
print("XGBT:")
low_xgbt<- run_xgbt(pred_low_tr)
saveRDS(low_xgbt, "~/Models/All_years_L_xgbt.rds")
print(ML_to_CSV(low_xgbt, pred_low))

sink()


sink("~/Results/CMAQ_years_individual_HL.txt")

print("High:")
print("Ranger:")
CMAQ_high_ranger<- run_ranger(CMAQ_pred_high_tr)
saveRDS(CMAQ_high_ranger, "~/Models/CMAQ_years_H_ranger.rds")
print(ML_to_CSV(CMAQ_high_ranger, CMAQ_pred_high))
print("XGBT:")
CMAQ_high_xgbt<- run_xgbt(CMAQ_pred_high_tr)
saveRDS(CMAQ_high_xgbt, "~/Models/CMAQ_years_H_xgbt.rds")
print(ML_to_CSV(CMAQ_high_xgbt, CMAQ_pred_high))

print("Low:")
print("Ranger:")
CMAQ_low_ranger<- run_ranger(CMAQ_pred_low_tr)
saveRDS(CMAQ_low_ranger, "~/Models/CMAQ_years_L_ranger.rds")
print(ML_to_CSV(CMAQ_low_ranger, CMAQ_pred_low))
print("XGBT:")
CMAQ_low_xgbt<- run_xgbt(CMAQ_pred_low_tr)
saveRDS(CMAQ_low_xgbt, "~/Models/CMAQ_years_L_xgbt.rds")
print(ML_to_CSV(CMAQ_low_xgbt, CMAQ_pred_low))

sink()


###ENSEMBLES: get functions from Ensembles.R

##2008-2018 no CMAQ:
high_ranger<- readRDS("~/Models/All_years_H_ranger.rds")
low_ranger<- readRDS("~/Models/All_years_L_ranger.rds")
high_xgbt<- readRDS("~/Models/All_years_H_xgbt.rds")
low_xgbt<- readRDS("~/Models/All_years_L_xgbt.rds")

high_list<- list(high_ranger, high_xgbt)
class(high_list)<- "caretList"
low_list<- list(low_ranger, low_xgbt)
class(low_list)<- "caretList"

sink("~/Results/Ens_GLM_HL.txt")
GLM_high<- caretStack(high_list, method = "glm", metric = "RMSE", trControl = stackControl)
print(getResults(GLM_high, pred_high))
saveRDS(GLM_high, "~/Models/Ens_GLM_H.rds")

GLM_low<- caretStack(low_list, method = "glm", metric = "RMSE", trControl = stackControl)
print(getResults(GLM_low, pred_low))
saveRDS(GLM_low, "~/Models/Ens_GLM_L.rds")
sink()


##2008-2016 with CMAQ:
CMAQ_high_ranger<- readRDS("~/Models/CMAQ_years_H_ranger.rds")
CMAQ_low_ranger<- readRDS("~/Models/CMAQ_years_L_ranger.rds")
CMAQ_high_xgbt<- readRDS("~/Models/CMAQ_years_H_xgbt.rds")
CMAQ_low_xgbt<- readRDS("~/Models/CMAQ_years_L_xgbt.rds")

CMAQ_high_list<- list(CMAQ_high_ranger, CMAQ_high_xgbt)
class(CMAQ_high_list)<- "caretList"
CMAQ_low_list<- list(CMAQ_low_ranger, CMAQ_low_xgbt)
class(CMAQ_low_list)<- "caretList"

sink("~/Results/Ens_GLM_CMAQ_HL.txt")
CMAQ_GLM_high<- caretStack(CMAQ_high_list, method = "glm", metric = "RMSE", trControl = stackControl)
print(getResults(CMAQ_GLM_high, CMAQ_pred_high))
saveRDS(CMAQ_GLM_high, "~/Models/Ens_GLM_CMAQ_H.rds")

sink("~/Results/Ens_GLM_CMAQ_L2.txt")
CMAQ_GLM_low<- caretStack(CMAQ_low_list, method = "glm", metric = "RMSE", trControl = stackControl)
print(getResults(CMAQ_GLM_low, CMAQ_pred_low))
saveRDS(CMAQ_GLM_low, "~/Models/Ens_GLM_CMAQ_L.rds")
sink()


##Testing subsets of the data:
load("Post-classification_data-sets.RData")

GLM_high<- readRDS("~/Models/Ens_GLM_H.rds")
GLM_low<- readRDS("~/Models/Ens_GLM_L.rds")
CMAQ_GLM_high<- readRDS("~/Models/Ens_GLM_CMAQ_H.rds")
CMAQ_GLM_low<- readRDS("~/Models/Ens_GLM_CMAQ_L.rds")

subset_Results(GLM_high, 2008:2012, pred_high)
subset_Results(CMAQ_GLM_high, 2008:2012, CMAQ_pred_high)
subset_Results(GLM_low, 2008:2012, pred_low)
subset_Results(CMAQ_GLM_low, 2008:2012, CMAQ_pred_low)

subset_Results(GLM_high, 2013:2016, pred_high)
subset_Results(CMAQ_GLM_high, 2013:2016, CMAQ_pred_high)
subset_Results(GLM_low, 2013:2016, pred_low)
subset_Results(CMAQ_GLM_low, 2013:2016, CMAQ_pred_low)

subset_Results(GLM_high, 2017:2018, pred_high)
subset_Results(GLM_low, 2017:2018, pred_low)

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
