source("~/Scripts/Data_prep.R")
source("~/Scripts/Base_ML_functions.R")

##2008-2018 no CMAQ:

train<- rbind(high_train, low_train)
test<- rbind(high_test, low_test)

sink("~/Results/All_years_individual.txt")

print("Ranger:")
ranger_model<- run_ranger(train)
saveRDS(ranger_model, "~/Models/All_years_ranger.rds")
print(ML_to_CSV(ranger_model, test))

print("XGBT:")
xgbt_model<- run_xgbt(train)
saveRDS(xgbt_model, "~/Models/All_years_xgbt.rds")
print(ML_to_CSV(xgbt_model, test))

sink()


##2008-2016 with CMAQ:

sink("~/Results/All_years_CMAQ_individual.txt")

print("Ranger:")
CMAQ_ranger_model<- run_ranger(CMAQ_train)
saveRDS(CMAQ_ranger_model, "~/Models/All_years_CMAQ_ranger.rds")
print(ML_to_CSV(CMAQ_ranger_model, CMAQ_test))

print("XGBT:")
CMAQ_xgbt_model<- run_xgbt(CMAQ_train)
saveRDS(CMAQ_xgbt_model, "~/Models/All_years_CMAQ_xgbt.rds")
print(ML_to_CSV(CMAQ_xgbt_model, CMAQ_test))

sink()

##Ensemble: get functions from Ensembles.R
ranger_model<- readRDS("~/Models/All_years_ranger.rds")
xgbt_model<- readRDS("~/Models/All_years_xgbt.rds")

my_list<- list(ranger_model, xgbt_model)
class(my_list)<- "caretList"

sink("~/Results/Ens_GLM_no-split.txt")
my_GLM<- caretStack(my_list, method = "glm", metric = "RMSE", trControl = stackControl)
print(getResults(my_GLM, test))
saveRDS(my_GLM, "~/Models/Ens_GLM_no-split.rds")
sink()

#With CMAQ:

CMAQ_ranger_model<- readRDS("~/Models/All_years_CMAQ_ranger.rds")
CMAQ_xgbt_model<- readRDS("~/Models/All_years_CMAQ_xgbt.rds")

CMAQ_my_list<- list(CMAQ_ranger_model, CMAQ_xgbt_model)
class(CMAQ_my_list)<- "caretList"

sink("~/Results/Ens_GLM_CMAQ_no-split.txt")
CMAQ_my_GLM<- caretStack(CMAQ_my_list, method = "glm", metric = "RMSE", trControl = stackControl)
print(getResults(CMAQ_my_GLM, CMAQ_test))
saveRDS(CMAQ_my_GLM, "~/Models/Ens_GLM_CMAQ_no-split.rds")
sink()

##Read in models again:
my_GLM<- readRDS("~/Models/Ens_GLM_no-split.rds")
CMAQ_my_GLM<- readRDS("~/Models/Ens_GLM_CMAQ_no-split.rds")

##Evaluate by high-low:
getResults(my_GLM, high_test)
getResults(my_GLM, low_test)

getResults(CMAQ_my_GLM, CMAQ_high_test)
getResults(CMAQ_my_GLM, CMAQ_low_test)

##Evaluate by high-low and by time period:
subset_Results(my_GLM, 2008:2012, high_test)
subset_Results(my_GLM, 2008:2012, low_test)

subset_Results(my_GLM, 2013:2016, high_test)
subset_Results(my_GLM, 2013:2016, low_test)

subset_Results(my_GLM, 2017:2018, high_test)
subset_Results(my_GLM, 2017:2018, low_test)

##CMAQ subsets:
subset_Results(CMAQ_my_GLM, 2008:2012, CMAQ_high_test)
subset_Results(CMAQ_my_GLM, 2008:2012, CMAQ_low_test)

subset_Results(CMAQ_my_GLM, 2013:2016, CMAQ_high_test)
subset_Results(CMAQ_my_GLM, 2013:2016, CMAQ_low_test)

##Calculate overall results:

my_GLM<- readRDS("~/Models/Ens_GLM_no-split.rds")
subset_Results(my_GLM, 2008:2018, rbind(high_test, low_test))

subset_Results(my_GLM, 2008:2016, rbind(high_test, low_test))

subset_Results(my_GLM, 2008:2016, high_test)
subset_Results(my_GLM, 2008:2016, low_test)
subset_Results(my_GLM, 2008:2012, rbind(high_test, low_test))
subset_Results(my_GLM, 2013:2016, rbind(high_test, low_test))
subset_Results(my_GLM, 2017:2018, rbind(high_test, low_test))


CMAQ_my_GLM<- readRDS("~/Models/Ens_GLM_CMAQ_no-split.rds")
subset_Results(CMAQ_my_GLM, 2008:2016, CMAQ_test)


##Make FVO plots:
my_GLM<- readRDS("~/Models/Ens_GLM_no-split.rds")

train_list_preds<- data.frame(predict(my_GLM$models, train))
train_stack_preds<- data.frame(predict(my_GLM$ens_model, train_list_preds))

train_DF<- data.frame(Preds = train_stack_preds[,1], Obs = train$PM2.5_Obs)
write.csv(train_DF, "~/Results/All_train_preds.csv", row.names = FALSE)

plot(train$PM2.5_Obs, train_stack_preds[,1], xlab = "Observed", ylab = "Predicted",
     main = "2008-2018, no CMAQ: Training Set", ylim = c(0,600))
lines(1:max(train$PM2.5_Obs), 1:max(train$PM2.5_Obs))

test_list_preds<- data.frame(predict(my_GLM$models, test))
test_stack_preds<- data.frame(predict(my_GLM$ens_model, test_list_preds))

test_DF<- data.frame(Preds = test_stack_preds[,1], Obs = test$PM2.5_Obs)
write.csv(test_DF, "~/Results/All_test_preds.csv", row.names = FALSE)

plot(test$PM2.5_Obs, test_stack_preds[,1], xlab = "Observed", ylab = "Predicted",
     main = "2008-2018, no CMAQ: Testing Set", ylim = c(0,600))
lines(1:max(train$PM2.5_Obs), 1:max(train$PM2.5_Obs))

plot(test$PM2.5_Obs, test_stack_preds[,1], xlab = "Observed", ylab = "Predicted",
     main = "2008-2018, no CMAQ: Truncated Testing Set", ylim = c(0,200), xlim = c(0,200))
lines(1:max(train$PM2.5_Obs), 1:max(train$PM2.5_Obs))

##With CMAQ:
CMAQ_my_GLM<- readRDS("~/Models/Ens_GLM_CMAQ_no-split.rds")

CMAQ_train_list_preds<- data.frame(predict(CMAQ_my_GLM$models, CMAQ_train))
CMAQ_train_stack_preds<- data.frame(predict(CMAQ_my_GLM$ens_model, CMAQ_train_list_preds))

CMAQ_train_DF<- data.frame(Preds = CMAQ_train_stack_preds[,1], Obs = CMAQ_train$PM2.5_Obs)
write.csv(CMAQ_train_DF, "~/Results/CMAQ_train_preds.csv", row.names = FALSE)

plot(CMAQ_train$PM2.5_Obs, CMAQ_train_stack_preds[,1], xlab = "Observed", ylab = "Predicted",
     main = "2008-2016, with CMAQ: Training Set", ylim = c(0,600))
lines(1:max(CMAQ_train$PM2.5_Obs), 1:max(CMAQ_train$PM2.5_Obs))

CMAQ_test_list_preds<- data.frame(predict(CMAQ_my_GLM$models, CMAQ_test))
CMAQ_test_stack_preds<- data.frame(predict(CMAQ_my_GLM$ens_model, CMAQ_test_list_preds))

CMAQ_test_DF<- data.frame(Preds = CMAQ_test_stack_preds[,1], Obs = CMAQ_test$PM2.5_Obs)
write.csv(CMAQ_test_DF, "~/Results/CMAQ_test_preds.csv", row.names = FALSE)

plot(CMAQ_test$PM2.5_Obs, CMAQ_test_stack_preds[,1], xlab = "Observed", ylab = "Predicted",
     main = "2008-2016, with CMAQ: Testing Set", ylim = c(0,600), xlim = c(0,800))
lines(1:max(CMAQ_train$PM2.5_Obs), 1:max(CMAQ_train$PM2.5_Obs))

plot(CMAQ_test$PM2.5_Obs, CMAQ_test_stack_preds[,1], xlab = "Observed", ylab = "Predicted",
     main = "2008-2016, with CMAQ: Truncated Testing Set", ylim = c(0,200), xlim = c(0,200))
lines(1:max(CMAQ_train$PM2.5_Obs), 1:max(CMAQ_train$PM2.5_Obs))

#Distributions:
dists<- cbind(quantile(summary(DATA$PM2.5_Obs[which(DATA$Binary_fire)]), probs = seq(0,1,.1)),
      quantile(summary(DATA$PM2.5_Obs[which(!DATA$Binary_fire)]), probs = seq(0,1,.1)))
dists<- as.data.frame(dists)
names(dists)<- c("Fire", "Not Fire")
dists
