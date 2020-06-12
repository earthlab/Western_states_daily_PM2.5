## Get functions from Ensembles.R

source("~/Scripts/Data_prep.R")
train<- rbind(high_train, low_train)
test<- rbind(high_test, low_test)

ranger_model<- readRDS("~/Models/All_years_ranger.rds")
xgbt_model<- readRDS("~/Models/All_years_xgbt.rds")

my_list<- list(ranger_model, xgbt_model)
class(my_list)<- "caretList"

ranger_control<- trainControl(method = "repeatedcv", number = 5, repeats = 1,
                           savePredictions = TRUE, verboseIter = TRUE)

# tgrid<- expand.grid( .mtry = 2, .splitrule = "variance", .min.node.size = 50)

sink("~/Results/Ens_Ranger_no-split.txt")
my_Ranger<- caretStack(my_list, method = "ranger", metric = "RMSE", trControl = ranger_control,
                    tuneLength = 1)
print(getResults(my_Ranger, test))
saveRDS(my_Ranger, "~/Models/Ens_Ranger_no-split.rds")
sink()

rm(list=c("my_Ranger", "ranger_model", "xgbt_model"))
gc()

#With CMAQ:

CMAQ_ranger_model<- readRDS("~/Models/All_years_CMAQ_ranger.rds")
CMAQ_xgbt_model<- readRDS("~/Models/All_years_CMAQ_xgbt.rds")

CMAQ_my_list<- list(CMAQ_ranger_model, CMAQ_xgbt_model)
class(CMAQ_my_list)<- "caretList"

sink("~/Results/Ens_Ranger_CMAQ_no-split.txt")
CMAQ_my_Ranger<- caretStack(CMAQ_my_list, method = "ranger", metric = "RMSE", trControl = ranger_control,
                            tuneLength = 1)
print(getResults(CMAQ_my_Ranger, CMAQ_test))
saveRDS(CMAQ_my_GLM, "~/Models/Ens_Ranger_CMAQ_no-split.rds")
sink()

