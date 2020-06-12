library(caret)
library(caretEnsemble)

source("~/Scripts/Data_prep.R")
rm(list=setdiff(ls(), c("Both", "CMAQ_Both"))) 

### Individual models
my_control<- trainControl(method = "none", savePredictions = "final")

## Non-CMAQ:

#Ranger:
sink("~/Results/Full_model_ranger.txt_with-VI")
tgrid<- expand.grid( .mtry = 15, .splitrule = "variance", .min.node.size = 6 )
start<- Sys.time()
ranger_model <- train(PM2.5_Obs ~ ., data = Both, method = "ranger",
                      tuneGrid = tgrid, num.trees = 150, trControl = my_control,
                      importance = "permutation")
end<- Sys.time()
print(end-start)
print(ranger_model$results)
VI<- varImp(ranger_model)
VI_df<- data.frame(Variable=row.names(VI$importance)[order(VI$importance, decreasing = TRUE)],
  Importance=round(VI$importance[order(VI$importance, decreasing = TRUE),],3))
write.csv(VI_df, "~/Results/VarImp_full_ranger.csv", row.names=FALSE)
saveRDS(ranger_model, "~/Models/Full_ranger_with-VI.rds")

sink()

# #XGBT:
# sink("~/Results/Full_model_xgbt.txt")
# tgrid<- expand.grid( .nrounds = 150, .max_depth = 3, .eta = 0.3,
#                      .gamma = 0, .colsample_bytree = 2/3, .min_child_weight = 1,
#                      .subsample = 1)
# start<- Sys.time()
# xgbt_model <- train(PM2.5_Obs ~ ., data = Both, method = "xgbTree",
#                     tuneGrid = tgrid, trControl = my_control)
# end<- Sys.time()
# print(end-start)
# print(xgbt_model$results)
# saveRDS(xgbt_model, "~/Models/Full_xgbt.rds")
# 
# sink()

## CMAQ:

#Ranger:
sink("~/Results/Full_CMAQ_model_ranger_with-VI.txt")
tgrid<- expand.grid( .mtry = 15, .splitrule = "variance", .min.node.size = 6 )
start<- Sys.time()
CMAQ_ranger_model <- train(PM2.5_Obs ~ ., data = CMAQ_Both, method = "ranger",
                      tuneGrid = tgrid, num.trees = 150, trControl = my_control,
                      importance = "permutation")
end<- Sys.time()
print(end-start)
print(CMAQ_ranger_model$results)
CMAQ_VI<- varImp(CMAQ_ranger_model)
CMAQ_VI_df<- data.frame(Variable=row.names(CMAQ_VI$importance)[order(CMAQ_VI$importance, decreasing = TRUE)],
                   Importance=round(CMAQ_VI$importance[order(CMAQ_VI$importance, decreasing = TRUE),],3))
write.csv(CMAQ_VI_df, "~/Results/VarImp_CMAQ_full_ranger.csv", row.names=FALSE)
saveRDS(CMAQ_ranger_model, "~/Models/Full_CMAQ_ranger_with-VI.rds")

sink()

# #XGBT:
# sink("~/Results/Full_CMAQ_model_xgbt.txt")
# tgrid<- expand.grid( .nrounds = 150, .max_depth = 3, .eta = 0.3,
#                      .gamma = 0, .colsample_bytree = 2/3, .min_child_weight = 1,
#                      .subsample = 1)
# start<- Sys.time()
# CMAQ_xgbt_model <- train(PM2.5_Obs ~ ., data = CMAQ_Both, method = "xgbTree",
#                     tuneGrid = tgrid, trControl = my_control)
# end<- Sys.time()
# print(end-start)
# print(CMAQ_xgbt_model$results)
# saveRDS(CMAQ_xgbt_model, "~/Models/Full_CMAQ_xgbt.rds")
# 
# sink()

### ENSEMBLES

## Non-CMAQ

ranger_model<- readRDS("~/Models/Full_ranger.rds")
xgbt_model<- readRDS("~/Models/Full_xgbt.rds")
Ranger<- predict.train(ranger_model, Both)
XGBT<- predict.train(xgbt_model, Both)
NC_training<- data.frame(Obs = Both$PM2.5_Obs, Ranger, XGBT)

sink("~/Results/Full_Ens.txt")
start<- Sys.time()
GLM<- train(Obs ~ Ranger + XGBT, data = NC_training, method = "glm")
end<- Sys.time()
print(end - start)
print(GLM$results)
sink()

NC_training$Preds<- predict.train(GLM, Both)
png(filename = "FVO_Full.png")
plot(NC_training$Obs, NC_training$Preds, xlab = "Observed", ylab = "Predicted",
     main = "Non-CMAQ Full Training Set", xlim = c(0,900), ylim = c(0,600))
abline(a=0, b=1)
dev.off()

# write.csv(NC_training, "~/Results/Full_Preds.csv", row.names = FALSE)
# 
# saveRDS(GLM, "~/Models/Full_Ens.rds")

rm(list=setdiff(ls(), "CMAQ_Both")) 
gc()

## CMAQ
CMAQ_ranger_model<- readRDS("~/Models/Full_CMAQ_ranger.rds")
CMAQ_xgbt_model<- readRDS("~/Models/Full_CMAQ_xgbt.rds")
CMAQ_Ranger<- predict.train(CMAQ_ranger_model, CMAQ_Both)
CMAQ_XGBT<- predict.train(CMAQ_xgbt_model, CMAQ_Both)
C_training<- data.frame(Obs = CMAQ_Both$PM2.5_Obs, CMAQ_Ranger, CMAQ_XGBT)

sink("~/Results/Full_CMAQ_Ens.txt")
start<- Sys.time()
CMAQ_GLM<- train(Obs ~ CMAQ_Ranger + CMAQ_XGBT, data = C_training, method = "glm")
end<- Sys.time()
print(end - start)
print(CMAQ_GLM$results)
sink()

C_training$Preds<- predict.train(CMAQ_GLM, CMAQ_Both)
png(filename = "FVO_CMAQ_Full.png")
plot(C_training$Obs, C_training$Preds, xlab = "Observed", ylab = "Predicted",
     main = "CMAQ Full Training Set", xlim = c(0,900), ylim = c(-10,600))
abline(a=0, b=1)
dev.off()

# write.csv(C_training, "~/Results/Full_CMAQ_Preds.csv", row.names = FALSE)
# 
# saveRDS(CMAQ_GLM, "~/Models/Full_CMAQ_Ens.rds")


