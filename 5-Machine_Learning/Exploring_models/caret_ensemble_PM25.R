# Code reference: https://rpubs.com/zxs107020/370699

# Load the required libraries
library(caret)
library(caretEnsemble)
library(dplyr) #for sample_n


#Read in the data and create train-test split
Fire_2010<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Fire_2010.csv")
NotFire_2010<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Not-Fire_2010.csv")
Fire_2017<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Fire_2017.csv")
NotFire_2017<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Not-Fire_2017.csv")

DATA<- Fire_2010

set.seed(321)
dataset<- sample_n(DATA, 50000)

n<- round(dim(dataset)[1]*0.1)
test_pos<- sample(1:(dim(dataset)[1]),n, replace = FALSE)
test<- dataset[test_pos,]
train<- dataset[-test_pos,]

#Set up control objects
myControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid", 
                        savePredictions = "final", index = createResample(train$PM2.5_Obs, 10),
                        verboseIter = TRUE)

stackControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                             savePredictions = TRUE, verboseIter = TRUE)
                            #For random forest: could use less reps, and specify splitrule? 

#Machine Learning functions...

R2<- function(pred, obs, formula = "corr", na.rm = FALSE) {
  n <- sum(complete.cases(pred))
  switch(formula,
         corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
         traditional = 1 - (sum((obs-pred)^2, na.rm = na.rm)/((n-1)*var(obs, na.rm = na.rm))))
}

VARIMP_rf<- function(mtry, min.node.size){
  tgrid<- expand.grid( .mtry = mtry, .splitrule = "variance", .min.node.size = min.node.size )
  ranger_model <- train(PM2.5_Obs ~ ., data = train, method = "ranger",
                        trControl = myControl, tuneGrid = tgrid, num.trees = 100,
                        importance = "permutation")
  print(ranger_model$results)
  VI_ordering<- order(varImp(ranger_model)$importance$Overall, decreasing = TRUE)
  print(cbind(row.names(varImp(ranger_model)$importance)[VI_ordering], varImp(ranger_model)$importance[VI_ordering,]))
}

#Before running this function, need to create train-test split, design and run a caretList
getResults<- function(model){
  print("Training: ")
  print(model$error)
  stack_test_preds <- data.frame(predict(model$models, test[,-(which(names(test)=="PM2.5_Obs"))]))
  compare<- cbind(stack_test_preds, test$PM2.5_Obs)
  print(paste("Testing set R^2 =", round(R2(pred = compare[,1], obs = compare[,2]), digits = 4)))
  print(paste("Testing set RMSE =", round(sqrt(mean((compare[,1] - compare[,2])^2)), digits = 4)))
}

#Create the list object, then run the ensemble

#This used the 5,000 sample 
LIST0<- caretList(PM2.5_Obs ~ ., data = train, trControl = myControl,
                  tuneList = list(
                    RF = caretModelSpec(method = "ranger",
                                        tuneGrid = expand.grid( .mtry = 3:6,
                                                                .splitrule = "variance",
                                                                .min.node.size = seq(5,9,2) )),
                    GBM = caretModelSpec(method = "gbm", 
                                         tuneGrid = expand.grid( .n.trees = seq(50,200,50),
                                                                 .interaction.depth = 3:6,
                                                                 .shrinkage = seq(0.15, 0.35, 0.05),
                                                                 .n.minobsinnode = seq(2,10,2)))
                  ), metric = "RMSE", continue_on_fail = TRUE)


VARIMP_rf(LIST0$RF$bestTune$mtry, LIST0$RF$bestTune$min.node.size)

GLM0<- caretStack(LIST0, method = "glm", metric = "RMSE", trControl = stackControl)
getResults(GLM0)

RF0<- caretStack(LIST0, method = "ranger", metric = "RMSE", trControl = stackControl)
getResults(RF0)

#Save Work:
saveRDS(LIST0, "C:\\Users\\elco2649\\Documents\\Machine Learning\\LIST0.rds")
saveRDS(GLM0, "C:\\Users\\elco2649\\Documents\\Machine Learning\\GLM0.rds")
saveRDS(RF0, "C:\\Users\\elco2649\\Documents\\Machine Learning\\RF0.rds")

#This used the 50,000 sample -- was taking way too long on this machine with all tuning parameter combos
LIST1<- caretList(PM2.5_Obs ~ ., data = train, trControl = myControl,
                  tuneList = list(
                    RF = caretModelSpec(method = "ranger",
                                        tuneGrid = expand.grid( .mtry = 6,
                                                                .splitrule = "variance",
                                                                .min.node.size = 5 )),
                    GBM = caretModelSpec(method = "gbm", 
                                         tuneGrid = expand.grid( .n.trees = 100,
                                                                 .interaction.depth = 6,
                                                                 .shrinkage = 0.15,
                                                                 .n.minobsinnode = 10))
                  ), metric = "RMSE", continue_on_fail = TRUE)


VARIMP_rf(LIST1$RF$bestTune$mtry, LIST1$RF$bestTune$min.node.size)

GLM1<- caretStack(LIST1, method = "glm", metric = "RMSE", trControl = stackControl)
getResults(GLM1)

RF1<- caretStack(LIST1, method = "ranger", metric = "RMSE", trControl = stackControl)
getResults(RF1)

#Save Work:
saveRDS(LIST1, "C:\\Users\\elco2649\\Documents\\Machine Learning\\LIST1.rds")
saveRDS(GLM1, "C:\\Users\\elco2649\\Documents\\Machine Learning\\GLM1.rds")
saveRDS(RF1, "C:\\Users\\elco2649\\Documents\\Machine Learning\\RF1.rds")
