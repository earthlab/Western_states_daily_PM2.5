source("~/Scripts/Data_prep.R")
source("~/Scripts/Base_ML_functions.R")

classify<- function(train){
  
  set.seed(321)
  IND<- createResample(train$High_Low, 10)
  
  myControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = "grid",
                            savePredictions = "final", index = IND,
                            verboseIter = FALSE, allowParallel = FALSE, classProbs = TRUE)
  start<- Sys.time()
  
  ranger_model <- train(High_Low ~ ., data = train, method = "ranger",
                        trControl = myControl, num.trees = 150,
                        importance = "permutation")
  
  
  end<- Sys.time()
  # PID<- Sys.getpid()
  # Mem_peak<- system(paste0('grep VmPeak /proc/', PID, '/status'), intern = TRUE)
  # 
  # print(Mem_peak)
  print(end-start)
  print(ranger_model$results)
  
  return(ranger_model)
}

#Use this function...
lev<- 15
Train<- rbind(high_train, low_train)
Train$High_Low<- as.factor(as.numeric(Train$PM2.5_Obs > lev))
Test<- rbind(high_test, low_test)
Test$High_Low<- as.factor(as.numeric(Test$PM2.5_Obs > lev))

levels(Train$High_Low)<- c("Low", "High")
levels(Test$High_Low)<- c("Low", "High")

class_train<- Train[,-which(names(Train) %in% c("PM2.5_Obs"))]
class_test<- Test[,-which(names(Test) %in% c("PM2.5_Obs"))]

class_model<- classify(class_train)
saveRDS(class_model, "~/Models/Classify_all-years.rds")

sink("~/Results/Classify_all-years.txt")
print("Training:")
confusionMatrix(class_model$pred[,"pred"], class_model$pred[,"obs"])
print("Testing:")
test_preds <- data.frame(predict(class_model, class_test))
confusionMatrix(test_preds[,1], class_test$High_Low)
sink()

#2008-2016 with CMAQ:
Train<- rbind(CMAQ_high_train, CMAQ_low_train)
Train$High_Low<- as.factor(as.numeric(Train$PM2.5_Obs > lev))
Test<- rbind(CMAQ_high_test, CMAQ_low_test)
Test$High_Low<- as.factor(as.numeric(Test$PM2.5_Obs > lev))

levels(Train$High_Low)<- c("Low", "High")
levels(Test$High_Low)<- c("Low", "High")

class_train<- Train[,-which(names(Train) %in% c("PM2.5_Obs"))]
class_test<- Test[,-which(names(Test) %in% c("PM2.5_Obs"))]

class_model<- classify(class_train)
saveRDS(class_model, "~/Models/Classify_all-years_CMAQ.rds")

sink("~/Results/Classify_all-years_CMAQ.txt")
print("Training:")
confusionMatrix(class_model$pred[,"pred"], class_model$pred[,"obs"])
print("Testing:")
test_preds <- data.frame(predict(class_model, class_test))
confusionMatrix(test_preds[,1], class_test$High_Low)
sink()
