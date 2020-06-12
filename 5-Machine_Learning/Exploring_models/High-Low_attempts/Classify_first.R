source("Base_ML_functions.R")
source("Test_subsets.R")

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
  PID<- Sys.getpid()
  Mem_peak<- system(paste0('grep VmPeak /proc/', PID, '/status'), intern = TRUE)
  
  print(Mem_peak)
  print(end-start)
  print(ranger_model$results)
  
  return(ranger_model)
}


######Test it out:
DATA$High_Low<- as.factor(as.numeric(DATA$PM2.5_Obs > quantile(DATA$PM2.5_Obs, 0.998)))
levels(DATA$High_Low)<- c("Low", "High")

class_data<- DATA[,-which(names(DATA) %in% c("PM2.5_Obs"))]

set.seed(321)
tr_pos<- sample(1:(dim(class_data)[1]),round(dim(class_data)[1]*0.9), replace = FALSE)
class_train<- class_data[tr_pos,]
class_test<- class_data[-tr_pos,]

class_model<- classify(class_train)
save.image("Classifying_first.RData")

confusionMatrix(Df_results[,"pred"], Df_results[,"obs"])

test_preds <- data.frame(predict(class_model, class_test))
confusionMatrix(test_preds[,1], class_test$High_Low)

sum((class_data$Binary_fire == 1)&(class_data$High_Low == "High"))
sum((class_data$Binary_fire == 1)&(class_data$High_Low == "Low"))
sum((class_data$Binary_fire == 0)&(class_data$High_Low == "High"))
sum((class_data$Binary_fire == 0)&(class_data$High_Low == "Low"))

#### Now run regression: (on First_ML.R)

### LATER attempts: 11-12-19
data$High_Low<- as.factor(as.numeric(DATA$PM2.5_Obs > 35)) #22626 observations
# data$High_Low<- as.factor(as.numeric(DATA$PM2.5_Obs > 60)) #5801 observations
levels(data$High_Low)<- c("Low", "High")

class_data<- data[,-which(names(data) %in% c("PM2.5_Obs"))]
set.seed(321)
tr_pos<- sample(1:(dim(class_data)[1]),round(dim(class_data)[1]*0.9), replace = FALSE)
class_train<- class_data[tr_pos,]
class_test<- class_data[-tr_pos,]

class_model<- classify(class_train)
saveRDS(class_model, "Classifying_thresh-35.rds")

test_preds <- data.frame(predict(class_model, class_test))
confusionMatrix(test_preds[,1], class_test$High_Low)

#Training metrics:
confusionMatrix(class_model$pred[,"pred"], class_model$pred[,"obs"])


