# Load the required libraries
library(caret)
library(caretEnsemble)
library(CAST)
library(dplyr) #for sample_n
library(parallel)
library(doParallel)

#Machine Learning functions...

R2<- function(pred, obs, formula = "corr", na.rm = FALSE) {
  n <- sum(complete.cases(pred))
  switch(formula,
         corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
         traditional = 1 - (sum((obs-pred)^2, na.rm = na.rm)/((n-1)*var(obs, na.rm = na.rm))))
}

#Before running this function, need to create train-test split, design and run a caretList
getResults<- function(model, test){
  print("Training: ")
  print(model$error) #change to "error" if for ensemble, "results" if for individual model within ensemble
  stack_test_preds <- data.frame(predict(model, test[,-(which(names(test)=="PM2.5_Obs"))]))
  compare<- cbind(stack_test_preds, test$PM2.5_Obs)
  print(paste("Testing set R^2 =", round(R2(pred = compare[,1], obs = compare[,2]), digits = 4)))
  print(paste("Testing set RMSE =", round(sqrt(mean((compare[,1] - compare[,2])^2)), digits = 4)))
  plot(compare[,2], compare[,1], xlab = "Observed", ylab = "Predicted")
}

#Read in the data
Fire_2010<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Fire_2010.csv") #55840, 67
NotFire_2010<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Not-Fire_2010.csv") #45495, 35
Fire_2017<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Fire_2017.csv") #66434, 66
NotFire_2017<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Not-Fire_2017.csv") #58854, 34

# Subsets<- list(Fire_2010, NotFire_2010, Fire_2017, NotFire_2017)
# 
# Names<- c("Fire_2010", "NotFire_2010", "Fire_2017", "NotFire_2017")

# sink("C:\\Users\\elco2649\\Documents\\Machine Learning\\RF-XGBT_results.txt")

myEnsemble<- function(dataset, Name, splitvar = NULL){
  start<- Sys.time()
  #Set up parallelization
  cluster <- makeCluster(detectCores() - 3)
  registerDoParallel(cluster)
  #Take sample
  set.seed(321)
  
  #Create train-test split and resampling folds
  if(!is.null(splitvar)){
    grouped<- as.numeric(as.factor(interaction(dataset[,splitvar])))
    
    test_locs<- sample(unique(grouped), round(length(unique(grouped))*0.1))
    test<- dataset[which(grouped %in% test_locs),]
    train<- dataset[-which(grouped %in% test_locs),]
  
    IND<- CreateSpacetimeFolds(cbind(train, group = grouped[-which(grouped %in% test_locs)]), spacevar = "group", k = 10)$index #Could also use timevar?
    
  }else{
    n<- round(dim(dataset)[1]*0.1)
    test_pos<- sample(1:(dim(dataset)[1]),n, replace = FALSE)
    test<- dataset[test_pos,]
    train<- dataset[-test_pos,]
    
    IND<- createResample(train$PM2.5_Obs, 10)
  }

  #Set up control objects
  myControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = "grid", 
                            savePredictions = "final", index = IND,
                            verboseIter = FALSE, allowParallel = TRUE)
  stackControl <- trainControl(method = "repeatedcv", number = 10, repeats = 2,
                               savePredictions = TRUE, verboseIter = TRUE, allowParallel = TRUE)

  myList<- caretList(PM2.5_Obs ~ ., data = train, trControl = myControl,
                     tuneList = list(
                       RF = caretModelSpec(method = "ranger",
                                           tuneGrid = expand.grid( .mtry = 15,
                                                                   .splitrule = "variance",
                                                                   .min.node.size = 6 )),
                       XGBT = caretModelSpec(method = "xgbTree",
                                            tuneGrid = expand.grid( .nrounds = 150,
                                                                    .max_depth = 3,
                                                                    .eta = 0.2,
                                                                    .gamma = 0.375,
                                                                    .colsample_bytree = 0.65,
                                                                    .min_child_weight = 3,
                                                                    .subsample = 0.83))),
                     metric = "RMSE", continue_on_fail = TRUE)
  # myList<- caretList(PM2.5_Obs ~ ., data = train, trControl = myControl,
  #                    methodList = c("ranger", "xgbTree"), metric = "RMSE", continue_on_fail = TRUE)

  saveRDS(myList, paste0("C:\\Users\\elco2649\\Documents\\Machine Learning\\RF-XGBT_List_STS_", Name, ".rds"))
  
  
  GLM<- caretStack(myList, method = "glm", metric = "RMSE", trControl = stackControl)
  saveRDS(GLM, paste0("C:\\Users\\elco2649\\Documents\\Machine Learning\\RF-XGBT_GLM-stack_STS_", Name, ".rds"))
  getResults(GLM$models, test)

  # myList<- readRDS(paste0("C:\\Users\\elco2649\\Documents\\Machine Learning\\RF-XGBT_List_", Name, ".rds"))
  # 
  # Ranger<- caretStack(myList, method = "ranger", metric = "RMSE", trControl = stackControl)
  # saveRDS(Ranger, paste0("C:\\Users\\elco2649\\Documents\\Machine Learning\\RF-XGBT_Ranger-stack_", Name, ".rds"))
  # getResults(Ranger$models, test)
  
  stopCluster(cluster)
  
  end<- Sys.time()
  print(end - start)
}


#Run the models!

myEnsemble(Fire_2010, "Fire_2010")
myEnsemble(Fire_2017, "Fire_2017")
myEnsemble(NotFire_2010, "NotFire_2010")
myEnsemble(NotFire_2017, "NotFire_2017")

#Use the models

GLM_F10<- readRDS("C:\\Users\\elco2649\\Documents\\Machine Learning\\RF-XGBT_GLM-stack_Fire_2010.rds")
GLM_F17<- readRDS("C:\\Users\\elco2649\\Documents\\Machine Learning\\RF-XGBT_GLM-stack_Fire_2017.rds")
GLM_NF10<- readRDS("C:\\Users\\elco2649\\Documents\\Machine Learning\\RF-XGBT_GLM-stack_NotFire_2010.rds")
GLM_NF17<- readRDS("C:\\Users\\elco2649\\Documents\\Machine Learning\\RF-XGBT_GLM-stack_NotFire_2017.rds")

set.seed(321)
getResults(GLM_F10$models$RF, Fire_2010[sample(1:(dim(Fire_2010)[1]),round(dim(Fire_2010)[1]*0.1), replace = FALSE),])
getResults(GLM_F10$models$XGBT, Fire_2010[sample(1:(dim(Fire_2010)[1]),round(dim(Fire_2010)[1]*0.1), replace = FALSE),])

set.seed(321)
getResults(GLM_F17$models$RF, Fire_2017[sample(1:(dim(Fire_2017)[1]),round(dim(Fire_2017)[1]*0.1), replace = FALSE),])
getResults(GLM_F17$models$XGBT, Fire_2017[sample(1:(dim(Fire_2017)[1]),round(dim(Fire_2017)[1]*0.1), replace = FALSE),])

set.seed(321)
getResults(GLM_NF10$models$RF, NotFire_2010[sample(1:(dim(NotFire_2010)[1]),round(dim(NotFire_2010)[1]*0.1), replace = FALSE),])
getResults(GLM_NF10$models$XGBT, NotFire_2010[sample(1:(dim(NotFire_2010)[1]),round(dim(NotFire_2010)[1]*0.1), replace = FALSE),])

set.seed(321)
getResults(GLM_NF17$models$RF, NotFire_2017[sample(1:(dim(NotFire_2017)[1]),round(dim(NotFire_2017)[1]*0.1), replace = FALSE),])
getResults(GLM_NF17$models$XGBT, NotFire_2017[sample(1:(dim(NotFire_2017)[1]),round(dim(NotFire_2017)[1]*0.1), replace = FALSE),])


#Run Spatio-Temporal Subset (STS) models
myEnsemble(Fire_2010, "Fire_2010", c("Latitude", "Longitude"))
myEnsemble(Fire_2017, "Fire_2017", c("Latitude", "Longitude"))
myEnsemble(NotFire_2010, "NotFire_2010", c("Latitude", "Longitude"))
myEnsemble(NotFire_2017, "NotFire_2017", c("Latitude", "Longitude"))

