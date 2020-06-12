# Load the required libraries
library(caret)
library(caretEnsemble)
library(dplyr) #for sample_n
library(parallel)
library(doParallel)

#Read in the data
Fire_2010<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Fire_2010.csv") #55840, 67
NotFire_2010<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Not-Fire_2010.csv") #45495, 35
Fire_2017<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Fire_2017.csv") #66434, 66
NotFire_2017<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Not-Fire_2017.csv") #58854, 34

Subsets<- list(Fire_2010, NotFire_2010, Fire_2017, NotFire_2017)

i<- 1

#Log results
sink("C:\\Users\\elco2649\\Documents\\Machine Learning\\Get_tuning_parameters_and_compare_algs_PARALLEL.txt")

for(SS in Subsets){
  print(paste("Subset =", i))
  i <- i+1
  for(seed in c(321, 654, 987)){
    #Parallelization: https://topepo.github.io/caret/parallel-processing.html
      #Note: I put the parallelization here so that the models aren't written to the sink file out-of-order
    cluster <- makeCluster(detectCores() - 2)
    registerDoParallel(cluster)
    
    print(paste("Seed =", seed))
    #Take sample
    set.seed(seed)
    dataset<- sample_n(SS, 5000)
    #Create train-test split
    n<- round(dim(dataset)[1]*0.1)
    test_pos<- sample(1:(dim(dataset)[1]),n, replace = FALSE)
    test<- dataset[test_pos,]
    train<- dataset[-test_pos,]
    #Set up control object
    myControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid", 
                              savePredictions = "final", index = createResample(train$PM2.5_Obs, 10),
                              verboseIter = FALSE, allowParallel = TRUE)
    #MACHINE LEARNING
    
    #Random Forest
    ranger_start<- Sys.time()
    ranger_grid<- expand.grid( .mtry = seq(5,length(names(train))-1,5), .splitrule = "variance", .min.node.size = seq(5,9,2) )
    ranger_model <- train(PM2.5_Obs ~ ., data = train, method = "ranger",
                          trControl = myControl, tuneGrid = ranger_grid, num.trees = 100,
                          importance = "permutation")
    print(ranger_model$results)
    print(ranger_model$bestTune)
    ranger_end<- Sys.time()
    print(paste("ranger:", ranger_end - ranger_start, "minutes"))
    
    
    other_algs<- c("gbm", "gaussprRadial", "xgbTree", "bagEarth", "glmnet")
    
    for(A in other_algs){
      model_start<- Sys.time()
      model<- train(PM2.5_Obs ~ ., data = train, method = A, trControl = myControl)
      print(model$results)
      print(model$bestTune)
      model_end<- Sys.time()
      print(paste0(A, ": ", model_end - model_start, " minutes"))
    }
    
    stopCluster(cluster)
  }
}

sink()




