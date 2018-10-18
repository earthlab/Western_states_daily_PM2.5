ML_PM25_estimation_parallal_wrapper.fn <- function(task_counter){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  
  if (task_counter == 1) {
    fit_type <- "ranger" # random forest 
    # "ranger" = random forest: # "ranger package is a rewrite of R's classic randomForest package that fits models much faster, 
    # but gives almost exactly the same results" - DataCamp
    # random forests have hyperparameters that are selected by hand before model is fit, 
    # mtry = most important hyperparameter = # randomly selected variables 
    # used at each split; lower = more random
    # grid search - caret can select hyperparameters based on out-of-sample error
    
    file_sub_label <- paste("ML_report_","_task_",task_counter,fit_type,sep = "") # file partial name, decide whether to include date in file name
    SinkFileName=file.path(ProcessedData.directory,paste(file_sub_label,".txt",sep = "")) # file name
    sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # start output to text file
    set.seed(set_seed) #set.seed(42) # set seed on random number generator so that results are reproducible
    # set fitting method

    #"lm" # linear regression model
    #"glm" # a more advanced verion of "lm"
    
    # set tuneLength, which tells caret how many variations to try (default is 3, and 10 is very fine tune parameter)
      # could using custom tuning grid - this requires a lot of knowledge of the algorithm - see DataCamp module
    
    # train the model
    this_model <- train( # start function for training model
      x = PM25_obs_shuffled[ ,predictor_variables], y = PM25_obs_shuffled[ ,col_PM25_obs],#Monitor_PM25 ~ ., # train to predict Monitor_PM25 using all of the other variables in the data set
      tuneLength = this_tuneLength, # tuneLength = tells caret how manhy different variations to try
      method = fit_type, # lm = linear model
      trControl = this_trainControl
      ) # this_model <- train( # start function for training model
    #data = PM25_obs_shuffled, # train for the prediction of Monitor_PM25 with the data PM25_obs_shuffled
    
    # plot model
    plot(this_model)
    
    # make predictions with the data
    PM25_prediction <- predict(this_model, PM25_obs_shuffled) # predict on the full data set
    print('change code to make predictions on the locations of interest instead of locations of monitors')
    
    # output report about model run
    ML_run_report.fn(SinkFileName, task_counter,fit_type,this_model,ProcessedData.directory)
    
    this_model
    
  } else if (task_counter == 2) {
    fit_type <- "glmnet"
    #"glmnet" # extention of generalized linear models, helps deal w/ collinearity & small sample sizes, tries to find simple model, 
    # 2 variations of model:
    # Lasso regression: penalizes number of non-zero coefficients
    # Ridge regression: penalizes absolute magnitude of coefficients
    # pairs well w/ randomForest
    # 2 tuning parameters
    # alpha [0, 1]: pure lasso to pure ridge
    # lambda (0, infinity): size of the penalty
    # "for a single value of alpha, all values of lambda fit simultaneously" - DataCamp
    max_lambda <- 1 # could go up to 10
    n_lambdas <- 10 # could also do 100
    this_grid <- expand.grid(alpha = c(0, 0.5, 1), # define tuning grid
                             lambda = seq(0.0001, max_lambda, length = 10))
    
    # train the model
    this_model <- train( # start function for training model
      Monitor_PM25 ~ ., # train to predict Monitor_PM25 using all of the other variables in the data set
      data = PM25_obs_shuffled, # train for the prediction of Monitor_PM25 with the data PM25_obs_shuffled
      tuneGrid = this_grid, # variations of lambda and alpha
      tuneLength = this_tuneLength, # tuneLength = tells caret how manhy different variations to try
      method = fit_type, # lm = linear model
      trControl = this_trainControl
    ) # this_model <- train( # start function for training model
    
    # plot model
    plot(this_model)
    
    # make predictions with the data
    PM25_prediction <- predict(this_model, PM25_obs_shuffled) # predict on the full data set
    print('change code to make predictions on the locations of interest instead of locations of monitors')
    
    print(paste("min RMSE:",min(this_model[["results"]][["RMSE"]])))  # Print maximum ROC statistic
    
    this_model # needs to be last thing in if-statement to get output from parallel processing
    
  } else if (task_counter == 3) {
    
  } else if (task_counter == 4) {
      
  } else if (task_counter == 5) {
    
  } else if (task_counter == 6) {
    
    
  } else if (task_counter == 7) {
    
  } else if (task_counter == 8) {
    
  } else if (task_counter == 9) {
     
  }# if (task_counter == 1) {
} # end function
