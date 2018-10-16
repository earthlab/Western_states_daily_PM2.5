process_PM25_parallal_wrapper.fn <- function(data_set_counter){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  
  if (data_set_counter == 1) {
    # Out-of-Sample error - fit on one data set and then predict on new data -> train/test split
    # error metric should be computed on new data
    # in-sample validation almost guarentees overfitting - don't overfit
    set.seed(42) # set seed on random number generator so that results are reproducible
    #n_repeats <- 5 # do n_repeats of the 10-fold cross-validation - couldn't get this to work
    
    # prepare data - get rid of extra variables not used for fitting and shuffle the rows
    predictor_variables <- c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75) # predictor variables from Colleen's work
    which_PM25 <- which(names(Full_PM25_obs)== "Monitor_PM25")
    PM25_obs_w_predictors_no_extra_col <- Full_PM25_obs[ ,c(which_PM25,predictor_variables)] #"Monitor_PM25")]#[ ,c("Monitor_PM25",predictor_variables)]
    rows <- sample(nrow(PM25_obs_w_predictors_no_extra_col)) # shuffle the row indices
    PM25_obs_shuffled <- PM25_obs_w_predictors_no_extra_col[rows, ] # shuffle the data set using the shuffled row indices
    
    # set fitting method
    fit_type <- "ranger" # random forest 
    # "ranger" = random forest: # "ranger package is a rewrite of R's classic randomForest package that fits models much faster, 
      # but gives almost exactly the same results" - DataCamp
      # random forests have hyperparameters that are selected by hand before model is fit, 
      # mtry = most important hyperparameter = # randomly selected variables 
        # used at each split; lower = more random
      # grid search - caret can select hyperparameters based on out-of-sample error
    #"lm" # linear regression model
    #"glm" # a more advanced verion of "lm"
    
    # set the control for the model to be trained
    this_trainControl <- trainControl( # specify control parameters for train
      method = "cv", number = 10, # specify 10-fold cross-validation # repeats = 5, # do n_repeats of the 10-fold cross-validation
      verboseIter = TRUE # display progress as model is running
    ) # trControl = trainControl( # specify training control
    
    # train the model
    this_model <- train( # start function for training model
      Monitor_PM25 ~ ., # train to predict Monitor_PM25 using all of the other variables in the data set
      data = PM25_obs_shuffled, # train for the prediction of Monitor_PM25 with the data PM25_obs_shuffled
      tuneLength = 1, # tuneLength - not sure what that is
      method = fit_type, # lm = linear model
      trControl = this_trainControl
      ) # this_model <- train( # start function for training model
    
    # plot model
    plot(this_model)
    
    # make predictions with the data
    PM25_prediction <- predict(this_model, PM25_obs_shuffled) # predict on the full data set
    print('change code to make predictions on the locations of interest instead of locations of monitors')
    
  } else if (data_set_counter == 2) {
   
    
  } else if (data_set_counter == 3) {
    
  } else if (data_set_counter == 4) {
      
  } else if (data_set_counter == 5) {
    
  } else if (data_set_counter == 6) {
    
    
  } else if (data_set_counter == 7) {
    
  } else if (data_set_counter == 8) {
    
  } else if (data_set_counter == 9) {
     
  }# if (data_set_counter == 1) {
} # end function
