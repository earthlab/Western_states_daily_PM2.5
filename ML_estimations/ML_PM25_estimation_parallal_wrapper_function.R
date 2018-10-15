process_PM25_parallal_wrapper.fn <- function(data_set_counter){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  
  if (data_set_counter == 1) {
    # exersizes from DataCamp
    predictor_variables <- c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75) # predictor variables from Colleen's work
    which_PM25 <- which(names(Full_PM25_obs)== "Monitor_PM25")
    PM25_obs_w_predictors_no_extra_col <- Full_PM25_obs[ ,c(which_PM25,predictor_variables)] #"Monitor_PM25")]#[ ,c("Monitor_PM25",predictor_variables)]
    #this_model <- lm(Monitor_PM25 ~ ., PM25_obs_w_predictors_no_extra_col) # linear regression - fit a model to PM2.5 data
    #PM25_prediction <- predict(this_model, PM25_obs_w_predictors_no_extra_col) # predict on full data (in-sample)
    #this_error <- PM25_prediction - PM25_obs_w_predictors_no_extra_col$Monitor_PM25 # compute error
    #this_RMSE <- sqrt(mean((PM25_prediction - PM25_obs_w_predictors_no_extra_col$Monitor_PM25)^2)) # calculate RMSE
      
    # Out-of-Sample error - fit on one data set and then predict on new data -> train/test split
    # error metric should be computed on new data
    # in-sample validation almost guarentees overfitting - don't overfit
    #createResamples() or createFolds() can be used to subset the data
    training_fraction <- 0.80 # will use 80% of data for training and 20% for testing
    set.seed(42) # set seed on random number generator so that results are reproducible
    rows <- sample(nrow(PM25_obs_w_predictors_no_extra_col)) # shuffle the row indices
    PM25_obs_shuffled <- PM25_obs_w_predictors_no_extra_col[rows, ] # shuffle the data set using the shuffled row indices
    this_split_row <- round(nrow(PM25_obs_shuffled) * training_fraction)  # determine which row to split between training and test data sets
    this_training_set <- PM25_obs_shuffled[1:this_split_row, ] # create data frame with only the training data
    this_test_set <- PM25_obs_shuffled[(this_split_row + 1):nrow(PM25_obs_shuffled), ] # create data frame with only the test data 
    this_model <- lm(Monitor_PM25 ~ ., this_training_set) # linear regression - fit a model to PM2.5 data
    PM25_prediction <- predict(this_model, this_test_set) # predict on test data (out-of-sample)
    this_error <- PM25_prediction - this_test_set$Monitor_PM25 # compute error
    this_RMSE <- sqrt(mean((PM25_prediction - PM25_obs_w_predictors_no_extra_col$Monitor_PM25)^2)) # calculate RMSE
    
    
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
