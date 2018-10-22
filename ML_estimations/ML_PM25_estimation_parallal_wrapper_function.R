ML_PM25_estimation_parallal_wrapper.fn <- function(task_counter){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  
  if (task_counter == 1) {
    ## What type of fit will be done?
    fit_type <- "ranger" # random forest 
    # "ranger" = random forest: # "ranger package is a rewrite of R's classic randomForest package that fits models much faster, 
    # but gives almost exactly the same results" - DataCamp, also uses less memory
    # random forests have hyperparameters that are selected by hand before model is fit, 
    # mtry = most important hyperparameter = # randomly selected variables 
    # used at each split; lower = more random
    
    # random forest - no need to normalize the predictors; capture threshold effects and variable interactions by default
    
    ## set up documentation files/variables
    #sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE) # initialize file
    file_sub_label <- paste("ML_report_task_",task_counter,fit_type,sep = "") # file partial name, decide whether to include date in file name
    title_string <- paste(fit_type,task_counter) # used in figure titles, etc
    LatexFileName=file.path(output.directory,paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
    LaTex_code_start_subsection.fn(LatexFileName, title_string, append_option = FALSE) # start subsection for latex code
    SinkFileName=file.path(ProcessedData.directory,paste(file_sub_label,".txt",sep = "")) # file name
    sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # start output to text file
    
    #set.seed(set_seed) #set.seed(42) # set seed on random number generator so that results are reproducible
    # set fitting method

    # train the model
    # tree-based methods generally need very little pre-processing (maybe imputation) - see ?preProcess for more detail and "Multiple preprocessing methods" video in chapter for of "Machine 
      # Learning Toolbox" course on DataCamp
    this_model <- train( # start function for training model
      x = PM25_obs_shuffled[ ,predictor_variables], y = PM25_obs_shuffled[ ,col_PM25_obs],#Monitor_PM25 ~ ., # train to predict Monitor_PM25 using all of the other variables in the data set
      tuneLength = this_tuneLength, # tuneLength = tells caret how manhy different variations to try
      method = fit_type, # lm = linear model
      trControl = this_trainControl
      ) # this_model <- train( # start function for training model
    #       metric = model_quality_metric
    #data = PM25_obs_shuffled, # train for the prediction of Monitor_PM25 with the data PM25_obs_shuffled
    
    # plot model #plot(this_model)
    ML_plot_model.fn(file_sub_label, this_model, SinkFileName, LatexFileName, title_string)
    
    # make predictions with the data
    PM25_prediction <- predict(this_model, PM25_obs_shuffled) # predict on the full data set
    print('change code to make predictions on the locations of interest instead of locations of monitors')
    
    # output report about model run
    ML_run_report.fn(SinkFileName, task_counter,fit_type,this_model,ProcessedData.directory)
    
    this_model # needs to be last thing in if-statement to get output from parallel processing
    
  } else if (task_counter == 2) {
    #"lm" # linear regression model
    #"glm" # a more advanced verion of "lm"
    fit_type <- "glmnet"
    #"glmnet" # extention of generalized linear models, helps deal w/ collinearity & small sample sizes, tries to find simple model, 
    # 2 variations of model:
    # Lasso regression: penalizes number of non-zero coefficients
    # Ridge regression: penalizes absolute magnitude of coefficients
    # pairs well w/ randomForest
    # see chapter 5 of datacamps ML course for more detail about glmnet
    # 2 tuning parameters
    # alpha [0, 1]: pure lasso to pure ridge
    # lambda (0, infinity): size of the penalty
    # "for a single value of alpha, all values of lambda fit simultaneously" - DataCamp
    max_lambda <- 1 # could go up to 10
    n_lambdas <- 10 # could also do 100
    # grid search - caret can select hyperparameters based on out-of-sample error
    this_grid <- expand.grid(alpha = c(0, 0.5, 1), # define tuning grid
                             lambda = seq(0.0001, max_lambda, length = 10))
    
    # from DataCamp: "Common 'recipe' for linear models (order matters!)'
    # imputation -> center -> scale -> fit glm
    # Principle components analysis happens after center/scale
    # See ?preProcess for more detail and "Multiple preprocessing methods" video in chapter for of "Machine 
        # Learning Toolbox" course on DataCamp
    
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
