# process PM2.5 data step 1: combine the various PM2.5 data sources
 
print("run Define_directories.R before this script") 

# start timer for code
start_code_timer <- proc.time()
print(paste("Start ML_PM25_estimation_step1.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
library(measurements)
library(caret) 
library(caretEnsemble)
#library(randomForest) 
library(polspline)
library(foreign)
#library(tcltk) # not sure if it loaded ok
#library(ResearchMethods) 
library(reshape) 
library(kernlab) 
library(foreach) 
library(earth) 
library(gbm) 
library(e1071) # needed for treebagFuncs
library(glmnet) 
library(ranger)

#### Call Load Functions that I created ####
source(file.path(ML_Code.directory,"ML_PM25_estimation_parallal_wrapper_function.R"))
source(file.path(ML_Code.directory,"ML_processing_functions.R"))
ML_processing_fn_list <- c("ML_run_report.fn", "ML_plot_model.fn")
source(file.path(ML_Code.directory,"Plotting_and_LaTex_functions.R"))
Plotting_and_LaTex_fn_list <- c("Plot_and_latex.fn", "LaTex_code_4_figure.fn", "LaTex_code_start_subsection.fn")
#input_mat_functions <- c("input_mat_change_data_classes.fn", "input_mat_extract_year_from_date.fn",
#                         "input_mat_extract_month_from_date.fn", "input_mat_extract_day_from_date.fn",
#                         "fancy_which.fn", "subset_data_frame_via_vector.fn", "EPA_codes_2_components_no_hyphens.fn")
#state_functions <- c("State_Abbrev_Definitions.fn","StateCode2StateName.fn","fill_in_StateNames_from_Code.fn")
# create vector with directories that will be needed in parallel functions
directories_vector <- c("ProcessedData.directory", "output.directory", "output.directory.short")

#### define constants and variables needed for all R workers ####
n_task_sets <- 2#2 # change to higher number as more code is written
set_seed <- 42 # set seed for reproducible results
validation_method <- "cv"
n_fold_validation <- 10
#model_quality_metric <- #"ROC"
#start_study_year <- 2008
#stop_study_year <- 2014
study_states_abbrev <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")

# Out-of-Sample error - fit on one data set and then predict on new data -> train/test split
# error metric should be computed on new data
# in-sample validation almost guarentees overfitting - don't overfit
#n_repeats <- 5 # do n_repeats of the 10-fold cross-validation - couldn't get this to work

# prepare data - get rid of extra variables not used for fitting and shuffle the rows
# see DataCamp for information about median imputation for missing data
this_source_file <- "AllforCaret_cleaned_StepPractice_2018-10-15_part_practice.csv"
Full_PM25_obs<-read.csv(file.path(ProcessedData.directory,this_source_file),header=TRUE) # load the AQS file
predictor_variables <- c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75) # predictor variables from Colleen's work
col_PM25_obs <- which(names(Full_PM25_obs)== "Monitor_PM25")
#PM25_obs_w_predictors_no_extra_col <- Full_PM25_obs[ ,c(which_PM25,predictor_variables)] #"Monitor_PM25")]#[ ,c("Monitor_PM25",predictor_variables)]
#rows <- sample(nrow(PM25_obs_w_predictors_no_extra_col)) # shuffle the row indices
#PM25_obs_shuffled <- PM25_obs_w_predictors_no_extra_col[rows, ] # shuffle the data set using the shuffled row indices
rows <- sample(nrow(Full_PM25_obs)) # shuffle the row indices
PM25_obs_shuffled <- Full_PM25_obs[rows, ] # shuffle the data set using the shuffled row indices
rm(Full_PM25_obs) # clear variable

# create report with plots/maps about the input data, consider removing any columns that have nearly constant values
print("create report with plots/maps about the input data, consider removing any columns that have nearly constant values")

# For running multiple models on the same input data, it is important to use the same training/test splits - create a shared trainControl object
# Create train/test indexes
set.seed(set_seed) #set.seed(42) # set seed on random number generator so that results are reproducible
myFolds <- createFolds(PM25_obs_shuffled$Monitor_PM25, k = n_fold_validation) # not quite sure what this does

# Compare class distribution (put this into generated report)
#i <- myFolds$Fold01 # not quite sure what this does
#table(PM25_obs_shuffled$Monitor_PM25[i]) / length(i) # not quite sure what this does

# set the control for the model to be trained
this_trainControl <- trainControl( # specify control parameters for train
  method = validation_method, number = n_fold_validation, # specify 10-fold cross-validation # repeats = 5, # do n_repeats of the 10-fold cross-validation
  verboseIter = TRUE, # display progress as model is running
  savePredictions = TRUE, # part of using same train/test splits across multiple models
  index = myFolds # use the same cross-validation folds for each model
) # trControl = trainControl( # specify training control

# set tuneLength, which tells caret how many variations to try (default is 3, and 10 is very fine tune parameter)
# could using custom tuning grid - this requires a lot of knowledge of the algorithm - see DataCamp module
this_tuneLength <- 5 

#### Run the parallel loop ####
n_cores <- detectCores() - 1 # Calculate the number of cores
print(paste(n_cores,"cores available for parallel processing",sep = " "))

# Initiate cluster
this_cluster <- makeCluster(n_cores)

# export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
clusterExport(cl = this_cluster, varlist = c("processed_data_version",directories_vector,"PM25_obs_shuffled","this_trainControl",
                                             "this_tuneLength","set_seed","this_source_file","validation_method","n_fold_validation",
                                             "col_PM25_obs","predictor_variables",
                                             Plotting_and_LaTex_fn_list, ML_processing_fn_list), envir = .GlobalEnv)

# send necessary libraries to each parallel worker
#clusterEvalQ(cl = this_cluster, library(rNOMADS)) # copy this line and call function again if another library is needed
clusterEvalQ(cl = this_cluster, library(caret)) # copy this line and call function again if another library is needed

# run function loop_NAM_run_times.parallel.fn in parallel
# X = 1:n_data_sets
par_output <- parLapply(this_cluster, X = 1:n_task_sets, fun = ML_PM25_estimation_parallal_wrapper.fn)#,
                       # input_header = input_header, ProcessedData.directory = ProcessedData.directory,
                        #AQSData.directory = AQSData.directory, FireCache.directory = FireCache.directory,
                        #UintahData.directory = UintahData.directory)

# End use of parallel computing #
stopCluster(this_cluster)
rm(this_cluster, n_cores)

#### Compare models that were run - generate as report, move to new function and call it here ###
# selection criteria for best fit of the data:
#1: highest average AUC
#2: lowest standard deviation in AUC
# use resamples function
# par_out is a list containing all of the model runs that were done
resamps <- resamples(par_output) # collect resamples from the CV folds
resamps

# re-name the models to be something useful
#new_name_step1 <- cbind(resamps$methods, 1:n_task_sets)
#new_name_step2 <- new_name_step1[ ,1]
#for (i in 1:dim(new_name_step1)[1]) { 
#  new_name_step2[i] <- paste(new_name_step1[i, ],collapse = "")
#}
#print(new_name_step2)
#resamps$models <- new_name_step2
summary(resamps) # summarize the results

# Box and Whisker Plots, all metrics plotted together
bwplot(resamps)

# Box and Whisker Plots, each metric plotted separately
bwplot(resamps, metric = "RMSE")

bwplot(resamps, metric = "Rsquared")

bwplot(resamps, metric = "MAE")

# Dot plots - shows simplified version of box and whisker plots, better when comparing many models
dotplot(resamps, metric = "RMSE")

dotplot(resamps, metric = "Rsquared")

dotplot(resamps, metric = "MAE")

# Density plot
densityplot(resamps, metric = "RMSE")

densityplot(resamps, metric = "Rsquared")

densityplot(resamps, metric = "MAE")

# Scatter plot
xyplot(resamps, metric = "RMSE")

xyplot(resamps, metric = "Rsquared")

xyplot(resamps, metric = "MAE")

#### Ensembling models ####
# create ensemble model: this_ensemble
#this_ensemble <- caretStack(par_output, method = "glm") # didn't work
#summary(this_ensemble) # look at summary # didn't work

#### Clear variables ####
#rm(n_data_sets, start_study_year, stop_study_year, voltage_threshold_upper, voltage_threshold_lower, input_header)
#rm(par_output, input_mat1)

#### End of file cleanup
#rm(uppermost.directory,output.directory)
#rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
#rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory) 
#rm(writingcode.directory,computer_system,NAM.directory,PythonProcessedData.directory)

print(paste("ML_PM25_estimation_step1.R completed at",Sys.time(),sep = " "))
# stop the timer
proc.time() - start_code_timer
rm(start_code_timer)
