# ML_PM25_estimation_step0.R - merge the various predictor variables together with the monitor data 
  # or dates/locations of interest

  print("run Define_directories.R before this script") 

# start timer for code
start_code_timer <- proc.time()
print(paste("Start ML_PM25_estimation_step0.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
library(plyr)

# set rounding digits
#options(digits=4)
#> y <- as.character("0.912345678")

#### Call Load Functions that I created ####
source(file.path(ML_Code.directory,"ML_merge_predictors_parallal_wrapper_function.R"))
source(file.path(ML_Code.directory,"ML_processing_functions.R"))
ML_processing_fn_list <- c("ML_input_report.fn", "ML_run_report.fn", "ML_plot_model.fn", "compare_multiple_models.fn")
source(file.path(ML_Code.directory,"Plotting_and_LaTex_functions.R"))
Plotting_and_LaTex_fn_list <- c("Plot_to_ImageFile.fn", "Plot_and_latex.fn", "LaTex_code_4_figure.fn", "LaTex_code_start_subsection.fn")
source(file.path(writingcode.directory,"State_Abbrev_Definitions_function.R"))
source(file.path(writingcode.directory,"input_mat_functions.R"))
input_mat_functions <- c("input_mat_change_data_classes.fn", "input_mat_extract_year_from_date.fn",
                         "input_mat_extract_month_from_date.fn", "input_mat_extract_day_from_date.fn",
                         "fancy_which.fn", "subset_data_frame_via_vector.fn", "EPA_codes_2_components_no_hyphens.fn")
#state_functions <- c("State_Abbrev_Definitions.fn","StateCode2StateName.fn","fill_in_StateNames_from_Code.fn")
# create vector with directories that will be needed in parallel functions
directories_vector <- c("ProcessedData.directory", "output.directory", "output.directory.short", "USMaps.directory")

#### define constants and variables needed for all R workers ####
#n_task_sets <- 2 # change to higher number as more code is written
#set_seed <- 42 # set seed for reproducible results
#set_seed <- 272 # same as Colleen's
GASP_file_name <- "GASP_extracted_part_c.csv"
Highways_file_name <- "Highways_part_c.csv"
MAIAC_file_name <- "MAIAC_extracted_part_c.csv"
NAM_file_name <- "NAM_Step3_part_bc.csv"
NED_file_name <- "ned_extract.csv"
NLCD_file_name <- "nlcd_extract.csv"
predictor_sub_folder <- "PredictorVariablesExtractedToDatesLocations"


#### Run the parallel loop ####
n_cores <- detectCores() - 1 # Calculate the number of cores
print(paste(n_cores,"cores available for parallel processing",sep = " "))

# Initiate cluster
this_cluster <- makeCluster(n_cores)

# export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
clusterExport(cl = this_cluster, varlist = c("processed_data_version",directories_vector,"PM25_obs_shuffled","this_trainControl",
                                             "this_tuneLength","set_seed","this_source_file","validation_method","n_fold_validation",
                                             "col_name_interest","predictor_variables",
                                             Plotting_and_LaTex_fn_list, ML_processing_fn_list), envir = .GlobalEnv)
# ,"col_PM25_obs"
# send necessary libraries to each parallel worker
#clusterEvalQ(cl = this_cluster, library(rNOMADS)) # copy this line and call function again if another library is needed
clusterEvalQ(cl = this_cluster, library(caret)) # copy this line and call function again if another library is needed

# run function loop_NAM_run_times.parallel.fn in parallel
# X = 1:n_data_sets
par_output <- parLapply(this_cluster, X = 1:n_task_sets, fun = ML_merge_predictors_parallal_wrapper.fn)#,
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

compare_multiple_models.fn(par_output = par_output)

#### Save par_output ####


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

print(paste("ML_PM25_estimation_step0.R completed at",Sys.time(),sep = " "))
# stop the timer
proc.time() - start_code_timer
rm(start_code_timer)
