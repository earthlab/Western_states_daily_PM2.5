# process PM2.5 data step 1: combine the various PM2.5 data sources
 
print("run Define_directories.R before this script") 

# start timer for code
start_code_timer <- proc.time()
print(paste("Start ML_PM25_estimation_step1.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
library(measurements)
library(randomForest) 
library(polspline)
library(foreign)
#library(tcltk) # not sure if it loaded ok
#library(ResearchMethods) 
library(reshape) 
library(kernlab) 
library(foreach) 
library(caret) # not sure if it loaded ok
library(earth) 
library(gbm) 
library(e1071) # needed for treebagFuncs
library(glmnet) 
library(ranger)

#### Call Load Functions that I created ####
source(file.path(ML_Code.directory,"ML_PM25_estimation_parallal_wrapper_function.R"))
#source(file.path(writingcode.directory,"process_PM25_EPA_data_source_function.R"))
#source(file.path(writingcode.directory,"process_PM25_Fire_Cache_data_source_function.R"))
#source(file.path(writingcode.directory,"Fire_Cache_specific_functions.R"))
#source(file.path(writingcode.directory,"separate_character_vec_at_comma_function.R"))
#source(file.path(writingcode.directory,"State_Abbrev_Definitions_function.R"))
#source(file.path(writingcode.directory,"input_mat_functions.R"))
#source(file.path(writingcode.directory,"process_PM25_Lyman_Uintah_Basin_functions.R"))
#source(file.path(writingcode.directory,"process_PM25_PCAPS_data_source_functions.R"))
#source(file.path(writingcode.directory,"process_PM25_IMPROVE_data_source_functions.R"))
#source(file.path(writingcode.directory,"separate_character_vec_at_comma_function.R"))

#Fire_cache_specific_functions <- c("Fire_Cache_consolidate_file_header.fn","Fire_Cache_comprehensive_header.fn",
#                                   "Fire_Cache_remove_repeat_headers.fn", "Fire_Cache_change_data_classes.fn",
#                                   "Fire_Cache_negative_longitudes.fn",
#                                   "Fire_Cache_daily_averages.fn", "Fire_Cache_1_day_1_col_w_flag.fn",
#                                   "Fire_Cache_1_day_ave.fn", "Fire_Cache_1_file_to_small_input_mat.fn")
#input_mat_functions <- c("input_mat_change_data_classes.fn", "input_mat_extract_year_from_date.fn",
#                         "input_mat_extract_month_from_date.fn", "input_mat_extract_day_from_date.fn",
#                         "fancy_which.fn", "subset_data_frame_via_vector.fn", "EPA_codes_2_components_no_hyphens.fn")
#state_functions <- c("State_Abbrev_Definitions.fn","StateCode2StateName.fn","fill_in_StateNames_from_Code.fn")
#Uintah_basin_functions <- c("process_PM25_Lyman_Uintah_data_source.fn", "fill_in_UB_stations_input_mat.fn")
#PCAPS_functions <- c("process_PM25_PCAPS_data_source.fn", "PCAPS_gather_lat_lon.fn")
#IMPROVE_functions <- c("process_PM25_IMPROVE_data_source.fn", "fill_in_FMLE_code_components.fn")
# create vector with directories that will be needed in parallel functions
directories_vector <- c("ProcessedData.directory")

#### define constants and variables needed for all R workers ####
n_task_sets <- 2 # change to higher number as more code is written
#start_study_year <- 2008
#stop_study_year <- 2014
processed_data_version <- "a"
study_states_abbrev <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")

#input_header <-  c('PM2.5_Obs','PM2.5_Lat','PM2.5_Lon','Datum','Date_Local','Year','Month','Day','State_Code','County_Code',
#                   'Site_Num','Parameter_Code','POC','Parameter_Name','Sample_Duration','Pollutant_Standard','Units_of_Measure',
#                   'Event_Type','Observation_Count','Observation_Percent','1st_Max_Value','1st_Max_Hour','AQI','Method_Code',
#                   'Method_Name','PM25_Station_Name','Address','State_Name','County_Name','City_Name','CBSA_Name',
#                   'Date_of_Last_Change', # columns in AQS data
#                   'State_Abbrev','Winter','Data_Source_Name_Display','Data_Source_Name_Short','Data_Source_Counter',
#                   'Source_File','Composite_of_N_rows','N_Negative_Obs', # other columns to include
#                   "flg.Lat","flg.Lon","Type","flg.Type","flg.Site_Num","flg.PM25_Obs","l/m Ave. Air Flw", # DRI variables
#                   "flg.AirFlw","Deg C Av Air Temp","flg.AirTemp","% Rel Humidty","flg.RelHumid","mbar Barom Press","flg.Barom Press", # DRI variables
#                   "deg C Sensor  Int AT","flg.deg C Sensor Int AT","% Sensor Int RH","flg.%SensorIntRH", # DRI variables
#                   "Wind Speed m/s","flg.WindSpeed","Battery Voltage volts","flg.BatteryVoltage","Alarm","flg.Alarm", # DRI variables
#                   "InDayLatDiff","InDayLonDiff","PlottingColor","SerialNumber")

# Out-of-Sample error - fit on one data set and then predict on new data -> train/test split
# error metric should be computed on new data
# in-sample validation almost guarentees overfitting - don't overfit
#n_repeats <- 5 # do n_repeats of the 10-fold cross-validation - couldn't get this to work

# prepare data - get rid of extra variables not used for fitting and shuffle the rows
# see DataCamp for information about median imputation for missing data
this_source_file <- "AllforCaret_cleaned_StepPractice_2018-10-15_part_practice.csv"
Full_PM25_obs<-read.csv(file.path(ProcessedData.directory,this_source_file),header=TRUE) # load the AQS file
predictor_variables <- c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75) # predictor variables from Colleen's work
which_PM25 <- which(names(Full_PM25_obs)== "Monitor_PM25")
PM25_obs_w_predictors_no_extra_col <- Full_PM25_obs[ ,c(which_PM25,predictor_variables)] #"Monitor_PM25")]#[ ,c("Monitor_PM25",predictor_variables)]
rows <- sample(nrow(PM25_obs_w_predictors_no_extra_col)) # shuffle the row indices
PM25_obs_shuffled <- PM25_obs_w_predictors_no_extra_col[rows, ] # shuffle the data set using the shuffled row indices
rm(this_source_file, Full_PM25_obs, predictor_variables, which_PM25, PM25_obs_w_predictors_no_extra_col)

# set the control for the model to be trained
this_trainControl <- trainControl( # specify control parameters for train
  method = "cv", number = 10, # specify 10-fold cross-validation # repeats = 5, # do n_repeats of the 10-fold cross-validation
  verboseIter = TRUE # display progress as model is running
) # trControl = trainControl( # specify training control

this_tuneLength <- 5 

#### Run the parallel loop ####
n_cores <- detectCores() - 1 # Calculate the number of cores
print(paste(n_cores,"cores available for parallel processing",sep = " "))

# Initiate cluster
this_cluster <- makeCluster(n_cores)

# export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
clusterExport(cl = this_cluster, varlist = c("processed_data_version",directories_vector,"PM25_obs_shuffled","this_trainControl",
                                             "this_tuneLength"), envir = .GlobalEnv)
                #c("start_study_year","stop_study_year","voltage_threshold_upper","voltage_threshold_lower","input_header",
                #                             ,"study_states_abbrev",
                #                             
                #                             "process_PM25_EPA_data_source.fn","separate_character_vec_at_comma.fn",state_functions,
                #                             "process_PM25_Fire_Cache_data_source.fn", Fire_cache_specific_functions, input_mat_functions,
                #                             Uintah_basin_functions, PCAPS_functions, IMPROVE_functions, "separate_character_vec_at_comma.fn")

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

#### concatinate the output from each iteration ####

# first data set
#input_mat1 <- par_output[[1]]
# subsequent data sets  

#input_mat1 <- do.call("rbind", par_output)

#### Save input_mat1 to csv file ####
#file_sub_label <- paste("PM25_Step1_",Sys.Date(),"_part_",processed_data_version,"_Sources_Merged",sep = "")
#write.csv(input_mat1,file = file.path(ProcessedData.directory,paste(file_sub_label,'.csv',sep = "")),row.names = FALSE)

#### Clear variables ####
rm(n_data_sets, start_study_year, stop_study_year, voltage_threshold_upper, voltage_threshold_lower, input_header)
rm(par_output, input_mat1)

#### End of file cleanup
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory) 
rm(writingcode.directory,computer_system,NAM.directory,PythonProcessedData.directory)

print(paste("ML_PM25_estimation_step1.R completed at",Sys.time(),sep = " "))
# stop the timer
proc.time() - start_code_timer
rm(start_code_timer)
