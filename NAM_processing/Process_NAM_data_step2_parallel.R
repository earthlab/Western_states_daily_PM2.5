# Process NAM data using parallel resources

print("run Define_directories.R before this script") 

# start timer for code
start_code_timer <- proc.time()
print(paste("Start Process_NAM_data_step2_parallel.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(rNOMADS)
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

#### Call Load Functions that I created ####
source(file.path(NAM_Code.directory,"NAM_processing_functions.R"))
source(file.path(NAM_Code.directory,"extract_NAM_data_parallel_function.R"))
source(file.path(NAM_Code.directory,"define_project_bounds_function.R"))
source(file.path(NAM_Code.directory,"loop_NAM_run_times.parallel_function.R"))

#source(file.path(writingcode.directory,"grb1to2_conversion_prep_function.R"))
#source(file.path(writingcode.directory,"which_type_of_grib_file_function.R"))
#source(file.path(writingcode.directory,"convert_grib1to2_function.R"))


#### Run function so that grib1>2 conversion will work ####
grb1to2_conversion_prep.fn()

#### define constants ####
study_start_date <- as.Date("20080101",format="%Y%m%d") # first date in study period
#study_start_date <- as.Date("20081124",format="%Y%m%d") # first date in study period
print(study_start_date)
#study_stop_date  <- as.Date("20180830",format="%Y%m%d") # last date in study period
study_stop_date  <- as.Date("20081231",format="%Y%m%d") # last date in study period
print(study_stop_date)
Date_vector <- seq(study_start_date,study_stop_date, by = "day") # vector of all dates for which meteo data will be extracted
n_days <- length(Date_vector)
#day_counter <- 1:n_days
forecast_times <- 00 # reanalysis - anything else would be a forecast
with_pause <- 0 # 1 = pause, 0=no pause

# Select which model to use
Model_in_use_abbrev <-  "namanl" # NAM Analysis
NAM_processed_data_version <- "bc"
sub_folder <- paste("NAM_data_part_",NAM_processed_data_version,sep = "")
output_file_name_sub <- paste("NAM_Step2_part_",NAM_processed_data_version,sep = "")

#### Load list of meteorology variables of interest ####
this_source_file <- paste("MeteoVariablesNAM.csv")
MeteoVarsMultiType <- read.csv(file.path(NAM_Code.directory,this_source_file))
rm(this_source_file)

#### Load Date/Locations of PM2.5 Obs ####
#this_location_date_file <- 'Locations_Dates_of_PM25_Obs_DeDuplicate'
this_location_date_file <- "NAM_Step1_part_bc_Locations_Dates_wNextDay"

#### Load _wNextDay data ####
PM25DateLoc <- read.csv(file.path(ProcessedData.directory,sub_folder,paste(this_location_date_file,".csv",sep = "")))
PM25DateLoc$Date <- as.Date(PM25DateLoc$Date) # recognize date column as dates

#### Run the parallel loop ####
# Calculate the number of cores
n_cores <- 2 #detectCores() - 1
print(paste(n_cores,"available for parallel processing",sep = " "))

# Initiate cluster
this_cluster <- makeCluster(n_cores)

# export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
clusterExport(cl = this_cluster, varlist = c("extract_NAM_data.parallel.fn","which_type_of_grib_file.fn",
                                             "convert_grib1to2.fn","define_project_bounds.fn",
                                             "PM25DateLoc","NAM.directory","sub_folder","uppermost.directory","with_pause"), envir = .GlobalEnv)

# send necessary librarys to each parallel worker
clusterEvalQ(cl = this_cluster, library(rNOMADS)) # copy this line and call function again if another library is needed
clusterEvalQ(cl = this_cluster, library(audio)) # copy this line and call function again if another library is needed


# run function loop_NAM_run_times.parallel.fn in parallel
par_out <- parLapply(this_cluster,X = 1:n_days, fun = loop_NAM_run_times.parallel.fn,
      Date_vector = Date_vector,
      ProcessedData.directory=ProcessedData.directory,
      this_location_date_file=this_location_date_file,
      MeteoVarsMultiType = MeteoVarsMultiType, forecast_times = 00,
      PM25DateLoc_time = PM25DateLoc, Model_in_use_abbrev =  "namanl", sub_folder = sub_folder)

# End use of parallel computing #
stopCluster(this_cluster)
rm(this_cluster)

#### Serial version of code ####
# for (day_counter in 1:n_days) {
#   loop_NAM_run_times.parallel.fn(day_counter, Date_vector, 
#                                  ProcessedData.directory, this_location_date_file,
#                                  MeteoVarsMultiType, forecast_times = 00, 
#                                  PM25DateLoc_time = PM25DateLoc, Model_in_use_abbrev =  "namanl")
# }

#### Clear variables ####
rm(study_start_date, study_stop_date, forecast_times, Model_in_use_abbrev)
rm(PM25DateLoc,this_location_date_file,MeteoVarsMultiType)

#### End of file cleanup
#rm(uppermost.directory,output.directory)
#rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
#rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
#rm(writingcode.directory,computer_system,NAM.directory,PythonProcessedData.directory)

print(paste("Process_NAM_data_step2_parallel.R completed at",Sys.time(),sep = " "))
# stop the timer
proc.time() - start_code_timer
