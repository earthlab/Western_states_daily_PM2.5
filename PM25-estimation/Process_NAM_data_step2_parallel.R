# Process NAM data using parallel resources

print("run Define_directories.R before this script") 

start_code_timer <- proc.time()
print(paste("Start Process_NAM_data_step2_parallel.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(rNOMADS)
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
#library(doParallel) # https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf
#library(foreach)

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"grb1to2_conversion_prep_function.R"))
source(file.path(writingcode.directory,"extract_NAM_data_parallel_function.R"))
source(file.path(writingcode.directory,"which_type_of_grib_file_function.R"))
source(file.path(writingcode.directory,"convert_grib1to2_function.R"))
source(file.path(writingcode.directory,"define_project_bounds_function.R"))
source(file.path(writingcode.directory,"loop_NAM_run_times.parallel_function.R"))

#### Run function so that grib1>2 conversion will work ####
grb1to2_conversion_prep.fn()

#### define constants ####
study_start_date <- as.Date("20080106",format="%Y%m%d") # first date in study period
#study_stop_date  <- as.Date("20180830",format="%Y%m%d") # last date in study period
study_stop_date  <- as.Date("20080130",format="%Y%m%d") # last date in study period
Date_vector <- seq(study_start_date,study_stop_date, by = "day") # vector of all dates for which meteo data will be extracted
n_days <- length(Date_vector)

print(Date_vector)

forecast_times <- 00 # reanalysis - anything else would be a forecast
# Select which model to use
Model_in_use_abbrev <-  "namanl" # NAM Analysis

#### Load list of meteorology variables of interest ####
this_source_file <- paste("MeteoVariablesNAM.csv")
MeteoVarsMultiType <- read.csv(file.path(writingcode.directory,this_source_file))
rm(this_source_file)

#### Load Date/Locations of PM2.5 Obs ####
this_location_date_file <- 'Locations_Dates_of_PM25_Obs_DeDuplicate'

#### Load _wNextDay data ####
PM25DateLoc <- read.csv(file.path(ProcessedData.directory,paste(this_location_date_file,"_wNextDay.csv",sep = "")))
PM25DateLoc$Date <- as.Date(PM25DateLoc$Date) # recognize date column as dates


#registerDoParallel(cl)
#registerDoParallel(cores=no_cores)
#### Run the parallel loop ####
# foreach info: https://cran.r-project.org/web/packages/foreach/foreach.pdf
#foreach(day_counter = 1:length(Date_vector), Date_vector, # variables to be used inside the parallel loops
#        ProcessedData.directory, this_location_date_file, # variables to be used inside the parallel loops
#        MeteoVarsMultiType, forecast_times = 00, # variables to be used inside the parallel loops
#        PM25DateLoc, Model_in_use_abbrev =  "namanl",
#        .packages= c("rNOMADS")) %dopar% {#, # variables to be used inside the parallel loops
#          getDoParWorkers()


#### Set up code for running in parallel ####
# Calculate the number of cores
n_cores <- detectCores() - 1
#print(paste(n_cores,"available for parallel processing",sep = " "))
# Initiate cluster
this_cluster <- makeCluster(n_cores)
#day_counter <-  1:n_days
#parLapply(this_cluster,day_counter,loop_NAM_run_times.parallel.fn(day_counter, Date_vector, 
#                                             ProcessedData.directory, this_location_date_file,
#                                             MeteoVarsMultiType, forecast_times = 00, 
#                                             PM25DateLoc_time = PM25DateLoc, Model_in_use_abbrev =  "namanl"))
clusterExport(cl = this_cluster, varlist = c("extract_NAM_data.parallel.fn","which_type_of_grib_file.fn"), envir = .GlobalEnv)
parLapply(this_cluster,X = 1:2, fun = loop_NAM_run_times.parallel.fn,
       Date_vector = Date_vector, 
       ProcessedData.directory=ProcessedData.directory, 
       this_location_date_file=this_location_date_file,
       MeteoVarsMultiType = MeteoVarsMultiType, forecast_times = 00, 
       PM25DateLoc_time = PM25DateLoc, Model_in_use_abbrev =  "namanl")


#for (day_counter in 1:n_days) {
#  loop_NAM_run_times.parallel.fn(day_counter, Date_vector, 
#                                 ProcessedData.directory, this_location_date_file,
#                                 MeteoVarsMultiType, forecast_times = 00, 
#                                 PM25DateLoc_time = PM25DateLoc, Model_in_use_abbrev =  "namanl")
#}



#### End use of parallel computing ####
#stopCluster(this_cluster)

#### Clear variables ####
rm(study_start_date, study_stop_date, forecast_times, Model_in_use_abbrev)
rm(PM25DateLoc,this_location_date_file,MeteoVarsMultiType)

#### End of file cleanup
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
rm(writingcode.directory,computer_system,NAM.directory,PythonProcessedData.directory)

print(paste("Process_NAM_data_step2_parallel.R completed at",Sys.time(),sep = " "))
# stop the timer
proc.time() - start_code_timer
