# Process NAM data using parallel resources

print("run Define_directories.R before this script") 

start_code_timer <- proc.time()
print(paste("Start Process_NAM_data_step2_parallel.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(rNOMADS)
#library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
library(foreach)

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
study_start_date <- as.Date("20080103",format="%Y%m%d") # first date in study period
#study_stop_date  <- as.Date("20180830",format="%Y%m%d") # last date in study period
study_stop_date  <- as.Date("20080103",format="%Y%m%d") # last date in study period
Date_vector <- seq(study_start_date,study_stop_date, by = "day") # vector of all dates for which meteo data will be extracted

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

#### Set up code for running in parallel ####
# Calculate the number of cores
#no_cores <- detectCores() - 1
# Initiate cluster
#cl <- makeCluster(no_cores)
#use parLapply() to cycle through dates in a parallel fashion


#### Run the parallel loop ####
# foreach info: https://cran.r-project.org/web/packages/foreach/foreach.pdf
foreach(day_counter, Date_vector, run_counter, # variables to be used inside the parallel loops
        ProcessedData.directory, this_location_date_file, # variables to be used inside the parallel loops
        MeteoVarsMultiType, theDate, forecast_times = 00, this_model.run, # variables to be used inside the parallel loops
        PM25DateLoc_time, Model_in_use_abbrev =  "namanl", # variables to be used inside the parallel loops
        .combine, .init, .final=NULL, .inorder=FALSE, # .inorder set to FALSE to improve performance
        .multicombine=FALSE,
        .maxcombine=if (.multicombine) 100 else 2,
        .errorhandling=c('stop'), # stop program if there is an error. Options are c('stop','remove','pass')
        .packages= c("rNOMADS"), .export=NULL, .noexport=NULL,
        .verbose=FALSE)
when(cond)
e1 %:% e2
obj %do% ex
obj %dopar% ex
times(n)

for (day_counter in 1:length(Date_vector)) { # cycle through dates of interest, for which meteo data will be extracted - need to parallelize
  
  loop_NAM_run_times.parallel.fn(Date_vector, run_counter,
                                             ProcessedData.directory, this_location_date_file,
                                             MeteoVarsMultiType, theDate, forecast_times = 00, this_model.run, 
                                             PM25DateLoc_time, Model_in_use_abbrev =  "namanl")
  
} # for (day_counter in 1:length(Date_vector)) { # cycle through dates of interest, for which meteo data will be extracted - need to parallelize

#### Clear variables ####
rm(study_start_date, study_stop_date, forecast_times, Model_in_use_abbrev)
rm(PM25DateLoc,this_location_date_file,MeteoVarsMultiType)

#### End of file cleanup
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
rm(writingcode.directory,computer_system,NAM.directory,PythonProcessedData.directory)

#### End use of parallel computing ####
#stopCluster(cl)

print(paste("Process_NAM_data_step2_parallel.R completed at",Sys.time(),sep = " "))
# stop the timer
proc.time() - start_code_timer
