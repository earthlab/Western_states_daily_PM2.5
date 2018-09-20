# Process NAM data using parallel resources

print("run Define_directories.R before this script") 

start_code_timer <- proc.time()
print(paste("Start Process_NAM_data_step2_parallel.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(rNOMADS)
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"grb1to2_conversion_prep_function.R"))
source(file.path(writingcode.directory,"extract_NAM_data_parallel_function.R"))
source(file.path(writingcode.directory,"which_type_of_grib_file_function.R"))
source(file.path(writingcode.directory,"convert_grib1to2_function.R"))
source(file.path(writingcode.directory,"define_project_bounds_function.R"))

#### Run function so that grib1>2 conversion will work ####
grb1to2_conversion_prep.fn()

#### define constants ####
study_start_date <- as.Date("20080102",format="%Y%m%d") # first date in study period
#study_stop_date  <- as.Date("20180802",format="%Y%m%d") # last date in study period
study_stop_date  <- as.Date("20080102",format="%Y%m%d") # last date in study period
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

#### Create data sets for each run time (UTC) to put weather data into ####
#PM25DateLoc_0000 <- PM25DateLoc
#PM25DateLoc_0600 <- PM25DateLoc
#PM25DateLoc_1200 <- PM25DateLoc
#PM25DateLoc_1800 <- PM25DateLoc

#### Set up code for running in parallel ####
# Calculate the number of cores
#no_cores <- detectCores() - 1
# Initiate cluster
#cl <- makeCluster(no_cores)
#use parLapply() to cycle through dates in a parallel fashion

#n_days <- as.numeric(study_stop_date-study_start_date)
Date_vector <- seq(study_start_date,study_stop_date, by = "day")
for (day_counter in 1:length(Date_vector)) {
  
  theDate <- as.Date(Date_vector[day_counter]) #study_start_date
  print(theDate)
  
  #for (run_counter in 1:4) { #UNCOMMENT
   for (run_counter in 3:3) {  #COMMENT
    
    if (run_counter == 1) {
      this_model.run <- "00"
      #PM25DateLoc_time <- PM25DateLoc_0000
    } else if (run_counter == 2) {
      this_model.run <- "06"
      #PM25DateLoc_time <- PM25DateLoc_0600
    } else if (run_counter == 3) {
      this_model.run <- "12"
      #PM25DateLoc_time <- PM25DateLoc_1200
    } else if (run_counter == 4) {
      this_model.run <- "18"
      #PM25DateLoc_time <- PM25DateLoc_1800
    }
    
    #this_model.run <- "18"
    #PM25DateLoc_time <- PM25DateLoc_1800
    extract_NAM_data.parallel.fn(MeteoVarsMultiType, theDate, forecast_times = 00, this_model.run, #which_theDate,
                                             PM25DateLoc, Model_in_use_abbrev =  "namanl")
  }
}

#### Clear variables ####
rm(study_start_date, study_stop_date, forecast_times, Model_in_use_abbrev)
rm(PM25DateLoc)

#### Output 4 data frames to csv files ####
#error("still need to output csv files")
#write.csv(PM25DateLoc_0000,file = file.path(ProcessedData.directory,paste(this_location_date_file,"_0000UTC.csv",sep = "")),row.names = FALSE)
#write.csv(PM25DateLoc_0600,file = file.path(ProcessedData.directory,paste(this_location_date_file,"_0600UTC.csv",sep = "")),row.names = FALSE)
#write.csv(PM25DateLoc_1200,file = file.path(ProcessedData.directory,paste(this_location_date_file,"_1200UTC.csv",sep = "")),row.names = FALSE)
#write.csv(PM25DateLoc_1800,file = file.path(ProcessedData.directory,paste(this_location_date_file,"_1800UTC.csv",sep = "")),row.names = FALSE)


#### Clear Variables
rm(this_location_date_file)
rm(MeteoVarsMultiType)

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
