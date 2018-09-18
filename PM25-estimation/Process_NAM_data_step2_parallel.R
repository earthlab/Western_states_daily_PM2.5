# Process NAM data using parallel resources

print("run Define_directories.R before this script") 

#### Call Packages (Library) ####
library(rNOMADS)
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"grb1to2_conversion_prep_function.R"))
source(file.path(writingcode.directory,"extract_NAM_data_parallel_function.R"))
source(file.path(writingcode.directory,"which_type_of_grib_file_function.R"))
source(file.path(writingcode.directory,"convert_grib1to2_function.R"))

#### Run function so that grib1>2 conversion will work ####
grb1to2_conversion_prep.fn()

#### define constants ####
study_start_date <- as.Date("20080101",format="%Y%m%d") # first date in study period
#study_stop_date  <- as.Date("20180830",format="%Y%m%d") # last date in study period
study_stop_date  <- as.Date("20080101",format="%Y%m%d") # last date in study period
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
PM25DateLoc_0000 <- PM25DateLoc
PM25DateLoc_0600 <- PM25DateLoc
PM25DateLoc_1200 <- PM25DateLoc
PM25DateLoc_1800 <- PM25DateLoc

# a function so it could run both where we have monitors and where we
# want to predict, just uses different input files
#extract_NAM_data.fn(ProcessedData.directory, this_location_date_file, MeteoVarsMultiType, 
#                                study_start_date, study_stop_date, forecast_times, 
#                                Model_in_use_abbrev)

#use parLapply() to cycle through dates in a parallel fashion

#### Set up code for running in parallel ####
# Calculate the number of cores
#no_cores <- detectCores() - 1
# Initiate cluster
#cl <- makeCluster(no_cores)

theDate <- study_start_date
this_model.run <- "18"
PM25DateLoc_time <- PM25DateLoc_1800
# find the locations that need data for this date
#which_theDate <- which(PM25DateLoc$Date == theDate)
extract_NAM_data.parallel.fn(MeteoVarsMultiType, theDate, forecast_times = 00, this_model.run, #which_theDate,
                                         PM25DateLoc_time, Model_in_use_abbrev =  "namanl")

#### Clear variables ####
rm(study_start_date, study_stop_date, forecast_times, Model_in_use_abbrev, MeteoVars)
rm(PM25DateLoc)

#### Output 4 data frames to csv files ####
#error("still need to output csv files")
#write.csv(PM25DateLoc_0000,file = file.path(ProcessedData.directory,paste(this_location_date_file,"_0000UTC.csv",sep = "")),row.names = FALSE)
#write.csv(PM25DateLoc_0600,file = file.path(ProcessedData.directory,paste(this_location_date_file,"_0600UTC.csv",sep = "")),row.names = FALSE)
#write.csv(PM25DateLoc_1200,file = file.path(ProcessedData.directory,paste(this_location_date_file,"_1200UTC.csv",sep = "")),row.names = FALSE)
#write.csv(PM25DateLoc_1800,file = file.path(ProcessedData.directory,paste(this_location_date_file,"_1800UTC.csv",sep = "")),row.names = FALSE)


#### Clear Variables
rm(study_start_date,study_stop_date,this_location_date_file,forecast_times,Model_in_use_abbrev)
rm(MeteoVarsMultiType)

#### End of file cleanup
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
rm(writingcode.directory,computer_system,NAM.directory,PythonProcessedData.directory)

#### End use of parallel computing ####
stopCluster(cl)
