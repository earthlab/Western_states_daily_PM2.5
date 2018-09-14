# Process NAM data

print("run Define_directories.R before this script") 

#### Call Packages (Library) ####
library(rNOMADS)

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"grb1to2_conversion_prep_function.R"))
source(file.path(writingcode.directory,"extract_NAM_data_function.R"))
source(file.path(writingcode.directory,"which_type_of_grib_file_function.R"))

#### Run function so that grib1>2 conversion will work ####
grb1to2_conversion_prep.fn()

#### define constants ####
study_start_date <- as.Date("20080101",format="%Y%m%d") # first date in study period
#study_stop_date  <- as.Date("20180830",format="%Y%m%d") # last date in study period
study_stop_date  <- as.Date("20080103",format="%Y%m%d") # last date in study period
forecast_times <- 00 # reanalysis - anything else would be a forecast
# Select which model to use
Model_in_use_abbrev <-  "namanl" # NAM Analysis

#### Load list of meteorology variables of interest ####
this_source_file <- paste("MeteoVariablesNAM.csv")
MeteoVarsMultiType <- read.csv(file.path(code.directory,this_source_file))
rm(this_source_file)

#### Load Date/Locations of PM2.5 Obs ####
this_location_date_file <- paste('Locations_Dates_of_PM25_Obs_DeDuplicate.csv',sep="")
# put in a loop so it could run both where we have monitors and where we
# want to predict, just uses different input files

extract_NAM_data.fn(ProcessedData.directory, this_location_date_file, MeteoVarsMultiType, 
                                study_start_date, study_stop_date, forecast_times, 
                                Model_in_use_abbrev)

#### End of file cleanup
rm(start_study_year,stop_study_year)
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
rm(writingcode.directory,computer_system)
