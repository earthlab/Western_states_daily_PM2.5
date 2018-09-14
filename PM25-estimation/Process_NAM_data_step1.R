# Process NAM data

print("run Define_directories.R before this script") 

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"add_next_day_date_loc_function.R"))

#### Load Date/Locations of interest ####
this_location_date_file <- 'Locations_Dates_of_PM25_Obs_DeDuplicate.csv'
print(this_location_date_file)
PM25DateLoc_orig <-read.csv(file.path(ProcessedData.directory,this_location_date_file),header=TRUE) # load the AQS file
PM25DateLoc_orig$Date <- as.Date(PM25DateLoc_orig$Date) # recognize date column as dates

#### put in a loop so it could run both where we have monitors and where we ####
# want to predict, just uses different input files

PM25DateLoc_wNextDay <- add_next_day_date_loc.fn(PM25DateLoc_orig) # put in the day following each date 
#in the file at each location so that all of the data will be gathered when using UTC 

#### Save output file ####

write.csv(PM25DateLoc_wNextDay,file = file.path(ProcessedData.directory,paste(this_location_date_file,"_wNextDay.csv",sep = "")),row.names = FALSE)

#### clear variables
rm(PM25DateLoc_orig,PM25DateLoc_wNextDay,this_location_date_file)

#### End of file cleanup
rm(start_study_year,stop_study_year)
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
rm(writingcode.directory,computer_system,PythonProcessedData.directory,NAM.directory)
