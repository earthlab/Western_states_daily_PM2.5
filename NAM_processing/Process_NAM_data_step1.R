# Process NAM data Step 1 - add next day to list of dates that need to be processed for NAM data
# this is because NAM data is in UTC (England) time, and has to be converted to the local time zones

#### Clear variables and sinks; define working directory ####
rm(list  =  ls()) # clear all variables
options(warn  =  2) # throw an error when there's a warning and stop the code from running further
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which  =  dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
working.directory  <-  "/home/rstudio"
setwd(working.directory) # set working directory

# start timer for code #
start_code_timer <- proc.time()
print(paste("Start Process_NAM_data_step1.R at",Sys.time(),sep = " "))

# Call Load Functions that I created #
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("NAM_Code.directory"),"NAM_processing_functions.R"))

# Define constants and paths #
sub_folder <- "NAM_data" # define sub-folder name
# create NAM_data folder if it doesn't already exist
if (exists(file.path(define_file_paths.fn("ProcessedData.directory"),"NAM_data")) == FALSE) {
  dir.create(file.path(define_file_paths.fn("ProcessedData.directory"),"NAM_data"))
}

# create NAM_Step1 folder if it doesn't already exist
if (exists(file.path(define_file_paths.fn("ProcessedData.directory"),"NAM_data","NAM_Step1")) == FALSE) {
  dir.create(file.path(define_file_paths.fn("ProcessedData.directory"),"NAM_data","NAM_Step1"))
}

# Define file names and file paths for Date/Locations of interest # 
#locations_files <- c(paste("PM25_Step3_part_",define_study_constants.fn("processed_data_version"),"_Locations_Dates_NAD83", sep = ""),paste("CountyCentroid_Locations_Dates_", define_study_constants.fn("start_date"),"to",define_study_constants.fn("end_date"),sep = ""))
#locations_subfolders <- c("PM25_Locations_Dates","CountyCentroid")

# Define file names and file paths for Date/Locations of interest - not for data sets for which we are processing all dates in study period at a list of locations 
locations_subfolders <- c(paste("PM25_data_part_",define_study_constants.fn("processed_data_version"),sep = ""))
locations_files <- c(paste("PM25_Step3_part_",define_study_constants.fn("processed_data_version"),"_Locations_Dates_NAD83", sep = ""))#,paste("CountyCentroid_Locations_Dates_", define_study_constants.fn("start_date"),"to",define_study_constants.fn("end_date"),sep = ""))

# lapply function to load data, add rows for all locations with next day, save to csv file
lapply(1:length(locations_files), function(x) { # start lapply function
  this_location_date_file <- locations_files[x] # get file name
  print(this_location_date_file) # print to screen
  locations_subfolder <- locations_subfolders[x] # get the name of the subfolder for this iteration
  PM25DateLoc_orig <-read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),locations_subfolder,paste(this_location_date_file,".csv",sep = "")),header=TRUE) # load the locations file
  PM25DateLoc_orig$Date <- as.Date(PM25DateLoc_orig$Date) # recognize date column as dates
  PM25DateLoc_wNextDay <- add_next_day_date_loc.fn(PM25DateLoc_orig) # put in the day following each date 
      #in the file at each location so that all of the data will be gathered when using UTC 
  output_file_name_sub <- paste("NAM_Step1_",this_location_date_file,sep = "") # define part of output file name
  write.csv(PM25DateLoc_wNextDay,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,"NAM_Step1",paste(output_file_name_sub,"_wNextDay.csv",sep = "")),row.names = FALSE) # Save output file
  rm(PM25DateLoc_orig,PM25DateLoc_wNextDay) # clear variables
  return(this_location_date_file) # return names of files processed
}) # end lapply function

#### End of file cleanup
rm(working.directory,start_code_timer,sub_folder) # clear variables
rm(locations_files,locations_subfolders) # clear variables
