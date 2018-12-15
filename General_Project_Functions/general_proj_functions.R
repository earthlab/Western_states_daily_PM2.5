# This is for functions that are used in several parts of the project

# extract file paths set in this function
define_file_paths.fn <- function(directory_interest,working.directory = "/home/rstudio") {
  #directory_interest <- "ML_Code.directory"
  writingcode.directory  <-  "/home/rstudio/estimate-pm25/PM25-estimation"
  ML_Code.directory <- file.path(working.directory,"estimate-pm25","ML_estimations")
  LocationsInterest_Code.directory <- file.path(working.directory,"estimate-pm25","Locations_of_interest")
  NAM_Code.directory <- file.path(working.directory,"estimate-pm25","NAM_processing")
  ProcessedData.directory <- file.path(working.directory,"Processed_Data")
  output.directory <- file.path(working.directory,"estimate-pm25","LaTeX_documentation","Code_Outputs")
  output.directory.short <- "Code_Outputs" # for LaTeX code
  #code.directory <- file.path(working.directory,"estimate-pm25","PM25-estimation")
  PythonProcessedData.directory <- file.path(working.directory,"Python_Processed_Data")
  UintahData.directory <- file.path(working.directory,"PM25_all_orig","PM25_Uintah_Basin")
  USMaps.directory <- file.path(working.directory,"Shapefiles_for_mapping","cp_2016_us_state_500k")
  #USMaps.directory <- file.path(working.directory,"Shapefiles_for_mapping")
  CountyMaps.directory <- file.path(working.directory,"Shapefiles_for_mapping","cp_2017_us_county_500k")
  PCAPSData.directory <- file.path(working.directory,"PM25_all_orig","PM25_PCAPS_Salt_Lake")
  AQSData.directory <- file.path(working.directory,"PM25_all_orig","AQS_Daily_Summaries")
  FMLE.directory <- file.path(working.directory,"PM25_all_orig","Federal_Land_Manager_Environmental_Database")
  FireCache.directory <- file.path(working.directory,"PM25_all_orig","Fire_Cache_Smoke_DRI")
  CARB.directory <- file.path(working.directory,"PM25_all_orig","PM25_CARB")
  UTDEQ.directory <- file.path(working.directory,"PM25_all_orig","PM25_UTDEQ")
  #NVDEQ.directory <- file.path(working.directory,"PM25_all_orig","PM25_NV-DEQ")
  NAM.directory <- file.path(working.directory,"NAM_data_orig")
  #NARR.directory <- file.path(working.directory,"NARR")
  #### Define the batch for PM2.5 data
  processed_data_version <- "e" #"d" #"b" # Do not go earlier in the alphabet than what is currently set

  directory_interest_path <- eval(parse(text = directory_interest))
} # end of define_file_paths.fn function

# define study contstants
define_study_constants.fn <- function(constant_interest) {
  start_date <- "2008-01-01"
  end_date <- "2018-12-31"
  directory_interest_path <- eval(parse(text = constant_interest))
} # end of define_study_constants.fn function


# # clear variables and start fresh (get rid of any lingering sinks)
# start_fresh.fn <- function() {
#   #### Clear all variables and start fresh ####
#   rm(list  =  ls())
#   options(warn  =  2) # throw an error when there's a warning and stop the code from running further
#   # make sure it isn't outputing text or figures to any files
#   if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
#     dev.off(which  =  dev.cur())
#   } # if (max(dev.cur())>1) {
#   while (sink.number()>0) {
#     sink()
#   } # while (sink.number()>0) {
#   #sink.number()
# } # end of start_fresh.fn function
