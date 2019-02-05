# This is for functions that are used in several parts of the project

# extract file paths set in this function
define_file_paths.fn <- function(directory_interest,working.directory = "/home/rstudio") {
  #directory_interest <- "ML_Code.directory"
  uppermost.directory  <-  "/home/rstudio"
  writingcode.directory  <-  "/home/rstudio/estimate-pm25/PM25-estimation"
  General_functions.directory <- file.path(working.directory,"estimate-pm25","General_Project_Functions")
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
  processed_data_version <- "e" #"d" #"e" #"d" #"b" # Do not go earlier in the alphabet than what is currently set
  start_date <- "2008-01-01"
  end_date <- "2018-12-31"
  study_states_abbrev <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY") # study area
  study_datum <- "NAD83"
  voltage_threshold_upper <- 17
  voltage_threshold_lower <- 11
  min_hourly_obs_daily <- 18/24*100 # minimum percent of hourly observations required to compute a 24-hr average
  # bounds that just have about 78 km east of Colorado 
  North_Edge <- 50
  South_Edge <- 25
  West_Edge <- -126
  East_Edge <- -101 # about 78 km east of eastern edge of Colorado
  round_lat_lon_digits <- 4#5
  constant_interest_value <- eval(parse(text = constant_interest)) # assign the value to the output variable
  return(constant_interest_value)
} # end of define_study_constants.fn function

# replace specified character in string (default to replace underscores with spaces)
replace_character_in_string.fn <- function(input_char,char2replace = "_",replacement_char = " ") {
  for (this_letter_i in 1:nchar(input_char)) {
    this_letter <- substr(input_char, this_letter_i, this_letter_i) # what is the current letter?
    if (this_letter == char2replace) { # this this letter a space?
      input_char <- paste(substr(input_char,1,(this_letter_i-1)),replacement_char,substr(input_char,(this_letter_i+1),nchar(input_char)),sep = "")
    } # if (this_letter == char2replace) { # this this letter a space?
  } # for (this_letter_i in 1:length(input_vec_char)) {   
  return(input_char)
}

# how many decimal places are in a number (directly from https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r)
decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

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
