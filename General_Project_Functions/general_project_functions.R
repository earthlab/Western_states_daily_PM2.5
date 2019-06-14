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
  processed_data_version <- "f" # Do not go earlier in the alphabet than what is currently set
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
  # bounding box that includes full extra row of states: 
  # NW corner 50,-126
  # SW corner 25, -126
  # NE corner 50, -93 # 
  # SE corner 25,-93
  round_lat_lon_digits <- 5#4#5
  allowed_in_day_LatLon_variation <- 0.001 # used in Process_PM25_data_step2.R
  #round_LatLon_digits <- 0.00001
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

# print number and value to file
print_name_value.fn <- function(this_var_name,this_var_value,this_var_units = NA) {
  if (is.na(this_var_units)) {
  cat(paste(this_var_name,": ",this_var_value,"\n",sep = ""))
  } else {
    cat(paste(this_var_name,": ",this_var_value," ",this_var_units,"\n",sep = ""))
  }
} # end of print_name_value.fn function

# checksum - check that the number of rows adds up properly
checksum.fn <- function(N_original,part_A,part_B) {
  if (N_original != part_A + part_B) {
    stop("Number of rows not adding up properly in checksum.fn")
  } # if
} # end of checksum.fn function

# Check columns for NA values
check_4_NAs.fn <- function(no_NAs_allowed_cols,input_data) { # throw an error message if the column has an NA value
  #no_NAs_allowed_cols <- c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day")
  #this_var <- no_NAs_allowed[1]
  for (this_var in no_NAs_allowed_cols) { # cycle through columns to check
    #print(this_var)
    which_na <- which(is.na(input_data[ , c(this_var)])) # identify any NA values in the column of interest
    if (length(which_na)>0) { # Are there any NA values? - Yes
      Check_data <- input_data[which_na, ] # isolate questionable data
      print(paste("****There should be no NA values for ",this_var,". There are ",length(which_na),"values.****"))
      return(Check_data) # output isolated questionable data
    } else { # if (length(which_na)>0) { # Are there any NA values? - No
      Check_data <- NA # output empty variable
    } # if (length(which_na)>0) { # Are there any NA values?
  } # for (this_var in no_NAs_allowed_cols) { # cycle through columns to check
} # end of check_4_NAs.fn function

#DateLoc_orig %>%
#  mutate(date_l1 = Date - 1) %>%
#  glimpse

# add_next_day_date_loc.fn function to add the next day to the list for every location and then de-duplicate
add_lags_for_date_loc.fn <- function(DateLoc_orig, lags = c(-7:-1), date_col = "Date") { # start function
  Lags_list <- lapply(X = lags, FUN = function(x){#,DateLoc_orig,lags,date_col){ # start lapply
    #DateLoc_orig[ ,date_col] <- as.Date(DateLoc_orig[ ,date_col],"%Y-%m-%d") # recognize dates as dates
    This_lag <- DateLoc_orig # duplicate date/location data frame to new variable name
    lag_day <- x#as.numeric((lags[x]))
    print(lag_day)
    This_lag$Date <- DateLoc_orig$Date-(lag_day)#lag_day #7#lag_day#1 #lags[x]
    return(This_lag)
  }) # end of lapply
  Lags_df_step <- do.call("rbind", Lags_list) # concatinate the output from each lapply iteration
  Lags_df_w_dups <- rbind(DateLoc_orig,Lags_df_step) # combine the original and lagged data frames
  Lags_df <- Lags_df_w_dups[!duplicated(Lags_df_w_dups), ]
  return(Lags_df)
} # end add_next_day_date_loc.fn function

# determine which file fitting a specified pattern has the most recent date in the file name
determine_recent_file.fn <- function(file_pattern_before_date = "",file_pattern_after_date = "",file_suffix = ".csv",file_path) {
  # determine which file from step 4 is most recent
  #file_name_pattern <- paste(file_pattern_before_date,"\\",file_pattern_after_date,file_suffix,"$", sep = "") #"\\.csv$" # only looking for .csv files (don't want to pick up the sub-folder)
  #file_name_pattern <- paste(file_pattern_before_date,"*",file_pattern_after_date,file_suffix,"$", sep = "") #"\\.csv$" # only looking for .csv files (don't want to pick up the sub-folder)
  #file_name_pattern <- paste("\\.csv$",sep = "")
  file_name_pattern <- paste("^",file_pattern_before_date,".*.",file_pattern_after_date,file_suffix,"$",sep = "")
  
  this_file_list <- list.files(path = file.path(file_path,"."), pattern = file_name_pattern, all.files = FALSE,
                               full.names = FALSE, recursive = FALSE,
                               ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE) # get list of all .csv file in this folder
  print(this_file_list)
  print(paste("There are ",length(this_file_list),"files matching the specified pattern")) # optional output statement
  date_list <- unlist(lapply(this_file_list, function(this_file_name){ # start lapply and start defining function used in lapply
    nchar_before <- nchar(file_pattern_before_date)
    nchar_after <- nchar(file_pattern_after_date)
    nchar_suffix <- nchar(file_suffix)
    #processed_date <- substr(x,nchar(x)-13,nchar(x)-4) # identify the time stamp for the file in this iteration
    processed_date <- substr(this_file_name,nchar_before+1,(nchar(this_file_name)-(nchar_after+nchar_suffix)))
    return(processed_date) # return the new file name so a new list of files can be created
  }))
  recent_processed_date <- max(as.Date(date_list)) # which date is the most recent file
  which_recent_file <- which(date_list == recent_processed_date) # locate the file name for the most recent file
  recent_file_name <- this_file_list[which_recent_file] # most recent file name
  print(paste(recent_file_name,"is the most recent file and will be used"))
  return(recent_file_name) # output from function
}

#PM25DateLoc_NextDay <- PM25DateLoc_temp # duplicate date/location data frame to new variable name
#PM25DateLoc_NextDay$Date <- PM25DateLoc_temp$Date+1 # shift all dates to the next day
#PM25DateLoc_step1 <- rbind(PM25DateLoc_temp,PM25DateLoc_NextDay) # combine the data frames with the day of interest and the next day
#PM25DateLoc <- PM25DateLoc_step1[!duplicated(PM25DateLoc_step1[,1:dim(PM25DateLoc_step1)[2]]), ] # get rid of any duplicates (which happens any time a monitor runs for 2 consecutive days in the same location)
#return(PM25DateLoc) # output from function

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



