# Define several functions that are specifically applicable to the Fire Cache data

# The headers for the Fire Cache files are spread across three rows; Fire_Cache_consolidate_file_header.fn compresses it down to one row
Fire_Cache_consolidate_file_header.fn <- function(FireCache.directory,this_source_file) {
  # the headers are spread across 3 rows - read in those rows
  three_header_rows <- read.csv(file.path(FireCache.directory,this_source_file),header=F,skip = 1,nrows = 3)
  # create a data frame for the consolidated header to go into
  one_row_header=data.frame()
  
  # The header in the original file is spread across three rows, the following for loop consolidates them
  for(this_col in 1:dim(three_header_rows)[2]){ # cycle through each row of header
    part1 <- as.character(three_header_rows[1,this_col]) # first row of header
    #print(part1)
    part2 <- as.character(three_header_rows[2,this_col]) # second row of header
    #print(part2)
    part3 <- as.character(three_header_rows[3,this_col]) # third row of header
    #print(part3)
    if (part3==' flg'){ # add the name of the variable to flag header
      part3 <- paste(' flg.',one_row_header[this_col-1],sep = "")
    } # if (part3==' flg'){ # add the name of the variable to flag header
    this_col_header <- paste(part1,part2,part3)
    rm(part1,part2,part3)
    one_row_header[1,this_col] <- this_col_header
    rm(this_col_header)
  } # for(this_col in 1:dim(three_header_rows)[2]){ # cycle through each row of header
  rm(three_header_rows,this_col) # clear variables that are no longer needed
  
  return(one_row_header) # output from function
} # end of Fire_Cache_consolidate_file_header.fn function

# not all Fire Cache files have the same header; create a comprehensive header, adding columns that are in the current
# file that have not been in previous files. Also, add a few columns to the header
Fire_Cache_comprehensive_header.fn <- function(this_file_counter, this_Fire_Cache_data_step) {
# need to check for how/whether headers are different among input files and make a comprehensive header
if (this_file_counter==1){
  #print('first file')
  # create the variable comprehensive header on first file
  comprehensive.header <- c(colnames(this_Fire_Cache_data_step),"N_neg","N_Obs","InDayLatDiff","InDayLonDiff","1st_Max_Value","1st_Max_Hour")
} else if (this_file_counter>1){ # not the first file
  #print(paste('this_file_counter is ',this_file_counter))
  this_file_header <- colnames(this_Fire_Cache_data_step) # get the header for this file
  #print(this_file_header) # show the header
  for (this_col in 1:length(this_file_header)) { # cycle through columns in header
    #print(paste('this_col = ',this_col)) 
    this_col_header <- this_file_header[this_col] # get the header for this column
    #print(this_col_header)
    which_col <- which(comprehensive.header==this_col_header) # find this header in the comprehensive header
    #print(paste('this_col (',this_col,') matches column ',which_col,' in comprehensive.header')) 
    if (length(which_col)!=1){ # if there is no matching column in the comprehensive header, a new column will be added to comprehensive header
      #print('adding new column header that was not in previous files:')
      #print(this_col_header)
      new_col_number <- length(comprehensive.header)+1 # add new column
      comprehensive.header[new_col_number] <- this_col_header # set header for new column
      rm(new_col_number) # clear variables
    } # if (length(which_col)!=1)
    rm(this_col_header,which_col)
  } # for (this_col in 1:length(this_file_header)) {
  rm(this_file_header,this_col)
} # else if (this_file_counter>1){
  return(comprehensive.header)
} # end of Fire_Cache_comprehensive_header.fn function

# The header is (sometimes/always?) repeated further down in the data. These rows need to be found and removed.
Fire_Cache_remove_repeat_headers.fn <- function(this_Fire_Cache_data_step){
  row_restart_header <- which(this_Fire_Cache_data_step[,1]==":          ") # this text is repeated when the header repeats part way through the data file
  if (length(row_restart_header)==0){this_Fire_Cache_data <- this_Fire_Cache_data_step # no change necessary to data if the header does not repeat (just change name of variable)
  } else { # the header does repeat in the file and needs to be removed
    for (header_repeat_counter in 1:length(row_restart_header)) { # cycle through the repititions
      if (header_repeat_counter==length(row_restart_header)) { # currently the code can handle 1 repetition
        
        part1 <- this_Fire_Cache_data_step[1:row_restart_header-1,] # get the data above the repeated header
        part2_rowstart <- row_restart_header+3 # figure out which row number is just below the repeated header
        part2_rowstop <- as.numeric(dim(this_Fire_Cache_data_step)[1]) # figure out which row is at the end of the file
        part2 <- this_Fire_Cache_data_step[part2_rowstart:part2_rowstop,] # get the data below the repeated header
        
        this_Fire_Cache_data <- rbind(part1,part2) # combine data data from above and below the repeated header
        rm(part1,part2,part2_rowstart,part2_rowstop) # clear variables
      } else {
        stop('expand code') # code will need to be expanded if there are multiple header repititions in the file
      } #if (header_repeat_counter==length(row_restart_header)) { # currently the code can handle 1 repetition
    } # for (header_repeat_counter in 1:length(row_restart_header)) { # cycle through the repititions
    rm(header_repeat_counter) # clear variables
  } #if/else (length(row_restart_header)...)
  rm(row_restart_header)
  return(this_Fire_Cache_data)
} # end of Fire_Cache_remove_repeat_headers.fn function

# Change class of various columns, e.g., get it to recognize dates as dates, etc
Fire_Cache_change_data_classes.fn <- function(this_Fire_Cache_data_step2) {
  this_Fire_Cache_data <- this_Fire_Cache_data_step2
  # recognize dates as dates ":           :   Date    :MM/DD/YYYY"  
  this_Fire_Cache_data[ ,c(":           :   Date    :MM/DD/YYYY")] <- as.Date(this_Fire_Cache_data_step2[,c(":           :   Date    :MM/DD/YYYY")],"%m/%d/%Y")

  #" GMT  Time    hh:mm "                
  #[3] " Deg    GPS     Lat. "                "           flg. Deg    GPS     Lat. "
  #[5] " Deg    GPS     Lon. "                "           flg. Deg    GPS     Lon. "
  #[7] "      Type           "                "           flg.      Type           "
  #[9] "ser # Serial  Number "                "           flg.ser # Serial  Number "
  #[11] "ug/m3 Conc     RT    "                "           flg.ug/m3 Conc     RT    "
  #[13] " Unk   Misc     #1   "                "           flg. Unk   Misc     #1   "
  #[15] " l/m   Ave.   Air Flw"                "           flg. l/m   Ave.   Air Flw"
  #[17] "Deg C  Av Air   Temp "                "           flg.Deg C  Av Air   Temp "
  #[19] "  %     Rel   Humidty"                "           flg.  %     Rel   Humidty"
  #[21] "mbar   Barom   Press "                "           flg.mbar   Barom   Press "
  #[23] "deg C Sensor  Int AT "                "           flg.deg C Sensor  Int AT "
  #[25] "  %   Sensor  Int RH "                "           flg.  %   Sensor  Int RH "
  #[27] " m/s    Wind    Speed"                "           flg. m/s    Wind    Speed"
  #[29] " Deg   Wind    Direc "                "           flg. Deg   Wind    Direc "
  #[31] "volts Battery Voltage"                "           flg.volts Battery Voltage"
  #[33] "      Alarm          "                "           flg.      Alarm          "
  #[35] "N_neg"                                "N_Obs"                               
  #[37] "InDayLatDiff"                         "InDayLonDiff"                        
  #[39] "1st_Max_Value"                        "1st_Max_Hour" 
  return(this_Fire_Cache_data)
} # end of Fire_Cache_change_data_classes.fn function

# Loop through days to create data frame of 24-hr averages (used in Fire_Cache_1_file_to_small_input_mat.fn below)
Fire_Cache_daily_averages.fn <- function(this_Fire_Cache_data,comprehensive.header) {
  date_col_number <- which(as.character(colnames(this_Fire_Cache_data)) == ":           :   Date    :MM/DD/YYYY") # identify column number for date information
  these_dates <- unique(this_Fire_Cache_data[,date_col_number]) # on what days does this monitor have data? (Each file should represent one monitor)
  #print(these_dates)
  # create data frame that will have one observation per day
  N_columns_Fire_Cache=length(comprehensive.header) # number of columns
  Daily_Fire_Cache=data.frame(matrix(NA,nrow=length(these_dates),ncol=N_columns_Fire_Cache)) # create empty data frame
  names(Daily_Fire_Cache)=comprehensive.header # give new data frame a header
  rm(N_columns_Fire_Cache)
  Daily_Fire_Cache <- Fire_Cache_change_data_classes.fn(Daily_Fire_Cache)
  print('still need to deal with some files having hour 20:00 data shifted a couple of columns')
  for (date_counter in 1:length(these_dates)) {
    this_date <- these_dates[date_counter]
    #print(this_date)
    # get the 24-hr data for 1-day
    One_day_Fire_Cache <- Fire_Cache_1_day_ave.fn(this_date, this_Fire_Cache_data, date_col_number, comprehensive.header)
    # put the one day of data into Daily_Fire_Cache
    Daily_Fire_Cache[date_counter, ] <- One_day_Fire_Cache
    rm(this_date)
  } # for (date_counter in 1:length(these_dates))  
  rm(date_counter,these_dates) # clear variables
  print('think about how best to handle flags in DRI data, wind direction etc.')
  return(Daily_Fire_Cache)
} # end of Fire_Cache_daily_averages.fn function

# Fill in single column (one day) of Fire Cache data and corresponding flag (used in Fire_Cache_1_day_ave.fn below)
Fire_Cache_1_day_1_col_w_flag.fn <- function(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data) {
  # example inputs:
  # this_col_name <- " Deg    GPS     Lat. "
  # summary_method <- "mean"
  # fill in Variable and corresponding flag
  if(this_col_name %in% colnames(date_all_Fire_Cache_data)) { # check if this column exists in data_all_Fire_Cache_data
  if (summary_method == "mean") {
  One_day_Fire_Cache[1,c(this_col_name)] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(this_col_name)])))
  } else if (summary_method == "unique") {
  One_day_Fire_Cache[1,c(this_col_name)] <- unique(as.numeric(as.character(date_all_Fire_Cache_data[,c(this_col_name)])))
  } else if (summary_method == "NA") { 
  One_day_Fire_Cache[1,c(this_col_name)] <- NA 
  } else {
    stop("invalid summary_method")
  }# if summary_method
  # flag is sometimes non-numeric, so an average cannot be taken
  flag_col <- paste("           flg.",this_col_name,sep = "") #"           flg. Deg    GPS     Lat. "
  all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
  if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    One_day_Fire_Cache[1,c(flag_col)] <- as.character(unique(date_all_Fire_Cache_data[,c(flag_col)]))
  } else {# there are multiple flags and they need to be stitched together
    combine_flags <- all_flags[1] # get the first flag
    for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
    } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
    One_day_Fire_Cache[1,c(flag_col)] <- as.character(combine_flags) # input the flags
    rm(flag_counter,combine_flags) # clear variables
  } # else; if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
  rm(flag_col,all_flags)
  } # if(this_col_name %in% colnames(date_all_Fire_Cache_data)){ # check if this column exists in data_all_Fire_Cache_data
  return(One_day_Fire_Cache)
} # end of Fire_Cache_1_day_1_col_w_flag.fn function

# calculate 1-day average of Fire Cache Data (used in Fire_Cache_daily_averages.fn above)
Fire_Cache_1_day_ave.fn <- function(this_date, this_Fire_Cache_data, date_col_number, comprehensive.header) {
  # create data frame that will have one day's observation
  One_day_Fire_Cache=data.frame(matrix(NA,nrow=1,ncol=length(comprehensive.header))) # create empty data frame
  names(One_day_Fire_Cache)=comprehensive.header # give new data frame a header
  
  # set the data types
  One_day_Fire_Cache <- Fire_Cache_change_data_classes.fn(One_day_Fire_Cache)
  
  # isolate the data for this date
  find_this_data_rows <- which(this_Fire_Cache_data[,date_col_number]==this_date) # which rows in this_Fire_Cache_data (this file) are from this_date?
  date_all_Fire_Cache_data <- this_Fire_Cache_data[find_this_data_rows,] # make a smaller data frame with just the data for this_date
  rm(find_this_data_rows) # clear variables
  
  # make a note of negative values
  date_this_conc_data <- as.numeric(as.character(date_all_Fire_Cache_data[,c("ug/m3 Conc     RT    ")])) # make column with just concentration data and recognize values as numerical
  which_negative <- which(date_this_conc_data<0) # find which rows are negative
  sum_negative <- length(which_negative) # count how many rows are negative
  #print(paste("number of negative observations in ",this_source_file,"on",this_date,"=",sum_negative))
  rm(which_negative) # clear variables
  
  # make a note of max value and when it occurred, used for "1st_Max_Value" and "1st_Max_Hour"
  max_conc_this_day <- max(date_this_conc_data) # what was the highest concentration for this day?
  which_max_conc <- which(date_this_conc_data==max_conc_this_day) # which row of date_this_conc_data was the highest concentration on?
  when_max_conc <- date_all_Fire_Cache_data[which_max_conc,c(" GMT  Time    hh:mm ")] # figure out the time corresponding to the highest concentration
  rm(date_this_conc_data,which_max_conc) # clear variables
  
  N_obs_this_day <- dim(date_all_Fire_Cache_data)[1] # how many observations are there on this day?
  #print(paste("Number of observations in ",this_source_file,"on",this_date,"=",N_obs_this_day))
  # check if there are more than 24 observations on a given day ... not expected
  if (N_obs_this_day>24){ # check for more than 24 observations on this_date (just a sanity check on the data)
    print(this_date) # print the date with more than 24 observations
    print(this_source_file) # print the name of the file with more than 24 observations
    stop('There appear to be more than 24 observations for this monitor') # this should crash the program so the discrepancy can be investigated
  } # if (dim(date_all_Fire_Cache_data)[1]>24){ # check for more than 24 observations on this_date (just a sanity check on the data)
  
  # input Date information from date_all_Fire_Cache_data to One_day_Fire_Cache
  check_date <- unique(date_all_Fire_Cache_data[,c(":           :   Date    :MM/DD/YYYY")]) # get the date directly from the data
  if (check_date != this_date) { # sanity check that date information lines up
    stop("something wrong with code or data - date information does not match where it should")
  } # if (check_date != this_date) { # sanity check that date information lines up
  One_day_Fire_Cache[1, c(date_col_number)] <- check_date # input date information
  
  # not filling int " GMT  Time    hh:mm " since this section of code compiles hourly data into a 24-hr average
  
  # fill in Latitude and corresponding flag, and calculate the difference between lat obs on a given day
  this_col_name <- " Deg    GPS     Lat. "
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  # calculate how much variation there is within a day in lat observations
  max_lat <- max(as.numeric(as.character(date_all_Fire_Cache_data[,c(this_col_name)])))
  min_lat <- min(as.numeric(as.character(date_all_Fire_Cache_data[,c(this_col_name)])))
  lat_diff <- max_lat-min_lat
  One_day_Fire_Cache[1,c("InDayLatDiff")] <- lat_diff
  rm(max_lat,min_lat,lat_diff)
  rm(this_col_name,summary_method)
  
  # fill in longitude and corresponding flag
  this_col_name <- " Deg    GPS     Lon. "
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  if (One_day_Fire_Cache[1,this_col_name]>1 ) {
    stop("figure out positive vs negative longitudes in the data, are some files one way and some the other?")
  }
  # calculate how much variation there is within a day in lon observations
  max_lon <- max(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))
  min_lon <- min(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))
  lon_diff <- max_lon-min_lon
  One_day_Fire_Cache[1,c("InDayLonDiff")] <- lon_diff
  rm(max_lon,min_lon,lon_diff)
  rm(this_col_name,summary_method)
  
  # fill in Type and corresponding flag (does not exist in all files)
  this_col_name <- "      Type           "
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)
  
  # input monitor serial # and corresponding flag (not all files have this)
  this_col_name <- "ser # Serial  Number "
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)
  
  # input concentration in corresponding flag
  this_col_name <- "ug/m3 Conc     RT    "
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)
  
  # Misc # 1 column does not exist in all of these files, so only fill it in when it exists:
  this_col_name <- " Unk   Misc     #1   "
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)
  
  #input " l/m   Ave.   Air Flw" and "           flg. l/m   Ave.   Air Flw"
  this_col_name <- " l/m   Ave.   Air Flw"
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)
  
  #input "Deg C  Av Air   Temp "                "           flg.Deg C  Av Air   Temp "
  this_col_name <- "Deg C  Av Air   Temp "
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)
  
  # input % Rel Humidity and corresponding flag
  this_col_name <- "  %     Rel   Humidty"
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)
  
  # Misc # 2 column does not exist in all of these files, so only fill it in when it exists:
  this_col_name <- " Unk   Misc     #2   "
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)
  
  # "mbar   Barom   Press " and "           flg.mbar   Barom   Press " columns do not exist in all of these files, so only fill it in when it exists:
  this_col_name <- "mbar   Barom   Press "
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)
  
  # input "deg C Sensor  Int AT " and "           flg.deg C Sensor  Int AT " (not all files have this collumn)
  this_col_name <- "deg C Sensor  Int AT "
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)
  
  # input %   Sensor  Int RH and corresponding flag: "  %   Sensor  Int RH " and "           flg.  %   Sensor  Int RH "
  this_col_name <- "  %   Sensor  Int RH "
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)

  # input " m/s    Wind    Speed" and "           flg. m/s    Wind    Speed"
  this_col_name <- " m/s    Wind    Speed"
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)
  
  # input Wind direction and corresponding flag, " Deg   Wind    Direc " and "           flg. Deg   Wind    Direc "
  # average wind direction calculation is slightly more complicated than a direct average, so not doing that for now,
  # but will still get flag information
  this_col_name <- " Deg   Wind    Direc "
  summary_method <- "NA"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)
  
  # input "volts Battery Voltage" and "           flg.volts Battery Voltage"
  this_col_name <- "volts Battery Voltage"
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)
  
  # input Alarm variable and corresponding flag; "      Alarm          "                "           flg.      Alarm          "
  this_col_name <- "      Alarm          "
  summary_method <- "mean"
  One_day_Fire_Cache <- Fire_Cache_1_day_1_col_w_flag.fn(this_col_name, summary_method,One_day_Fire_Cache,date_all_Fire_Cache_data)
  rm(this_col_name,summary_method)

  # input the number of negative values "N_neg"                                
  One_day_Fire_Cache[1,c("N_neg")] <- sum_negative
  rm(sum_negative)
  
  # input the number of observations "N_Obs"
  One_day_Fire_Cache[1,c("N_Obs")] <- N_obs_this_day 
  rm(N_obs_this_day)
  
  # input max conc for day and what time it occurred
  #Note: if the maximum concentration is repeated the time of the first occurrence is recorded in 1st_Max_Hour
  One_day_Fire_Cache[1,c("1st_Max_Value")] <- max_conc_this_day
  One_day_Fire_Cache[1,c("1st_Max_Hour")] <- as.character(when_max_conc[1])#when_max_conc
  rm(when_max_conc,max_conc_this_day)

  return(One_day_Fire_Cache)
} # end of Fire_Cache_1_day_ave.fn function

# input 1 file's worth of Fire Cache data into an input_mat-like matrix
Fire_Cache_1_file_to_small_input_mat.fn <- function(Daily_Fire_Cache, input_header) {
  small_input_mat <- data.frame(matrix(NA,nrow=dim(Daily_Fire_Cache)[1],ncol=length(input_header))) # create data frame for small_input_mat
  names(small_input_mat) <- input_header # assign the header to small_input_mat
  row_stop <- 1 # start row counter
print('pick up writing code here')
  ## fill in small_input_mat with Daily_Fire_Cache data for this DRI file
  
  # put serial # in the Site_Num column of small_input_mat
  print(paste("input DRI serial number into ","Site_Num" ," in small_input_mat"))   
  small_input_mat[row_start:row_stop,c("Site_Num")] <- as.numeric(as.character(Daily_Fire_Cache[,c("ser # Serial  Number ")]))
  
  # input lat and lon ("PM2.5_Lat" and "PM2.5_Lon")
  which_colLat <- which(colnames(Daily_Fire_Cache)==" Deg    GPS     Lat. ")
  small_input_mat[row_start:row_stop,c('PM2.5_Lat')] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLat]))
  rm(which_colLat)
  which_colLon <- which(colnames(Daily_Fire_Cache)==" Deg    GPS     Lon. ")
  small_input_mat[row_start:row_stop,c('PM2.5_Lon')] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLon]))
  rm(which_colLon)
  # input InDayLatDiff and InDayLonDiff - tells how much variation there is in the Lat & Lon obs within a day (relevant for hourly observations)
  which_colLatDayDiff <- which(colnames(Daily_Fire_Cache) == "InDayLatDiff") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("InDayLatDiff")] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLatDayDiff])) # input that column into small_input_mat
  rm(which_colLatDayDiff) # remove column variable
  print("line 1040")
  which_colLonDayDiff <- which(colnames(Daily_Fire_Cache) == "InDayLonDiff") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("InDayLonDiff")] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLonDayDiff])) # input that column into small_input_mat
  print("line 1042")
  rm(which_colLonDayDiff) # remove column variable  
  
  # input flags for Lat & Lon observations
  which_colLatFlg <- which(colnames(Daily_Fire_Cache)=="           flg. Deg    GPS     Lat. ") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("flg.Lat")] <- as.character(Daily_Fire_Cache[,which_colLatFlg])# input that column into small_input_mat
  rm(which_colLatFlg)
  which_colLonFlg <- which(colnames(Daily_Fire_Cache)=="           flg. Deg    GPS     Lon. ") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("flg.Lon")] <- as.character(Daily_Fire_Cache[,which_colLonFlg])# input that column into small_input_mat
  rm(which_colLonFlg)
  
  # input "Sample_Duration"          
  small_input_mat[row_start:row_stop,c("Sample_Duration")] <- "1 HOUR"
  
  # input "Date_Local" into small_input_mat
  this_col_input_mat <- "Date_Local"
  this_col_source <- ":           :   Date    :MM/DD/YYYY"
  SourceVar <- as.Date(Daily_Fire_Cache[,c(this_col_source)],"%Y-%m-%d")
  #print(SourceVar)
  SourceVarChar <- format(SourceVar,"%Y-%m-%d")
  #print(SourceVarChar)
  small_input_mat[row_start:row_stop,c(this_col_input_mat)] <- SourceVarChar
  rm(this_col_input_mat,this_col_source,SourceVar,SourceVarChar)
  
  # input "Units_of_Measure" into small_input_mat
  small_input_mat[row_start:row_stop,c("Units_of_Measure")] <- "ug/m3 Conc     RT    "
  
  # input "Observation_Count" 
  small_input_mat[row_start:row_stop,c("Observation_Count")] <- Daily_Fire_Cache[,c("N_Obs")]
  
  # input "Observation_Percent"     
  small_input_mat[row_start:row_stop,c("Observation_Percent")] <- Daily_Fire_Cache[,c("N_Obs")]/24*100
  
  # input "PM2.5_Obs"               
  which_colConc <- which(colnames(Daily_Fire_Cache)=="ug/m3 Conc     RT    ")
  #concentration_vector <- Daily_Fire_Cache[,which_colConc]
  small_input_mat[row_start:row_stop,c('PM2.5_Obs')] <- as.numeric(as.character(Daily_Fire_Cache[,which_colConc]))
  rm(which_colConc)
  
  # input flag for PM2.5 Obs
  which_ConcFlg <- which(colnames(Daily_Fire_Cache)=="           flg.ug/m3 Conc     RT    ") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("flg.PM25_Obs")] <- as.character(Daily_Fire_Cache[,which_ConcFlg])# input that column into small_input_mat
  rm(which_ConcFlg)
  
  # input "1st_Max_Value"            
  which_col <- which(colnames(Daily_Fire_Cache)=="1st_Max_Value")
  data_vector <- Daily_Fire_Cache[,which_col]
  small_input_mat[row_start:row_stop,c("1st_Max_Value")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col,data_vector)
  
  # "1st_Max_Hour"            
  which_col <- which(colnames(Daily_Fire_Cache)=="1st_Max_Hour")
  data_vector <- Daily_Fire_Cache[,which_col]
  small_input_mat[row_start:row_stop,c("1st_Max_Hour")] <- as.character(Daily_Fire_Cache[,which_col])#as.integer(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col,data_vector)
  
  # input "PM25_Station_Name"        
  small_input_mat[row_start:row_stop,c("PM25_Station_Name")] <- this_name
  rm(this_name)
  
  #"Data_Source_Name_Display" 
  small_input_mat[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
  
  # "Data_Source_Name_Short"  
  small_input_mat[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short
  
  # input "Data_Source_Counter" - indicates if this isFire Cache. data or field data, etc.
  small_input_mat[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
  
  # input color for plotting this data source (totally arbitrary choice of color)
  small_input_mat[row_start:row_stop,c("PlottingColor")] <- "red"
  #c("darkgoldenrod","green","blue") "PlottingColor"
  
  # input source file name ("Source_File")
  small_input_mat[row_start:row_stop,c('Source_File')] <- this_source_file
  
  # "Composite_of_N_rows"      
  small_input_mat[row_start:row_stop,c("Composite_of_N_rows")] <- Daily_Fire_Cache[,c("N_Obs")] 
  
  # "N_Negative_Obs"   
  small_input_mat[row_start:row_stop,c("N_Negative_Obs")] <- Daily_Fire_Cache[,c("N_neg")]
  
  # input air flow and flag information
  which_col <- which(colnames(Daily_Fire_Cache)==" l/m   Ave.   Air Flw")
  small_input_mat[row_start:row_stop,c("l/m Ave. Air Flw")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg. l/m   Ave.   Air Flw") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("flg.AirFlw")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into small_input_mat
  rm(which_col)
  
  # input Air Temp and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="Deg C  Av Air   Temp ")
  small_input_mat[row_start:row_stop,c("Deg C Av Air Temp")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.Deg C  Av Air   Temp ") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("flg.AirTemp")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into small_input_mat
  rm(which_col)
  
  # input Relative Humidity and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="  %     Rel   Humidty")
  small_input_mat[row_start:row_stop,c("% Rel Humidty")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.  %     Rel   Humidty") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("flg.RelHumid")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into small_input_mat
  rm(which_col)            
  
  # input Barometric Pressure and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="mbar   Barom   Press ")
  small_input_mat[row_start:row_stop,c("mbar Barom Press")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.mbar   Barom   Press ") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("flg.Barom Press")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into small_input_mat
  rm(which_col)  
  
  # input Internal Temperature and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="deg C Sensor  Int AT ")
  small_input_mat[row_start:row_stop,c("deg C Sensor  Int AT")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.deg C Sensor  Int AT ") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("flg.deg C Sensor Int AT")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into small_input_mat
  rm(which_col)    
  
  # input Internal Relative Humidity and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="  %   Sensor  Int RH ")
  small_input_mat[row_start:row_stop,c("% Sensor Int RH")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.  %   Sensor  Int RH ") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("flg.%SensorIntRH")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into small_input_mat
  rm(which_col) 
  
  # input Wind Speed and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)==" m/s    Wind    Speed")
  small_input_mat[row_start:row_stop,c("Wind Speed m/s")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg. m/s    Wind    Speed") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("flg.WindSpeed")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into small_input_mat
  rm(which_col) 
  
  # input Battery Voltage and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="volts Battery Voltage")
  small_input_mat[row_start:row_stop,c("Battery Voltage volts")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.volts Battery Voltage") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("flg.BatteryVoltage")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into small_input_mat
  rm(which_col) 
  
  # input Alarm and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="      Alarm          ")
  small_input_mat[row_start:row_stop,c("Alarm")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.      Alarm          ") # identify which column in Daily_Fire_Cache we are looking for 
  small_input_mat[row_start:row_stop,c("flg.Alarm")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into small_input_mat
  rm(which_col) 
  
  # "Datum"
  small_input_mat[row_start:row_stop,c("Datum")] <- this_Datum
  
  ## columns of data in small_input_mat to figure out in DRI data
  # "State_Name" "County_Name" "State_Abbrev"  "Month" "Day"  
  # "Winter" "Year"  
  
  ## think about whether we need to figure out how to fill in these variables in small_input_mat:
  # "Parameter_Code"  "Parameter_Name" "Pollutant_Standard" "Event_Type" "AQI" "Method_Code"              "Method_Name" 
  # "City_Name" "POC"                               
  # "Address" "CBSA_Name"  "Date_of_Last_Change"                                      
  # "flg.Site_Num"   "Type"                     "flg.Type"       
  
  ## think about whether any of thes variables in Daily_Fire_Cache need to be brought into small_input_mat
  # " GMT  Time    hh:mm "  "      Type           " , "           flg.      Type           "
  # "           flg.ser # Serial  Number ", " Unk   Misc     #1   " ,"           flg. Unk   Misc     #1   "
  #  " Deg   Wind    Direc "                "           flg. Deg   Wind    Direc "
  
  # if (this_file_counter==length(all_DRI_Files)){stop("on last file")}
  rm(Daily_Fire_Cache,this_Fire_Cache_data)
  # tick up the row counter
  #row_start <- row_stop+1
  
  return(small_input_mat)
} # end of Fire_Cache_1_file_to_small_input_mat.fn function