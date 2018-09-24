# Define several functions that are specifically applicable to the Fire Cache data

# The headers for the Fire Cache files are spread across three rows; Fire_Cache_consolidate_file_header.fn compresses it down to one row
Fire_Cache_consolidate_file_header.fn <- function(FireCache.directory,this_source_file) {
  # the headers are spread across 3 rows - read in those rows
  three_header_rows <- read.csv(file.path(FireCache.directory,this_source_file),header=F,skip = 1,nrows = 3)
  # create a data frame for the consolidated header to go into
  one_row_header=data.frame()
  
  # The header in the original file is spread across three rows, the following for loop consolidates them
  for(this_col in 1:dim(three_header_rows)[2]){
    part1 <- as.character(three_header_rows[1,this_col]) # first row of header
    #print(part1)
    part2 <- as.character(three_header_rows[2,this_col]) # second row of header
    #print(part2)
    part3 <- as.character(three_header_rows[3,this_col]) # third row of header
    #print(part3)
    if (part3==' flg'){ # add the name of the variable to flag header
      part3 <- paste(' flg.',one_row_header[this_col-1],sep = "")
    }
    this_col_header <- paste(part1,part2,part3)
    rm(part1,part2,part3)
    one_row_header[1,this_col] <- this_col_header
    rm(this_col_header)
  }  
  rm(three_header_rows,this_col) # clear variables that are no longer needed
  
  return(one_row_header) # output from function
} # end function

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
} # end function

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
      } # else
    } # for
    rm(header_repeat_counter) # clear variables
  } #if/else (length(row_restart_header)...)
  rm(row_restart_header)
  return(this_Fire_Cache_data)
} # end function

# Loop through days to create data frame of 24-hr averages
Fire_Cache_daily_averages.fn <- function() {
  date_col_number <- which(as.character(colnames(this_Fire_Cache_data)) == ":           :   Date    :MM/DD/YYYY")
  # on what days does this monitor have data? (Each file should represent one monitor)
  these_dates <- unique(this_Fire_Cache_data[,date_col_number])
  #print(these_dates)
  # create data frame that will have one observation per day
  N_columns_Fire_Cache=length(comprehensive.header) # number of columns
  Daily_Fire_Cache=data.frame(matrix(NA,nrow=length(these_dates),ncol=N_columns_Fire_Cache)) # create empty data frame
  names(Daily_Fire_Cache)=comprehensive.header # give new data frame a header
  rm(N_columns_Fire_Cache)
  print('still need to deal with some files having hour 20:00 data shifted a couple of columns')
  for (date_counter in 1:length(these_dates)) {
    this_date <- these_dates[date_counter]
    #print(this_date)
    
    Fire_Cache_1_day_ave.fn(this_date, this_Fire_Cache_data)
    
    rm(this_date)
  } # for (date_counter in 1:length(these_dates))  
  rm(date_counter,these_dates,date_all_Fire_Cache_data) # clear variables
  print('think about how best to handle flags in DRI data, wind direction etc.')
} # end of Fire_Cache_daily_averages.fn function

# calculate 1-day average of Fire Cache Data
Fire_Cache_1_day_ave.fn <- function(this_date, this_Fire_Cache_data) {
  # isolate the data for this date
  find_this_data_rows <- which(this_Fire_Cache_data[,date_col_number]==this_date)
  date_all_Fire_Cache_data <- this_Fire_Cache_data[find_this_data_rows,]
  rm(find_this_data_rows)
  
  # make a note of negative values
  date_this_conc_data <-as.numeric(as.character(date_all_Fire_Cache_data[,c("ug/m3 Conc     RT    ")]))
  which_negative <- which(date_this_conc_data<0)
  sum_negative <- length(which_negative)
  #print(paste("number of negative observations in ",this_source_file,"on",this_date,"=",sum_negative))
  rm(which_negative)
  
  # make a note of max value and when it occurred, used for "1st_Max_Value" and "1st_Max_Hour"
  max_conc_this_day <- max(date_this_conc_data)
  #print(max_conc_this_day)
  which_max_conc <- which(date_this_conc_data==max_conc_this_day)
  #print(which_max_conc)
  when_max_conc <- date_all_Fire_Cache_data[which_max_conc,c(" GMT  Time    hh:mm ")]
  #print(when_max_conc)
  rm(date_this_conc_data,which_max_conc)
  
  # check if there are more than 24 observations on a given day ... not expected
  if (dim(date_all_Fire_Cache_data)[1]>24){#(length(find_this_data_rows)>24){
    print(this_date)
    print(this_source_file)
    stop('There appear to be more than 24 observations for this monitor')
  }
  N_obs_this_day <- dim(date_all_Fire_Cache_data)[1]
  #print(paste("Number of observations in ",this_source_file,"on",this_date,"=",N_obs_this_day))
  
  # input Date information from date_all_Fire_Cache_data to Daily_Fire_Cache
  this_col_input_mat <- ":           :   Date    :MM/DD/YYYY"
  this_col_AQS <- "R_Dates"
  AQSVar <- as.Date(unique(date_all_Fire_Cache_data[,c(this_col_AQS)]),"%Y-%m-%d")
  #print(AQSVar)
  AQSVarChar <- format(AQSVar,"%Y-%m-%d")
  #print(AQSVarChar)
  Daily_Fire_Cache[date_counter,c(this_col_input_mat)] <- AQSVarChar
  rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
  
  # not filling int " GMT  Time    hh:mm " since this section of code compiles hourly data into a 24-hr average
  
  # fill in Latitude and corresponding flag, and calculate the difference between lat obs on a given day
  Daily_Fire_Cache[date_counter,c(" Deg    GPS     Lat. ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lat. ")])))
  #Daily_Fire_Cache[date_counter,c("           flg. Deg    GPS     Lat. ")] <- unique(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Deg    GPS     Lat. ")])))
  # flag is sometimes non-numeric, so an average cannot be taken
  flag_col <- "           flg. Deg    GPS     Lat. "
  all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
  #print(all_flags)
  if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(unique(date_all_Fire_Cache_data[,c(flag_col)]))
  } else {# there are multiple flags and they need to be stitched together
    combine_flags <- all_flags[1] # get the first flag
    #print(combine_flags)
    for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
      #print(combine_flags)
    } # for
    Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(combine_flags) # input the flags
    rm(flag_counter,combine_flags) # clear variables
  } # else
  rm(flag_col,all_flags)
  # calculate how much variation there is within a day in lat observations
  # max_lat <- max(date_all_Fire_Cache_data[,c(" Deg    GPS     Lat. ")])
  max_lat <- max(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lat. ")])))
  #print(max_lat)
  #min_lat <- min(date_all_Fire_Cache_data[,(" Deg    GPS     Lat. ")])
  min_lat <- min(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lat. ")])))
  #print(min_lat)
  lat_diff <- max_lat-min_lat
  #print(lat_diff)
  Daily_Fire_Cache[date_counter,c("InDayLatDiff")] <- lat_diff
  rm(max_lat,min_lat,lat_diff)
  
  # fill in longitude and corresponding flag
  if (mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))>0){
    Daily_Fire_Cache[date_counter,c(" Deg    GPS     Lon. ")] <- (-1)*mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))
    print('longitude value was positive, so it was multiplied by -1 to make it negative')
  } else { # if (mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))>0){
    Daily_Fire_Cache[date_counter,c(" Deg    GPS     Lon. ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))
  } # if/else (mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))>0){
  # flag for Lon is sometimes non-numeric, so an average cannot be taken
  all_flags <- unique(date_all_Fire_Cache_data[,c("           flg. Deg    GPS     Lon. ")]) # what are all the flags on this day?
  #print(all_flags)
  if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    Daily_Fire_Cache[date_counter,c("           flg. Deg    GPS     Lon. ")] <- as.character(unique(date_all_Fire_Cache_data[,c("           flg. Deg    GPS     Lon. ")]))
  } else {# there are multiple flags and they need to be stitched together
    combine_flags <- all_flags[1] # get the first flag
    #print(combine_flags)
    for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
      #print(combine_flags)
    } # for
    Daily_Fire_Cache[date_counter,c("           flg. Deg    GPS     Lon. ")] <- as.character(combine_flags) # input the flags
    rm(flag_counter,combine_flags) # clear variables
  } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
  # calculate how much variation there is within a day in lon observations
  max_lon <- max(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))
  #print(max_lon)
  min_lon <- min(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))
  #print(min_lon)
  lon_diff <- max_lon-min_lon
  #print(lon_diff)
  Daily_Fire_Cache[date_counter,c("InDayLonDiff")] <- lon_diff
  rm(max_lon,min_lon,lon_diff,all_flags)
  
  # fill in Type and corresponding flag (does not exist in all files)
  if("      Type           " %in% colnames(date_all_Fire_Cache_data)){
    Daily_Fire_Cache[date_counter,c("      Type           ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("      Type           ")])))
    #Daily_Fire_Cache[date_counter,c("           flg.      Type           ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.      Type           ")])))
    # flag is sometimes non-numeric, so an average cannot be taken
    flag_col <- "           flg.      Type           "
    all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
    #print(all_flags)
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c(flag_col)] <- unique(as.character(date_all_Fire_Cache_data[,c(flag_col)]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
        #print(combine_flags)
      } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(combine_flags) # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    rm(flag_col,all_flags)
  } #if(" 
  
  # input monitor serial # and corresponding flag (not all files have this)
  if("ser # Serial  Number " %in% colnames(date_all_Fire_Cache_data)) {
    Daily_Fire_Cache[date_counter,c("ser # Serial  Number ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("ser # Serial  Number ")])))
    #Daily_Fire_Cache[date_counter,c("ser # Serial  Number ")] <- mean(as.numeric(date_all_Fire_Cache_data[,c("ser # Serial  Number ")]))
    
    # flag for serial # is sometimes non-numeric, so an average cannot be taken
    all_serial_flags <- unique(date_all_Fire_Cache_data[,c("           flg.ser # Serial  Number ")]) # what are all the flags on this day?
    #print(all_serial_flags)
    if (length(all_serial_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c("           flg.ser # Serial  Number ")] <- unique(as.character(date_all_Fire_Cache_data[,c("           flg.ser # Serial  Number ")]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_serial_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_serial_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_serial_flags[flag_counter],sep = " ")
        #print(combine_flags)
      }
      Daily_Fire_Cache[date_counter,c("           flg.ser # Serial  Number ")] <- as.character(combine_flags) # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # if/else (length(all_serial_flags)==1){ # there is only 1 flag, so it can be put in directly
    rm(all_serial_flags)
  } # if("ser # Serial  Number " %in% colnames(date_all_Fire_Cache_data)) {
  
  # input concentration in corresponding flag
  Daily_Fire_Cache[date_counter,c("ug/m3 Conc     RT    ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("ug/m3 Conc     RT    ")])))
  #Daily_Fire_Cache[date_counter,c("           flg.ug/m3 Conc     RT    ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.ug/m3 Conc     RT    ")])))
  # flag is sometimes non-numeric, so an average cannot be taken
  flag_col <- "           flg.ug/m3 Conc     RT    "
  all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
  #print(all_flags)
  if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    Daily_Fire_Cache[date_counter,c(flag_col)] <- unique(as.character(date_all_Fire_Cache_data[,c(flag_col)]))
  } else {# there are multiple flags and they need to be stitched together
    combine_flags <- all_flags[1] # get the first flag
    #print(combine_flags)
    for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
      #print(combine_flags)
    } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
    Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(combine_flags) # input the flags
    rm(flag_counter,combine_flags) # clear variables
  } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
  rm(flag_col,all_flags)
  
  # Misc # 1 column does not exist in all of these files, so only fill it in when it exists:
  if(" Unk   Misc     #1   " %in% colnames(date_all_Fire_Cache_data)) {
    Daily_Fire_Cache[date_counter,c(" Unk   Misc     #1   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Unk   Misc     #1   ")])))
    # flag for Unk Misc #1 is sometimes non-numeric, so an average cannot be taken
    all_flags <- unique(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #1   ")]) # what are all the flags on this day?
    #print(all_flags)
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      # Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #1   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #1   ")])))
      Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #1   ")] <- as.character(unique(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #1   ")]))
      
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
        #print(combine_flags)
      } # for
      Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #1   ")] <- as.character(combine_flags) # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # else
    #Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #1   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #1   ")])))
    rm(all_flags)
  } #else {print('Nope, column is not here')} # if(" Unk   Misc     #1   " %in% colnames(date_all_Fire_Cache_data)) {
  
  #input " l/m   Ave.   Air Flw" and "           flg. l/m   Ave.   Air Flw"
  Daily_Fire_Cache[date_counter,c(" l/m   Ave.   Air Flw")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" l/m   Ave.   Air Flw")])))
  #Daily_Fire_Cache[date_counter,c("           flg. l/m   Ave.   Air Flw")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. l/m   Ave.   Air Flw")])))
  # flag is sometimes non-numeric, so an average cannot be taken
  flag_col <- "           flg. l/m   Ave.   Air Flw"
  all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
  #print(all_flags)
  if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    Daily_Fire_Cache[date_counter,c(flag_col)] <- unique(as.character(date_all_Fire_Cache_data[,c(flag_col)]))
  } else {# there are multiple flags and they need to be stitched together
    combine_flags <- all_flags[1] # get the first flag
    #print(combine_flags)
    for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
      #print(combine_flags)
    } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
    Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(combine_flags) # input the flags
    rm(flag_counter,combine_flags) # clear variables
  } # if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
  rm(flag_col,all_flags)
  
  #input "Deg C  Av Air   Temp "                "           flg.Deg C  Av Air   Temp "
  Daily_Fire_Cache[date_counter,c("Deg C  Av Air   Temp ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("Deg C  Av Air   Temp ")])))
  #Daily_Fire_Cache[date_counter,c("           flg.Deg C  Av Air   Temp ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.Deg C  Av Air   Temp ")])))
  # flag is sometimes non-numeric, so an average cannot be taken
  flag_col <- "           flg.Deg C  Av Air   Temp "
  all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
  #print(all_flags)
  if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    Daily_Fire_Cache[date_counter,c(flag_col)] <- unique(as.character(date_all_Fire_Cache_data[,c(flag_col)]))
  } else {# there are multiple flags and they need to be stitched together
    combine_flags <- all_flags[1] # get the first flag
    #print(combine_flags)
    for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
      #print(combine_flags)
    } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
    Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(combine_flags) # input the flags
    rm(flag_counter,combine_flags) # clear variables
  } # if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
  rm(flag_col,all_flags)
  
  # input % Rel Humidity and corresponding flag
  # % Rel Humidity column may not exist in all of these files, so only fill it in when it exists:
  if("  %     Rel   Humidty" %in% colnames(date_all_Fire_Cache_data)) {
    Daily_Fire_Cache[date_counter,c("  %     Rel   Humidty")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("  %     Rel   Humidty")])))
    # flag for Wind Direc is sometimes non-numeric, so an average cannot be taken
    all_flags <- unique(date_all_Fire_Cache_data[,c("           flg.  %     Rel   Humidty")]) # what are all the flags on this day?
    #print(all_flags)
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c("           flg.  %     Rel   Humidty")] <- as.character(unique(date_all_Fire_Cache_data[,c("           flg.  %     Rel   Humidty")]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
        #print(combine_flags)
      } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      Daily_Fire_Cache[date_counter,c("           flg.  %     Rel   Humidty")] <- as.character(combine_flags) # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # else
    rm(all_flags)
  } #else {print('Nope, column is not here')} # if("  %     Rel   Humidty" %in% colnames(date_all_Fire_Cache_data)) {    
  
  # Misc # 2 column does not exist in all of these files, so only fill it in when it exists:
  if (" Unk   Misc     #2   " %in% colnames(date_all_Fire_Cache_data)) {
    Daily_Fire_Cache[date_counter,c(" Unk   Misc     #2   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Unk   Misc     #2   ")])))
    # flag for Unk Misc #2 is sometimes non-numeric, so an average cannot be taken
    all_flags <- unique(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #2   ")]) # what are all the flags on this day?
    #print(all_flags)
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      #Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #2   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #2   ")])))
      Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #2   ")] <- as.character(unique(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #2   ")]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
        #print(combine_flags)
      } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #2   ")] <- as.character(combine_flags) # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    rm(all_flags)
  } # if (" Unk   Misc     #2   " %in% colnames(date_all_Fire_Cache_data)) {
  
  # "mbar   Barom   Press " and "           flg.mbar   Barom   Press " columns do not exist in all of these files, so only fill it in when it exists:
  col_interest <- "mbar   Barom   Press "
  if(col_interest %in% colnames(date_all_Fire_Cache_data)){
    Daily_Fire_Cache[date_counter,c(col_interest)] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(col_interest)])))
    # flag is sometimes non-numeric, so an average cannot be taken
    flag_col <- "           flg.mbar   Barom   Press "
    all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(unique(date_all_Fire_Cache_data[,c(flag_col)]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
      } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(combine_flags) # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    rm(flag_col,all_flags)
  } # if (col_interest %in% colnames(date_all_Fire_Cache_data)){
  rm(col_interest)
  
  # input "deg C Sensor  Int AT " and "           flg.deg C Sensor  Int AT " (not all files have this collumn)
  if("deg C Sensor  Int AT " %in% colnames(date_all_Fire_Cache_data)) {
    Daily_Fire_Cache[date_counter,c("deg C Sensor  Int AT ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("deg C Sensor  Int AT ")])))
    # flag for deg C Sensor  Int AT is sometimes non-numeric, so an average cannot be taken
    all_flags <- unique(date_all_Fire_Cache_data[,c("           flg.deg C Sensor  Int AT ")]) # what are all the flags on this day?
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c("           flg.deg C Sensor  Int AT ")] <- as.character(unique(date_all_Fire_Cache_data[,c("           flg.deg C Sensor  Int AT ")]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
      } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      Daily_Fire_Cache[date_counter,c("           flg.deg C Sensor  Int AT ")] <- as.character(combine_flags) # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    rm(all_flags)
  } # if("deg C Sensor  Int AT " %in% colnames(date_all_Fire_Cache_data)) {
  
  # input %   Sensor  Int RH and corresponding flag: "  %   Sensor  Int RH " and "           flg.  %   Sensor  Int RH "
  # %   Sensor  Int RH column may not exist in all of these files, so only fill it in when it exists:
  if("  %   Sensor  Int RH " %in% colnames(date_all_Fire_Cache_data)) {
    Daily_Fire_Cache[date_counter,c("  %   Sensor  Int RH ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("  %   Sensor  Int RH ")])))
    # flag for % Sensor Int RH is sometimes non-numeric, so an average cannot be taken
    all_flags <- unique(date_all_Fire_Cache_data[,c("           flg.  %   Sensor  Int RH ")]) # what are all the flags on this day?
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c("           flg.  %   Sensor  Int RH ")] <- as.character(unique(date_all_Fire_Cache_data[,c("           flg.  %   Sensor  Int RH ")]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
      } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      Daily_Fire_Cache[date_counter,c("           flg.  %   Sensor  Int RH ")] <- as.character(combine_flags) # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    rm(all_flags)
  } # if("  %   Sensor  Int RH " %in% colnames(date_all_Fire_Cache_data)) {
  
  # input " m/s    Wind    Speed" and "           flg. m/s    Wind    Speed"
  Daily_Fire_Cache[date_counter,c(" m/s    Wind    Speed")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" m/s    Wind    Speed")])))
  # flag is sometimes non-numeric, so an average cannot be taken
  flag_col <- "           flg. m/s    Wind    Speed"
  all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
  if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(unique(date_all_Fire_Cache_data[,c(flag_col)]))
  } else {# there are multiple flags and they need to be stitched together
    combine_flags <- all_flags[1] # get the first flag
    for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
    } # for
    Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(combine_flags) # input the flags
    rm(flag_counter,combine_flags) # clear variables
  } # else
  rm(flag_col,all_flags)
  #rm(col_interest)
  
  # input " Deg   Wind    Direc " and "           flg. Deg   Wind    Direc "
  # input Wind direction and corresponding flag
  # Wind direction column may not exist in all of these files, so only fill it in when it exists:
  if(" Deg   Wind    Direc " %in% colnames(date_all_Fire_Cache_data)) {
    Daily_Fire_Cache[date_counter,c(" Deg   Wind    Direc ")] <- NA # average wind direction calculation is slightly more complicated than a direct average, so not doing that for now
    # flag for Wind Direc is sometimes non-numeric, so an average cannot be taken
    all_flags <- unique(date_all_Fire_Cache_data[,c("           flg. Deg   Wind    Direc ")]) # what are all the flags on this day?
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c("           flg. Deg   Wind    Direc ")] <- as.character(unique(date_all_Fire_Cache_data[,c("           flg. Deg   Wind    Direc ")]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
      } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      Daily_Fire_Cache[date_counter,c("           flg. Deg   Wind    Direc ")] <- as.character(combine_flags) # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # else
    rm(all_flags)
  } # if(" Deg   Wind    Direc " %in% colnames(date_all_Fire_Cache_data)) {
  
  # input "volts Battery Voltage" and "           flg.volts Battery Voltage"
  Daily_Fire_Cache[date_counter,c("volts Battery Voltage")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")])))
  # flag is sometimes non-numeric, so an average cannot be taken
  if (max(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")])))>voltage_threshold_upper|min(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")])))<voltage_threshold_lower) {
    added_flags <- c(max(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")]))),min(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")]))))
  } else {added_flags <- 0} # if (max(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")])))>voltage_threshold_upper|min(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")])))<voltage_threshold_lower) {
  flag_col <- "           flg.volts Battery Voltage"
  all_flags <- paste
  all_flags <- unique(c(as.character(date_all_Fire_Cache_data[,c(flag_col)]),as.character(added_flags))) # what are all the flags on this day?
  #print(all_flags)
  if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(unique(date_all_Fire_Cache_data[,c(flag_col)]))
  } else {# there are multiple flags and they need to be stitched together
    combine_flags <- all_flags[1] # get the first flag
    for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
    } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
    Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(combine_flags) # input the flags
    rm(flag_counter,combine_flags,all_flags) # clear variables
  } # else
  rm(flag_col,added_flags)
  
  # "      Alarm          "                "           flg.      Alarm          "
  # input Alarm variable and corresponding flag
  # Alarm column may not exist in all of these files, so only fill it in when it exists:
  if("      Alarm          " %in% colnames(date_all_Fire_Cache_data)) {
    Daily_Fire_Cache[date_counter,c("      Alarm          ")] <-  mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("      Alarm          ")])))
    # flag for Wind Direc is sometimes non-numeric, so an average cannot be taken
    all_flags <- unique(date_all_Fire_Cache_data[,c("           flg.      Alarm          ")]) # what are all the flags on this day?
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c("           flg.      Alarm          ")] <- as.character(unique(date_all_Fire_Cache_data[,c("           flg.      Alarm          ")]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
      } # for
      Daily_Fire_Cache[date_counter,c("           flg.      Alarm          ")] <- as.character(combine_flags) # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # else
    rm(all_flags)
  } # if("      Alarm          " %in% colnames(date_all_Fire_Cache_data)) {
  
  # input the number of negative values "N_neg"                                
  Daily_Fire_Cache[date_counter,c("N_neg")] <- sum_negative
  rm(sum_negative)
  
  # input the number of observations "N_Obs"
  Daily_Fire_Cache[date_counter,c("N_Obs")] <- N_obs_this_day 
  rm(N_obs_this_day)
  
  # input max conc for day and what time it occurred
  Daily_Fire_Cache[date_counter,c("1st_Max_Value")] <- max_conc_this_day
  Daily_Fire_Cache[date_counter,c("1st_Max_Hour")] <- as.character(when_max_conc[1])#when_max_conc
  rm(when_max_conc,max_conc_this_day)
  #Note: if the maximum concentration is repeated the time of the first occurrence is recorded in 1st_Max_Hour refer
  
} # end of Fire_Cache_1_day_ave.fn function