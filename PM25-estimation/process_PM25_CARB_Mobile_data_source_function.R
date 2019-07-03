process_PM25_CARB_Mobile_data_source.fn <- function(input_header, ProcessedData.directory, CARBMobile.directory, data_set_counter, this_plotting_color = "red") {
library(lubridate) # package needed for handling dates and time zones
  # combine CARB Mobile. PM2.5 data files into 1 dataframe

#### Pull in CARB Mobile data #################
  data_source_counter <- data_set_counter # counter to distinguish between the various data sources (differentiate by color on  maps)
  Data_Source_Name_Short <- "CARBMobile"
  Data_Source_Name_Display <- "CARB Mobile Monitor "
  this_Datum <- "NAD83" # assuming it's the same datum as was used for DRI data - CARB contact (Joseph McCormack) didn't know what datum was used for CARB Mobile data
  this_time_zone <- "America/Los_Angeles"
   
##### Create Sink output file and create its header ####
  file_sub_label <- paste("PM25_",Data_Source_Name_Short,"_Step1_part_",processed_data_version,sep = "")
  SinkFileName=file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,"_combining_sink.txt",sep = ""))

  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
  cat("Code and R output for process_PM25_CARB_Mobile_data_source_function.R \n \n")
  cat("Title: process_PM25_CARB_Mobile_data_source_function.R \n")
  cat("Author: Melissa May Maestas, PhD \n")
  cat("Original Date: June 18, 2019 \n")
  cat("Latest Update: July 3, 2019 \n")
  cat(paste("Script ran and this text file created ",Sys.time(),"\n",sep = ""))
  cat("This program reads in and PM2.5 data from the CARB Mobile Monitor Data \n")
  cat("Assuming it is the same datum as was used for DRI data - CARB contact (Joseph McCormack) did not know what datum was used for CARB Mobile data \n \n")
  
  # what files are in the CARBMobile.directory?
  # https://stat.ethz.ch/R-manual/R-devel/library/base/html/list.files.html
  all_CARBMobile_Files <- list.files(path = file.path(define_file_paths.fn("CARBMobile.directory"),"."), pattern = NULL, all.files = FALSE,
                              full.names = FALSE, recursive = FALSE,
                              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  #print(all_CARBMobile_Files)
  # open and process each file - merge all CARB data into one large data frame
  lapply_output <- lapply(1:length(all_CARBMobile_Files), function(this_file_counter) { # start lapply function
    
    this_source_file <- all_CARBMobile_Files[this_file_counter]
    cat(paste('this_file_counter = ',this_file_counter,"; ",this_source_file," \n", sep = "")) 
    
    # load main part of this data file
    this_CARB_Mobile_data_step <- read.csv(file.path(define_file_paths.fn("CARBMobile.directory"),this_source_file))#,header = F,skip = 4)
    
    # fill in lat/lon to all of the rows (raw data only shows lat/lon at the beginning of a sequence of observations)
    which_lat_lon <- which(!is.na(this_CARB_Mobile_data_step$Latitude) & !is.na(this_CARB_Mobile_data_step$Longitude)) # which rows of data have lat/lon information?
    if (length(which_lat_lon)>0) { # check that file is not empty
      for (counter_i in 1:length(which_lat_lon)) { # cycle through the rows with lat/lon obs and fill in the rest of the rows
      obs_row_i <- which_lat_lon[counter_i] # get the row number of a lat/lon obs
      if (counter_i < length(which_lat_lon)) { # if-statement to handle end of file slightly different
        counter_i_plus_1 <- counter_i + 1 # count up to find the next lat/lon obs row
        next_obs_row <- which_lat_lon[counter_i_plus_1] # get the row number of the next lat/lon observation - to avoid over-writing it
      } else { # handle end of file slightly different
        counter_i_plus_1 <- NA # not needed for end of file
        next_obs_row <- dim(this_CARB_Mobile_data_step)[1]+1 # find last row number in file and add one so that the code below works whether or not it's on the last lat/lon obs
      } # if (counter_i < length(which_lat_lon)) { # if-statement to handle end of file slightly different
      rm(counter_i_plus_1)
      # fill in rows of data with missing lat/lon data
      this_CARB_Mobile_data_step[(obs_row_i+1):(next_obs_row-1),c("Latitude")] <- this_CARB_Mobile_data_step[obs_row_i,c("Latitude")] # fill in latitude
      this_CARB_Mobile_data_step[(obs_row_i+1):(next_obs_row-1),c("Longitude")] <- this_CARB_Mobile_data_step[obs_row_i,c("Longitude")] # fill in longitude
      } # for (counter_i in 1:length(which_lat_lon)) { # cycle through the rows with lat/lon obs and fill in the rest of the rows
      rm(counter_i)
      # add column with file name
      this_CARB_Mobile_data_step$FileName <- this_source_file  
    } else {
      this_CARB_Mobile_data_step$FileName <- this_CARB_Mobile_data_step$Latitude # still need to add (empty) column
    } # if (length(which_lat_lon)>0) { # check that file is not empty

    # handle time information from TimeStamp column
    date_format <- determine_date_format.fn(check_date = this_CARB_Mobile_data_step[1,c("TimeStamp")])
    this_CARB_Mobile_data_step$TimeStampParsed <- parse_date_time(this_CARB_Mobile_data_step$TimeStamp, date_format, tz = "UTC") # see pages 41-42 of https://cran.r-project.org/web/packages/lubridate/lubridate.pdf
    rm(date_format)
    this_CARB_Mobile_data_step$TimeStampTruncated <- update(this_CARB_Mobile_data_step$TimeStampParsed, minute = 0, second = 0)
    this_CARB_Mobile_data_step$TimeStampLocal <- with_tz(this_CARB_Mobile_data_step$TimeStampTruncated, tz = this_time_zone)#"America/Los_Angeles") # time zone needed: "America/Los_Angeles"
    this_CARB_Mobile_data_step$Date.Local <- as.Date(this_CARB_Mobile_data_step$TimeStampLocal, tz = this_time_zone)
    
    # handle time data from Date.Time.GMT column, if present, and check that date-times from different columns are consistent
    if ("Date.Time.GMT" %in% names(this_CARB_Mobile_data_step)) { # handle time data from Date.Time.GMT column, if present, and check that date-times from different columns are consistent
    date_format <- determine_date_format.fn(check_date = this_CARB_Mobile_data_step[1,c("Date.Time.GMT")])
    this_CARB_Mobile_data_step$Date.Time.GMT.Parsed <- parse_date_time(this_CARB_Mobile_data_step$Date.Time.GMT, date_format, tz = "UTC") # see pages 41-42 of https://cran.r-project.org/web/packages/lubridate/lubridate.pdf
    rm(date_format)
    this_CARB_Mobile_data_step$Date.Time.Local <- with_tz(this_CARB_Mobile_data_step$Date.Time.GMT.Parsed, tz = "America/Los_Angeles") # time zone needed: "America/Los_Angeles"
    TF_list <- unlist(lapply(1:dim(this_CARB_Mobile_data_step)[1], function(x){ # check if date/times from different columns are consistent
      return(this_CARB_Mobile_data_step$Date.Time.Local == this_CARB_Mobile_data_step$TimeStampLocal) # return TRUE/FALSE about whether date/times match
    })) # TF_list <- unlist(lapply(1:dim(this_CARB_Mobile_data_step)[1], function(x){ # check if date/times from different columns are consistent
    if (FALSE %in% TF_list) {stop("dates not matching, investigate.")} # make sure that the local times derived from two different columns match
    } # if ("Date.Time.GMT" %in% names(this_CARB_Mobile_data_step)) { # handle time data from Date.Time.GMT column, if present, and check that date-times from different columns are consistent
    
    # add in missing columns and change names that don't match
    goal_header <- c("MasterTable_ID","Alias","Latitude","Longitude","Date.Time.GMT","Start.Date.Time..GMT.",
                     "COncRT","ConcHr","Flow","W.S","W.D","AT","RHx","RHi","BV","FT",                   
                     "Alarm","Type","Serial.Number","Version","Sys..Volts","TimeStamp","PDate",
                     "TimeStampParsed","TimeStampTruncated","TimeStampLocal","Date.Time.GMT.Parsed",
                     "Date.Time.Local","FileName","Date.Local")
    
    for (x in 1:length(goal_header)) { # check if column is in data; add if needed
      this_header <- goal_header[x]
      if (this_header %in% names(this_CARB_Mobile_data_step)) { # is this column already in the header?
        #print(paste("column ",this_header," is in data header for this file."))# No changes needed."))
      } else if (this_header == "Date.Time.GMT" & this_header %!in% names(this_CARB_Mobile_data_step)) {
        this_CARB_Mobile_data_step$Date.Time.GMT <- NA
        print(paste("column ",this_header," is added with NAs."))
      } else if (this_header == "Start.Date.Time..GMT." & this_header %!in% names(this_CARB_Mobile_data_step)) {
        this_CARB_Mobile_data_step$Start.Date.Time..GMT. <- NA  
        print(paste("column ",this_header," is added with NAs."))
      } else if (this_header == "Serial.Number" & this_header %!in% names(this_CARB_Mobile_data_step)) {
        this_CARB_Mobile_data_step$Serial.Number <- NA
        print(paste("column ",this_header," is added with NAs."))
      } else if (this_header == "Version" & this_header %!in% names(this_CARB_Mobile_data_step)) {
        this_CARB_Mobile_data_step$Version <- NA  
        print(paste("column ",this_header," is added with NAs."))
      } else if (this_header == "Sys..Volts" & this_header %!in% names(this_CARB_Mobile_data_step)) {
        this_CARB_Mobile_data_step <- replace_column_names.fn(df_in = this_CARB_Mobile_data_step, old_col_name = "Oceaneering.Unit.Voltage", new_col_name = "Sys..Volts")
        print(paste("column 'Oceaneering.Unit.Voltage' is re-named ",this_header,"."))
      } else if (this_header == "Date.Time.GMT.Parsed" & this_header %!in% names(this_CARB_Mobile_data_step)) {
        this_CARB_Mobile_data_step$Date.Time.GMT.Parsed <- NA
        print(paste("column ",this_header," is added with NAs."))
      } else if (this_header == "Date.Time.Local" & this_header %!in% names(this_CARB_Mobile_data_step)) {
        this_CARB_Mobile_data_step$Date.Time.Local <- this_CARB_Mobile_data_step$TimeStampLocal
        print(paste("column ",this_header,"is copied from 'TimeStampLocal column."))
      } else if (this_header == "FileName" & this_header %!in% names(this_CARB_Mobile_data_step)) {
        this_CARB_Mobile_data_step$FileName <- NA
        print(paste("column ",this_header," is added with NAs."))
      } else {  
        stop(paste(this_header,"is not in data header for ",this_source_file))
      } # if (this_header %in% names(this_CARB_Mobile_data_step)) { # is this column already in the header?
    } # for (x in 1:length(goal_header)) { # check if column is in data; add if needed 
    
    # fill in voltage to all of the rows (raw data only shows value at the beginning of a sequence of observations)
    this_CARB_Mobile_data_step <- drag_values_to_next_value.fn(df_in = this_CARB_Mobile_data_step, col_interest = "Sys..Volts")
    
    return(this_CARB_Mobile_data_step) # return processed data
  }) # end lapply function
  Merged_CARB_Mobile_step1 <- do.call("rbind", lapply_output) #concatinate the output from each iteration
  rm(lapply_output)
  Merged_CARB_Mobile_step2 <- CARB_Mobile_change_data_classes.fn(Merged_CARB_Mobile = Merged_CARB_Mobile_step1) # set classes for columns
  rm(Merged_CARB_Mobile_step1)
  # only keep rows with known lat, lon, conc, and date/time
  which_data_rows <- which(!is.na(Merged_CARB_Mobile_step2$Longitude) & 
                             !is.na(Merged_CARB_Mobile_step2$Latitude) &
                             !is.na(Merged_CARB_Mobile_step2$Date.Time.Local) &
                             !is.na(Merged_CARB_Mobile_step2$ConcHr) &
                             !is.na(Merged_CARB_Mobile_step2$RHi) &
                             !is.na(Merged_CARB_Mobile_step2$Flow) &
                             !is.na(Merged_CARB_Mobile_step2$Sys..Volts))
  
  Merged_CARB_Mobile_step3 <- Merged_CARB_Mobile_step2[which_data_rows, ]
  rm(Merged_CARB_Mobile_step2,which_data_rows)
  
  Merged_CARB_Mobile_step3$ConcHr_mug_m3 <- Merged_CARB_Mobile_step3$ConcHr*1000 # change units from milli-grams/meter-cubed to micro-grams/meter-cubed

#### take 24-hr averages for this 1 file
  input_mat1 <- CARB_Mobile_daily_averages.fn(Merged_CARB_Mobile = Merged_CARB_Mobile_step3, this_plotting_color = this_plotting_color,this_Datum = this_Datum,
                                              Data_Source_Name_Display = Data_Source_Name_Display, Data_Source_Name_Short = Data_Source_Name_Short, data_set_counter = data_set_counter)
  rm(Merged_CARB_Mobile_step3)

  cat("summary of the data output: \n")
  print(summary(input_mat1)) # give summary of current state of data
  
  cat(paste("This data has",dim(input_mat1)[1],"rows of PM2.5 observations. \n")) # how many rows of data?
  cat(paste("finished processing ", Data_Source_Name_Display," \n"))
  sink() # stop outputting to sink file

# output to file #  
write.csv(input_mat1,file = file.path(ProcessedData.directory,sub_folder,paste(file_sub_label,'.csv',sep = "")),row.names = FALSE)
  
# clear variables
#   rm(this_Datum) 
  
#### output input_mat1 from function ####  
  return(input_mat1) # output from function
} # end function