process_PM25_CARB_Mobile_data_source.fn <- function(input_header, ProcessedData.directory, CARBMobile.directory, data_set_counter, this_plotting_color = "red") {
library(lubridate)
  # combine CARB Mobile. PM2.5 data files into 1 dataframe

#### Pull in CARB Mobile data #################
  data_source_counter <- data_set_counter # counter to distinguish between the various data sources (differentiate by color on  maps)
  Data_Source_Name_Short <- "CARBMobile"
  Data_Source_Name_Display <- "CARB Mobile Monitor "
  this_Datum <- "NeedToFindOut" # 
  print("**** Still need to find out what datum was used ***")
   
##### Create Sink output file and create its header ####
  file_sub_label <- paste("PM25_",Data_Source_Name_Short,"_Step1_part_",processed_data_version,sep = "")
  SinkFileName=file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,"_combining_sink.txt",sep = ""))

  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
  cat("Code and R output for process_PM25_CARB_Mobile_data_source_function.R \n \n")
  cat("Title: process_PM25_CARB_Mobile_data_source_function.R \n")
  cat("Author: Melissa May Maestas, PhD \n")
  cat("Original Date: June 18, 2019 \n")
  cat("Latest Update: June 20, 2019 \n")
  cat(paste("Script ran and this text file created ",Sys.time(),"\n",sep = ""))
  cat("This program reads in and PM2.5 data from the CARB Mobile Monitor Data \n")

  #### Create data frame  ####
  input_mat1 <- data.frame(matrix(NA,nrow=0,ncol=length(input_header))) # create data frame for input_mat1
  names(input_mat1) <- input_header # assign the header to input_mat1
  input_mat1 <- input_mat_change_data_classes.fn(input_mat1)

  # what files are in the CARBMobile.directory?
  # https://stat.ethz.ch/R-manual/R-devel/library/base/html/list.files.html
  all_CARBMobile_Files <- list.files(path = file.path(define_file_paths.fn("CARBMobile.directory"),"."), pattern = NULL, all.files = FALSE,
                              full.names = FALSE, recursive = FALSE,
                              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  print(all_CARBMobile_Files)
  # open and process each file
  lapply_output <- lapply(1:length(all_CARBMobile_Files), function(this_file_counter) { # start lapply function
    
    this_source_file <- all_CARBMobile_Files[this_file_counter]
    print(paste('this_file_counter = ',this_file_counter,"; ",this_source_file, sep = "")) 
    
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
      # fill in rows of data with missing lat/lon data
      this_CARB_Mobile_data_step[(obs_row_i+1):(next_obs_row-1),c("Latitude")] <- this_CARB_Mobile_data_step[obs_row_i,c("Latitude")] # fill in latitude
      this_CARB_Mobile_data_step[(obs_row_i+1):(next_obs_row-1),c("Longitude")] <- this_CARB_Mobile_data_step[obs_row_i,c("Longitude")] # fill in longitude
    } # for (counter_i in 1:length(which_lat_lon)) { # cycle through the rows with lat/lon obs and fill in the rest of the rows
    } # if (length(which_lat_lon)>0) { # check that file is not empty
    
    # handle time information from TimeStamp column
    date_format <- determine_date_format.fn(check_date = this_CARB_Mobile_data_step[1,c("TimeStamp")])
    #this_CARB_Mobile_data_step$TimeStampParsed <- parse_date_time(this_CARB_Mobile_data_step$TimeStamp, "%m/%d/%Y H:M:S !p", tz = "UTC") # see pages 41-42 of https://cran.r-project.org/web/packages/lubridate/lubridate.pdf
    this_CARB_Mobile_data_step$TimeStampParsed <- parse_date_time(this_CARB_Mobile_data_step$TimeStamp, date_format, tz = "UTC") # see pages 41-42 of https://cran.r-project.org/web/packages/lubridate/lubridate.pdf
    rm(date_format)
    this_CARB_Mobile_data_step$TimeStampTruncated <- update(this_CARB_Mobile_data_step$TimeStampParsed, minute = 0, second = 0)
    this_CARB_Mobile_data_step$TimeStampLocal <- with_tz(this_CARB_Mobile_data_step$TimeStampTruncated, tz = "America/Los_Angeles") # time zone needed: "America/Los_Angeles"
    
    # handle time data from Date.Time.GMT column, if present, and check that date-times from different columns are consistent
    if ("Date.Time.GMT" %in% names(this_CARB_Mobile_data_step)) { # handle time data from Date.Time.GMT column, if present, and check that date-times from different columns are consistent
    date_format <- determine_date_format.fn(check_date = this_CARB_Mobile_data_step[1,c("Date.Time.GMT")])
    #this_CARB_Mobile_data_step$Date.Time.GMT.Parsed <- parse_date_time(this_CARB_Mobile_data_step$Date.Time.GMT, "%m/%d/%Y H:M:S !p", tz = "UTC") # see pages 41-42 of https://cran.r-project.org/web/packages/lubridate/lubridate.pdf
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
                     "TimeStampParsed","TimeStampTruncated","TimeStampLocal","Date.Time.GMT.Parsed","Date.Time.Local")
    
    #header_lapply <- unlist(lapply(1:length(goal_header), function(x){
    for (x in 1:length(goal_header)) {
      this_header <- goal_header[x]
      if (this_header %in% names(this_CARB_Mobile_data_step)) {
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
      } else {  
        stop(paste(this_header,"is not in data header for ",this_source_file))
      }
      
    } # end of for  
    #})) # end of header_lapply
    
    #this_CARB_Mobile_data_step <- subset(this_CARB_Mobile_data_step, select=-c("Oceaneering.Unit.Voltage"))
 
    return(this_CARB_Mobile_data_step) # return names of files processed
  }) # end lapply function
  Merged_CARB_Mobile <- do.call("rbind", lapply_output) #concatinate the output from each iteration
  

#     # load monitor name
#     this_name <- as.character(read.csv(file.path(define_file_paths.fn("CARBMobile.directory"),this_source_file),header = F,nrows = 1)[1,1])
#     #print(this_name)
#     print(paste('this_file_counter = ',this_file_counter,": ",this_source_file,"; Monitor name = ",this_name,sep = ""))
#     
#     # get file header, consolidated
#     one_row_header <- CARB_Mobile_consolidate_file_header.fn(define_file_paths.fn("CARBMobile.directory"),this_source_file)
#     
    
#     # attach the header compiled in the for loop above to the data
#     names(this_CARB_Mobile_data_step) <- one_row_header
#     rm(one_row_header)
#     
#     # some of the files have a slightly different name for the PM2.5 concentration column (and it's flag column), so this needs to be replaces so the files can be merged
#     this_CARB_Mobile_data_step <- replace_column_names.fn(df_in = this_CARB_Mobile_data_step, old_col_name = "ug/m3 Conc    Hly Av ",new_col_name = "ug/m3 Conc     RT    ")
#     this_CARB_Mobile_data_step <- replace_column_names.fn(df_in = this_CARB_Mobile_data_step, old_col_name = "           flg.ug/m3 Conc    Hly Av ",new_col_name = "           flg.ug/m3 Conc     RT    ")
#     
#     # not all CARB Mobile files have the same header; create a comprehensive header, adding columns that are in the current
#     # file that have not been in previous files. Also, add a few columns to the header
#     if(this_file_counter == 1) {
#     comprehensive_header <- CARB_Mobile_comprehensive_header.fn(this_file_counter, this_CARB_Mobile_data_step)
#     } else {
#     comprehensive_header <- CARB_Mobile_comprehensive_header.fn(this_file_counter, this_CARB_Mobile_data_step, comprehensive_header = comprehensive_header)
#     }
#     
#     # The header is (sometimes/always?) repeated further down in the data. These rows need to be found and removed.
#     this_CARB_Mobile_data_step2 <- CARB_Mobile_remove_repeat_headers.fn(this_CARB_Mobile_data_step)
#     rm(this_CARB_Mobile_data_step) # clear variables
#     
#     # Change class of various columns, e.g., get it to recognize dates as dates, etc
#     this_CARB_Mobile_data_step3 <- CARB_Mobile_change_data_classes.fn(this_CARB_Mobile_data_step2)
#     rm(this_CARB_Mobile_data_step2)
#     
#     # some longitudes are positive, but should be negative - make them all negative and 
#     # note which data sets were changed
#     this_CARB_Mobile_data <- CARB_Mobile_negative_longitudes.fn(this_CARB_Mobile_data_step3,this_source_file)
#     rm(this_CARB_Mobile_data_step3)
#     
#     #### take 24-hr averages for this 1 file
#     #Daily_CARB_Mobile <- CARB_Mobile_daily_averages.fn(this_CARB_Mobile_data,comprehensive_header)
#     Daily_CARB_Mobile <- CARB_Mobile_daily_averages.fn(this_CARB_Mobile_data = this_CARB_Mobile_data,comprehensive_header = comprehensive_header)
#     rm(this_CARB_Mobile_data)
#     
#     #### Input CARB Mobile data into input_mat
#     # one_file_small_input_mat <- CARB_Mobile_1_file_to_small_input_mat.fn(Daily_CARB_Mobile, input_header, this_name,
#     #                                                                     this_Datum, this_plotting_color = this_plotting_color, this_source_file = this_source_file,
#     #                                                                     Data_Source_Name_Display = Data_Source_Name_Display, Data_Source_Name_Short = Data_Source_Name_Short,
#     #                                                                     data_source_counter = data_source_counter)
#     
#     one_file_small_input_mat <- CARB_Mobile_1_file_to_small_input_mat.fn(Daily_CARB_Mobile = Daily_CARB_Mobile, input_header = input_header, this_name = this_name, 
#                                                                         this_Datum = this_Datum, this_plotting_color = this_plotting_color, this_source_file = this_source_file, 
#                                                                         Data_Source_Name_Display = Data_Source_Name_Display, Data_Source_Name_Short = Data_Source_Name_Short,
#                                                                         data_source_counter = data_source_counter)
#     
#     # write code to concatinate data from the various files
#     input_mat1 <- rbind(input_mat1,one_file_small_input_mat)
#     rm(one_file_small_input_mat)
#     #print(paste("Done processing ",this_source_file))
#     rm(this_source_file)
#   } # for (this_file_counter in 1:length(all_CARBMobile_Files)){
#   rm(all_CARBMobile_Files,this_file_counter,comprehensive_header)
#   
#   print(paste("This data has",dim(input_mat1)[1],"rows of PM2.5 observations.")) # how many rows of data?
#   print(paste("finished processing ", Data_Source_Name_Display))
#   sink() # stop outputting to sink file
#   
#   # output to file #  
#   #write.csv(input_mat1,file = file.path(ProcessedData.directory,paste(Data_Source_Name_Short,"_",Sys.Date(),'_Step1_part_',processed_data_version,'.csv',sep = "")),row.names = FALSE)
#   write.csv(input_mat1,file = file.path(ProcessedData.directory,sub_folder,paste(file_sub_label,'.csv',sep = "")),row.names = FALSE)
#   
#   # clear variables
#   #rm(Data_Source_Name_Display,Data_Source_Name_Short)
#   rm(this_Datum) 
#   
# #### output input_mat1 from function ####  
  return(input_mat1) # output from function
} # end function