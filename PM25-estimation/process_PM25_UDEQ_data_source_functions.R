#process_PM25_UDEQ_data_source_functions.R

process_PM25_UDEQ_data_source.fn <- function(input_header, data_set_counter, this_plotting_color) {
  # combine UDEQ PM2.5 data files into 1 dataframe
  
  data_source_counter <- data_set_counter#data_source_counter + 1 # counter to distinguish between the various data sources
  Data_Source_Name_Short <- "UtahDEQ"
  Data_Source_Name_Display <- "Utah DEQ"
  
  # Create Sink output file and create its header
  file_sub_label <- paste("PM25_",Data_Source_Name_Short,"_Step1_part_",processed_data_version,sep = "")
  SinkFileName=file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,"_combining_sink.txt",sep = ""))
  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # divert output from console to sink file
  cat("Code and R output for process_PM25_UDEQ_data_source_function.R \n \n")
  cat("Title: process_PM25_UDEQ_data_source_function.R \n")
  cat("Author: Melissa May Maestas, PhD \n")
  cat("Original Date: October 14, 2018 \n")
  cat("Latest Update: July 3, 2019 \n")
  cat(paste("Script ran and this text file created ",Sys.time()," \n",sep = ""))
  cat("This program reads in and PM2.5 data from the UDEQ. \n")
  
  #### Pull in Utah DEQ PM2.5 data ####
  UTDEQ_units <- "UG/M3"
  #  site location - see documentation for sources of this info
  UT_site_loc <- data.frame(matrix(NA,nrow=3,ncol=14)) # create data frame 
  names(UT_site_loc) <- c("EPACode","Latitude","Longitude","StateCode","CountyCode","SiteNum","POC","County_Name","Parameter_Code","Parameter_Name","Sample_Duration","Address","City_Name","State_Abbrev")
  UT_site_loc[1,1:3] <- c(490490002,40.253611,-111.663056) # see documentation for source of lat/lon for this site
  UT_site_loc[1,c("POC","Parameter_Name","County_Name","Parameter_Code","Sample_Duration","Address","City_Name","State_Abbrev")] <- c(NA,"PM2.5 - Local Conditions","Utah",NA,"1 HOUR","1355 NORTH 200 WEST PROVO UT","Provo","UT")
  
  
  UT_site_loc[2,1:3] <- c(490530007,37.179125,-113.305096)
  UT_site_loc[2,c("POC","Parameter_Name","County_Name","Parameter_Code","Sample_Duration","Address","City_Name","State_Abbrev")] <- c(NA,"PM2.5 - Local Conditions","Washington",NA,"1 HOUR","147 N 870 W, Hurrricane, Utah","Hurricane","UT")
  
  
  UT_site_loc[3,1:3] <- c(490130002,40.2941780318,-110.00973229)
  UT_site_loc[3,c("POC","Parameter_Name","County_Name","Parameter_Code","Sample_Duration","Address","City_Name","State_Abbrev")] <- c(NA,"PM2.5 - Local Conditions","Duchesne",NA,"1 HOUR","290 S. 1000 W.","Roosevelt","UT")
  
  Datum_used = "NAD27" # see email from Ellen on May 29, 2018
  
  # fill in State Code, County Code, and Site Num
  for (this_row in 1:dim(UT_site_loc)[1]) { # cycle through each row in UT_site_loc data to determine state code, county code, and site num
    this_EPACode <- as.character((UT_site_loc[this_row,c("EPACode")])) # isolate the EPA code for this row of data
    if (is.na(this_EPACode)==TRUE) {
      UT_site_loc[this_row,c("StateCode")] <- NA
      UT_site_loc[this_row,c("CountyCode")] <- NA
      UT_site_loc[this_row,c("SiteNum")] <- NA
    } else if (nchar(this_EPACode)==8) { # determine how many characters are in EPACode (leading zeros are not in the data)
      UT_site_loc[this_row,c("StateCode")] <- substr(this_EPACode,1,1) # isolate state code
      UT_site_loc[this_row,c("CountyCode")] <- substr(this_EPACode,2,4) # isolate county code
      UT_site_loc[this_row,c("SiteNum")] <- substr(this_EPACode,5,8)  # isolate site num
    } else if (nchar(this_EPACode)==9) {
      UT_site_loc[this_row,c("StateCode")] <- substr(this_EPACode,1,2) # isolate state code
      UT_site_loc[this_row,c("CountyCode")] <- substr(this_EPACode,3,5) # isolate county code
      UT_site_loc[this_row,c("SiteNum")] <- substr(this_EPACode,6,9)  # isolate site num
    } else {# if (nchar(this_EPACode)==8) { # unexpected input
      stop("check data/code")
    } # for (this_row in 1:dim(UT_site_loc)[1]) { # cycle through each row in UT_site_loc data to determine state code, county code, and site num
    rm(this_EPACode)
  } # for (this_row in row_start:row_stop) { # cycle through each row in UT_site_loc data to determine state code, county code, and site num and put into input_mat1
  rm(this_row)
  
  this_source_file <- 'Utah_state-only_data.csv'
  print(this_source_file)
  
  UTDEQ_data<-read.csv(file.path(define_file_paths.fn("UTDEQ.directory"),this_source_file),header=TRUE,skip = 1) # load the UT DEQ file
  
  # create and fill in data frame for 24-hr data (originally hourly data)
  date_station <- data.frame(matrix(NA,nrow = dim(UTDEQ_data)[1], ncol = 2)) # create empty matrix
  all_date_times <- as.Date(UTDEQ_data$Date,"%m/%d/%Y") # get dates in UT DEQ data
  date_station[,1] <- all_date_times # fill in dates (with repeats) into date_station
  date_station[,2] <- UTDEQ_data$Station # fill in station names into date_station
  rm(all_date_times) # clear variables
  
  unique_date_station <- date_station[!duplicated(date_station[,c(1,2)]),] # figure out how many unique station-days are in the DEQ data
  rm(date_station)
  
  UTDEQ_data$X <- as.Date(UTDEQ_data$Date,format = "%m/%d/%Y") # fill in dates (without times) into an empty column in UTDEQ_data
  
  UTDEQ_24hr_ave <- data.frame(matrix(NA,nrow = dim(unique_date_station)[1],ncol = 20)) # create data frame
  names(UTDEQ_24hr_ave) <- c("Date","Station","PM25Conc","EPACode","Latitude","Longitude","StateCode","CountyCode","SiteNum","N_Obs","PercentObs","N_neg","POC","County_Name","Parameter_Code","Parameter_Name","Sample_Duration","Address","City_Name","State_Abbrev") # assign the header            
  
  UTDEQ_24hr_ave$Date <- unique_date_station[,1] # Date
  UTDEQ_24hr_ave$Station <- unique_date_station[,2] # Station
  rm(unique_date_station)    
  # fill in 24hr averages in UTDEQ_24hr_ave
  for (this_row in 1:dim(UTDEQ_24hr_ave)[1]) { # fill in 24hr averages in UTDEQ_24hr_ave
    # get Date for this row 
    this_date <- UTDEQ_24hr_ave[this_row,c("Date")]
    #get Station for this row of data
    this_station <- UTDEQ_24hr_ave[this_row,c("Station")]
    
    # figure out which rows in UTDEQ_data correspond to this date and station
    which_this_date_station <- which(UTDEQ_data$X==this_date & UTDEQ_data$Station==this_station)
    rm(this_date,this_station)
    if (length(which_this_date_station)>24) {stop("too many rows of data picked up, check code and data")} # check on data/code
    
    #PM25Conc
    these_PM25 <- UTDEQ_data[which_this_date_station,c("UG.M3")] # isolate PM2.5 data from this date/location
    which_negative <- which(these_PM25<0)
    which_not_NA <- which(!is.na(these_PM25))
    UTDEQ_24hr_ave[this_row,c("PM25Conc")] <- mean(these_PM25[which_not_NA])
    UTDEQ_24hr_ave[this_row,c("N_Obs")] <- length(which_not_NA) #length(which_positive)+length(which_negative)
    UTDEQ_24hr_ave[this_row,c("PercentObs")] <- length(which_not_NA)/24*100 #length(which_positive)+length(which_negative)
    UTDEQ_24hr_ave[this_row,c("N_neg")] <- length(which_negative)
    rm(which_negative,which_not_NA,these_PM25)
    
    #Location: "StateCode"
    this_EPACode <- unique(UTDEQ_data[which_this_date_station,c("EPA.code")])
    UTDEQ_24hr_ave[this_row,c("EPACode")] <- this_EPACode
    this_state_code <- UT_site_loc[which(UT_site_loc$EPACode==this_EPACode),c("StateCode")]
    UTDEQ_24hr_ave[this_row,c("StateCode")] <- this_state_code
    
    which_UT_site_loc <- which(UT_site_loc$EPACode==this_EPACode)
    UTDEQ_24hr_ave[this_row,c("CountyCode")] <- UT_site_loc[which_UT_site_loc,c("CountyCode")]
    UTDEQ_24hr_ave[this_row,c("SiteNum")] <- UT_site_loc[which_UT_site_loc,c("SiteNum")]
    UTDEQ_24hr_ave[this_row,c("Latitude")] <- UT_site_loc[which_UT_site_loc,c("Latitude")]
    UTDEQ_24hr_ave[this_row,c("Longitude")] <- UT_site_loc[which_UT_site_loc,c("Longitude")]
    
    UTDEQ_24hr_ave[this_row,c("POC")] <- UT_site_loc[which_UT_site_loc,c("POC")]
    UTDEQ_24hr_ave[this_row,c("County_Name")] <- UT_site_loc[which_UT_site_loc,c("County_Name")]
    UTDEQ_24hr_ave[this_row,c("Parameter_Code")] <- UT_site_loc[which_UT_site_loc,c("Parameter_Code")]
    UTDEQ_24hr_ave[this_row,c("Parameter_Name")] <- UT_site_loc[which_UT_site_loc,c("Parameter_Name")]
    UTDEQ_24hr_ave[this_row,c("Sample_Duration")] <- UT_site_loc[which_UT_site_loc,c("Sample_Duration")]
    UTDEQ_24hr_ave[this_row,c("Address")] <- UT_site_loc[which_UT_site_loc,c("Address")]
    UTDEQ_24hr_ave[this_row,c("City_Name")] <- UT_site_loc[which_UT_site_loc,c("City_Name")]
    UTDEQ_24hr_ave[this_row,c("State_Abbrev")] <- UT_site_loc[which_UT_site_loc,c("State_Abbrev")]
    
    rm(which_this_date_station,which_UT_site_loc)
    rm(this_EPACode,this_state_code)
  } # for (this_row in 1:dim(UTDEQ_24hr_ave)[1]) { # fill in 24hr averages in UTDEQ_24hr_ave
  rm(UT_site_loc,this_row)
  
  # incorporate more recent UT DEQ files, which are already daily values
  recent_source_files <- c("UT-PM2.5-2015.csv","UT-PM2.5-2016.csv","UT-PM2.5-2017.csv","UT-PM2.5-2018.csv")
  full_UTDEQ_data <- merge_recent_UTDEQ_files.fn(recent_source_files = recent_source_files, UTDEQ_data_in = UTDEQ_24hr_ave, UTDEQ.directory = define_file_paths.fn("UTDEQ.directory")) 
  rm(UTDEQ_24hr_ave)
  UTDEQ_24hr_ave <- full_UTDEQ_data
  rm(full_UTDEQ_data)
  
  # Create input_mat1 data frame
  input_mat1 <- data.frame(matrix(NA,nrow=dim(UTDEQ_24hr_ave)[1],ncol=length(input_header))) # create data frame for input_mat1
  names(input_mat1) <- input_header # assign the header to input_mat1
  input_mat1 <- input_mat_change_data_classes.fn(input_mat1)
  
  ## fill in input_mat1
  
  # input 'State_Code' into input_mat1
  input_mat1$State_Code <- UTDEQ_24hr_ave$StateCode
  
  # input 'County_Code' into input_mat1
  input_mat1$County_Code <- UTDEQ_24hr_ave$CountyCode
  
  # input 'Site_Num' into input_mat1
  input_mat1$Site_Num <- UTDEQ_24hr_ave$SiteNum
  
  # input 'Parameter_Code' into input_mat1
  input_mat1$Parameter_Code <- UTDEQ_24hr_ave$Parameter_Code
  
  # input 'POC' into input_mat1
  input_mat1$POC <- UTDEQ_24hr_ave$POC
  
  # input latitude and longitude ('PM2.5_Lat','PM2.5_Lon')
  input_mat1$PM2.5_Lat <- UTDEQ_24hr_ave$Latitude
  input_mat1$PM2.5_Lon <- UTDEQ_24hr_ave$Longitude
  
  # input 'Datum' into input_mat1
  input_mat1$Datum <- Datum_used
  #this_col <- 'Datum'
  #AQSVar <- UTDEQ_data[,c(this_col)]
  #AQSVarChar <- as.character(AQSVar)
  #input_mat1[row_start:row_stop,c(this_col)] <- # Not sure what to put for datum
  #rm(this_col,AQSVar,AQSVarChar)
  
  # input 'Parameter_Name' into input_mat1
  #this_col_input_mat <- 'Parameter_Name'
  #this_col_AQS <- 'Parameter.Name'
  #AQSVar <- UTDEQ_data[,c(this_col_AQS)]
  #AQSVarChar <- as.character(AQSVar)
  #input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
  input_mat1$Parameter_Name <- UTDEQ_24hr_ave$Parameter_Name
  #rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
  
  # input "Sample_Duration" into input_mat1
  #this_col_input_mat <- "Sample_Duration"
  #this_col_AQS <- 'Sample.Duration'
  #AQSVar <- UTDEQ_data[,c(this_col_AQS)]
  #AQSVarChar <- as.character(AQSVar)
  input_mat1$Sample_Duration <- UTDEQ_24hr_ave$Sample_Duration # not sure if it's 24-hr data or hourly
  #rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
  
  # input 'Pollutant_Standard' into input_mat1
  #this_col_input_mat <- 'Pollutant_Standard'
  #this_col_AQS <- 'Pollutant.Standard'
  #AQSVar <- UTDEQ_data[,c(this_col_AQS)]
  #AQSVarChar <- as.character(AQSVar)
  #input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar # not sure what to put here
  #rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
  
  # input 'Date_Local' into input_mat1
  #input_mat1$Date_Local <- as.Date(UTDEQ_24hr_ave[,c(this_col_source)], format = "%m/%d/%y HH:MM")
  input_mat1$Date_Local <- as.Date(UTDEQ_24hr_ave[,c("Date")], format = "%m/%d/%Y")
  #old: this_col_input_mat <- 'Date_Local'
  #old: this_col_source <- 'Date'
  #old: SourceVar <- as.Date(UTDEQ_24hr_ave[,c(this_col_source)],"%Y-%m-%d")
  #old: SourceVarChar <- format(SourceVar,"%Y-%m-%d")
  #old: input_mat1[ ,c(this_col_input_mat)] <- SourceVarChar
  #old: rm(this_col_input_mat,this_col_source,SourceVar,SourceVarChar)
  
  # input 'Units_of_Measure' into input_mat1
  input_mat1$Units_of_Measure <- UTDEQ_units#"UG/M3"
  
  # input 'Event_Type' into input_mat1
  #this_col_input_mat <- 'Event_Type'
  #this_col_AQS <- 'Event.Type'
  #AQSVar <- UTDEQ_data[,c(this_col_AQS)]
  #AQSVarChar <- as.character(AQSVar)
  #input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar # Not sure what to put for Event Type
  #rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
  
  # input 'Observation_Count' into input_mat1
  input_mat1$Observation_Count <- UTDEQ_24hr_ave$N_Obs
  
  # input 'Observation_Percent' into input_mat1
  input_mat1$Observation_Percent <- UTDEQ_24hr_ave$PercentObs
  
  # input PM2.5 concentration
  input_mat1$PM2.5_Obs <- UTDEQ_24hr_ave$PM25Conc
  
  # input '1st_Max_Value'
  #input_mat1$1st_Max_Value <- # Not sure what to put UTDEQ_data[,c("X1st.Max.Value")]
  
  # input '1st_Max_Hour'
  #input_mat1$1st_Max_Hour')] <- # Not sure what to put#UTDEQ_data[,c("X1st.Max.Hour")]
  
  # input 'AQI'
  #input_mat1$AQI')] <- # not sure what to put # UTDEQ_data[,c('AQI')]
  
  # input 'Method_Code'
  #input_mat1$Method_Code')] <- # not sure what to put #UTDEQ_data[,c('Method.Code')]
  
  # input 'Method_Name' into input_mat1
  #this_col_input_mat <- 'Method_Name'
  #this_col_AQS <- 'Method.Name'
  #AQSVar <- UTDEQ_data[,c(this_col_AQS)]
  #AQSVarChar <- as.character(AQSVar)
  #input_mat1[row_start:row_stop,c(this_col_input_mat)] <- # not sure what to put # AQSVarChar
  #rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
  
  # input 'PM25_Station_Name' into input_mat1
  input_mat1$PM25_Station_Name <- UTDEQ_24hr_ave$Station
  
  # input 'Address' into input_mat1
  input_mat1$Address <- UTDEQ_24hr_ave$Address
  
  # input 'State_Name' into input_mat1
  input_mat1$State_Name <- "Utah"
  
  # input 'County_Name' into input_mat1
  #this_col_input_mat <- 'County_Name'
  #this_col_AQS <- 'County.Name'
  #AQSVar <- UTDEQ_data[,c(this_col_AQS)]
  #print(AQSVar)
  #AQSVarChar <- as.character(AQSVar)
  #print(AQSVarChar)
  input_mat1$County_Name <- UTDEQ_24hr_ave$County_Name
  #rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
  
  # input 'City_Name' into input_mat1
  input_mat1$City_Name <- UTDEQ_24hr_ave$City_Name 
  
  # input 'CBSA_Name' into input_mat1
  #this_col_input_mat <- 'CBSA_Name'
  #this_col_AQS <- 'CBSA.Name'
  #AQSVar <- UTDEQ_data[,c(this_col_AQS)]
  #print(AQSVar)
  #AQSVarChar <- as.character(AQSVar)
  #print(AQSVarChar)
  #input_mat1[row_start:row_stop,c(this_col_input_mat)] <- # Not sure what to put here # AQSVarChar
  #rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
  
  # input 'Date_of_Last_Change' into input_mat1
  #this_col_input_mat <- 'Date_of_Last_Change'
  #this_col_AQS <- 'Date.of.Last.Change'
  #AQSVar <- as.Date(UTDEQ_data[,c(this_col_AQS)],"%Y-%m-%d")
  #print(AQSVar)
  #AQSVarChar <- format(AQSVar,"%Y-%m-%d")
  #print(AQSVarChar)
  #input_mat1[row_start:row_stop,c(this_col_input_mat)] <- # Not sure what to put here # AQSVarChar
  #rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
  
  # input 'State_Abbrev' into input_mat1
  input_mat1$State_Abbrev <- UTDEQ_24hr_ave$State_Abbrev
  
  # Note: 'Winter' is filled in near the end of the script
  
  # Note: 'Year' is filled in near the end of the script
  
  # Note: 'Month' is filled in near the end of the script
  
  # Note: 'Day' is filled in near the end of the script
  
  # input 'Data_Source_Name_Display' into input_mat1
  input_mat1$Data_Source_Name_Display <- Data_Source_Name_Display
  
  # input 'Data_Source_Name_Short' into input_mat1
  input_mat1$Data_Source_Name_Short <- Data_Source_Name_Short
  
  # input data source counter - indicates if this is EPA data or field data, etc.
  input_mat1$Data_Source_Counter <- data_source_counter
  
  # input color for this data source for plots (totally arbitrary choice)
  input_mat1$PlottingColor <- this_plotting_color
  
  # input 'Source_File' name
  input_mat1$Source_File <- this_source_file
  
  # input the 'Composite_of_N_rows' - this variable indicates how many separate rows of 
  # data were composited to form this row of data. This will be relevant when getting rid of repeated data.
  # For now, this is set to 1 because repeated rows of data will be consolidated in a later script.
  input_mat1$Composite_of_N_rows <- UTDEQ_24hr_ave$N_Obs
  
  # input 'N_Negative_Obs' into input_mat1 - this is to note negative concentrations
  #which_negative <- which(UTDEQ_data$Ranchos..PM25LC.UG.M3.<0)
  #neg_flag_vec <- c(1:dim(UTDEQ_data)[1])*0
  #neg_flag_vec[which_negative] <- 1
  #input_mat1[,c('N_Negative_Obs')] <- neg_flag_vec
  #rm(which_negative,neg_flag_vec)
  input_mat1$N_Negative_Obs <- UTDEQ_24hr_ave$N_neg
  
  # input "InDayLatDiff","InDayLonDiff" - which will all be zero for AQS data since there is only one
  # row of data for lat & lon on a given day
  # (for the DRI data, there are multiple measurements of lat/lon in a day and sometimes they don't all match, these variables give max-min for lat & lon in a given day)
  input_mat1$InDayLatDiff <- 0
  input_mat1$InDayLonDiff <- 0
  
  #"Year" 
  input_mat1[ ,c("Year")] <- input_mat_extract_year_from_date.fn(input_mat1$Date_Local)
  
  #"Month" 
  input_mat1[ ,c("Month")] <- input_mat_extract_month_from_date.fn(input_mat1$Date_Local)
  
  #"Day"  
  input_mat1[ ,c("Day")] <- input_mat_extract_day_from_date.fn(input_mat1$Date_Local)
  
  # update row counter
  #row_start=row_stop+1
  
  # clear variables before moving on
  #rm(this_source_file,UTDEQ_data)
  #rm(Data_Source_Name_Display,Data_Source_Name_Short)
  #rm(UTDEQ_24hr_ave,UTDEQ_units)
  #rm(Datum_used)
  
  print("summary of the data output:")
  print(summary(input_mat1)) # give summary of current state of data
  
  print(paste("This data has",dim(input_mat1)[1],"rows of PM2.5 observations.")) # how many rows of data?
  print(paste("finished processing ", Data_Source_Name_Display))
  sink() # stop outputting to sink file 
  
  # output to file #  
  write.csv(input_mat1,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,'.csv',sep = "")),row.names = FALSE)
  
  # output input_mat1 from function #  
  return(input_mat1) # output from function
} # end function

#merge_recent_UTDEQ_files.fn(recent_source_files = recent_source_files, UTDEQ_data_in = UTDEQ_24hr_ave, UTDEQ.directory = define_file_paths.fn("UTDEQ.directory"))
merge_recent_UTDEQ_files.fn <- function(recent_source_files, UTDEQ_data_in,UTDEQ.directory) {
  # load and merge all of the recent files since they should all have the same headers
  lapply_output <- lapply(1:length(recent_source_files), function(this_file_counter) { # start lapply function
    recent_source_file <- recent_source_files[this_file_counter]
    print(paste('this_file_counter = ',this_file_counter,"; ",recent_source_file, sep = "")) 
    this_recent_UTDEQ_data <- read.csv(file.path(UTDEQ.directory,recent_source_file)) # load data file
    return(this_recent_UTDEQ_data) # return processed data
  }) # end lapply function
  Merged_recent_UTDEQ_step1 <- do.call("rbind", lapply_output) #concatinate the output from each iteration
  rm(lapply_output)
  
  goal_header <- names(UTDEQ_data_in)
  Merged_recent_UTDEQ_step2 <- data.frame(matrix(NA,nrow = dim(Merged_recent_UTDEQ_step1)[1],ncol = length(goal_header)))
  names(Merged_recent_UTDEQ_step2) <- goal_header
  print(goal_header)
  # put the recent data in a new data frame with all of the same columns as the first file
  # "UTM.Northing" "UTM.Easting"
  
  Merged_recent_UTDEQ_step2$Date <- as.Date(Merged_recent_UTDEQ_step1$Date)
  Merged_recent_UTDEQ_step2$Station <- Merged_recent_UTDEQ_step1$Name      
  #Merged_recent_UTDEQ_step2$PM25Conc <- Merged_recent_UTDEQ_step1$X
  Merged_recent_UTDEQ_step2$PM25Conc <- Merged_recent_UTDEQ_step1$PM2.5
  Merged_recent_UTDEQ_step2$EPACode <- Merged_recent_UTDEQ_step1$Station
  Merged_recent_UTDEQ_step2$Latitude <- Merged_recent_UTDEQ_step1$Latitude
  Merged_recent_UTDEQ_step2$Longitude <- Merged_recent_UTDEQ_step1$Longitude
  #Merged_recent_UTDEQ_step2$StateCode <- Merged_recent_UTDEQ_step1$
  #Merged_recent_UTDEQ_step2$CountyCode <- Merged_recent_UTDEQ_step1$
  #Merged_recent_UTDEQ_step2$SiteNum <- Merged_recent_UTDEQ_step1$
  ##date_format <- determine_date_format.fn(check_date <- Merged_recent_UTDEQ_step2[1,c("Date")]) # figure out date format
  ##Merged_recent_UTDEQ_step2$Date <- as.Date(Merged_recent_UTDEQ_step2$Date, format = date_format) # fix class
  ##rm(date_format)
  Merged_recent_UTDEQ_step2$N_Obs <- 1 #Merged_recent_UTDEQ_step1$
  Merged_recent_UTDEQ_step2$PercentObs <- 100
  
  Merged_recent_UTDEQ_step2$N_neg <- 1
  which_neg <- which(Merged_recent_UTDEQ_step2$PM25Conc <0)
  Merged_recent_UTDEQ_step2[which_neg, "N_neg"] <- 1
  #Merged_recent_UTDEQ_step2$POC <- NA
  #Merged_recent_UTDEQ_step2$County_Name <- Merged_recent_UTDEQ_step1$
  #Merged_recent_UTDEQ_step2$Parameter_Code <- NA
  #Merged_recent_UTDEQ_step2$Parameter_Name <- NA
  #Merged_recent_UTDEQ_step2$Sample_Duration
  #Merged_recent_UTDEQ_step2$Address
  #Merged_recent_UTDEQ_step2$City_Name
  #Merged_recent_UTDEQ_step2$State_Abbrev  
    
  # remove NA rows
  which_not_na <- which(!is.na(Merged_recent_UTDEQ_step2$PM25Conc))
  Merged_recent_UTDEQ_step3 <- Merged_recent_UTDEQ_step2[which_not_na, ]
  
  # add Sample Duration - different for old vs new files
  Merged_recent_UTDEQ_step3$Sample_Duration <- "24 HOUR"
  
  # merge the recent file data frame with the UTDEQ_data_in data frame  
  UTDEQ_data_out <- rbind(UTDEQ_data_in,Merged_recent_UTDEQ_step3)
  
  return(UTDEQ_data_out)
} # end of merge_recent_UTDEQ_files.fn function
