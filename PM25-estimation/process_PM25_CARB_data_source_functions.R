#process_PM25_CARB_data_source_functions.R

process_PM25_CARB_data_source.fn <- function(input_header, data_set_counter, this_plotting_color) {
  # combine CARB PM2.5 data files into 1 dataframe
  
  # Create Sink output file and create its header
  SinkFileName=file.path(ProcessedData.directory,"PM25_data_source_CARB_combining_sink.txt") # name of text file for console output
  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # divert output from console to sink file
  cat("Code and R output for process_PM25_CARB_data_source_function.R \n \n")
  cat("Title: process_PM25_CARB_data_source_function.R \n")
  cat("Author: Melissa May Maestas, PhD \n")
  cat("Original Date: October 14, 2018 \n")
  cat("Latest Update: October 14, 2018 \n")
  cat(paste("Script ran and this text file created ",Sys.time()," \n",sep = ""))
  cat("This program reads in and PM2.5 data from the CARB. \n")
  
  #### Pull in new California PM2.5 data ####
  data_source_counter=data_set_counter # counter to distinguish between the various data sources
  Data_Source_Name_Short <- "CARB"
  Data_Source_Name_Display <- "CARB"
  this_Datum <- "WGS84" # per Ellen's emails with CARB - see email from 5/29/2018
  
  # load concentration data file
  this_source_file <- "2008-2014_PM25_daily_averages_20180402_all_characters.csv" # name of data source file
  print(this_source_file) # display name of source file
  CARB_data <- read.csv(file.path(CARB.directory,this_source_file), header = T, sep = ",",blank.lines.skip = F, skip = 2, encoding = 'latin1') # load CARB data
  
  # Create input_mat1 data frame
  input_mat1 <- data.frame(matrix(NA,nrow=dim(CARB_data)[1],ncol=length(input_header))) # create data frame for input_mat1
  names(input_mat1) <- input_header # assign the header to input_mat1
  input_mat1 <- input_mat_change_data_classes.fn(input_mat1)
  
  # Load location meta-data files
  this_meta_data_file <- "Site_Info_for_Bob_Weller-May_29_2018.csv" # name of first meta-data file
  print(this_meta_data_file) # display name of 1st meta-data file
  CARB_meta_data <- read.csv(file.path(CARB.directory,this_meta_data_file), header = T, sep = ",") # load first meta-data file
  #CARB_meta_data$Latitude <- as.numeric(as.character(CARB_meta_data$Latitude)) # set class
  #CARB_meta_data$Longitude <- as.numeric(CARB_meta_data$Longitude) # set class
  rm(this_meta_data_file) # clear variable
  this_meta_data_file <- "Missing_Site_Info_for_Ellen-6-4-18.csv" # name of second meta-data file
  print(this_meta_data_file)
  second_meta_data_file <- read.csv(file.path(CARB.directory,this_meta_data_file), header = T, sep = ",") # load second meta-data file
  rm(this_meta_data_file) # clear variable
  
  ### compile all location info between CARB_data and CARB_meta_data

  
  ### fill in each column of input_mat1 ###
  # Split FMLE EPACode into State_Code, County_Code and Site_Num for IMPROVE data and put them into input_mat1
  N_CARB_EPACodes <- length(unique(CARB_data$AQS.Site.ID))
  CARB_EPACode_header <-  c("EPACode","StateCode","CountyCode","SiteNum","ARB.Site.Name","Latitude","Longitude","Elevation..m.","ARB.Site..","State","County","Site.ID","Address","City","Zip.Code","AQMIS.Code","LocationDataSource","Datum")
  N_EPACode_columns <- length(CARB_EPACode_header) # how many columns are in header?
  CARB_EPACode <- data.frame(matrix(NA,nrow=N_CARB_EPACodes,ncol=N_EPACode_columns)) # create data frame for input_mat1
  names(CARB_EPACode) <- CARB_EPACode_header # assign the header to input_mat1
  CARB_EPACode$EPACode <- unique(CARB_data$AQS.Site.ID)
  rm(N_EPACode_columns)
  
  # Split CARB EPACode into State_Code, County_Code and Site_Num and put them into input_mat1
  missing_meta_data <- NA
  for (this_row in 1:N_CARB_EPACodes) { # cycle through each row in CARB data to determine state code, county code, and site num and put into input_mat1
    this_EPACode <- as.character((CARB_EPACode[this_row,c("EPACode")])) # isolate the EPA code for this row of data
    print(this_EPACode)
    if (is.na(this_EPACode)==TRUE) {
      CARB_EPACode[this_row,c("StateCode")] <- NA
      CARB_EPACode[this_row,c("CountyCode")] <- NA
      CARB_EPACode[this_row,c("SiteNum")] <- NA
    } else { # if (is.na(this_EPACode)==TRUE) {
      if (nchar(this_EPACode)==8) { # determine how many characters are in EPACode (leading zeros are not in the data)
        #print("8 characters")
        
        CARB_EPACode[this_row,c("StateCode")] <- substr(this_EPACode,1,1) # isolate state code
        CARB_EPACode[this_row,c("CountyCode")] <- substr(this_EPACode,2,4) # isolate county code
        CARB_EPACode[this_row,c("SiteNum")] <- substr(this_EPACode,5,8)  # isolate site num
        
      } else if (nchar(this_EPACode)==9) {
        #print("9 characters")
        CARB_EPACode[this_row,c("StateCode")] <- substr(this_EPACode,1,2) # isolate state code
        CARB_EPACode[this_row,c("CountyCode")] <- substr(this_EPACode,3,5) # isolate county code
        CARB_EPACode[this_row,c("SiteNum")] <- substr(this_EPACode,6,9)  # isolate site num
      } else {# if (nchar(this_EPACode)==8) { # determine how many characters are in EPACode (leading zeros are not in the data)
        stop("check data/code")
      } # if (nchar(this_EPACode)==8) { # determine how many characters are in EPACode (leading zeros are not in the data)
    } # if (is.na(this_EPACode)==TRUE) {
    
    if (is.na(this_EPACode)==FALSE) { # don't fill in rows with missing EPA code
      which_meta_row <- which(CARB_meta_data$AQS.ID==this_EPACode)
      print(which_meta_row)
      #if (length(which_meta_row)!=1) {#stop("check code and data")
      #  missing_meta_data <- c(missing_meta_data,this_EPACode)
      #  } else {
      which_compare_loc <- which(as.character(all_CARB_location_data$Site.ID)==this_EPACode)
      CARB_EPACode[this_row,c("Datum")] <- all_CARB_location_data[which_compare_loc,c("Datum")]
      # check Burton data first, then second Burton file, then use original location info
      if (length(which_meta_row)==1) { # there is Burton data (first file) for this site
        print("Burton first file.")
        CARB_EPACode[this_row,c("ARB.Site.Name")] <- as.character(CARB_meta_data[which_meta_row,c("ARB.Site.Name")])
        CARB_EPACode[this_row,c("Latitude")] <- as.character(CARB_meta_data[which_meta_row,c("Latitude")])
        CARB_EPACode[this_row,c("Longitude")] <- as.character(CARB_meta_data[which_meta_row,c("Longitude")])
        CARB_EPACode[this_row,c("LocationDataSource")] <- "BurtonFirstFile"
        CARB_EPACode[this_row,c("Elevation..m.")] <- as.character(CARB_meta_data[which_meta_row,c("Elevation..m.")])
        CARB_EPACode[this_row,c("ARB.Site..")] <- as.character(CARB_meta_data[which_meta_row,c("ARB.Site..")])
        CARB_EPACode[this_row,c("State")] <- as.character(CARB_meta_data[which_meta_row,c("State")])
        CARB_EPACode[this_row,c("County")] <- as.character(CARB_meta_data[which_meta_row,c("County")])
        CARB_EPACode[this_row,c("Site.ID")] <- as.character(CARB_meta_data[which_meta_row,c("Site.ID")])
        CARB_EPACode[this_row,c("Address")] <- as.character(CARB_meta_data[which_meta_row,c("Address")])
        CARB_EPACode[this_row,c("City")] <- as.character(CARB_meta_data[which_meta_row,c("City")])
        CARB_EPACode[this_row,c("Zip.Code")] <- as.character(CARB_meta_data[which_meta_row,c("Zip.Code")])
        CARB_EPACode[this_row,c("AQMIS.Code")] <- as.character(CARB_meta_data[which_meta_row,c("AQMIS.Code")])
      } else { # this site is not in the first Burton file
        # look for this site in the compare data frame
        #which_compare_loc <- which(as.character(all_CARB_location_data$Site.ID)==this_EPACode)
        
        if (!is.na(all_CARB_location_data[which_compare_loc,c("Burton.Lat")])) {stop("check code")}
        if (!is.na(all_CARB_location_data[which_compare_loc,c("Second.Burton.Lat")])) {
          print("Location info in second Burton File")
          #stop("pick up code here")
          CARB_EPACode[this_row,c("Latitude")] <- as.character(all_CARB_location_data[which_compare_loc,c("Second.Burton.Lat")])
          CARB_EPACode[this_row,c("Longitude")] <- as.character(all_CARB_location_data[which_compare_loc,c("Second.Burton.Lon")])
          CARB_EPACode[this_row,c("LocationDataSource")] <- "BurtonSecondFile"
          
        } else if (!is.na(all_CARB_location_data[which_compare_loc,c("Lat.w.PM25")])) {
          print("using data in origninal file from CARB")
          #stop("pick up writing code with orig CARB location info")
          CARB_EPACode[this_row,c("Latitude")] <- as.character(all_CARB_location_data[which_compare_loc,c("Lat.w.PM25")])
          CARB_EPACode[this_row,c("Longitude")] <- as.character(all_CARB_location_data[which_compare_loc,c("Lon.w.PM25")])
          CARB_EPACode[this_row,c("LocationDataSource")] <- "OrigCarbFile"
        } # if (!is.na(all_CARB_location_data[which_compare_loc,c("Second.Burton.Lat")])) {
      } # if (length(which_meta_row)==1) { # there is Burton data (first file) for this site
      rm(which_meta_row,which_compare_loc)
    } # if (is.na(this_EPACode)==FALSE) { # don't fill in rows with missing EPA code
    rm(this_EPACode)
  } # for (this_row in row_start:row_stop) { # cycle through each row in CARB data to determine state code, county code, and site num and put into input_mat1
  rm(this_row,missing_meta_data)
  
  # add new columns at the end of CARB_EPACode with state code, county code, and site number (which were derived from the EPAcode)
  # add column for state code
  new_col_number <- dim(CARB_data)[2]+1 # figure out how many columns are in data and then add 1
  CARB_data[,new_col_number] <- NA # add column at end of data and fill it with NA
  colnames(CARB_data)[new_col_number] <- "State_Code"
  rm(new_col_number)
  
  # add column for county code
  new_col_number <- dim(CARB_data)[2]+1 # figure out how many columns are in data and then add 1
  CARB_data[,new_col_number] <- NA # add column at end of data and fill it with NA
  colnames(CARB_data)[new_col_number] <- "County_Code"
  rm(new_col_number)
  
  # add column for site code
  new_col_number <- dim(CARB_data)[2]+1 # figure out how many columns are in data and then add 1
  CARB_data[,new_col_number] <- NA # add column at end of data and fill it with NA
  colnames(CARB_data)[new_col_number] <- "Site_Num"
  rm(new_col_number)
  
  # # add column for Burton Latitude (Latitude info received via email from Eric Burton at CARB)
  # new_col_number <- dim(CARB_data)[2]+1 # figure out how many columns are in data and then add 1
  # CARB_data[,new_col_number] <- NA # add column at end of data and fill it with NA
  # colnames(CARB_data)[new_col_number] <- "Burton_Lat"
  # rm(new_col_number)
  # 
  # # add column for Burton Longitude (Longitude info received via email from Eric Burton at CARB)
  # new_col_number <- dim(CARB_data)[2]+1 # figure out how many columns are in data and then add 1
  # CARB_data[,new_col_number] <- NA # add column at end of data and fill it with NA
  # colnames(CARB_data)[new_col_number] <- "Burton_Lon"
  # rm(new_col_number)
  
  # add column for Latitude to be used
  new_col_number <- dim(CARB_data)[2]+1 # figure out how many columns are in data and then add 1
  CARB_data[,new_col_number] <- NA # add column at end of data and fill it with NA
  colnames(CARB_data)[new_col_number] <- "Use_this_Lat"
  rm(new_col_number)
  
  # add column for Burton Longitude (Longitude info received via email from Eric Burton at CARB)
  new_col_number <- dim(CARB_data)[2]+1 # figure out how many columns are in data and then add 1
  CARB_data[,new_col_number] <- NA # add column at end of data and fill it with NA
  colnames(CARB_data)[new_col_number] <- "Use_this_Lon"
  rm(new_col_number)
  
  # add column for Datum (Datum info received via email from Eric Burton at CARB)
  new_col_number <- dim(CARB_data)[2]+1 # figure out how many columns are in data and then add 1
  CARB_data[,new_col_number] <- NA # add column at end of data and fill it with NA
  colnames(CARB_data)[new_col_number] <- "Datum"
  rm(new_col_number)
  
  for (this_row in 1:dim(CARB_EPACode)[1]) { # put columns of state code, county code, and site number into CARB_StudyStates_sepCodes
    # what are the codes for this row of CARB_EPACode?
    this_code <- CARB_EPACode[this_row,c("EPACode")]
    this_state <- CARB_EPACode[this_row,c("StateCode")]
    this_county <- CARB_EPACode[this_row,c("CountyCode")]
    this_siteNum <- CARB_EPACode[this_row,c("SiteNum")]
    
    #print(this_code) # this row of code
    # what rows in CARB_StudyStates_sepCodes has this EPA code?
    rows_of_interest <- which(CARB_data$AQS.Site.ID==this_code)
    CARB_data[rows_of_interest,c("State_Code")] <- this_state
    CARB_data[rows_of_interest,c("County_Code")] <- this_county
    CARB_data[rows_of_interest,c("Site_Num")] <- this_siteNum
    
    
    CARB_data[rows_of_interest,c("Use_this_Lat")] <- CARB_EPACode[this_row,c("Latitude")]
    CARB_data[rows_of_interest,c("Use_this_Lon")] <- CARB_EPACode[this_row,c("Longitude")]
    CARB_data[rows_of_interest,c("Datum")] <- CARB_EPACode[this_row,c("Datum")]
    
    #if (!is.na(CARB_EPACode[this_row,c("Latitude")])) {
    #  CARB_data[rows_of_interest,c("Datum")] <- this_Datum
    #}
    
    
    rm(rows_of_interest,this_code,this_state,this_county,this_siteNum)
  } # for (this_row in 1:dim(CARB_EPACode)[1]) { # put columns of state code, county code, and site number into CARB_StudyStates_sepCodes
  rm(this_row)
  
  # State Code
  input_mat1[row_start:row_stop,c("State_Code")] <- as.character(CARB_data$State_Code)
  
  # County Code
  input_mat1[row_start:row_stop,c("County_Code")] <- as.character(CARB_data$County_Code)
  
  # Site Number
  input_mat1[row_start:row_stop,c("Site_Num")] <- as.character(CARB_data$Site_Num)
  
  #"PM25_Station_Name"
  input_mat1[row_start:row_stop,c("PM25_Station_Name")] <- paste(CARB_data$Basin,CARB_data$Site.Name,CARB_data$Site,CARB_data$Monitor,sep = " ")
  
  # "County_Name"
  input_mat1[row_start:row_stop,c("County_Name")] <- as.character(CARB_data$County)
  
  # input latitude and longitude ('PM2.5_Lat','PM2.5_Lon')
  #input_mat1[row_start:row_stop,c("PM2.5_Lat")] <- CARB_data[,c('Latitude')]
  #input_mat1[row_start:row_stop,c("PM2.5_Lon")] <- CARB_data[,c('Longitude')]
  # use location info from Eric Burton wherever available
  input_mat1[row_start:row_stop,c("PM2.5_Lat")] <- CARB_data[,c("Use_this_Lat")]
  input_mat1[row_start:row_stop,c("PM2.5_Lon")] <- CARB_data[,c("Use_this_Lon")]
  
  # "Datum"
  input_mat1[row_start:row_stop,c("Datum")] <- CARB_data[,c("Datum")]
  
  # "Observation_Count"
  input_mat1[row_start:row_stop,c("Observation_Count")] <- CARB_data$Number.of.Observations
  
  # "PM2.5_Obs" 
  input_mat1[row_start:row_stop,c("PM2.5_Obs")] <- CARB_data$"Daily.Average..?g.m3."
  
  # Distinguish between hourly & 24-hr data
  # add column for Sample_Duration
  new_col_number <- dim(CARB_data)[2]+1 # figure out how many columns are in data and then add 1
  CARB_data[,new_col_number] <- NA # add column at end of data and fill it with NA
  colnames(CARB_data)[new_col_number] <- "Sample_Duration"
  rm(new_col_number)
  
  # add column for "Observation_Percent" 
  new_col_number <- dim(CARB_data)[2]+1 # figure out how many columns are in data and then add 1
  CARB_data[,new_col_number] <- NA # add column at end of data and fill it with NA
  colnames(CARB_data)[new_col_number] <- "Observation_Percent" # name the new column
  rm(new_col_number) # remove column number variable
  
  which_hourly <- which(CARB_data$Observation.Type=="H") # find the rows in CARB_data that are hourly observations
  CARB_data[which_hourly,c("Sample_Duration")] <- "1 HOUR" # indicate that this corresponds to "1 HOUR" in input_mat1
  CARB_data[which_hourly,c("Observation_Percent")] <- CARB_data[which_hourly,c("Number.of.Observations")]/24*100 # calculate the percent of hours in a day that have observations
  rm(which_hourly)
  which_daily <- which(CARB_data$Observation.Type=="D") # find the rows in CARB_data that are daily obs (24-hr)
  CARB_data[which_daily,c("Sample_Duration")] <- "24 HOUR" # indicate that this corresponds to "24 HOUR" in input_mat1
  CARB_data[which_daily,c("Observation_Percent")] <- CARB_data[which_daily,c("Number.of.Observations")]/1*100 # calculate the percent of hours in a day that have observations
  rm(which_daily)
  #"Sample_Duration"
  input_mat1[row_start:row_stop,c("Sample_Duration")] <- CARB_data$Sample_Duration 
  
  # "flg.PM25_Obs"
  input_mat1[row_start:row_stop,c("flg.PM25_Obs")] <- CARB_data$Source
  
  # input 'Date_Local' into input_mat1
  input_mat1[row_start:row_stop,c("Date_Local")] <- as.Date(CARB_data$Date,"%m/%d/%Y") 
  #,"%Y-%m-%d")
  #CARB_data$Date <- as.Date(CARB_data$Date,"%Y-%m-%d")
  this_col_input_mat <- 'Date_Local'
  this_col_source_file <- "Date"
  #SourceVar <- as.Date(CARB_data[,c(this_col_source_file)],"%Y-%m-%d")
  SourceVar <- as.Date(CARB_data$Date,"%m/%d/%Y") 
  #print(AQSVar)
  SourceVarChar <- format(SourceVar,"%Y-%m-%d")
  #print(AQSVarChar)
  input_mat1[row_start:row_stop,c(this_col_input_mat)] <- SourceVarChar
  #rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
  rm(this_col_input_mat,SourceVar,SourceVarChar,this_col_source_file)
  
  # "Units_of_Measure" 
  input_mat1[row_start:row_stop,c("Units_of_Measure")] <- "?g.m3"
  
  # "State_Name"
  input_mat1[row_start:row_stop,c("State_Name")] <- "California"
  
  # "State_Abbrev"
  input_mat1[row_start:row_stop,c("State_Abbrev")] <- "CA"
  
  # input 'Data_Source_Name_Display' into input_mat1
  input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
  
  # input 'Data_Source_Name_Short' into input_mat1
  input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short
  
  # input data source counter - indicates if this is EPA data or field data, etc.
  input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
  
  # input color for plotting this data source (totally arbitrary choice of color)
  input_mat1[row_start:row_stop,c("PlottingColor")] <- "blueviolet"
  
  # input 'Source_File' name
  input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
  
  # input the 'Composite_of_N_rows' - this variable indicates how many separate rows of 
  # data were composited to form this row of data. This will be relevant when getting rid of repeated data.
  # For now, this is set to 1 because repeated rows of data will be consolidated in a later script.
  input_mat1[row_start:row_stop,c('Composite_of_N_rows')] <- 1
  
  # add column for "N_Negative_Obs" 
  new_col_number <- dim(CARB_data)[2]+1 # figure out how many columns are in data and then add 1
  CARB_data[,new_col_number] <- NA # add column at end of data and fill it with NA
  colnames(CARB_data)[new_col_number] <- "N_Negative_Obs" # name the new column
  rm(new_col_number) # remove column number variable
  
  which_negative <- which(CARB_data$Daily.Average..?g.m3.<0)
  CARB_data[which_negative,c("N_Negative_Obs")] <- 1 # indicate that these rows had 1 negative observation
  rm(which_negative)
  
  which_positive <- which(CARB_data$Daily.Average..?g.m3.>=0)
  CARB_data[which_positive,c("N_Negative_Obs")] <- 0 # indicate that rows with positive concentrations have 0 negative valuse
  rm(which_positive)
  
  input_mat1[row_start:row_stop,c("N_Negative_Obs")] <- CARB_data$N_Negative_Obs
  
  # InDayLatDiff and InDayLonDiff used to figure out if lat/lon observations within a day do not agree (relevant for DRI data), set to 0 for this data
  input_mat1[row_start:row_stop,c("InDayLatDiff")] <- 0
  input_mat1[row_start:row_stop,c("InDayLonDiff")] <- 0
  
  # Think about whether to try to include these variables from CARB_data into input_mat1
  # "Number.of.Hours"     "Notes."    
  
  # need to figure out whether/how to fill in these variable in input_mat1 for the CARB_data
  # "Parameter_Code" "POC" "Parameter_Name" "Pollutant_Standard" "Event_Type"
  # "1st_Max_Value" "1st_Max_Hour" "AQI" "Method_Code" "Method_Name" "Address"
  # "City_Name" "CBSA_Name" "Date_of_Last_Change" "Winter" "Year" "Month" "Day"
  # "flg.Lat" "flg.Lon" "Type" "flg.Type" "flg.Site_Num" "l/m Ave. Air Flw" "flg.AirFlw"
  # "Deg C Av Air Temp" "flg.AirTemp" "% Rel Humidty" "flg.RelHumid" "mbar Barom Press"         
  # "flg.Barom Press" "deg C Sensor  Int AT" "flg.deg C Sensor Int AT" "% Sensor Int RH"
  # "flg.%SensorIntRH" "Wind Speed m/s" "flg.WindSpeed" "Battery Voltage volts" 
  # "flg.BatteryVoltage" "Alarm" "flg.Alarm"
  
  # update row counter
  row_start <- row_stop+1
  
  # clear variables before moving on to next iteration of loop
  rm(this_source_file,CARB_data, CARB_EPACode,N_CARB_EPACodes,CARB_EPACode_header) 
  rm(Data_Source_Name_Display,Data_Source_Name_Short,this_Datum)
  rm(CARB_meta_data,all_CARB_location_data,all_CARB_location_data_header,second_meta_data_file)
  
  
  
  print(paste("This data has",dim(input_mat1)[1],"rows of PM2.5 observations.")) # how many rows of data?
  
  # output to file #  
  write.csv(input_mat1,file = file.path(ProcessedData.directory,paste(Data_Source_Name_Short,"_",Sys.Date(),'_Step1_part_',processed_data_version,'.csv',sep = "")),row.names = FALSE)
  
  # clear variables    
  rm(ParameterCode_vec,this_year,this_ParamCode)
  rm(Data_Source_Name_Display,Data_Source_Name_Short)
  
  # output input_mat1 from function #  
  return(input_mat1) # output from function
} # end function

### compile all location info between CARB_data and CARB_meta_data
compile_all_CARB_location_info.fn <- function(CARB_data, CARB_meta_data, second_meta_data_file, this_Datum) {
  
  all_sites <- unique(CARB_data[c("AQS.Site.ID", "Site", "Site.Name")])
  #all_sites_step <- unique(c(CARB_data$AQS.Site.ID,CARB_meta_data$AQS.ID,second_meta_data_file$x)) # create list of all site codes among the three loaded files - don't actually care about sites that don't have PM2.5 data
  #all_sites <- all_sites_step[which(!is.na(all_sites_step))] # get rid of NA values
  print(all_sites) # display list of all sites
   
  all_CARB_location_data_header <- c("AQS.Site.ID", "Site.4.digit", "Site.Name","Burton.Lat","Burton.Lon","Second.Burton.Lat","Second.Burton.Lon","Lat.w.PM25","Lon.w.PM25","Datum") # header for compiling all loc info
  all_CARB_location_data <- data.frame(matrix(NA,nrow = dim(all_sites)[1],ncol = length(all_CARB_location_data_header)))
  names(all_CARB_location_data) <- all_CARB_location_data_header
  all_CARB_location_data$AQS.Site.ID <- as.numeric(all_CARB_location_data$AQS.Site.ID) # set class
  all_CARB_location_data$Site.4.digit <- as.numeric(all_CARB_location_data$Site.4.digit) # set class
  all_CARB_location_data$Burton.Lat <- as.numeric(all_CARB_location_data$Burton.Lat) # set class
  all_CARB_location_data$Burton.Lon <- as.numeric(all_CARB_location_data$Burton.Lon) # set class
  all_CARB_location_data$Second.Burton.Lat <- as.numeric(all_CARB_location_data$Second.Burton.Lat) # set class
  all_CARB_location_data$Second.Burton.Lon <- as.numeric(all_CARB_location_data$Second.Burton.Lon) # set class
  all_CARB_location_data$Lat.w.PM25 <- as.numeric(all_CARB_location_data$Lat.w.PM25) # set class
  all_CARB_location_data$Lon.w.PM25 <- as.numeric(all_CARB_location_data$Lon.w.PM25) # set class
  
  all_CARB_location_data$AQS.Site.ID <- all_sites$AQS.Site.ID # put site id's into all_CARB_location_data
  
  all_CARB_location_data$Site.4.digit <- all_sites$Site # input 4-digit site number
  all_CARB_location_data$Site.Name <- all_sites$Site.Name
  all_CARB_location_data$Datum <- this_Datum # fill in Datum
  rm(all_sites) # clear variable
  
  for (site_counter in 1:dim(all_CARB_location_data)[1]) { # cycle through sites and fill in location info
    print(site_counter)
    this_AQS.Site.ID <- all_CARB_location_data[site_counter,c("AQS.Site.ID")]
    print(this_AQS.Site.ID)
    this_Site.4.digit <- all_CARB_location_data[site_counter,c("Site.4.digit")]
    print(this_Site.4.digit)
    this_Site.Name <- all_CARB_location_data[site_counter,c("Site.Name")]
    print(this_Site.Name)
    
    # find the location info from the CARB data and put it into all_CARB_location_data
    #if (is.na(this_AQS.Site.ID)) {
    #  stop("write more code")
    #}
    which_rows <- which(CARB_data$AQS.Site.ID==this_AQS.Site.ID | CARB_data$Site == this_Site.4.digit | CARB_data$Site.Name == this_Site.Name) # locate the data for this site in CARB_data
    if (length(which_rows) > 0) { # input location info if there is any
    all_CARB_location_data[site_counter,c("Lat.w.PM25")] <- unique(CARB_data[which_rows,c("Latitude")]) # input the latitude data into all_CARB_location_data
    all_CARB_location_data[site_counter,c("Lon.w.PM25")] <- unique(CARB_data[which_rows,c("Longitude")]) # input the latitude data into all_CARB_location_data
    } else {# if (length(which_rows) ) { # input location info if there is any
      stop(paste("there is no location info in CARB_data for site",this_Site.Name))
    } # if (length(which_rows) ) { # input location info if there is any
    rm(which_rows) # clear variable
    
    # find the location info from the first meta-data file and put it into all_CARB_location_data
    which_rows <- which(CARB_meta_data$AQS.ID==this_AQS.Site.ID | CARB_meta_data$AQMIS.Code == this_Site.4.digit | as.character(CARB_meta_data$ARB.Site.Name) == as.character(this_Site.Name)) # locate the data for this site in CARB_meta_data
    if (length(which_rows) == 1 ) { # input location info if there is any
      is_there_a_space <- is_there_a_space.fn(as.character(CARB_meta_data[which_rows,c("Latitude")]))
      if (is_there_a_space == 1) { # need to convert to decimals
        # convert from decimal minutes to decimal degrees
        this_lat <- measurements::conv_unit(as.character(CARB_meta_data[which_rows,c("Latitude")]), from = 'deg_min_sec', to = 'dec_deg')
        this_lon_step <- measurements::conv_unit(as.character(CARB_meta_data[which_rows,c("Longitude")]), from = 'deg_min_sec', to = 'dec_deg')
        this_lon <- (-1)*as.numeric(this_lon_step)
        all_CARB_location_data[site_counter,c("Burton.Lat")] <- as.character(this_lat) # input the latitude data into all_CARB_location_data
        all_CARB_location_data[site_counter,c("Burton.Lon")] <- this_lon # input the latitude data into all_CARB_location_data
        
      } else {
      
      all_CARB_location_data[site_counter,c("Burton.Lat")] <- as.numeric(as.character(CARB_meta_data[which_rows,c("Latitude")])) # input the latitude data into all_CARB_location_data
      all_CARB_location_data[site_counter,c("Burton.Lon")] <- as.numeric(as.character(CARB_meta_data[which_rows,c("Longitude")])) # input the latitude data into all_CARB_location_data
      }
    } else if (length(which_rows) == 0) {# if (length(which_rows) ) { # input location info if there is any
      print(paste("there is no location info in CARB_meta_data for site",this_Site.Name))
    } else if (length(which_rows) > 1) {
      stop('write more code')
    }# if (length(which_rows) ) { # input location info if there is any
    rm(which_rows) # clear variable
 
    # find the location info from the second meta-data file and put it into all_CARB_location_data
    which_rows <- which(second_meta_data_file$x==this_AQS.Site.ID | as.character(second_meta_data_file$AIS.Site.Name) == as.character(this_Site.Name)) # locate the data for this site in second_meta_data_file
    if (length(which_rows) == 1 ) { # input location info if there is any
      meta2_this_lat_lon <- as.character(second_meta_data_file[which_rows,c("Known.Coordinates")])
      print(meta2_this_lat_lon)
      #find the comma and take everything before the comma as the lat and everything after as the lon
      output_list <- separate_character_vec_at_comma.fn(input_vec = meta2_this_lat_lon)
      rm(meta2_this_lat_lon)
      this_lat <- output_list[[1]]
      print(this_lat)
      this_lon <- output_list[[2]]
      print(this_lon)
      
      all_CARB_location_data[site_counter,c("Second.Burton.Lat")] <- as.numeric(this_lat)# input the latitude data into all_CARB_location_data
      all_CARB_location_data[site_counter,c("Second.Burton.Lon")] <- as.numeric(this_lon) # input the latitude data into all_CARB_location_data
    } else if (length(which_rows) == 0) {# if (length(which_rows) ) { # input location info if there is any
      print(paste("there is no location info in second_meta_data_file for site",this_Site.Name))
    } else if (length(which_rows) > 1) {
      stop('write more code')
    }# if (length(which_rows) ) { # input location info if there is any
    rm(which_rows) # clear variable    
    
  } # for (site_counter in 1:dim(all_CARB_location_data)[1]) { # cycle through sites and fill in location info
  
#-----------
  #for (i_Burton_row in 1:dim(CARB_meta_data)[1]) { # cycle through sites in first file from Burton and fill in location data
    #this_EPA_code <- CARB_meta_data[i_Burton_row,c("AQS.ID")]
    #print(this_EPA_code)
    #which_compare <- which(all_CARB_location_data$Site.ID==this_EPA_code)
    #print(which_compare)
    #all_CARB_location_data[which_compare,c("Burton.Lat")] <- as.character(CARB_meta_data[i_Burton_row,c("Latitude")])
   # all_CARB_location_data[which_compare,c("Burton.Lon")] <- as.character(CARB_meta_data[i_Burton_row,c("Longitude")])
  #  all_CARB_location_data[which_compare,c("Datum")] <- this_Datum
  #  print(all_CARB_location_data[which_compare,c("Burton.Lat","Burton.Lon")])
  #  rm(this_EPA_code,which_compare)
  #} # for (i_Burton_row in 1:dim(CARB_meta_data)[1]) {
  #rm(i_Burton_row)
  #for (i_compare_row in 1:dim(all_CARB_location_data)[1]) {
  #  this_EPA_code <- all_CARB_location_data[i_compare_row,c("Site.ID")]
  #  print(this_EPA_code)
    #which_compare <- which(CARB_data$AQS.Site.ID==this_EPA_code)
    #print(which_compare[1])
    #all_CARB_location_data[i_compare_row,c("Lat.w.PM25")] <- as.character(CARB_data[which_compare[1],c("Latitude")])
    #all_CARB_location_data[i_compare_row,c("Lon.w.PM25")] <- as.character(CARB_data[which_compare[1],c("Longitude")])
    #print(all_CARB_location_data[i_compare_row,])
    #rm(this_EPA_code,which_compare)
  #} # for (i_compare_row in 1:dim(all_CARB_location_data)[1]) {
  #rm(i_compare_row)
  
  for (i_Burton_row in 1:dim(second_meta_data_file)[1]) { # cycle through sites in second file from Burton and fill in location data
    this_EPA_code <- second_meta_data_file[i_Burton_row,c("x")]
    print(this_EPA_code)
    which_compare <- which(all_CARB_location_data$Site.ID==this_EPA_code)
    print(which_compare)
    # isolate the lat/lon coordinates from the second Burton file
    meta2_this_lat_lon <- as.character(second_meta_data_file[i_Burton_row,c("Known.Coordinates")])
    print(meta2_this_lat_lon)
    if (nchar(meta2_this_lat_lon)<=1) { # second Burton data does not have location info
      all_CARB_location_data[which_compare,c("Second.Burton.Lat")] <- NA
      all_CARB_location_data[which_compare,c("Second.Burton.Lon")] <- NA
      
    } else { # there is data
      # #find the comma and take everything before the comma as the lat and everything after as the lon
      # input_vec <- meta2_this_lat_lon
      # output_list <- separate_character_vec_at_comma_fn(input_vec)
      # this_lat <- output_list[[1]]
      # print(this_lat)
      # this_lon <- output_list[[2]]
      # print(this_lon)
      #all_CARB_location_data[which_compare,c("Second.Burton.Lat")] <- this_lat
      all_CARB_location_data[which_compare,c("Second.Burton.Lon")] <- this_lon
      all_CARB_location_data[which_compare,c("Datum")] <- this_Datum
      rm(input_vec,output_list,this_lat,this_lon)
    } # if (length(meta2_this_lat_lon)<=1) { # second Burton data does not have location info
    rm(this_EPA_code,which_compare,meta2_this_lat_lon)
  } # for (i_Burton_row in 1:dim(second_meta_data_file)[1]) {
  rm(i_Burton_row)
  
  # print to csv
  write.csv(all_CARB_location_data,file = file.path(ProcessedData.directory,'All_CARB_locations.csv'),row.names = FALSE)
  
  # find the sites that are missing Burton Data
  which_missing_Burton <- which(is.na(all_CARB_location_data$Burton.Lat) & is.na(all_CARB_location_data$Second.Burton.Lat))
  sites_needing_LatLon <- all_CARB_location_data[which_missing_Burton,c("Site.ID")]
  write.csv(sites_needing_LatLon,file = file.path(ProcessedData.directory,'missing_CARB_LatLon.csv'),row.names = FALSE)
  rm(sites_needing_LatLon,which_missing_Burton)
  
  
} # end of compile_all_CARB_location_info.fn function

