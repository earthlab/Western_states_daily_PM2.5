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
  cat("Latest Update: October 23, 2018 \n")
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
  
  # Load location meta-data files
  this_meta_data_file <- "Site_Info_for_Bob_Weller-May_29_2018.csv" # name of first meta-data file
  print(this_meta_data_file) # display name of 1st meta-data file
  CARB_meta_data <- read.csv(file.path(CARB.directory,this_meta_data_file), header = T, sep = ",") # load first meta-data file
  rm(this_meta_data_file) # clear variable
  this_meta_data_file <- "Missing_Site_Info_for_Ellen-6-4-18.csv" # name of second meta-data file
  print(this_meta_data_file)
  second_meta_data_file <- read.csv(file.path(CARB.directory,this_meta_data_file), header = T, sep = ",") # load second meta-data file
  rm(this_meta_data_file) # clear variable
  
  ### compile all location info between CARB_data and CARB_meta_data
  all_CARB_location_data <- compile_all_CARB_location_info.fn(CARB_data, CARB_meta_data, second_meta_data_file, this_Datum)
  
  # Split EPA site codes (no hyphens) into state, county, site number components as a new data frame - write new function for codes with hyphens
  EPACode_components <- EPA_codes_2_components_no_hyphens.fn(unique(CARB_data$AQS.Site.ID)) 

  ### fill in each column of input_mat1 ###
  # Create input_mat1 data frame
  input_mat1 <- data.frame(matrix(NA,nrow=dim(CARB_data)[1],ncol=length(input_header))) # create data frame for input_mat1
  names(input_mat1) <- input_header # assign the header to input_mat1
  input_mat1 <- input_mat_change_data_classes.fn(input_mat1)
  
  # fill in columns
  input_mat1$PM25_Station_Name <- paste(CARB_data$Basin,CARB_data$Site.Name,CARB_data$Site,CARB_data$Monitor,sep = " ") #"PM25_Station_Name"
  input_mat1$County_Name <- as.character(CARB_data$County) # "County_Name"
  input_mat1$Datum <- this_Datum # "Datum"
  input_mat1$Observation_Count <- CARB_data$Number.of.Observations # "Observation_Count"
  input_mat1$PM2.5_Obs <- CARB_data$`Daily.Average..µg.m3.`#CARB_data$"Daily.Average..?g.m3." # "PM2.5_Obs" 
  input_mat1$flg.PM25_Obs <- CARB_data$Source # "flg.PM25_Obs"
  input_mat1$Date_Local <- as.Date(CARB_data$Date,"%m/%d/%Y") # input 'Date_Local' into input_mat1
  input_mat1$Units_of_Measure <- "ug.m3" # "Units_of_Measure"
  input_mat1$State_Name <- "California" # "State_Name"
  input_mat1$State_Abbrev <- "CA" # "State_Abbrev"
  input_mat1$Data_Source_Name_Display <- Data_Source_Name_Display # input 'Data_Source_Name_Display' into input_mat1
  input_mat1$Data_Source_Name_Short <- Data_Source_Name_Short # input 'Data_Source_Name_Short' into input_mat1
  input_mat1$Data_Source_Counter <- data_source_counter # input data source counter - indicates if this is EPA data or field data, etc.
  input_mat1$PlottingColor <- this_plotting_color#"blueviolet" # input color for plotting this data source (totally arbitrary choice of color)
  input_mat1$Source_File <- this_source_file # input 'Source_File' name
  # input the 'Composite_of_N_rows' - this variable indicates how many separate rows of 
  # data were composited to form this row of data. This will be relevant when getting rid of repeated data.
  # For now, this is set to 1 because repeated rows of data will be consolidated in a later script.
  input_mat1$Composite_of_N_rows <- 1
  which_negative <- which(CARB_data$`Daily.Average..µg.m3.`<0)
  input_mat1[which_negative,c("N_Negative_Obs")] <- 1 # indicate that these rows had 1 negative observation
  rm(which_negative)
  which_positive <- which(CARB_data$`Daily.Average..µg.m3.`>=0)
  input_mat1[which_positive,c("N_Negative_Obs")] <- 0 # indicate that rows with positive concentrations have 0 negative valuse
  rm(which_positive)
  input_mat1$InDayLatDiff <- 0 # InDayLatDiff and InDayLonDiff used to figure out if lat/lon observations within a day do not agree (relevant for DRI data), set to 0 for this data
  input_mat1$InDayLonDiff <- 0 # InDayLatDiff and InDayLonDiff used to figure out if lat/lon observations within a day do not agree (relevant for DRI data), set to 0 for this data
  
  # fill in location information
  for (CARB_loc_i in 1:dim(all_CARB_location_data)[1]) { # cycle through all locations in CARB data
    this_CARB_site <- all_CARB_location_data[CARB_loc_i, c("Site.4.digit")]
    print(this_CARB_site)
  
    # find this site in CARB_data 
    which_rows <- which(CARB_data$Site == this_CARB_site)
    if (length(which_rows)==0) {stop("check code")}
    print(length(which_rows))
    
    # fill in location information
    # get lat/lon info
    if (is.na(all_CARB_location_data[CARB_loc_i,c("Burton.Lat")]) == FALSE) { # fill in lat/lon from all_CARB_location_data, preferring Burton lat/lon if available (provided by Burton)
      input_mat1[which_rows, c('PM2.5_Lat')] <- all_CARB_location_data[CARB_loc_i, c("Burton.Lat")]
      input_mat1[which_rows, c('PM2.5_Lon')] <- all_CARB_location_data[CARB_loc_i, c("Burton.Lon")]
    } else if (is.na(all_CARB_location_data[CARB_loc_i,c("Second.Burton.Lat")]) == FALSE) {
      input_mat1[which_rows, c('PM2.5_Lat')] <- all_CARB_location_data[CARB_loc_i, c("Second.Burton.Lat")]
      input_mat1[which_rows, c('PM2.5_Lon')] <- all_CARB_location_data[CARB_loc_i, c("Second.Burton.Lon")]
    } else {
      input_mat1[which_rows, c("PM2.5_Lat")] <- all_CARB_location_data[CARB_loc_i,c("Lat.w.PM25")]
      input_mat1[which_rows, c("PM2.5_Lon")] <- all_CARB_location_data[CARB_loc_i,c("Lon.w.PM25")]
    } # if (is.na(all_CARB_location_data[CARB_loc_i,c("Burton.Lat")]) == FALSE) { # fill in lat/lon from all_CARB_location_data, preferring Burton lat/lon if available (provided by Burton)
    
    input_mat1[which_rows,c("State_Code")] <- StateAbbrev2StateCode.fn("CA")
    
    this_EPA_Code <- unique(CARB_data[which_rows,c("AQS.Site.ID")]) # what is the EPA code for this site (if any)?
    print(this_EPA_Code)
    if (is.na(this_EPA_Code)==FALSE) { # only run code if there is an EPA code
    components_row <- which(EPACode_components$EPACode == this_EPA_Code)
    if (length(components_row) != 1) {stop("check code")}
    
    input_mat1[which_rows, c("County_Code")] <- EPACode_components[components_row,c("CountyCode")] # find county code
    input_mat1[which_rows, c("Site_Num")] <- EPACode_components[components_row, c("SiteNum")]
    
    } # if (is.na(this_EPA_Code)==FALSE) { # only run code if there is an EPA code
    
  } # for (CARB_loc_i in 1:dim(all_CARB_location_data)[1]) { # cycle through all locations in CARB data
  
  
  
  #"Sample_Duration"
  #input_mat1[row_start:row_stop,c("Sample_Duration")] <- CARB_data$Sample_Duration 
  which_hourly <- which(CARB_data$Observation.Type=="H") # find the rows in CARB_data that are hourly observations
  input_mat1[which_hourly,c("Sample_Duration")] <- "1 HOUR" # indicate that this corresponds to "1 HOUR" in input_mat1
  input_mat1[which_hourly,c("Observation_Percent")] <- CARB_data[which_hourly,c("Number.of.Observations")]/24*100 # calculate the percent of hours in a day that have observations
  rm(which_hourly)
  which_daily <- which(CARB_data$Observation.Type=="D") # find the rows in CARB_data that are daily obs (24-hr)
  input_mat1[which_daily,c("Sample_Duration")] <- "24 HOUR" # indicate that this corresponds to "24 HOUR" in input_mat1
  input_mat1[which_daily,c("Observation_Percent")] <- CARB_data[which_daily,c("Number.of.Observations")]/1*100 # calculate the percent of hours in a day that have observations
  rm(which_daily)

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
  
  # clear variables before moving on to next iteration of loop
  #rm(this_source_file,CARB_data, CARB_EPACode,N_CARB_EPACodes,CARB_EPACode_header) 
  #rm(Data_Source_Name_Display,Data_Source_Name_Short,this_Datum)
  #rm(CARB_meta_data,all_CARB_location_data,all_CARB_location_data_header,second_meta_data_file)

  print(paste("This data has",dim(input_mat1)[1],"rows of PM2.5 observations.")) # how many rows of data?
  
  # output to file #  
  write.csv(input_mat1,file = file.path(ProcessedData.directory,paste(Data_Source_Name_Short,"_",Sys.Date(),'_Step1_part_',processed_data_version,'.csv',sep = "")),row.names = FALSE)
  
  # clear variables    
  #rm(ParameterCode_vec,this_year,this_ParamCode)
  #rm(Data_Source_Name_Display,Data_Source_Name_Short)
  
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
  
  # print to csv
  write.csv(all_CARB_location_data,file = file.path(ProcessedData.directory,'All_CARB_locations.csv'),row.names = FALSE)
  
  # find the sites that are missing Burton Data
  which_missing_Burton <- which(is.na(all_CARB_location_data$Burton.Lat) & is.na(all_CARB_location_data$Second.Burton.Lat))
  sites_needing_LatLon <- all_CARB_location_data[which_missing_Burton,c("Site.ID")]
  write.csv(sites_needing_LatLon,file = file.path(ProcessedData.directory,'missing_CARB_LatLon.csv'),row.names = FALSE)
  rm(sites_needing_LatLon,which_missing_Burton)
  
  return(all_CARB_location_data) # output from function
  
} # end of compile_all_CARB_location_info.fn function

