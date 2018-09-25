process_PM25_EPA_data_source.fn <- function(input_header, ProcessedData.directory, AQSData.directory, data_set_counter) {
  # combine EPA PM2.5 data files into 1 dataframe
  
  ##### Create Sink output file and create its header ####
  # sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
  SinkFileName=file.path(ProcessedData.directory,"PM25_data_source_EPA_combining_sink.txt")
  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
  cat("Code and R output for process_PM25_EPA_data_source_function.R \n \n")
  cat("Title: process_PM25_EPA_data_source_function.R \n")
  cat("Author: Melissa May Maestas, PhD \n")
  cat("Original Date: September 24, 2018 \n")
  cat("Latest Update: September 24, 2018 \n")
  cat("This program reads in and PM2.5 data from the EPA. \n")
  
  #### Create data frame ####
  N_columns <- length(input_header) # how many columns are in header?
  input_mat1 <- data.frame(matrix(NA,nrow=10,ncol=N_columns)) # create data frame for input_mat1
  names(input_mat1) <- input_header # assign the header to input_mat1
  rm(N_columns)
  
  #### Pull in AQS data #################
  data_source_counter <- data_set_counter #0 # counter to distinguish between the various data sources
  Data_Source_Name_Short <- "EPA_PM25"
  Data_Source_Name_Display <- "EPA PM2.5"
  row_start=1 # start row counter
  ParameterCode_vec <- cbind(88101,88502)
  
  # cycle through files
  for(this_year in start_study_year:stop_study_year){     # cycle through years
    #print(this_year)
    for(this_ParamCode in ParameterCode_vec){ # cycle through Parameter Codes
      #print(this_ParamCode)
      this_source_file <- paste('daily_',as.character(this_ParamCode),'_',as.character(this_year),'.csv',sep="")
      print(this_source_file)
      
      ThisAQSdata<-read.csv(file.path(AQSData.directory,this_source_file),header=TRUE) # load the AQS file
      
      # isolate data in study states
      #class(ThisAQSdata$State.Code)
      # only study area states: #
      ThisAQSdata_StudyStates <- ThisAQSdata[which(ThisAQSdata$State.Code==4|ThisAQSdata$State.Code==6|ThisAQSdata$State.Code==8|ThisAQSdata$State.Code==16|ThisAQSdata$State.Code==30|ThisAQSdata$State.Code==32|ThisAQSdata$State.Code==35|ThisAQSdata$State.Code==41|ThisAQSdata$State.Code==49|ThisAQSdata$State.Code==53|ThisAQSdata$State.Code==56), ]
      # study states and bordering states:
      #ThisAQSdata_StudyStates <- ThisAQSdata[which(ThisAQSdata$State.Code==4|ThisAQSdata$State.Code==6|ThisAQSdata$State.Code==8|ThisAQSdata$State.Code==16|ThisAQSdata$State.Code==30|ThisAQSdata$State.Code==32|ThisAQSdata$State.Code==35|ThisAQSdata$State.Code==41|ThisAQSdata$State.Code==49|ThisAQSdata$State.Code==53|ThisAQSdata$State.Code==56|ThisAQSdata$State.Code==38|ThisAQSdata$State.Code==46|ThisAQSdata$State.Code==31|ThisAQSdata$State.Code==20|ThisAQSdata$State.Code==40|ThisAQSdata$State.Code==48), ]
      
      rm(ThisAQSdata) # clear variables
      
      row_stop <- row_start+dim(ThisAQSdata_StudyStates)[1]-1 # what is the last row number in input_mat1 for inputing this block of data?
      
      # #### fill in each column of input_mat1 ###########
      
      # input 'State_Code' into input_mat1
      input_mat1[row_start:row_stop,c("State_Code")] <- ThisAQSdata_StudyStates[,c("State.Code")]
      
      # input 'County_Code' into input_mat1
      input_mat1[row_start:row_stop,c('County_Code')] <- ThisAQSdata_StudyStates[,c("County.Code")]
      
      # input 'Site_Num' into input_mat1
      input_mat1[row_start:row_stop,c('Site_Num')] <- ThisAQSdata_StudyStates[,c("Site.Num")]
      
      # input 'Parameter_Code' into input_mat1
      input_mat1[row_start:row_stop,c('Parameter_Code')] <- ThisAQSdata_StudyStates[,c("Parameter.Code")]
      
      # input 'POC' into input_mat1
      input_mat1[row_start:row_stop,c('POC')] <- ThisAQSdata_StudyStates[,c('POC')]
      
      # input latitude and longitude ('PM2.5_Lat','PM2.5_Lon')
      input_mat1[row_start:row_stop,c("PM2.5_Lat")] <- ThisAQSdata_StudyStates[,c('Latitude')]
      input_mat1[row_start:row_stop,c("PM2.5_Lon")] <- ThisAQSdata_StudyStates[,c('Longitude')]
      
      # input 'Datum' into input_mat1
      this_col <- 'Datum'
      AQSVar <- ThisAQSdata_StudyStates[,c(this_col)]
      AQSVarChar <- as.character(AQSVar)
      input_mat1[row_start:row_stop,c(this_col)] <- AQSVarChar
      rm(this_col,AQSVar,AQSVarChar)
      
      # input 'Parameter_Name' into input_mat1
      this_col_input_mat <- 'Parameter_Name'
      this_col_AQS <- 'Parameter.Name'
      AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
      AQSVarChar <- as.character(AQSVar)
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # input "Sample_Duration" into input_mat1
      this_col_input_mat <- "Sample_Duration"
      this_col_AQS <- 'Sample.Duration'
      AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
      AQSVarChar <- as.character(AQSVar)
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # input 'Pollutant_Standard' into input_mat1
      this_col_input_mat <- 'Pollutant_Standard'
      this_col_AQS <- 'Pollutant.Standard'
      AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
      AQSVarChar <- as.character(AQSVar)
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # input 'Date_Local' into input_mat1
      this_col_input_mat <- 'Date_Local'
      this_col_AQS <- 'Date.Local'
      AQSVar <- as.Date(ThisAQSdata_StudyStates[,c(this_col_AQS)],"%Y-%m-%d")
      AQSVarChar <- format(AQSVar,"%Y-%m-%d")
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # input 'Units_of_Measure' into input_mat1
      this_col_input_mat <- 'Units_of_Measure'
      this_col_AQS <- 'Units.of.Measure'
      AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
      AQSVarChar <- as.character(AQSVar)
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # input 'Event_Type' into input_mat1
      this_col_input_mat <- 'Event_Type'
      this_col_AQS <- 'Event.Type'
      AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
      AQSVarChar <- as.character(AQSVar)
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # input 'Observation_Count' into input_mat1
      input_mat1[row_start:row_stop,c('Observation_Count')] <- ThisAQSdata_StudyStates[,c('Observation.Count')]
      
      # input 'Observation_Percent' into input_mat1
      input_mat1[row_start:row_stop,c('Observation_Percent')] <- ThisAQSdata_StudyStates[,c('Observation.Percent')]
      
      # input PM2.5 concentration
      input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- ThisAQSdata_StudyStates[,c("Arithmetic.Mean")]
      
      # input '1st_Max_Value'
      input_mat1[row_start:row_stop,c('1st_Max_Value')] <- ThisAQSdata_StudyStates[,c("X1st.Max.Value")]
      
      # input '1st_Max_Hour'
      input_mat1[row_start:row_stop,c('1st_Max_Hour')] <- ThisAQSdata_StudyStates[,c("X1st.Max.Hour")]
      
      # input 'AQI'
      input_mat1[row_start:row_stop,c('AQI')] <- ThisAQSdata_StudyStates[,c('AQI')]
      
      # input 'Method_Code'
      input_mat1[row_start:row_stop,c('Method_Code')] <- ThisAQSdata_StudyStates[,c('Method.Code')]
      
      # input 'Method_Name' into input_mat1
      this_col_input_mat <- 'Method_Name'
      this_col_AQS <- 'Method.Name'
      AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
      AQSVarChar <- as.character(AQSVar)
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # input 'PM25_Station_Name' into input_mat1
      this_col_input_mat <- 'PM25_Station_Name'
      this_col_AQS <- 'Local.Site.Name'
      AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
      #print(AQSVar)
      AQSVarChar <- as.character(AQSVar)
      #print(AQSVarChar)
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # input 'Address' into input_mat1
      this_col_input_mat <- 'Address'
      this_col_AQS <- 'Address'
      AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
      #print(AQSVar)
      AQSVarChar <- as.character(AQSVar)
      #print(AQSVarChar)
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # input 'State_Name' into input_mat1
      this_col_input_mat <- 'State_Name'
      this_col_AQS <- 'State.Name'
      AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
      #print(AQSVar)
      AQSVarChar <- as.character(AQSVar)
      #print(AQSVarChar)
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # input 'County_Name' into input_mat1
      this_col_input_mat <- 'County_Name'
      this_col_AQS <- 'County.Name'
      AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
      #print(AQSVar)
      AQSVarChar <- as.character(AQSVar)
      #print(AQSVarChar)
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # input 'City_Name' into input_mat1
      this_col_input_mat <- 'City_Name'
      this_col_AQS <- 'City.Name'
      AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
      #print(AQSVar)
      AQSVarChar <- as.character(AQSVar)
      #print(AQSVarChar)
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # input 'CBSA_Name' into input_mat1
      this_col_input_mat <- 'CBSA_Name'
      this_col_AQS <- 'CBSA.Name'
      AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
      #print(AQSVar)
      AQSVarChar <- as.character(AQSVar)
      #print(AQSVarChar)
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # input 'Date_of_Last_Change' into input_mat1
      this_col_input_mat <- 'Date_of_Last_Change'
      this_col_AQS <- 'Date.of.Last.Change'
      AQSVar <- as.Date(ThisAQSdata_StudyStates[,c(this_col_AQS)],"%Y-%m-%d")
      #print(AQSVar)
      AQSVarChar <- format(AQSVar,"%Y-%m-%d")
      #print(AQSVarChar)
      input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
      rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
      
      # Note: 'State_Abbrev' is filled in after the double for loop
      
      # Note: 'Winter' is filled in near the end of the script
      
      # Note: 'Year' is filled in near the end of the script
      
      # Note: 'Month' is filled in near the end of the script
      
      # Note: 'Day' is filled in near the end of the script
      
      # input 'Data_Source_Name_Display' into input_mat1
      input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
      
      # input 'Data_Source_Name_Short' into input_mat1
      input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short
      
      # input data source counter - indicates if this is EPA data or field data, etc.
      input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
      
      # input color for this data source for plots (totally arbitrary choice)
      input_mat1[row_start:row_stop,c("PlottingColor")] <- "black"
      
      # input 'Source_File' name
      input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
      
      # input the 'Composite_of_N_rows' - this variable indicates how many separate rows of 
      # data were composited to form this row of data. This will be relevant when getting rid of repeated data.
      # For now, this is set to 1 because repeated rows of data will be consolidated in a later script.
      input_mat1[row_start:row_stop,c('Composite_of_N_rows')] <- 1
      
      # 'N_Negative_Obs' is filled in below the double for loop
      
      # update row counter
      row_start=row_stop+1
      
      # clear variables before moving on to next iteration of loop
      rm(this_source_file,ThisAQSdata_StudyStates)
    } # for(this_POC in POC_vec){ # cycle through POC
  } # for(this_year in start_study_year:stop_study_year){     # cycle through years
  
  # # put in state abbreviations
  #repeated_name_numbers=input_mat1[,c("State_Name","State_Code")]
  #non_repeat_name_numbers <- repeated_name_numbers[!duplicated(repeated_name_numbers), ]
  #print(non_repeat_name_numbers)
  #rm(repeated_name_numbers,non_repeat_name_numbers)
  
  # put in state abbreviations, 'State_Abbrev' 
  all_state_codes <- unique(input_mat1$State_Code)
  for (state_code in all_state_codes) { # cycle through all of the state codes found in the data
    state_rows <- which(input_mat1$State_Code == state_code) # which rows are for this state?
    state_abbrev <- State_Abbrev_Definitions.fn(state_code) # what is the abbreviation for this state?
    input_mat1[state_rows,c("State_Abbrev")] <- state_abbrev # fill in the abbreviation in the rows for this state
  } # for (state_code in all_state_abbrevs) {
  
  # input 'N_Negative_Obs' into input_mat1 - this is to note negative concentrations
  which_negative <- which(input_mat1[,c("PM2.5_Obs")]<0)
  input_mat1[,c('N_Negative_Obs')] <- 0 # set all to zero to avoid NA's
  input_mat1[which_negative,c('N_Negative_Obs')] <- 1 # set rows with negative values to 1 so they'll be easy to find later
  rm(which_negative)
  
  # input "InDayLatDiff","InDayLonDiff" - which will all be zero for AQS data since there is only one
  # row of data for lat & lon on a given day
  # (for the DRI data, there are multiple measurements of lat/lon in a day and sometimes they don't all match, these variables give max-min for lat & lon in a given day)
  input_mat1[,c("InDayLatDiff")] <- 0
  input_mat1[,c("InDayLonDiff")] <- 0
  
  rm(ParameterCode_vec,this_year,this_ParamCode)
  rm(Data_Source_Name_Display,Data_Source_Name_Short)

#### output input_mat1 from function ####  
  return(input_mat1) # output from function
} # end function