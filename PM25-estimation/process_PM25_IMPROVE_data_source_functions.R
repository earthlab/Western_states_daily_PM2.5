process_PM25_IMPROVE_data_source.fn <- function(input_header, ProcessedData.directory, #IMPROVEData.directory, 
                                                data_set_counter, this_plotting_color, this_source_file, 
                                                this_source_file_full, skip_n_lines, column_prefix) { # short_name,
  
  #### Fill in data from Federal Land Managers - IMPROVE  ######################
  data_source_counter <- data_set_counter
  print(this_source_file)
  stop("put in datum information, WGS84 per email from Bret Schichtel on October 22, 2018")
  # load FMLE data
  FMLEdata_all_states <- read.csv(file.path(FMLE.directory,this_source_file), header = T, sep = ",",blank.lines.skip = F)
  print(FMLE.directory)
  # load parameter description
  FMLEdata_Parameter_MetaData <- read.csv(file.path(FMLE.directory,this_source_file_full), header = T, sep = ",",blank.lines.skip = T,nrows = 1,skip = skip_n_lines) #240)
  
  Data_Source_Name_Short <- unique(paste(as.character(  FMLEdata_all_states$Dataset),as.character(FMLEdata_Parameter_MetaData$Code),as.character(FMLEdata_Parameter_MetaData$AQSCode),"_",as.character(FMLEdata_Parameter_MetaData$DatasetID),sep = "")) # "Data_Source_Name_Display" 
  print(Data_Source_Name_Short) # display data source name (with spaces)
  
  ##### Create Sink output file and create its header ####
  # sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
  #SinkFileName=file.path(ProcessedData.directory,paste("PM25_data_source_",Data_Source_Name_Short,"_combining_sink_part_",processed_data_version,".txt", sep = ""))
  file_sub_label <- paste("PM25_",Data_Source_Name_Short,"_Step1_",Sys.Date(),"_part_",processed_data_version,sep = "")
  SinkFileName=file.path(ProcessedData.directory,paste(file_sub_label,"_combining_sink.txt",sep = ""))
  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
  cat(paste("Code and R output for process_PM25_IMPROVE_data_source_functions.R for",this_source_file," \n \n"))
  cat("Title: process_PM25_IMPROVE_data_source_function.R \n")
  cat("Author: Melissa May Maestas, PhD \n")
  cat("Original Date: October 11, 2018 \n")
  cat("Latest Update: October 14, 2018 \n")
  cat(paste("Script ran and this text file created ",Sys.time(),sep = ""))
  cat(paste("This program reads in and PM2.5 data from the ",Data_Source_Name_Short,". \n",sep = ""))
  
  # isolate data from the Study area
  FMLE_StudyStates <- subset_data_frame_via_vector.fn(vector_for_subset = study_states_abbrev, full_data_frame = FMLEdata_all_states, col_for_subset = "State")
  rm(FMLEdata_all_states)

  # Split FMLE EPACode into State_Code, County_Code and Site_Num for IMPROVE data and put them into input_mat1
  EPACode_components <- EPA_codes_2_components_no_hyphens.fn(EPA_codes_vec = unique(FMLE_StudyStates$EPACode))
  
  # create a new version of of FMLE_StudyStates that has the State Code, County Code, and Site num as columns at the end
  FMLE_StudyStates_sepCodes <- data.frame(matrix(NA,nrow=dim(FMLE_StudyStates)[1],ncol=dim(FMLE_StudyStates)[2]+3)) # create data frame
  names(FMLE_StudyStates_sepCodes) <- c(colnames(FMLE_StudyStates),"StateCode","CountyCode","SiteNum") # assign the header 
  FMLE_StudyStates_sepCodes[,1:dim(FMLE_StudyStates)[2]] <- FMLE_StudyStates # fill in all but last 3 columns wirh FMLE_StudyStates
  rm(FMLE_StudyStates) # clear variable
  
  # fill in the EPA code components into FMLE_StudyStates_sepCodes
  FMLE_StudyStates_sepCodes <- fill_in_FMLE_code_components.fn(FMLE_StudyStates_sepCodes,EPACode_components)
  
  # Create input_mat1 data frame
  input_mat1 <- data.frame(matrix(NA,nrow=dim(FMLE_StudyStates_sepCodes)[1],ncol=length(input_header))) # create data frame for input_mat1
  names(input_mat1) <- input_header # assign the header to input_mat1
  input_mat1 <- input_mat_change_data_classes.fn(input_mat1) # set data classes for columns in input_mat1
  
  # fill in input_mat1
  input_mat1$State_Code <- as.character(FMLE_StudyStates_sepCodes$StateCode) # State Code
  input_mat1$County_Code <- as.character(FMLE_StudyStates_sepCodes$CountyCode) # County Code
  input_mat1$Site_Num <- as.character(FMLE_StudyStates_sepCodes$SiteNum) # Site Number
  input_mat1$Parameter_Code <- FMLEdata_Parameter_MetaData$AQSCode # "Parameter_Code" 
  input_mat1$Parameter_Name <- as.character(FMLEdata_Parameter_MetaData$Parameter) # "Parameter_Name"
  input_mat1$Method_Code <- as.character(FMLEdata_Parameter_MetaData$Code) # "Method_Code" 
  input_mat1$Method_Name <- as.character(FMLEdata_Parameter_MetaData$Description) # "Method_Name" 
  input_mat1$POC <- FMLE_StudyStates_sepCodes$POC # "POC" 
  input_mat1$PM2.5_Lat <- FMLE_StudyStates_sepCodes$Latitude # "PM2.5_Lat" 
  input_mat1$PM2.5_Lon <- FMLE_StudyStates_sepCodes$Longitude #"PM2.5_Lon" 
  input_mat1$Sample_Duration <- "24 HOUR" #"Sample_Duration", these are daily observations
  input_mat1$Date_Local <- as.Date(FMLE_StudyStates_sepCodes$Date,format = "%m/%d/%Y") # input "Date_Local" into input_mat1
  #input_mat1$Units_of_Measure <- as.character(FMLE_StudyStates_sepCodes[,c("MF.Unit")]) # "Units_of_Measure"
  input_mat1$Units_of_Measure <- as.character(FMLEdata_Parameter_MetaData$Units) # "Units_of_Measure"
  input_mat1$Observation_Count <- 1 # "Observation_Count"  
  input_mat1$Observation_Percent <- 100 # "Observation_Percent"
  #input_mat1$PM2.5_Obs <- as.numeric(FMLE_StudyStates_sepCodes[,c("MF.Val")]) # "PM2.5_Obs"
  input_mat1$PM2.5_Obs <- as.numeric(FMLE_StudyStates_sepCodes[,c(paste(column_prefix,".Val", sep = ""))]) # "PM2.5_Obs"
  input_mat1$PM25_Station_Name <- as.character(paste(FMLE_StudyStates_sepCodes$SiteName,FMLE_StudyStates_sepCodes$SiteCode)) # "PM25_Station_Name"  #"SiteName" "SiteCode"
  input_mat1$Data_Source_Name_Display <-paste(as.character(FMLE_StudyStates_sepCodes$Dataset),as.character(FMLEdata_Parameter_MetaData$Code),as.character(FMLEdata_Parameter_MetaData$AQSCode),as.character(FMLEdata_Parameter_MetaData$DatasetID),sep = " ") # "Data_Source_Name_Display" 
  Data_Source_Name_Display <- unique(input_mat1$Data_Source_Name_Display) # define Data_Source_Name_Short
  print(Data_Source_Name_Display) # display data source name (with spaces)
  
  input_mat1$Data_Source_Name_Short <-paste(as.character(FMLE_StudyStates_sepCodes$Dataset),as.character(FMLEdata_Parameter_MetaData$Code),as.character(FMLEdata_Parameter_MetaData$AQSCode),"_",as.character(FMLEdata_Parameter_MetaData$DatasetID),sep = "") # "Data_Source_Name_Display" 
  Data_Source_Name_Short <- unique(input_mat1$Data_Source_Name_Short) # define Data_Source_Name_Short
  print(Data_Source_Name_Short) # display data source name (with spaces)
  
  #input_mat1$Data_Source_Name_Short <- paste(as.character(FMLE_StudyStates_sepCodes[,c("Dataset")]),as.character(FMLEdata_Parameter_MetaData$Code),as.character(FMLEdata_Parameter_MetaData$AQSCode,as.character(FMLEdata_Parameter_MetaData$DatasetID)),sep = "") # "Data_Source_Name_Short" 
  #Data_Source_Name_Short <- unique(input_mat1$Data_Source_Name_Short) # define Data_Source_Name_Short
  #print(Data_Source_Name_Short) # display data source name (without spaces)
  input_mat1$State_Abbrev <- as.character(FMLE_StudyStates_sepCodes$State) # "State_Abbrev"
  input_mat1$Data_Source_Counter <- data_source_counter # "Data_Source_Counter" 
  input_mat1$PlottingColor <- this_plotting_color #"lightsalmon4" #"blue" # input color for plotting this data source (totally arbitrary choice of color)
  input_mat1$Source_File <- this_source_file # "Source_File" 
  input_mat1$Composite_of_N_rows <- 1 # "Composite_of_N_rows", not a composite of anything
  input_mat1$N_Negative_Obs <- 0 # "N_Negative_Obs", initially set all to 0 and then set the ones with negative values to 1
  which_negative <- which(input_mat1$PM2.5_Obs<0) # "N_Negative_Obs", which rows have negative PM2.5 obs?
  input_mat1[which_negative,c("N_Negative_Obs")] <- 1 # "N_Negative_Obs",
  print(paste(length(which_negative),"negative concentrations")) # display number of negative concentrations
  rm(which_negative) # clear variable
  input_mat1$InDayLatDiff <- 0 # "InDayLatDiff", with only one observations on a given day, you can't have any variation in the location of that observation
  input_mat1$InDayLonDiff <- 0 # "InDayLonDiff", with only one observations on a given day, you can't have any variation in the location of that observation
  input_mat1$Year <- input_mat_extract_year_from_date.fn(input_mat1$Date_Local) # "Year" 
  input_mat1$Month <- input_mat_extract_month_from_date.fn(input_mat1$Date_Local) # "Month"
  input_mat1$Day <- input_mat_extract_day_from_date.fn(input_mat1$Date_Local) # "Day"
  input_mat1 <- fill_in_StateNames_from_Code.fn(input_data_frame = input_mat1, state_code_col = "State_Code", state_name_col = "State_Name") #"State_Name"
  
  # think about whether to add anything for these variables for IMPROVE data
  #         "flg.Lat"                  "flg.Lon"                 
  # "Type"                     "flg.Type"                 "flg.Site_Num"             "flg.PM25_Obs"             "l/m Ave. Air Flw"         "flg.AirFlw"              
  #"Deg C Av Air Temp"        "flg.AirTemp"              "% Rel Humidty"            "flg.RelHumid"             "mbar Barom Press "        ",flg.,Barom,Press"       
  #"deg C Sensor  Int AT"     "flg.deg C Sensor Int AT"  "% Sensor Int RH"          "flg.%SensorIntRH"         "Wind Speed m/s"           "flg.WindSpeed"           
  #"Battery Voltage volts"    "flg.BatteryVoltage"       "Alarm"                    "flg.Alarm"                
  
  # need to find ways to fill in these variables in input_mat1:
  # "Datum" "Winter"         
  # decide if these variables need to be filled in:
  # "Event_Type"   "1st_Max_Value" "1st_Max_Hour"   "AQI"  "Pollutant_Standard"    "Address"       
  # "County_Name"              "City_Name"                "CBSA_Name"                "Date_of_Last_Change"     
  
  #colnames(FMLE_StudyStates)
  #[1]         "Aggregation"         "Elevation"  
  #      "MF.Method"     "MF.Unc"        "MF.Mdl" "MF.StatusFlag" "MF.Flag1"      "MF.Flag2"     
  #[21] "MF.Flag3"      "MF.Flag4"      "MF.Flag5"      "MF.AuxValue1"  "MF.AuxValue2" 

  print(paste("This data has",dim(input_mat1)[1],"rows of PM2.5 observations.")) # how many rows of data?
  
  # output to file #  
  #write.csv(input_mat1,file = file.path(ProcessedData.directory,paste(Data_Source_Name_Short,"_",Sys.Date(),'_Step1_part_',processed_data_version,'.csv',sep = "")),row.names = FALSE)
  write.csv(input_mat1,file = file.path(ProcessedData.directory,paste(file_sub_label,'.csv',sep = "")),row.names = FALSE)
  
  print(paste("finished processing ", Data_Source_Name_Display))
  
  # clear variables
  rm(this_source_file)
  rm(FMLE_StudyStates_sepCodes)
  rm(Data_Source_Name_Display,Data_Source_Name_Short)
  rm(this_source_file_full)
  rm(EPACode_components,FMLEdata_Parameter_MetaData)
  
  # output input_mat1 from function #  
  return(input_mat1) # output from function  
  
} # end of process_PM25_IMPROVE_data_source.fn function

# fill in the EPA code components into FMLE_StudyStates_sepCodes
fill_in_FMLE_code_components.fn <- function(FMLE_StudyStates_sepCodes,EPACode_components) {
  for (this_row in 1:dim(EPACode_components)[1]) { # put columns of state code, county code, and site number into FMLE_StudyStates_sepCodes
    this_code <- EPACode_components[this_row,c("EPACode")] # what are the codes for this row of EPACode_components?
    #print(this_code) # this row of code
    rows_of_interest <- which(FMLE_StudyStates_sepCodes$EPACode==EPACode_components[this_row,c("EPACode")]) # what rows in FMLE_StudyStates_sepCodes has this EPA code?
    FMLE_StudyStates_sepCodes[rows_of_interest,c("StateCode")] <- EPACode_components[this_row,c("StateCode")] # input state code
    FMLE_StudyStates_sepCodes[rows_of_interest,c("CountyCode")] <- EPACode_components[this_row,c("CountyCode")] # input county code
    FMLE_StudyStates_sepCodes[rows_of_interest,c("SiteNum")] <- EPACode_components[this_row,c("SiteNum")] # input site number
    rm(this_code,rows_of_interest) # clear variable
  } # for (this_row in 1:dim(EPACode_components)[1]) { # put columns of state code, county code, and site number into FMLE_StudyStates_sepCodes
  rm(this_row) # clear variable
  return(FMLE_StudyStates_sepCodes) # output from function
} # end of fill_in_FMLE_code_components.fn function
