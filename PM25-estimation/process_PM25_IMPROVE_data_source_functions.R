process_PM25_IMPROVE_data_source.fn <- function(input_header, ProcessedData.directory, IMPROVEData.directory, 
                                                data_set_counter, this_plotting_color, this_source_file, 
                                                this_source_file_full, short_name, skip_n_lines) {
  # combine IMPROVE PM2.5 data files into 1 dataframe
  
  ##### Create Sink output file and create its header ####
  # sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
  SinkFileName=file.path(ProcessedData.directory,paste("PM25_data_source_",short_name,"_combining_sink.txt", sep = ""))
  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
  cat(paste("Code and R output for process_PM25_IMPROVE_data_source_functions.R for",this_source_file," \n \n"))
  cat("Title: process_PM25_IMPROVE_data_source_function.R \n")
  cat("Author: Melissa May Maestas, PhD \n")
  cat("Original Date: October 11, 2018 \n")
  cat("Latest Update: October 11, 2018 \n")
  cat(paste("Script ran and this text file created ",Sys.time(),sep = ""))
  cat(paste("This program reads in and PM2.5 data from the ",short_name,". \n",sep = ""))
 
  #### Fill in data from Federal Land Managers - IMPROVE  ######################
  data_source_counter <- data_set_counter

  print(this_source_file)
  
  # load FMLE data
  FMLEdata_all_states <- read.csv(file.path(FMLE.directory,this_source_file), header = T, sep = ",",blank.lines.skip = F)
  # load parameter description
  FMLEdata_Parameter_MetaData <- read.csv(file.path(FMLE.directory,this_source_file_full), header = T, sep = ",",blank.lines.skip = T,nrows = 1,skip = skip_n_lines) #240)
  
  # isolate data from the Study area
  FMLE_StudyStates <- FMLEdata_all_states[which(FMLEdata_all_states$State=="AZ"|
                                                  FMLEdata_all_states$State=="CA"|
                                                  FMLEdata_all_states$State=="CO"|
                                                  FMLEdata_all_states$State=="ID"|
                                                  FMLEdata_all_states$State=="MT"|
                                                  FMLEdata_all_states$State=="NV"|
                                                  FMLEdata_all_states$State=="NM"|
                                                  FMLEdata_all_states$State=="OR"|
                                                  FMLEdata_all_states$State=="UT"|
                                                  FMLEdata_all_states$State=="WA"|
                                                  FMLEdata_all_states$State=="WY"|
                                                  FMLEdata_all_states$State=="ND"|
                                                  FMLEdata_all_states$State=="SD"|
                                                  FMLEdata_all_states$State=="NE"|
                                                  FMLEdata_all_states$State=="KS"|
                                                  FMLEdata_all_states$State=="OK"|
                                                  FMLEdata_all_states$State=="TX"), ]
  # State Abbreviations 
  # 4 "AZ"
  # 6 "CA"
  # 8 "CO"
  # 16 "ID"
  # 20 "KS"
  # 30 "MT"
  # 31 "NE"
  # 32 "NV"
  # 35 "NM"
  # 38 "ND"
  # 40 "OK"
  # 41 "OR"
  # 46 "SD"
  # 48 "TX"
  # 49 "UT"
  # 53 "WA"
  # 56 "WY"
  rm(FMLEdata_all_states)
  
  row_stop <- row_start+dim(FMLE_StudyStates)[1]-1 # what is the last row number in input_mat1 for inputing this block of data?
  
  #### Create data frame  ####
  input_mat1 <- data.frame(matrix(NA,nrow=dim(FMLE_StudyStates)[1],ncol=length(input_header))) # create data frame for input_mat1
  names(input_mat1) <- input_header # assign the header to input_mat1
  input_mat1 <- input_mat_change_data_classes.fn(input_mat1)
  
  
  # fill in columns of data
  
  # Split FMLE EPACode into State_Code, County_Code and Site_Num for IMPROVE data and put them into input_mat1
  N_FMLE_EPACodes <- length(unique(FMLE_StudyStates$EPACode))
  FMLE_EPACode_header <-  c("EPACode","StateCode","CountyCode","SiteNum")
  N_EPACode_columns <- length(FMLE_EPACode_header) # how many columns are in header?
  FMLE_EPACode <- data.frame(matrix(NA,nrow=N_FMLE_EPACodes,ncol=N_EPACode_columns)) # create data frame for input_mat1
  names(FMLE_EPACode) <- FMLE_EPACode_header # assign the header to input_mat1
  FMLE_EPACode$EPACode <- unique(FMLE_StudyStates$EPACode)
  
  # Split FMLE EPACode into State_Code, County_Code and Site_Num for IMPROVE data and put them into input_mat1
  
  for (this_row in 1:N_FMLE_EPACodes) { # cycle through each row in FMLE data to determine state code, county code, and site num and put into input_mat1
    #for (this_row in 1:dim(FMLE_StudyStates)[1]) { # cycle through each row in FMLE data to determine state code, county code, and site num and put into input_mat1
    # print("this_row")
    #  print(this_row)   
    #    #print(cat("this row = ",this_row))
    #  #print("loop starting on row 1569 of Create_ML_Input_File.R")
    #  #print((this_row-row_start)/(row_stop-row_start)*100)
    #  #print("% done")
    #  FMLE_row <- FMLE_row+1
    #  print("FMLE_row")
    #  print(FMLE_row)
    #  print("of")
    #  print(dim(FMLE_StudyStates)[1])
    this_EPACode <- as.character((FMLE_EPACode[this_row,c("EPACode")])) # isolate the EPA code for this row of data
    print(this_EPACode)
    if (is.na(this_EPACode)==TRUE) {
      FMLE_EPACode[this_row,c("StateCode")] <- NA
      FMLE_EPACode[this_row,c("CountyCode")] <- NA
      FMLE_EPACode[this_row,c("SiteNum")] <- NA
    } else if (nchar(this_EPACode)==8) { # determine how many characters are in EPACode (leading zeros are not in the data)
      print("8 characters")
      
      FMLE_EPACode[this_row,c("StateCode")] <- substr(this_EPACode,1,1) # isolate state code
      FMLE_EPACode[this_row,c("CountyCode")] <- substr(this_EPACode,2,4) # isolate county code
      FMLE_EPACode[this_row,c("SiteNum")] <- substr(this_EPACode,5,8)  # isolate site num
      
    } else if (nchar(this_EPACode)==9) {
      print("9 characters")
      FMLE_EPACode[this_row,c("StateCode")] <- substr(this_EPACode,1,2) # isolate state code
      FMLE_EPACode[this_row,c("CountyCode")] <- substr(this_EPACode,3,5) # isolate county code
      FMLE_EPACode[this_row,c("SiteNum")] <- substr(this_EPACode,6,9)  # isolate site num
    } else {# if (nchar(this_EPACode)==8) { # determine how many characters are in EPACode (leading zeros are not in the data)
      stop("check data/code")
    }
    # # fill in variables
    # input_mat1[this_row,c("State_Code")] <- this_state_code # input state code
    # input_mat1[this_row,c("County_Code")] <- this_county_code # input county code
    # input_mat1[this_row,c("Site_Num")] <- this_site_num # input site num
    # rm(this_EPACode,this_state_code,this_county_code,this_site_num) # clear variables
    rm(this_EPACode)
  } # for (this_row in row_start:row_stop) { # cycle through each row in FMLE data to determine state code, county code, and site num and put into input_mat1
  rm(this_row)
  
  # add new columns at the end of FMLE_EPACode with state code, county code, and site number (which were derived from the EPAcode)
  
  # # add column for state code
  #new_col_number <- length(this_Fire_Cache_data)+1 # figure out how many columns are in data and then add 1
  #this_Fire_Cache_data[,new_col_number] <- as.Date(this_Fire_Cache_data[,1],"%m/%d/%Y") # add column at end of data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
  #colnames(this_Fire_Cache_data)[new_col_number] <- "R_Dates"
  #rm(new_col_number)
  
  # create a new version of of FMLE_StudyStates that has the State Code, County Code, and Site num as columns at the end
  #N_FMLE_EPACodes <- length(unique(FMLE_StudyStates$EPACode))
  FMLE_StudyStates_sepCodes <- data.frame(matrix(NA,nrow=dim(FMLE_StudyStates)[1],ncol=dim(FMLE_StudyStates)[2]+3)) # create data frame for input_mat1
  names(FMLE_StudyStates_sepCodes) <- c(colnames(FMLE_StudyStates),"StateCode","CountyCode","SiteNum") # assign the header 
  FMLE_StudyStates_sepCodes[,1:dim(FMLE_StudyStates)[2]] <- FMLE_StudyStates
  rm(FMLE_StudyStates)
  
  for (this_row in 1:dim(FMLE_EPACode)[1]) { # put columns of state code, county code, and site number into FMLE_StudyStates_sepCodes
    # what are the codes for this row of FMLE_EPACode?
    this_code <- FMLE_EPACode[this_row,c("EPACode")]
    this_state <- FMLE_EPACode[this_row,c("StateCode")]
    this_county <- FMLE_EPACode[this_row,c("CountyCode")]
    this_siteNum <- FMLE_EPACode[this_row,c("SiteNum")]
    print(this_code) # this row of code
    # what rows in FMLE_StudyStates_sepCodes has this EPA code?
    rows_of_interest <- which(FMLE_StudyStates_sepCodes$EPACode==this_code)
    FMLE_StudyStates_sepCodes[rows_of_interest,c("StateCode")] <- this_state
    FMLE_StudyStates_sepCodes[rows_of_interest,c("CountyCode")] <- this_county
    FMLE_StudyStates_sepCodes[rows_of_interest,c("SiteNum")] <- this_siteNum
    rm(this_code,this_state,this_county,this_siteNum,rows_of_interest)
  }
  rm(this_row)
  
  # State Code
  input_mat1[row_start:row_stop,c("State_Code")] <- as.character(FMLE_StudyStates_sepCodes$StateCode)
  
  # County Code
  input_mat1[row_start:row_stop,c("County_Code")] <- as.character(FMLE_StudyStates_sepCodes$CountyCode)
  
  # Site Number
  input_mat1[row_start:row_stop,c("Site_Num")] <- as.character(FMLE_StudyStates_sepCodes$SiteNum)
  
  # "Parameter_Code" 
  input_mat1[row_start:row_stop,c("Parameter_Code")] <- FMLEdata_Parameter_MetaData$AQSCode
  # "Parameter_Name" 
  input_mat1[row_start:row_stop,c("Parameter_Name")] <- as.character(FMLEdata_Parameter_MetaData$Parameter)
  # "Method_Code" 
  input_mat1[row_start:row_stop,c("Method_Code")] <- as.character(FMLEdata_Parameter_MetaData$Code)
  # "Method_Name" 
  input_mat1[row_start:row_stop,c("Method_Name")] <- as.character(FMLEdata_Parameter_MetaData$Description)
  
  # "POC"  
  input_mat1[row_start:row_stop,c("POC")] <- FMLE_StudyStates_sepCodes$POC
  
  # "PM2.5_Lat"               
  input_mat1[row_start:row_stop,c("PM2.5_Lat")] <- FMLE_StudyStates_sepCodes$Latitude
  
  #"PM2.5_Lon"                
  input_mat1[row_start:row_stop,c("PM2.5_Lon")] <- FMLE_StudyStates_sepCodes$Longitude
  
  #"Sample_Duration"
  input_mat1[row_start:row_stop,c("Sample_Duration")] <- "24 HOUR" # these are daily observations
  
  # input "Date_Local" into input_mat1
  my_date_col <- factor(FMLE_StudyStates_sepCodes[,c("Date")])
  input_mat1[row_start:row_stop,c("Date_Local")] <- as.character(as.Date(my_date_col,format = "%m/%d/%Y"))
  rm(my_date_col)
  
  # "Units_of_Measure"
  input_mat1[row_start:row_stop,c("Units_of_Measure")] <- as.character(FMLE_StudyStates_sepCodes[,c("MF.Unit")])
  
  # "Observation_Count"        
  input_mat1[row_start:row_stop,c("Observation_Count")] <- 1
  
  # "Observation_Percent"      
  input_mat1[row_start:row_stop,c("Observation_Percent")] <- 100
  
  # "PM2.5_Obs"   
  input_mat1[row_start:row_stop,c("PM2.5_Obs")] <- as.numeric(FMLE_StudyStates_sepCodes[,c("MF.Val")])
  
  # "PM25_Station_Name" 
  input_mat1[row_start:row_stop,c("PM25_Station_Name")] <- as.character(paste(FMLE_StudyStates_sepCodes[,c("SiteName")],FMLE_StudyStates_sepCodes[,c("SiteCode")])) #"SiteName" "SiteCode"
  
  # "Data_Source_Name_Display"
  input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <-paste(as.character(FMLE_StudyStates_sepCodes[,c("Dataset")]),as.character(FMLEdata_Parameter_MetaData$Code),"II",sep = " ") #as.character(FMLE_StudyStates_sepCodes[,c("Dataset")]) #"Dataset" 
  
  # "Data_Source_Name_Short"
  input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- paste(as.character(FMLE_StudyStates_sepCodes[,c("Dataset")]),as.character(FMLEdata_Parameter_MetaData$Code),"II",sep = "")#as.character(FMLE_StudyStates_sepCodes[,c("Dataset")])# "Dataset" 
  
  # "State_Abbrev" 
  input_mat1[row_start:row_stop,c("State_Abbrev")] <- as.character(FMLE_StudyStates_sepCodes[,c("State")])
  
  # "Data_Source_Counter"      
  input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
  
  # input color for plotting this data source (totally arbitrary choice of color)
  input_mat1[row_start:row_stop,c("PlottingColor")] <- "lightsalmon4" #"blue"
  
  # "Source_File"              
  input_mat1[row_start:row_stop,c("Source_File")] <- this_source_file
  rm(this_source_file)
  
  # "Composite_of_N_rows"      
  input_mat1[row_start:row_stop,c("Composite_of_N_rows")] <- 1 # not a composite of anything
  
  # "N_Negative_Obs"  
  # which rows have negative PM2.5 obs?
  input_mat1[row_start:row_stop,c("N_Negative_Obs")] <- 0 # initially set all to 0 and then set the ones with negative values to 1
  which_negative <- which(input_mat1$Data_Source_Counter==data_source_counter & input_mat1$PM2.5_Obs<0)
  input_mat1[which_negative,c("N_Negative_Obs")] <- 1
  rm(which_negative)
  
  # "InDayLatDiff"
  input_mat1[row_start:row_stop,c("InDayLatDiff")] <- 0 # with only one observations on a given day, you can't have any variation in the location of that observation
  
  # "InDayLonDiff"  
  input_mat1[row_start:row_stop,c("InDayLonDiff")] <- 0 # with only one observations on a given day, you can't have any variation in the location of that observation
  
  # think about whether to add anything for these variables for IMPROVE data
  #         "flg.Lat"                  "flg.Lon"                 
  # "Type"                     "flg.Type"                 "flg.Site_Num"             "flg.PM25_Obs"             "l/m Ave. Air Flw"         "flg.AirFlw"              
  #"Deg C Av Air Temp"        "flg.AirTemp"              "% Rel Humidty"            "flg.RelHumid"             "mbar Barom Press "        ",flg.,Barom,Press"       
  #"deg C Sensor  Int AT"     "flg.deg C Sensor Int AT"  "% Sensor Int RH"          "flg.%SensorIntRH"         "Wind Speed m/s"           "flg.WindSpeed"           
  #"Battery Voltage volts"    "flg.BatteryVoltage"       "Alarm"                    "flg.Alarm"                
  
  # need to find ways to fill in these variables in input_mat1:
  # "Datum" "State_Name" "Winter"    "Year"                     "Month"                    "Day"      
  # decide if these variables need to be filled in:
  # "Event_Type"   "1st_Max_Value" "1st_Max_Hour"   "AQI"  "Pollutant_Standard"    "Address"       
  # "County_Name"              "City_Name"                "CBSA_Name"                "Date_of_Last_Change"     
  
  #colnames(FMLE_StudyStates)
  #[1]         "Aggregation"         "Elevation"  
  #      "MF.Method"     "MF.Unc"        "MF.Mdl" "MF.StatusFlag" "MF.Flag1"      "MF.Flag2"     
  #[21] "MF.Flag3"      "MF.Flag4"      "MF.Flag5"      "MF.AuxValue1"  "MF.AuxValue2" 
  row_start <- row_stop+1
  rm(FMLE_EPACode,FMLE_StudyStates_sepCodes,FMLEdata_Parameter_MetaData)
  rm(FMLE_EPACode_header,N_EPACode_columns,N_FMLE_EPACodes)
  
  
  
  # output to file #  
  write.csv(input_mat1,file = file.path(ProcessedData.directory,paste(Data_Source_Name_Short,Sys.Date(),'_Step1.csv',sep = "")),row.names = FALSE)
  
  print(paste("finished processing ", Data_Source_Name_Display))
  
  # clear variables
  rm(this_column, this_name)
  rm(new_col_number,this_column,this_name,this_source_file) 
  rm(IMPROVEdata,IMPROVELocations)#,IMPROVEstationsChar,IMPROVEstations)
  rm(Data_Source_Name_Display,Data_Source_Name_Short)
  rm(this_source_file_full)
  
  # output input_mat1 from function #  
  return(input_mat1) # output from function  
  
} # end of process_PM25_IMPROVE_data_source.fn function