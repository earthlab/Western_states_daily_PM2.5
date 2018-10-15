process_PM25_PCAPS_data_source.fn <- function(input_header, ProcessedData.directory, PCAPSData.directory, data_set_counter, this_plotting_color) {
  # combine PCAPS PM2.5 data files into 1 dataframe
  
  #### Fill in Salt Lake City PCAPS data ############################
  data_source_counter=data_set_counter
  Data_Source_Name_Short <- "PCAPS"
  Data_Source_Name_Display <- "PCAPS (Salt Lake Valley)"
  this_datum <- "WGS84" # per email from Erik Crosman Oct 12, 2018
  
  ##### Create Sink output file and create its header ####
  # sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
  #SinkFileName=file.path(ProcessedData.directory,paste("PM25_data_source_PCAPS_combining_sink_part",processed_data_version,".txt",sep = ""))
  file_sub_label <- paste("PM25_",Data_Source_Name_Short,"_Step1_",Sys.Date(),"_part_",processed_data_version,sep = "")
  SinkFileName=file.path(ProcessedData.directory,paste(file_sub_label,"_combining_sink.txt",sep = ""))
  
  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
  cat("Code and R output for process_PM25_PCAPS_data_source_functions.R \n \n")
  cat("Title: process_PM25_PCAPS_data_source_function.R \n")
  cat("Author: Melissa May Maestas, PhD \n")
  cat("Original Date: October 11, 2018 \n")
  cat("Latest Update: October 11, 2018 \n")
  cat(paste("Script ran and this text file created ",Sys.time(),sep = ""))
  cat("This program reads in and PM2.5 data from the PCAPS. \n")
  
  # load the data file
  this_source_file <- "MiniVol_data_dates.csv"
  print(this_source_file)
  PCAPSdata<-read.csv(file.path(PCAPSData.directory,this_source_file),header=TRUE) 
  
  # load file containing lat/lon info for PCAPS sites
  PCAPSLocations<-read.csv(file.path(PCAPSData.directory,"PCAPS_Site_Locations.csv"),header=TRUE) 
  
  #### Create data frame  ####
  input_mat1 <- data.frame(matrix(NA,nrow=dim(PCAPSdata)[1],ncol=length(input_header))) # create data frame for input_mat1
  names(input_mat1) <- input_header # assign the header to input_mat1
  input_mat1 <- input_mat_change_data_classes.fn(input_mat1)

  
  
  # handle date information
  new_col_number <- length(PCAPSdata)+1
  PCAPSdata[,new_col_number] <- as.Date(PCAPSdata[,c("Dates")],"%m/%d/%Y") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
  colnames(PCAPSdata)[new_col_number] <- "R_Dates"
  
  # input station names
  input_mat1$PM25_Station_Name <- as.character(PCAPSdata$Location)
  
  # input state information
  input_mat1$State_Code <- 49 # "State_Code" 
  input_mat1$State_Name <- "Utah" # "State_Name"
  input_mat1$State_Abbrev <- "UT" # "State_Abbrev"
  
  # input PM2.5 concentration
  input_mat1$PM2.5_Obs <- PCAPSdata$ug.m3
  
  # input lat/lon information  "PM2.5_Lat", "PM2.5_Lon" 
  input_mat1 <- PCAPS_gather_lat_lon.fn(PCAPSdata, input_mat1, PCAPSLocations)
 
  # "Datum" 
  input_mat1$Datum <- this_datum
  
  # input flag for negative concentrations
  input_mat1$N_Negative_Obs <- 0 # initially, set all rows to 0 for N_Negative_Obs
  which_neg <- which(input_mat1$PM2.5_Obs < 0 & is.na(input_mat1$PM2.5_Obs) == FALSE)
  length(which_neg)
  input_mat1[which_neg, c("N_Negative_Obs")] <- 1

  # input dates
  input_mat1$Date_Local <- format(PCAPSdata$R_Dates, "%Y-%m-%d") # "Date_Local"
  
  # "Year"                  
  input_mat1$Year <- input_mat_extract_year_from_date.fn(input_mat1$Date_Local)
  
  #"Month"  
  input_mat1$Month <- input_mat_extract_month_from_date.fn(input_mat1$Date_Local)
  
  # "Day"
  input_mat1$Day <- input_mat_extract_day_from_date.fn(input_mat1$Date_Local)
  
  # input "Source_File"
  input_mat1$Source_File <- this_source_file # "Source_File"
  
  # input data source counter - indicates if this is EPA data or field data, etc.
  input_mat1$Data_Source_Counter <- data_source_counter # "Data_Source_Counter"
  input_mat1$Data_Source_Name_Short <- Data_Source_Name_Short # "Data_Source_Name_Short" 
  input_mat1$Data_Source_Name_Display <- Data_Source_Name_Display # "Data_Source_Name_Display"
  
  # input color for plotting this data source (totally arbitrary choice of color)
  input_mat1$PlottingColor <- this_plotting_color #"green"
  
  # "Units_of_Measure"
  input_mat1$Units_of_Measure <- "ug/m3"
  
  # "Observation_Count"
  input_mat1$Observation_Count <- 1
  
  # "Observation_Percent"
  input_mat1$Observation_Percent <- 100
  
  "Sample_Duration"
  input_mat1$Sample_Duration <-  "24 HOUR"
  
  # "Method_Name"
  input_mat1$Method_Name <- "MiniVol"
  
  # "Composite_of_N_rows"      
  input_mat1$Composite_of_N_rows <- 1
  
  # "InDayLatDiff"      
  input_mat1$InDayLatDiff <- 0
  
  # "InDayLonDiff"   
  input_mat1$InDayLonDiff <- 0
  
  # Think about whether to include any other columns of data from the PCAPS data in input_mat1
  #> colnames(PCAPSdata)
  #             "SN.."             
  # "actual.L..per.minute." "Hours..on."            "Hours..off."           "Hours..collected."    
  # "actual.m3"             "gross.wt...mg."        "tare.wt...mg."         "wt...mg"              
  # "weather"               "notes"                 "X"   
  
  # think about whether to try to fill anything in for these columns:
  #> colnames(input_mat1)
  #                          "Site_Num"                 "Parameter_Code"          
  #[5] "POC"
  #[9] "Parameter_Name" "Pollutant_Standard"                     
  #"Event_Type"     
  #"1st_Max_Value"            "1st_Max_Hour"             "AQI"                     
  #[21] "Method_Code" "Address"                 
  #            "County_Name"              "City_Name"                "CBSA_Name"               
  #29] "Date_of_Last_Change"          "Winter"                   "Year"                    
  #[33] "Month"                    "Day"   
  #[41] "flg.Lat"                  "flg.Lon"                  "Type"                     "flg.Type"                
  #[45] "flg.Site_Num"             "flg.PM25_Obs"             "l/m Ave. Air Flw"         "flg.AirFlw"              
  #[49] "Deg C Av Air Temp"        "flg.AirTemp"              "% Rel Humidty"            "flg.RelHumid"            
  #[53] "mbar Barom Press "        ",flg.,Barom,Press"        "deg C Sensor  Int AT"     "flg.deg C Sensor Int AT" 
  #[57] "% Sensor Int RH"          "flg.%SensorIntRH"         "Wind Speed m/s"           "flg.WindSpeed"           
  #[61] "Battery Voltage volts"    "flg.BatteryVoltage"       "Alarm"                    "flg.Alarm"               
  
  # variables to be filled in at the end of the script     
  "County_Code"            
  
  # think about whether any of these columns can be filled in for PCAPS data
  # "County_Code" "Site_Num" "Parameter_Code" "POC"                     
  # "Datum" "Parameter_Name"                
  # "Pollutant_Standard"  "Event_Type" "1st_Max_Value" "1st_Max_Hour" "AQI"
  #"Method_Code" "Address" "County_Name" "City_Name"                "CBSA_Name"                "Date_of_Last_Change"
  # "flg.Lat"                  "flg.Lon"                  "Type"                     "flg.Type"                 "flg.Site_Num"
  # "flg.PM25_Obs"             "l/m Ave. Air Flw"         "flg.AirFlw"               "Deg C Av Air Temp"        "flg.AirTemp" 
  # "% Rel Humidty"            "flg.RelHumid"             "mbar Barom Press "        ",flg.,Barom,Press"        "deg C Sensor  Int AT"
  # "flg.deg C Sensor Int AT"  "% Sensor Int RH"          "flg.%SensorIntRH"         "Wind Speed m/s"           "flg.WindSpeed"
  # "Battery Voltage volts"    "flg.BatteryVoltage"       "Alarm"                    "flg.Alarm"
  
  # these columns can be filled in near the end of this script
  # "Winter"                   "Year"                     "Month"                    "Day"       
  
  print(paste("This data has",dim(input_mat1)[1],"rows of PM2.5 observations.")) # how many rows of data?
  
  # output to file #  
  #write.csv(input_mat1,file = file.path(ProcessedData.directory,paste(Data_Source_Name_Short,"_",Sys.Date(),'_Step1_part_',processed_data_version,'.csv',sep = "")),row.names = FALSE)
  write.csv(input_mat1,file = file.path(ProcessedData.directory,paste(file_sub_label,'.csv',sep = "")),row.names = FALSE)
  
  print(paste("finished processing ", Data_Source_Name_Display))
  
  # clear variables
  rm(new_col_number,this_source_file) 
  rm(PCAPSdata,PCAPSLocations)#,PCAPSstationsChar,PCAPSstations)
  rm(Data_Source_Name_Display,Data_Source_Name_Short)
  
  # output input_mat1 from function #  
  return(input_mat1) # output from function  
  
} # end of process_PM25_PCAPS_data_source.fn function

PCAPS_gather_lat_lon.fn <- function(PCAPSdata, input_mat1, PCAPSLocations) {
  
  all_locations <- unique(PCAPSdata$Location)
  #print(all_locations)
  for (this_location_i in 1:length(all_locations)) { # cycle through locations and input lat/lon info
    this_name <- all_locations[this_location_i] # identify name of location
    print(as.character(this_name))
    
    which_this_loc <- which(PCAPSdata$Location == this_name) # which rows are for this location?
    #print(length(which_this_loc))
    
    if(this_name=="Hawthorne"){
      this_lat <- PCAPSLocations[2,c('Latitude')]
      this_lon <- PCAPSLocations[2,c('Longitude')] 
    } else if(this_name=="2nd Ave"){
      this_lat <- PCAPSLocations[4,c('Latitude')]
      this_lon <- PCAPSLocations[4,c('Longitude')]
    } else if(this_name=="9th Ave"){
      this_lat <- PCAPSLocations[5,c('Latitude')]
      this_lon <- PCAPSLocations[5,c('Longitude')]   
    } else if(this_name=="Hilltop"){
      this_lat <- PCAPSLocations[7,c('Latitude')]
      this_lon <- PCAPSLocations[7,c('Longitude')]   
    } else if(this_name=="5400 ft"){
      this_lat <- PCAPSLocations[8,c('Latitude')]
      this_lon <- PCAPSLocations[8,c('Longitude')]   
    } else if(this_name=="5680 ft"){
      this_lat <- PCAPSLocations[9,c('Latitude')]
      this_lon <- PCAPSLocations[9,c('Longitude')]   
    } else if(this_name=="5820 ft"){
      this_lat <- PCAPSLocations[10,c('Latitude')]
      this_lon <- PCAPSLocations[10,c('Longitude')]   
    } else {
      stop(1, call. = TRUE, domain = NULL)
      geterrmessage("Loop should not have called this path in the if-statement")
    }
    
    input_mat1[which_this_loc,c("PM2.5_Lat")] <- this_lat # input latitude
    input_mat1[which_this_loc,c("PM2.5_Lon")] <- this_lon # input longitude
    
    rm(this_name, which_this_loc, this_lat, this_lon) # clear variables
    
  } # for (this_location_i in 1:length(all_locations)) { # cycle through locations and input lat/lon info
  
  # clear variables
  rm(all_locations, this_location_i)
  
  # output input_mat1 from function #  
  return(input_mat1) # output from function    
  
} # end of PCAPS_gather_lat_lon.fn function
