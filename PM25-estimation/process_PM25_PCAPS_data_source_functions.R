process_PM25_PCAPS_data_source.fn <- function(input_header, ProcessedData.directory, PCAPSData.directory, data_set_counter, this_plotting_color) {
  # combine PCAPS PM2.5 data files into 1 dataframe
  
  ##### Create Sink output file and create its header ####
  # sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
  SinkFileName=file.path(ProcessedData.directory,"PM25_data_source_PCAPS_combining_sink.txt")
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
  
  #### Create data frame  ####
  input_mat1 <- data.frame(matrix(NA,nrow=dim(PCAPSdata)[1],ncol=length(input_header))) # create data frame for input_mat1
  names(input_mat1) <- input_header # assign the header to input_mat1
  input_mat1 <- input_mat_change_data_classes.fn(input_mat1)

  #### Fill in Salt Lake City PCAPS data ############################
  data_source_counter=data_set_counter
  Data_Source_Name_Short <- "PCAPS"
  Data_Source_Name_Display <- "PCAPS (Salt Lake Valley)"
  
  # handle date information
  new_col_number <- length(PCAPSdata)+1
  PCAPSdata[,new_col_number] <- as.Date(PCAPSdata[,c("Dates")],"%m/%d/%Y") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
  colnames(PCAPSdata)[new_col_number] <- "R_Dates"
  
  # input station names
  input_mat1$PM25_Station_Name <- as.character(PCAPSdata$Location)
  
  # input state information
  input_mat1$State_Code <- 49 # "State_Code" 
  input_mat1[row_start:row_stop,c("State_Name")] <- "Utah" # "State_Name"
  input_mat1[row_start:row_stop,c("State_Abbrev")] <- "UT" # "State_Abbrev"
  
  # input PM2.5 concentration
  this_column <- which(colnames(PCAPSdata)=="ug.m3")
  #print(paste("Column number = ",this_column))
  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- PCAPSdata[,this_column] # "PM2.5_Obs"
  
  # load file containing lat/lon info for PCAPS sites
  PCAPSLocations<-read.csv(file.path(PCAPSData.directory,"PCAPS_Site_Locations.csv"),header=TRUE) 
  # input lat/lon information  "PM2.5_Lat", "PM2.5_Lon" 
  for(this_row in row_start:row_stop){     
    
    this_name <- input_mat1[this_row,c('PM25_Station_Name')]
    #print(this_name)
    
    if (input_mat1[this_row,c("PM2.5_Obs")]<0 & is.na(input_mat1[this_row,c("PM2.5_Obs")])==FALSE) {
      input_mat1[this_row,c("N_Negative_Obs")] <- 1
      #print("Negative obs")
    } else if (input_mat1[this_row,c("PM2.5_Obs")]>=0 & is.na(input_mat1[this_row,c("PM2.5_Obs")])==FALSE){
      input_mat1[this_row,c("N_Negative_Obs")] <- 0
      #print("Positive obs")
    } #else {
    #print("unknown conc")
    #}
    
    if(this_name=="Hawthorne"){
      input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[2,c('Latitude')]
      input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[2,c('Longitude')] 
    } 
    else if(this_name=="2nd Ave"){
      input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[4,c('Latitude')]
      input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[4,c('Longitude')]
    }
    else if(this_name=="9th Ave"){
      input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[5,c('Latitude')]
      input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[5,c('Longitude')]   
    }
    else if(this_name=="Hilltop"){
      input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[7,c('Latitude')]
      input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[7,c('Longitude')]   
    } 
    else if(this_name=="5400 ft"){
      input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[8,c('Latitude')]
      input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[8,c('Longitude')]   
    } 
    else if(this_name=="5680 ft"){
      input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[9,c('Latitude')]
      input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[9,c('Longitude')]   
    } 
    else if(this_name=="5820 ft"){
      input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[10,c('Latitude')]
      input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[10,c('Longitude')]   
    } 
    else {
      stop(1, call. = TRUE, domain = NULL)
      geterrmessage("Loop should not have called this path in the if-statement")
    }
    
  }
  rm(this_row)
  
  # input dates
  input_mat1[row_start:row_stop,c("Date_Local")] <- format(PCAPSdata[,c("R_Dates")], "%Y-%m-%d") # "Date_Local"
  
  # input "Source_File"
  input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file # "Source_File"
  
  
  # input data source counter - indicates if this is EPA data or field data, etc.
  input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter # "Data_Source_Counter"
  input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short # "Data_Source_Name_Short" 
  input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display # "Data_Source_Name_Display"
  
  # input color for plotting this data source (totally arbitrary choice of color)
  input_mat1[row_start:row_stop,c("PlottingColor")] <- "green"
  #c("blue") "PlottingColor"
  
  # "Units_of_Measure"
  input_mat1[row_start:row_stop,c("Units_of_Measure")] <- "ug/m3"
  
  # "Observation_Count"
  input_mat1[row_start:row_stop,c("Observation_Count")] <- 1
  
  # "Observation_Percent"
  input_mat1[row_start:row_stop,c("Observation_Percent")] <- 100
  
  "Sample_Duration"
  input_mat1[row_start:row_stop,c("Sample_Duration")] <-  "24 HOUR"
  
  # "Method_Name"
  input_mat1[row_start:row_stop,c("Method_Name")] <- "MiniVol"
  
  # "Composite_of_N_rows"      
  input_mat1[row_start:row_stop,c("Composite_of_N_rows")] <- 1
  
  # "InDayLatDiff"      
  input_mat1[row_start:row_stop,c("InDayLatDiff")] <- 0
  
  # "InDayLonDiff"   
  input_mat1[row_start:row_stop,c("InDayLonDiff")] <- 0
  
  # Think about whether to include any other columns of data from the PCAPS data in input_mat1
  #> colnames(PCAPSdata)
  #             "SN.."             
  # "actual.L..per.minute." "Hours..on."            "Hours..off."           "Hours..collected."    
  # "actual.m3"             "gross.wt...mg."        "tare.wt...mg."         "wt...mg"              
  # "weather"               "notes"                 "X"   
  
  # think about whether to try to fill anything in for these columns:
  #> colnames(input_mat1)
  #                          "Site_Num"                 "Parameter_Code"          
  #[5] "POC" "Datum"                   
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
  
  rm(new_col_number,this_column,this_name,this_source_file) 
  row_start <- row_stop+1
  #row_stop=row_start+dim(PCAPSdata)[1]-1
  rm(PCAPSdata,PCAPSLocations)#,PCAPSstationsChar,PCAPSstations)
  rm(Data_Source_Name_Display,Data_Source_Name_Short)
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
  
  
  # clear variables
  rm(this_column, this_name)
  
  # output input_mat1 from function #  
  return(input_mat1) # output from function  
  
} # end of process_PM25_PCAPS_data_source.fn function