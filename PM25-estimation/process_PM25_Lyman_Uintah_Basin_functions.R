process_PM25_Lyman_Uintah_data_source.fn <- function(input_header, ProcessedData.directory, UintahData.directory, data_set_counter, this_plotting_color = "darkgoldenrod") {
  # combineFire Cache. PM2.5 data files into 1 dataframe
  
  #### Fill in Lyman Uintah Basin data ########################
  #data_source_counter <- data_set_counter 
  Data_Source_Name_Short <- "UintahBasin"
  Data_Source_Name_Display <- "Uintah Basin" # Data_Source_Name_Display
  this_Datum <- "WGS84" # per email from Seth Lyman on October 11, 2018
  
  ##### Create Sink output file and create its header ####
  # sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
  #SinkFileName=file.path(ProcessedData.directory,"PM25_data_source_Lyman_Uintah_Basin_combining_sink.txt")
  file_sub_label <- paste("PM25_",Data_Source_Name_Short,"_Step1_",Sys.Date(),"_part_",processed_data_version,sep = "")
  SinkFileName=file.path(ProcessedData.directory,paste(file_sub_label,"_combining_sink.txt",sep = ""))
  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
  cat("R output for process_PM25_Lyman_Uintah_Basin_data_source_functions.R \n \n")
  cat("Title: process_PM25_Lyman_Uintah_Basin_data_source_function.R \n")
  cat("Author: Melissa May Maestas, PhD \n")
  cat("Original Date: October 10, 2018 \n")
  cat("Latest Update: October 10, 2018 \n")
  cat(paste("Script ran and this text file created ",Sys.time()," \n",sep = ""))
  cat("This program reads in and PM2.5 data from the Uintah Basin (provided by Seth Lyman) \n")
  print(file_sub_label)
  #print(UintahData.directory)
  print(Data_Source_Name_Display)
  
  #### Create data frame  ####
  input_mat1 <- data.frame(matrix(NA,nrow=0,ncol=length(input_header))) # create data frame for input_mat1
  names(input_mat1) <- input_header # assign the header to input_mat1
  input_mat1 <- input_mat_change_data_classes.fn(input_mat1)
  


this_source_file <- "FinalPM2.5_multiyear_thruwint2017_sheet1_dates.csv" # "Source_File"
print(paste("Source file:",this_source_file))

# load data
#print(UintahData.directory)
UB_data<-read.csv(file.path(UintahData.directory,this_source_file),header=TRUE) 
#print("dim(UB_data): ")
#print(dim(UB_data))
#print(UintahData.directory)

# load file with lat/lon for Uintah Basin stations
UBLocations <- read.csv(file.path(UintahData.directory,"FinalPM2.5_multiyear_thruwint2017_GISsheet.csv"),header=TRUE)

# handle date information
new_col_number <- length(UB_data)+1 # figure out how many columns are in UB_data and then add 1
UB_data[,new_col_number] <- as.Date(UB_data[,c("Dates")],"%m/%d/%Y") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
colnames(UB_data)[new_col_number] <- "R_Dates"
rm(new_col_number)

# cycle UB columns (stations) of data
input_mat1 <- fill_in_UB_stations_input_mat.fn(UB_data, UBLocations, input_mat1, data_source_counter = data_set_counter,
                                               Data_Source_Name_Short, Data_Source_Name_Display, this_source_file,
                                               this_plotting_color, this_Datum)
# "Year"                  
input_mat1$Year <- input_mat_extract_year_from_date.fn(input_mat1$Date_Local)

#"Month"  
input_mat1$Month <- input_mat_extract_month_from_date.fn(input_mat1$Date_Local)

# "Day"
input_mat1$Day <- input_mat_extract_day_from_date.fn(input_mat1$Date_Local)

# think about whether to try to fill anything in for these columns:
#"County_Name"              "City_Name"                "CBSA_Name"                "Date_of_Last_Change"                  
# "flg.Lat" "flg.Lon"  "Type" "flg.Type"  "flg.Site_Num"
#  "flg.PM25_Obs" "l/m Ave. Air Flw" "flg.AirFlw" "Deg C Av Air Temp" "flg.AirTemp"
# "% Rel Humidty" "flg.RelHumid" "mbar Barom Press "  ",flg.,Barom,Press" "deg C Sensor  Int AT"
# "flg.deg C Sensor Int AT"  "% Sensor Int RH" "flg.%SensorIntRH" "Wind Speed m/s" "flg.WindSpeed" 
# "Battery Voltage volts" "flg.BatteryVoltage" "Alarm" "flg.Alarm"

print(paste("This data has",dim(input_mat1)[1],"rows of PM2.5 observations.")) # how many rows of data?

# output to file #  
write.csv(input_mat1,file = file.path(ProcessedData.directory,paste(file_sub_label,'.csv',sep = "")),row.names = FALSE)

print(paste("finished processing ", Data_Source_Name_Display))

# clear variables 
rm(this_source_file)
rm(UB_data,UBLocations)
rm(Data_Source_Name_Display,Data_Source_Name_Short)#,this_Datum)

# output input_mat1 from function #  
return(input_mat1) # output from function

} # end of process_PM25_Lyman_Uintah_data_source.fn function

# fill in columns of UB data
fill_in_UB_stations_input_mat.fn <- function(UB_data, UBLocations, input_mat1, data_source_counter,
                                             Data_Source_Name_Short, Data_Source_Name_Display, this_source_file,
                                             this_plotting_color, this_Datum) {
#  print("size of input_mat1")
#  print(dim(input_mat1))
  # define row counters
  row_start <- 1
  row_stop=row_start+dim(UB_data)[1]-1
  
for(this_column in 6:15){ # cycle through various stations 
#  print("size of input_mat1")
#  print(dim(input_mat1))
#  print(paste("Column number = ",this_column))
  this_name=colnames(UB_data)[this_column]
#  print(this_name)
  
  # input data source counter - indicates if this is EPA data or field data, etc.
  input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter #  "Data_Source_Counter"
  input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short # "Data_Source_Name_Short"
  input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
  
  # input color for plotting this data source (totally arbitrary choice of color)
  input_mat1[row_start:row_stop,c("PlottingColor")] <- this_plotting_color # "darkgoldenrod"
  #c(,"green","blue") "PlottingColor"
  
  # "State_Code"               
  # input state information
  #input_mat1[row_start:row_stop,c("State_Code")] <- 49 
  #input_mat1[row_start:row_stop,c("State_Name")] <- "Utah" # "State_Name"
  #input_mat1[row_start:row_stop,c("State_Abbrev")] <- "UT" # "State_Abbrev"
  #print('check if any Uintah basin sites are in CO')
  
  # "County_Code"   
  # fill in County Code where missing
  
  #"Site_Num" 
  # think about whether Site_Num could be filled in for this data
  
  #"Parameter_Code"
  # think about whether Parameter_Code could be filled in for this data
  
  #"POC"                     
  # think about whether POC could be filled in for this data
  
  # input station names into input_mat1 "PM25_Station_Name"
  input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- this_name
  
  # input PM2.5 concentration "PM2.5_Obs"
  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- UB_data[,this_column]
  
  # input source file name
  input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file

  # input dates "Date_Local"
  input_mat1[row_start:row_stop,c("Date_Local")] <- format(UB_data[,c("R_Dates")], "%Y-%m-%d")
  
  # fill in "PM2.5_Lat" and "PM2.5_Lon"
  # input lat and lon
  if(this_name=="Roosevelt..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[1,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[1,c('long')]
    # "Method_Name"
    input_mat1[row_start:row_stop,c("Method_Name")] <- "EPA AQS database"
  } else if(this_name=="Vernal..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[2,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[2,c('long')]
    # "Method_Name"
    input_mat1[row_start:row_stop,c("Method_Name")] <- "EPA AQS database"
  } else if(this_name=="Ouray..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[3,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[3,c('long')]
    # "Method_Name"
    input_mat1[row_start:row_stop,c("Method_Name")] <- "EPA AQS database"
  } else if(this_name=="Red.Wash..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[4,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[4,c('long')]
    # "Method_Name"
    input_mat1[row_start:row_stop,c("Method_Name")] <- "EPA AQS database"
  } else if(this_name=="Myton..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[5,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[5,c('long')]
    # "Method_Name"
    input_mat1[row_start:row_stop,c("Method_Name")] <- "EPA AQS database"
  } else if(this_name=="RabbitMtn..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[6,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[6,c('long')]
    # "Method_Name"
    input_mat1[row_start:row_stop,c("Method_Name")] <- "EPA AQS database/BAM 1020"
  } else if(this_name=="Horsepool..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[7,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[7,c('long')]
    # "Method_Name"
    input_mat1[row_start:row_stop,c("Method_Name")] <- "BAM 1020"
  } else if(this_name=="Ft..Duchesne"){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[8,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[8,c('long')]
    # "Method_Name"
    input_mat1[row_start:row_stop,c("Method_Name")] <- "24-hr filter/gravimetric"
  } else if(this_name=="Randlett"){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[9,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[9,c('long')]
    # "Method_Name"
    input_mat1[row_start:row_stop,c("Method_Name")] <- "24-hr fileter/gravimetric"
  } else if(this_name=="Rangely"){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[10,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[10,c('long')]
    # "Method_Name"
    input_mat1[row_start:row_stop,c("Method_Name")] <- "EPA AQS database"
  } else {
    stop(1, call. = TRUE, domain = NULL)
    geterrmessage("Loop should not have called this path in the if-statement")
  } # if(this_name=="Roosevelt..24hr.avg.PM2.5."){
  
  # input other information
  input_mat1[row_start:row_stop,c('Winter')] <- UB_data[,"Winter."] # "Winter"
  #input_mat1[row_start:row_stop,c('Year')] <- UB_data[,"year"] # "Year" # skipping this since year information is extracted from date information
  
  # fill in "Datum"        
  input_mat1[row_start:row_stop,c("Datum")] <- this_Datum

  
  # figure out how to fill in "Parameter_Name"           
  
  #"Sample_Duration"  
  input_mat1[row_start:row_stop,c('Sample_Duration')] <- "24 HOUR"
  
  # "Pollutant_Standard" 
  input_mat1[row_start:row_stop,c('Pollutant_Standard')] <- NA
  
  # "Units_of_Measure" 
  input_mat1[row_start:row_stop,c('Units_of_Measure')] <- "Micrograms/cubic meter"
  
  # Event_Type
  input_mat1[row_start:row_stop,c('Event_Type')] <- NA
  
  # "Observation_Count" 
  input_mat1[row_start:row_stop,c('Observation_Count')] <- 1
  
  # "Observation_Percent"
  input_mat1[row_start:row_stop,c('Observation_Percent')] <- 100
  
  # "1st_Max_Value"
  input_mat1[row_start:row_stop,c("1st_Max_Value")] <- NA
  
  # "1st_Max_Hour" 
  input_mat1[row_start:row_stop,c("1st_Max_Hour")] <- NA
  
  # "AQI" 
  input_mat1[row_start:row_stop,c("AQI")] <- NA
  
  # "Method_Code"
  input_mat1[row_start:row_stop,c("Method_Code")] <- NA
  
  # "Address"
  input_mat1[row_start:row_stop,c("Address")] <- NA
  
  # "Composite_of_N_rows"
  input_mat1[row_start:row_stop,c("Composite_of_N_rows")] <- 1
  
  # "N_Negative_Obs" - fill in after for loop
  
  # "InDayLatDiff"
  input_mat1[row_start:row_stop,c("InDayLatDiff")] <- 0
  
  # "InDayLonDiff"
  input_mat1[row_start:row_stop,c("InDayLonDiff")] <- 0
  
  row_start <- row_stop+1
  row_stop <- row_start+dim(UB_data)[1]-1
} # for(this_column in 6:15){ # cycle through various stations 
  
  # "N_Negative_Obs"
  input_mat1$N_Negative_Obs <- 0 # initially set them all to zero
  which_neg <- which(input_mat1$PM2.5_Obs<0) # find the negative observations
  input_mat1[which_neg, c("N_Negative_Obs")] <- 1 # indicate that rows with negative values as such
  print(paste(length(which_neg),"negative concentrations")) # display number of negative concentrations
  rm(which_neg) # clear variables
  
  # clear variables
  rm(this_column, this_name)
  
  # output input_mat1 from function #  
  return(input_mat1) # output from function
  
} # end of fill_in_UB_stations_input_mat.fn function

