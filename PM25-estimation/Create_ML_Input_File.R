rm(list = ls())
#gc(VERBOSE = TRUE)
###### Create input file for Machine Learning estimation of PM2.5 for the western US, 2008-2014 ######
# Create_ML_Input_File.R >> compiles the various PM2.5 data sources into data frame called input_mat1 which mimics Colleen's AllforCaret.csv, but for the western US. 

# To clear all variables and start fresh:
# rm(list = ls())

options(warn=2) # throw an error when there's a warning and stop the code from running further

#### define directories and constants ####
#setwd("D:/S3_bucket_image/")
#uppermost.directory="/home/rstudio" # In Docker
uppermost.directory="D:/S3_bucket_image/" # without docker
working.directory=uppermost.directory 
setwd(working.directory)
#output.directory=file.path(working.directory,"Code_Outputs")
#output.directory=file.path(working.directory,"estimate-pm25","LaTeX_documentation","Code_Outputs")
ProcessedData.directory=file.path(working.directory,"Processed_Data")
StartData.directory=file.path(working.directory,"PM25_Uintah_Basin")
USMaps.directory=file.path(working.directory,"Shapefiles_for_mapping","cp_2016_us_state_500k")
PCAPSData.directory=file.path(working.directory,"PM25_PCAPS_Salt_Lake")
AQSData.directory=file.path(working.directory,"AQS_Daily_Summaries")
FMLE.directory=file.path(working.directory,"Federal_Land_Manager_Environmental_Database")
FireCache.directory=file.path(working.directory,"Fire_Cache_Smoke_DRI")
CARB.directory=file.path(working.directory,"PM25_CARB")

start_study_year <- 2008
stop_study_year <- 2014
voltage_threshold_upper <- 17
voltage_threshold_lower <- 11

##### Create Sink output file ####
# sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
SinkFileName=file.path(ProcessedData.directory,"Create_ML_Input_File_sink.txt")
sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
cat("Code and R output for Create_ML_Input_File.R \n \n")
###
cat("Title: Create_ML_Input_File.R \n")
cat("Author: Melissa May Maestas \n")
cat("Original Date: January 23, 2018 \n")
cat("Latest Update: April 30, 2018 \n")
cat("This program reads in and PM2.5 data from several sources. \n")
###

#### Call Packages (Library) ####
cat("Libraries")
#library(maps)
#library(mapproj)
library(ggplot2)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)
#library(tmap)

#### Start Input file for machine learning ####
input_header <-  c('PM2.5_Obs','PM2.5_Lat','PM2.5_Lon','Datum','Date_Local','Year','Month','Day','State_Code','County_Code','Site_Num','Parameter_Code','POC','Parameter_Name','Sample_Duration','Pollutant_Standard','Units_of_Measure','Event_Type','Observation_Count','Observation_Percent','1st_Max_Value','1st_Max_Hour','AQI','Method_Code','Method_Name','PM25_Station_Name','Address','State_Name','County_Name','City_Name','CBSA_Name','Date_of_Last_Change', # columns in AQS data
                'State_Abbrev','Winter','Data_Source_Name_Display','Data_Source_Name_Short','Data_Source_Counter','Source_File','Composite_of_N_rows','N_Negative_Obs', # other columns to include
                "flg.Lat","flg.Lon","Type","flg.Type","flg.Site_Num","flg.PM25_Obs","l/m Ave. Air Flw", # DRI variables
                "flg.AirFlw","Deg C Av Air Temp","flg.AirTemp","% Rel Humidty","flg.RelHumid","mbar Barom Press","flg.Barom Press", # DRI variables
                "deg C Sensor  Int AT","flg.deg C Sensor Int AT","% Sensor Int RH","flg.%SensorIntRH", # DRI variables
                "Wind Speed m/s","flg.WindSpeed","Battery Voltage volts","flg.BatteryVoltage","Alarm","flg.Alarm", # DRI variables
                "InDayLatDiff","InDayLonDiff","PlottingColor")
N_columns <- length(input_header) # how many columns are in header?
input_mat1 <- data.frame(matrix(NA,nrow=10,ncol=N_columns)) # create data frame for input_mat1
names(input_mat1) <- input_header # assign the header to input_mat1
rm(N_columns)
                
# skipping these DRI variables
# " Unk   Misc     #1   "                "           flg. Unk   Misc     #1   "
#" Deg   Wind    Direc "                "           flg. Deg   Wind    Direc "
# " Unk   Misc     #2   "                "           flg. Unk   Misc     #2   "

# note: change to column names (will need to be changed below as well)
# Parameter -> Parameter_Code
# Method -> Method_Code
# State_Number -> State_Code

# AQS columns renamed:
# Latitude -> PM2.5_Lat
# Longitude -> PM2.5_Lon
# Arithmetic_Mean -> PM2.5_Obs
# Local_Site_Name -> 'PM25_Station_Name'

# note: the Serial number from the DRI data is put into the Site_Num column

# not sure if I need to add this column: 'ID', 'RDates',


############################## Pull in AQS data #################
data_source_counter=0 # counter to distinguish between the various data sources
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
    # only study area states: #ThisAQSdata_StudyStates <- ThisAQSdata[which(ThisAQSdata$State.Code==4|ThisAQSdata$State.Code==6|ThisAQSdata$State.Code==8|ThisAQSdata$State.Code==16|ThisAQSdata$State.Code==30|ThisAQSdata$State.Code==32|ThisAQSdata$State.Code==35|ThisAQSdata$State.Code==41|ThisAQSdata$State.Code==49|ThisAQSdata$State.Code==53|ThisAQSdata$State.Code==56), ]
    ThisAQSdata_StudyStates <- ThisAQSdata[which(ThisAQSdata$State.Code==4|ThisAQSdata$State.Code==6|ThisAQSdata$State.Code==8|ThisAQSdata$State.Code==16|ThisAQSdata$State.Code==30|ThisAQSdata$State.Code==32|ThisAQSdata$State.Code==35|ThisAQSdata$State.Code==41|ThisAQSdata$State.Code==49|ThisAQSdata$State.Code==53|ThisAQSdata$State.Code==56|ThisAQSdata$State.Code==38|ThisAQSdata$State.Code==46|ThisAQSdata$State.Code==31|ThisAQSdata$State.Code==20|ThisAQSdata$State.Code==40|ThisAQSdata$State.Code==48), ]
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

# put in state abbreviations
repeated_name_numbers=input_mat1[,c("State_Name","State_Code")]
#duplicated(repeated_name_numbers)
#repeated_name_numbers[duplicated(repeated_name_numbers), ]
non_repeat_name_numbers <- repeated_name_numbers[!duplicated(repeated_name_numbers), ]
print(non_repeat_name_numbers)
rm(repeated_name_numbers,non_repeat_name_numbers)

# input 'State_Abbrev' 
print('think about whether to move input of State_Abbrev to the end of the script')
state_rows <- which(input_mat1$State_Code==4)
input_mat1[state_rows,c("State_Abbrev")] <- "AZ"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==6)
input_mat1[state_rows,c("State_Abbrev")] <- "CA"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==8)
input_mat1[state_rows,c("State_Abbrev")] <- "CO"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==16)
input_mat1[state_rows,c("State_Abbrev")] <- "ID"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==20)
input_mat1[state_rows,c("State_Abbrev")] <- "KS"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==30)
input_mat1[state_rows,c("State_Abbrev")] <- "MT"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==31)
input_mat1[state_rows,c("State_Abbrev")] <- "NE"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==32)
input_mat1[state_rows,c("State_Abbrev")] <- "NV"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==35)
input_mat1[state_rows,c("State_Abbrev")] <- "NM"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==38)
input_mat1[state_rows,c("State_Abbrev")] <- "ND"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==40)
input_mat1[state_rows,c("State_Abbrev")] <- "OK"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==41)
input_mat1[state_rows,c("State_Abbrev")] <- "OR"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==46)
input_mat1[state_rows,c("State_Abbrev")] <- "SD"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==48)
input_mat1[state_rows,c("State_Abbrev")] <- "TX"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==49)
input_mat1[state_rows,c("State_Abbrev")] <- "UT"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==53)
input_mat1[state_rows,c("State_Abbrev")] <- "WA"
rm(state_rows)
state_rows <- which(input_mat1$State_Code==56)
input_mat1[state_rows,c("State_Abbrev")] <- "WY"
rm(state_rows)

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

############################# Pull in Fire Cache Smoke (DRI) data #################
print('still need to download the files that have been password protected.')
# increase dummy counter by 1 (used for differentiating data sources by color in map)
data_source_counter <- data_source_counter+1 # counter to distinguish between the various data sources
Data_Source_Name_Short <- "FireCacheDRI"
Data_Source_Name_Display <- "Fire Cache Smoke Monitor (DRI)"

# these lines for running code skipping AQS data above
#data_source_counter <- 1
#row_start <- 1

# this line is for running code having run the AQS data above without re-running that code
# row_start <- 1864583

# what files are in the FireCache.directory?
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/list.files.html
all_DRI_Files <- list.files(path = file.path(FireCache.directory,"."), pattern = NULL, all.files = FALSE,
                            full.names = FALSE, recursive = FALSE,
                            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
#print(all_DRI_Files)

# cycle through files
for (this_file_counter in 1:length(all_DRI_Files)){  
  #print(paste('this_file_counter =',this_file_counter))
  this_source_file <- all_DRI_Files[this_file_counter]
  print(this_source_file)
  
  # load monitor name
  this_name <- as.character(read.csv(file.path(FireCache.directory,this_source_file),header = F,nrows = 1)[1,1])
  print(this_name)
  
  # the headers are spread across 3 rows - read in those rows
  three_header_rows <- read.csv(file.path(FireCache.directory,this_source_file),header=F,skip = 1,nrows = 3)
  # create a data frame for the consolidated header to go into
  one_row_header=data.frame()
  
  # The header in the original file is spread across three rows, the following for loop consolidates them
  for(this_col in 1:dim(three_header_rows)[2]){
    part1 <- as.character(three_header_rows[1,this_col]) # first row of header
    #print(part1)
    part2 <- as.character(three_header_rows[2,this_col]) # second row of header
    #print(part2)
    part3 <- as.character(three_header_rows[3,this_col]) # third row of header
    #print(part3)
    if (part3==' flg'){ # add the name of the variable to flag header
      part3 <- paste(' flg.',one_row_header[this_col-1],sep = "")
    }
    this_col_header <- paste(part1,part2,part3)
    rm(part1,part2,part3)
    one_row_header[1,this_col] <- this_col_header
    rm(this_col_header)
  }  
  rm(three_header_rows,this_col) # clear variables that are no longer needed
  
  # load main part of this data file
  this_Fire_Cache_data_step <- read.csv(file.path(FireCache.directory,this_source_file),header = F,skip = 4)
  
  # attach the header compiled in the for loop above to the data
  names(this_Fire_Cache_data_step) <- one_row_header
  rm(one_row_header)
  
  # need to check for how/whether headers are different among input files and make a comprehensive header
  if (this_file_counter==1){
    #print('first file')
    # create the variable comprehensive header on first file
    comprehensive.header <- c(colnames(this_Fire_Cache_data_step),"N_neg","N_Obs","InDayLatDiff","InDayLonDiff","1st_Max_Value","1st_Max_Hour")
  } else if (this_file_counter>1){ # not the first file
    print(paste('this_file_counter is ',this_file_counter))
    this_file_header <- colnames(this_Fire_Cache_data_step) # get the header for this file
    #print(this_file_header) # show the header
    for (this_col in 1:length(this_file_header)) { # cycle through columns in header
      #print(paste('this_col = ',this_col)) 
      this_col_header <- this_file_header[this_col] # get the header for this column
      #print(this_col_header)
      which_col <- which(comprehensive.header==this_col_header) # find this header in the comprehensive header
      #print(paste('this_col (',this_col,') matches column ',which_col,' in comprehensive.header')) 
      if (length(which_col)!=1){ # if there is no matching column in the comprehensive header, a new column will be added to comprehensive header
        print('adding new column header that was not in previous files:')
        print(this_col_header)
        new_col_number <- length(comprehensive.header)+1 # add new column
        comprehensive.header[new_col_number] <- this_col_header # set header for new column
        rm(new_col_number) # clear variables
      } # if (length(which_col)!=1)
      rm(this_col_header,which_col)
    } # for (this_col in 1:length(this_file_header)) {
    rm(this_file_header,this_col)
  } # else if (this_file_counter>1){
  
  # The header is (sometimes/always?) repeated further down in the data. These rows need to be found and removed.
  row_restart_header <- which(this_Fire_Cache_data_step[,1]==":          ") # this text is repeated when the header repeats part way through the data file
  if (length(row_restart_header)==0){this_Fire_Cache_data <- this_Fire_Cache_data_step # no change necessary to data if the header does not repeat (just change name of variable)
  } else { # the header does repeat in the file and needs to be removed
    for (header_repeat_counter in 1:length(row_restart_header)) { # cycle through the repititions
      if (header_repeat_counter==length(row_restart_header)) { # currently the code can handle 1 repetition
        
        part1 <- this_Fire_Cache_data_step[1:row_restart_header-1,] # get the data above the repeated header
        part2_rowstart <- row_restart_header+3 # figure out which row number is just below the repeated header
        part2_rowstop <- as.numeric(dim(this_Fire_Cache_data_step)[1]) # figure out which row is at the end of the file
        part2 <- this_Fire_Cache_data_step[part2_rowstart:part2_rowstop,] # get the data below the repeated header
        
        this_Fire_Cache_data <- rbind(part1,part2) # combine data data from above and below the repeated header
        rm(part1,part2,part2_rowstart,part2_rowstop) # clear variables
      } else {
        stop('expand code') # code will need to be expanded if there are multiple header repititions in the file
      } # else
    } # for
    rm(header_repeat_counter) # clear variables
  } #if/else (length(row_restart_header)...)
  rm(this_Fire_Cache_data_step,row_restart_header) # clear variables
  
  # handle date information
  new_col_number <- length(this_Fire_Cache_data)+1 # figure out how many columns are in data and then add 1
  this_Fire_Cache_data[,new_col_number] <- as.Date(this_Fire_Cache_data[,1],"%m/%d/%Y") # add column at end of data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
  colnames(this_Fire_Cache_data)[new_col_number] <- "R_Dates"
  rm(new_col_number)
  
  #### take 24-hr averages
  # on what days does this monitor have data? (Each file should represent one monitor)
  these_dates <- unique(this_Fire_Cache_data[,c("R_Dates")])
  #print(these_dates)
  # create data frame that will have one observation per day
  N_columns_Fire_Cache=length(comprehensive.header) # number of columns
  Daily_Fire_Cache=data.frame(matrix(NA,nrow=length(these_dates),ncol=N_columns_Fire_Cache)) # create empty data frame
  names(Daily_Fire_Cache)=comprehensive.header # give new data frame a header
  rm(N_columns_Fire_Cache)
  print('still need to deal with some files having hour 20:00 data shifted a couple of columns')
  for (date_counter in 1:length(these_dates)) {
    this_date <- these_dates[date_counter]
    #print(this_date)
    
    # isolate the data for this date
    find_this_data_rows <- which(this_Fire_Cache_data[,c("R_Dates")]==this_date)
    date_all_Fire_Cache_data <- this_Fire_Cache_data[find_this_data_rows,]
    rm(find_this_data_rows)
    
    # make a note of negative values
    #print('need to decide whether we should just be removing the negative values and keeping the others within a 24-hr period')
    date_this_conc_data <-as.numeric(as.character(date_all_Fire_Cache_data[,c("ug/m3 Conc     RT    ")]))
    which_negative <- which(date_this_conc_data<0)
    sum_negative <- length(which_negative)
    #print(paste("number of negative observations in ",this_source_file,"on",this_date,"=",sum_negative))
    #if (length(which_negative)>0){print(paste(length(which_negative),' data points are removed from ',this_name,' on ',this_date))}
    rm(which_negative)
    
    # make a note of max value and when it occurred, used for "1st_Max_Value" and "1st_Max_Hour"
    max_conc_this_day <- max(date_this_conc_data)
    #print(max_conc_this_day)
    which_max_conc <- which(date_this_conc_data==max_conc_this_day)
    #print(which_max_conc)
    when_max_conc <- date_all_Fire_Cache_data[which_max_conc,c(" GMT  Time    hh:mm ")]
    #print(when_max_conc)
    rm(date_this_conc_data,which_max_conc)

    # check if there are more than 24 observations on a given day ... not expected
    if (dim(date_all_Fire_Cache_data)[1]>24){#(length(find_this_data_rows)>24){
      print(this_date)
      print(this_source_file)
      stop('There appear to be more than 24 observations for this monitor')
    }
    N_obs_this_day <- dim(date_all_Fire_Cache_data)[1]
    #print(paste("Number of observations in ",this_source_file,"on",this_date,"=",N_obs_this_day))
    
    # combine all of the hourly observations for this day into one row of data in Daily_Fire_Cache
    #  ######## Fill in all needed columns:
    # fill in date information
    # Daily_Fire_Cache[date_counter,c(":           :   Date    :MM/DD/YYYY")] <- as.Date(unique(date_all_Fire_Cache_data[,c("R_Dates")]),"%Y-%m-%d")
    
    # input Date information from date_all_Fire_Cache_data to Daily_Fire_Cache
    this_col_input_mat <- ":           :   Date    :MM/DD/YYYY"
    this_col_AQS <- "R_Dates"
    AQSVar <- as.Date(unique(date_all_Fire_Cache_data[,c(this_col_AQS)]),"%Y-%m-%d")
    #print(AQSVar)
    AQSVarChar <- format(AQSVar,"%Y-%m-%d")
    #print(AQSVarChar)
    Daily_Fire_Cache[date_counter,c(this_col_input_mat)] <- AQSVarChar
    rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
    
    # not filling int " GMT  Time    hh:mm " since this section of code compiles hourly data into a 24-hr average
    
    # fill in Latitude and corresponding flag, and calculate the difference between lat obs on a given day
    Daily_Fire_Cache[date_counter,c(" Deg    GPS     Lat. ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lat. ")])))
    #Daily_Fire_Cache[date_counter,c("           flg. Deg    GPS     Lat. ")] <- unique(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Deg    GPS     Lat. ")])))
    # flag is sometimes non-numeric, so an average cannot be taken
    flag_col <- "           flg. Deg    GPS     Lat. "
    all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
    #print(all_flags)
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c(flag_col)] <- unique(date_all_Fire_Cache_data[,c(flag_col)])
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
        #print(combine_flags)
      } # for
      Daily_Fire_Cache[date_counter,c(flag_col)] <- combine_flags # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # else
    print("line 608")
    rm(flag_col,all_flags)
    # calculate how much variation there is within a day in lat observations
    # max_lat <- max(date_all_Fire_Cache_data[,c(" Deg    GPS     Lat. ")])
    max_lat <- max(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lat. ")])))
    #print(max_lat)
    #min_lat <- min(date_all_Fire_Cache_data[,(" Deg    GPS     Lat. ")])
    min_lat <- min(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lat. ")])))
    #print(min_lat)
    lat_diff <- max_lat-min_lat
    #print(lat_diff)
    Daily_Fire_Cache[date_counter,c("InDayLatDiff")] <- lat_diff
    rm(max_lat,min_lat,lat_diff)
    
    
    # fill in longitude and corresponding flag
    if (mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))>0){
      Daily_Fire_Cache[date_counter,c(" Deg    GPS     Lon. ")] <- (-1)*mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))
      print('longitude value was positive, so it was multiplied by -1 to make it negative')
    } else { # if (mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))>0){
      Daily_Fire_Cache[date_counter,c(" Deg    GPS     Lon. ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))
    } # if/else (mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))>0){
    # flag for Lon is sometimes non-numeric, so an average cannot be taken
    all_flags <- unique(date_all_Fire_Cache_data[,c("           flg. Deg    GPS     Lon. ")]) # what are all the flags on this day?
    #print(all_flags)
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c("           flg. Deg    GPS     Lon. ")] <- unique(date_all_Fire_Cache_data[,c("           flg. Deg    GPS     Lon. ")])
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
        #print(combine_flags)
      } # for
      Daily_Fire_Cache[date_counter,c("           flg. Deg    GPS     Lon. ")] <- combine_flags # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    # calculate how much variation there is within a day in lon observations
    max_lon <- max(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))
    #print(max_lon)
    min_lon <- min(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))
    #print(min_lon)
    lon_diff <- max_lon-min_lon
    #print(lon_diff)
    Daily_Fire_Cache[date_counter,c("InDayLonDiff")] <- lon_diff
    rm(max_lon,min_lon,lon_diff,all_flags)
    
    # fill in Type and corresponding flag (does not exist in all files)
    if("      Type           " %in% colnames(date_all_Fire_Cache_data)){
    Daily_Fire_Cache[date_counter,c("      Type           ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("      Type           ")])))
    #Daily_Fire_Cache[date_counter,c("           flg.      Type           ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.      Type           ")])))
    # flag is sometimes non-numeric, so an average cannot be taken
    flag_col <- "           flg.      Type           "
    all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
    #print(all_flags)
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c(flag_col)] <- unique(as.character(date_all_Fire_Cache_data[,c(flag_col)]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
        #print(combine_flags)
      } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      Daily_Fire_Cache[date_counter,c(flag_col)] <- combine_flags # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    rm(flag_col,all_flags)
    } #if(" 
    
    # input monitor serial # and corresponding flag (not all files have this)
    if("ser # Serial  Number " %in% colnames(date_all_Fire_Cache_data)) {
    Daily_Fire_Cache[date_counter,c("ser # Serial  Number ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("ser # Serial  Number ")])))
    # flag for serial # is sometimes non-numeric, so an average cannot be taken
    all_serial_flags <- unique(date_all_Fire_Cache_data[,c("           flg.ser # Serial  Number ")]) # what are all the flags on this day?
    #print(all_serial_flags)
    if (length(all_serial_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c("           flg.ser # Serial  Number ")] <- unique(as.character(date_all_Fire_Cache_data[,c("           flg.ser # Serial  Number ")]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_serial_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_serial_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_serial_flags[flag_counter],sep = " ")
        #print(combine_flags)
      }
      Daily_Fire_Cache[date_counter,c("           flg.ser # Serial  Number ")] <- combine_flags # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # if/else (length(all_serial_flags)==1){ # there is only 1 flag, so it can be put in directly
    rm(all_serial_flags)
    } # if("ser # Serial  Number " %in% colnames(date_all_Fire_Cache_data)) {
    
    # input concentration in corresponding flag
    Daily_Fire_Cache[date_counter,c("ug/m3 Conc     RT    ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("ug/m3 Conc     RT    ")])))
    #Daily_Fire_Cache[date_counter,c("           flg.ug/m3 Conc     RT    ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.ug/m3 Conc     RT    ")])))
    # flag is sometimes non-numeric, so an average cannot be taken
    flag_col <- "           flg.ug/m3 Conc     RT    "
    all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
    #print(all_flags)
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c(flag_col)] <- unique(as.character(date_all_Fire_Cache_data[,c(flag_col)]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
        #print(combine_flags)
      } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      Daily_Fire_Cache[date_counter,c(flag_col)] <- combine_flags # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    rm(flag_col,all_flags)
    
    # Misc # 1 column does not exist in all of these files, so only fill it in when it exists:
    if(" Unk   Misc     #1   " %in% colnames(date_all_Fire_Cache_data)) {
      Daily_Fire_Cache[date_counter,c(" Unk   Misc     #1   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Unk   Misc     #1   ")])))
      # flag for Unk Misc #1 is sometimes non-numeric, so an average cannot be taken
      all_flags <- unique(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #1   ")]) # what are all the flags on this day?
      #print(all_flags)
      if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
        # Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #1   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #1   ")])))
        Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #1   ")] <- unique(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #1   ")])
        
      } else {# there are multiple flags and they need to be stitched together
        combine_flags <- all_flags[1] # get the first flag
        #print(combine_flags)
        for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
          combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
          #print(combine_flags)
        } # for
        Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #1   ")] <- combine_flags # input the flags
        rm(flag_counter,combine_flags) # clear variables
      } # else
      #Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #1   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #1   ")])))
      rm(all_flags)
    } #else {print('Nope, column is not here')} # if(" Unk   Misc     #1   " %in% colnames(date_all_Fire_Cache_data)) {
    
    #input " l/m   Ave.   Air Flw" and "           flg. l/m   Ave.   Air Flw"
    Daily_Fire_Cache[date_counter,c(" l/m   Ave.   Air Flw")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" l/m   Ave.   Air Flw")])))
    #Daily_Fire_Cache[date_counter,c("           flg. l/m   Ave.   Air Flw")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. l/m   Ave.   Air Flw")])))
    # flag is sometimes non-numeric, so an average cannot be taken
    flag_col <- "           flg. l/m   Ave.   Air Flw"
    all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
    #print(all_flags)
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c(flag_col)] <- unique(as.character(date_all_Fire_Cache_data[,c(flag_col)]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
        #print(combine_flags)
      } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      Daily_Fire_Cache[date_counter,c(flag_col)] <- combine_flags # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    rm(flag_col,all_flags)
    
    #input "Deg C  Av Air   Temp "                "           flg.Deg C  Av Air   Temp "
    Daily_Fire_Cache[date_counter,c("Deg C  Av Air   Temp ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("Deg C  Av Air   Temp ")])))
    #Daily_Fire_Cache[date_counter,c("           flg.Deg C  Av Air   Temp ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.Deg C  Av Air   Temp ")])))
    # flag is sometimes non-numeric, so an average cannot be taken
    flag_col <- "           flg.Deg C  Av Air   Temp "
    all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
    #print(all_flags)
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c(flag_col)] <- unique(as.character(date_all_Fire_Cache_data[,c(flag_col)]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
        #print(combine_flags)
      } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      Daily_Fire_Cache[date_counter,c(flag_col)] <- combine_flags # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    rm(flag_col,all_flags)
    
    # input % Rel Humidity and corresponding flag
    #Daily_Fire_Cache[date_counter,c("  %     Rel   Humidty")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("  %     Rel   Humidty")])))
    #Daily_Fire_Cache[date_counter,c("           flg.  %     Rel   Humidty")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.  %     Rel   Humidty")])))
    # % Rel Humidity column may not exist in all of these files, so only fill it in when it exists:
    if("  %     Rel   Humidty" %in% colnames(date_all_Fire_Cache_data)) {
      Daily_Fire_Cache[date_counter,c("  %     Rel   Humidty")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("  %     Rel   Humidty")])))
      # flag for Wind Direc is sometimes non-numeric, so an average cannot be taken
      all_flags <- unique(date_all_Fire_Cache_data[,c("           flg.  %     Rel   Humidty")]) # what are all the flags on this day?
      #print(all_flags)
      if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
        Daily_Fire_Cache[date_counter,c("           flg.  %     Rel   Humidty")] <- unique(date_all_Fire_Cache_data[,c("           flg.  %     Rel   Humidty")])
      } else {# there are multiple flags and they need to be stitched together
        combine_flags <- all_flags[1] # get the first flag
        #print(combine_flags)
        for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
          combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
          #print(combine_flags)
        } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        Daily_Fire_Cache[date_counter,c("           flg.  %     Rel   Humidty")] <- combine_flags # input the flags
        rm(flag_counter,combine_flags) # clear variables
      } # else
      rm(all_flags)
    } #else {print('Nope, column is not here')} # if("  %     Rel   Humidty" %in% colnames(date_all_Fire_Cache_data)) {    
    
    # Misc # 2 column does not exist in all of these files, so only fill it in when it exists:
    if (" Unk   Misc     #2   " %in% colnames(date_all_Fire_Cache_data)) {
      Daily_Fire_Cache[date_counter,c(" Unk   Misc     #2   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Unk   Misc     #2   ")])))
      # flag for Unk Misc #2 is sometimes non-numeric, so an average cannot be taken
      all_flags <- unique(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #2   ")]) # what are all the flags on this day?
      #print(all_flags)
      if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
        #Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #2   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #2   ")])))
        Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #2   ")] <- unique(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #2   ")])
      } else {# there are multiple flags and they need to be stitched together
        combine_flags <- all_flags[1] # get the first flag
        #print(combine_flags)
        for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
          combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
          #print(combine_flags)
        } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #2   ")] <- combine_flags # input the flags
        rm(flag_counter,combine_flags) # clear variables
      } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      rm(all_flags)
    } # if (" Unk   Misc     #2   " %in% colnames(date_all_Fire_Cache_data)) {
    
    # "mbar   Barom   Press " and "           flg.mbar   Barom   Press " columns do not exist in all of these files, so only fill it in when it exists:
    col_interest <- "mbar   Barom   Press "
    if(col_interest %in% colnames(date_all_Fire_Cache_data)){
      Daily_Fire_Cache[date_counter,c(col_interest)] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(col_interest)])))
      # flag is sometimes non-numeric, so an average cannot be taken
      flag_col <- "           flg.mbar   Barom   Press "
      all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
      #print(all_flags)
      if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
        Daily_Fire_Cache[date_counter,c(flag_col)] <- unique(date_all_Fire_Cache_data[,c(flag_col)])
      } else {# there are multiple flags and they need to be stitched together
        combine_flags <- all_flags[1] # get the first flag
        #print(combine_flags)
        for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
          combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
          #print(combine_flags)
        } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        Daily_Fire_Cache[date_counter,c(flag_col)] <- combine_flags # input the flags
        rm(flag_counter,combine_flags) # clear variables
      } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      rm(flag_col,all_flags)
    } # if (col_interest %in% colnames(date_all_Fire_Cache_data)){
    rm(col_interest)
    
    # input "deg C Sensor  Int AT " and "           flg.deg C Sensor  Int AT " (not all files have this collumn)
    if("deg C Sensor  Int AT " %in% colnames(date_all_Fire_Cache_data)) {
    Daily_Fire_Cache[date_counter,c("deg C Sensor  Int AT ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("deg C Sensor  Int AT ")])))
    #Daily_Fire_Cache[date_counter,c("           flg.deg C Sensor  Int AT ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.deg C Sensor  Int AT ")])))
    # flag for deg C Sensor  Int AT is sometimes non-numeric, so an average cannot be taken
    all_flags <- unique(date_all_Fire_Cache_data[,c("           flg.deg C Sensor  Int AT ")]) # what are all the flags on this day?
    #print(all_flags)
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c("           flg.deg C Sensor  Int AT ")] <- unique(date_all_Fire_Cache_data[,c("           flg.deg C Sensor  Int AT ")])
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
        #print(combine_flags)
      } # for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
      Daily_Fire_Cache[date_counter,c("           flg.deg C Sensor  Int AT ")] <- combine_flags # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
    rm(all_flags)
    } # if("deg C Sensor  Int AT " %in% colnames(date_all_Fire_Cache_data)) {
    
    # input %   Sensor  Int RH and corresponding flag: "  %   Sensor  Int RH " and "           flg.  %   Sensor  Int RH "
    # %   Sensor  Int RH column may not exist in all of these files, so only fill it in when it exists:
    if("  %   Sensor  Int RH " %in% colnames(date_all_Fire_Cache_data)) {
      Daily_Fire_Cache[date_counter,c("  %   Sensor  Int RH ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("  %   Sensor  Int RH ")])))
      # flag for % Sensor Int RH is sometimes non-numeric, so an average cannot be taken
      all_flags <- unique(date_all_Fire_Cache_data[,c("           flg.  %   Sensor  Int RH ")]) # what are all the flags on this day?
      #print(all_flags)
      if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
        Daily_Fire_Cache[date_counter,c("           flg.  %   Sensor  Int RH ")] <- unique(date_all_Fire_Cache_data[,c("           flg.  %   Sensor  Int RH ")])
      } else {# there are multiple flags and they need to be stitched together
        combine_flags <- all_flags[1] # get the first flag
        #print(combine_flags)
        for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
          combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
          #print(combine_flags)
        } # for
        Daily_Fire_Cache[date_counter,c("           flg.  %   Sensor  Int RH ")] <- combine_flags # input the flags
        rm(flag_counter,combine_flags) # clear variables
      } # if/else (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      rm(all_flags)
    } # if("  %   Sensor  Int RH " %in% colnames(date_all_Fire_Cache_data)) {
      
    # input " m/s    Wind    Speed" and "           flg. m/s    Wind    Speed"
      Daily_Fire_Cache[date_counter,c(" m/s    Wind    Speed")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" m/s    Wind    Speed")])))
    #Daily_Fire_Cache[date_counter,c("           flg. m/s    Wind    Speed")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. m/s    Wind    Speed")])))
    # flag is sometimes non-numeric, so an average cannot be taken
    flag_col <- "           flg. m/s    Wind    Speed"
    all_flags <- unique(date_all_Fire_Cache_data[,c(flag_col)]) # what are all the flags on this day?
    #print(all_flags)
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c(flag_col)] <- unique(date_all_Fire_Cache_data[,c(flag_col)])
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
        #print(combine_flags)
      } # for
      Daily_Fire_Cache[date_counter,c(flag_col)] <- combine_flags # input the flags
      rm(flag_counter,combine_flags) # clear variables
    } # else
    rm(flag_col,all_flags)
    #rm(col_interest)
    
    # input " Deg   Wind    Direc " and "           flg. Deg   Wind    Direc "
    # input Wind direction and corresponding flag
    #Daily_Fire_Cache[date_counter,c(" Deg   Wind    Direc ")] <- NA
    #Daily_Fire_Cache[date_counter,c("           flg. Deg   Wind    Direc ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Deg   Wind    Direc ")])))
    # Wind direction column may not exist in all of these files, so only fill it in when it exists:
    if(" Deg   Wind    Direc " %in% colnames(date_all_Fire_Cache_data)) {
      Daily_Fire_Cache[date_counter,c(" Deg   Wind    Direc ")] <- NA # average wind direction calculation is slightly more complicated than a direct average, so not doing that for now
      # flag for Wind Direc is sometimes non-numeric, so an average cannot be taken
      all_flags <- unique(date_all_Fire_Cache_data[,c("           flg. Deg   Wind    Direc ")]) # what are all the flags on this day?
      #print(all_flags)
      if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
        Daily_Fire_Cache[date_counter,c("           flg. Deg   Wind    Direc ")] <- unique(date_all_Fire_Cache_data[,c("           flg. Deg   Wind    Direc ")])
      } else {# there are multiple flags and they need to be stitched together
        combine_flags <- all_flags[1] # get the first flag
        #print(combine_flags)
        for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
          combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
          #print(combine_flags)
        } # for
        Daily_Fire_Cache[date_counter,c("           flg. Deg   Wind    Direc ")] <- combine_flags # input the flags
        rm(flag_counter,combine_flags) # clear variables
      } # else
      rm(all_flags)
    } #else {print('Nope, column is not here')} # if(" Deg   Wind    Direc " %in% colnames(date_all_Fire_Cache_data)) {
    
    # input "volts Battery Voltage" and "           flg.volts Battery Voltage"
    Daily_Fire_Cache[date_counter,c("volts Battery Voltage")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")])))
    #Daily_Fire_Cache[date_counter,c("           flg.volts Battery Voltage")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.volts Battery Voltage")])))
    # flag is sometimes non-numeric, so an average cannot be taken
    #voltage_threshold_upper <- 17
    #voltage_threshold_lower <- 11
    if (max(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")])))>voltage_threshold_upper|min(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")])))<voltage_threshold_lower) {
      added_flags <- c(max(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")]))),min(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")]))))
    } else {added_flags <- 0} # if (max(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")])))>voltage_threshold_upper|min(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")])))<voltage_threshold_lower) {
      flag_col <- "           flg.volts Battery Voltage"
      all_flags <- paste
    all_flags <- unique(c(as.character(date_all_Fire_Cache_data[,c(flag_col)]),as.character(added_flags))) # what are all the flags on this day?
    print(all_flags)
    if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
      Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(unique(date_all_Fire_Cache_data[,c(flag_col)]))
    } else {# there are multiple flags and they need to be stitched together
      combine_flags <- all_flags[1] # get the first flag
      #print(combine_flags)
      for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
        combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
        #print(combine_flags)
      } # for
      Daily_Fire_Cache[date_counter,c(flag_col)] <- as.character(combine_flags) # input the flags
      rm(flag_counter,combine_flags,all_flags) # clear variables
    } # else
    rm(flag_col,added_flags)
    
    # "      Alarm          "                "           flg.      Alarm          "
    # input Alarm variable and corresponding flag
    #Daily_Fire_Cache[date_counter,c("      Alarm          ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("      Alarm          ")])))
    #Daily_Fire_Cache[date_counter,c("           flg.      Alarm          ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.      Alarm          ")])))
    # Alarm column may not exist in all of these files, so only fill it in when it exists:
    if("      Alarm          " %in% colnames(date_all_Fire_Cache_data)) {
      Daily_Fire_Cache[date_counter,c("      Alarm          ")] <-  mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("      Alarm          ")])))
      # flag for Wind Direc is sometimes non-numeric, so an average cannot be taken
      all_flags <- unique(date_all_Fire_Cache_data[,c("           flg.      Alarm          ")]) # what are all the flags on this day?
      #print(all_flags)
      if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
        Daily_Fire_Cache[date_counter,c("           flg.      Alarm          ")] <- unique(date_all_Fire_Cache_data[,c("           flg.      Alarm          ")])
      } else {# there are multiple flags and they need to be stitched together
        combine_flags <- all_flags[1] # get the first flag
        #print(combine_flags)
        for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
          combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
          #print(combine_flags)
        } # for
        Daily_Fire_Cache[date_counter,c("           flg.      Alarm          ")] <- combine_flags # input the flags
        rm(flag_counter,combine_flags) # clear variables
      } # else
      rm(all_flags)
    } # if("      Alarm          " %in% colnames(date_all_Fire_Cache_data)) {
    
    # input the number of negative values "N_neg"                                
    Daily_Fire_Cache[date_counter,c("N_neg")] <- sum_negative
    rm(sum_negative)
    
    # input the number of observations "N_Obs"
    Daily_Fire_Cache[date_counter,c("N_Obs")] <- N_obs_this_day 
    rm(N_obs_this_day)
    
    # input max conc for day and what time it occurred
    Daily_Fire_Cache[date_counter,c("1st_Max_Value")] <- max_conc_this_day
    Daily_Fire_Cache[date_counter,c("1st_Max_Hour")] <- when_max_conc[1]#when_max_conc
    rm(when_max_conc,max_conc_this_day)
    #Note: if the maximum concentration is repeated the time of the first occurrence is recorded in 1st_Max_Hour refer
    
    #Daily_Fire_Cache[date_counter,c("R_Dates")] <- format(unique(date_all_Fire_Cache_data[,c("R_Dates")]), "%Y-%m-%d")
    
    print('think about how best to handle flags, wind direction etc.')
    rm(this_date)
  } # for (date_counter in 1:length(these_dates))  
  rm(date_counter,these_dates,date_all_Fire_Cache_data) # clear variables
  
  # figure out what row we're on in input_mat
  row_stop <- row_start+dim(Daily_Fire_Cache)[1]-1
  
  ## fill in input_mat1 with Daily_Fire_Cache data for this DRI file
  
  # put serial # in the Site_Num column of input_mat1
  print(paste("input DRI serial number into ","Site_Num" ," in input_mat1"))   
  input_mat1[row_start:row_stop,c("Site_Num")] <- as.numeric(as.character(Daily_Fire_Cache[,c("ser # Serial  Number ")]))
  
  # input lat and lon ("PM2.5_Lat" and "PM2.5_Lon")
  which_colLat <- which(colnames(Daily_Fire_Cache)==" Deg    GPS     Lat. ")
  input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLat]))
  rm(which_colLat)
  which_colLon <- which(colnames(Daily_Fire_Cache)==" Deg    GPS     Lon. ")
  input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLon]))
  rm(which_colLon)
  # input InDayLatDiff and InDayLonDiff - tells how much variation there is in the Lat & Lon obs within a day (relevant for hourly observations)
  which_colLatDayDiff <- which(colnames(Daily_Fire_Cache) == "InDayLatDiff") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("InDayLatDiff")] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLatDayDiff])) # input that column into input_mat1
  rm(which_colLatDayDiff) # remove column variable
  print("line 1040")
  which_colLonDayDiff <- which(colnames(Daily_Fire_Cache) == "InDayLonDiff") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("InDayLonDiff")] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLonDayDiff])) # input that column into input_mat1
  print("line 1042")
  rm(which_colLonDayDiff) # remove column variable  
  
  # input flags for Lat & Lon observations
  which_colLatFlg <- which(colnames(Daily_Fire_Cache)=="           flg. Deg    GPS     Lat. ") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("flg.Lat")] <- as.character(Daily_Fire_Cache[,which_colLatFlg])# input that column into input_mat1
  rm(which_colLatFlg)
  which_colLonFlg <- which(colnames(Daily_Fire_Cache)=="           flg. Deg    GPS     Lon. ") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("flg.Lon")] <- as.character(Daily_Fire_Cache[,which_colLonFlg])# input that column into input_mat1
  rm(which_colLonFlg)
  
  # input "Sample_Duration"          
  input_mat1[row_start:row_stop,c("Sample_Duration")] <- "1 HOUR"
  
  # input "Date_Local" into input_mat1
  this_col_input_mat <- "Date_Local"
  this_col_source <- ":           :   Date    :MM/DD/YYYY"
  SourceVar <- as.Date(Daily_Fire_Cache[,c(this_col_source)],"%Y-%m-%d")
  #print(SourceVar)
  SourceVarChar <- format(SourceVar,"%Y-%m-%d")
  #print(SourceVarChar)
  input_mat1[row_start:row_stop,c(this_col_input_mat)] <- SourceVarChar
  rm(this_col_input_mat,this_col_source,SourceVar,SourceVarChar)
  
  # input "Units_of_Measure" into input_mat1
  input_mat1[row_start:row_stop,c("Units_of_Measure")] <- "ug/m3 Conc     RT    "
  
  # input "Observation_Count" 
  input_mat1[row_start:row_stop,c("Observation_Count")] <- Daily_Fire_Cache[,c("N_Obs")]
  
  # input "Observation_Percent"     
  input_mat1[row_start:row_stop,c("Observation_Percent")] <- Daily_Fire_Cache[,c("N_Obs")]/24*100
  
  # input "PM2.5_Obs"               
  which_colConc <- which(colnames(Daily_Fire_Cache)=="ug/m3 Conc     RT    ")
  #concentration_vector <- Daily_Fire_Cache[,which_colConc]
  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- as.numeric(as.character(Daily_Fire_Cache[,which_colConc]))
  rm(which_colConc)
  
  # input flag for PM2.5 Obs
  which_ConcFlg <- which(colnames(Daily_Fire_Cache)=="           flg.ug/m3 Conc     RT    ") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("flg.PM25_Obs")] <- as.character(Daily_Fire_Cache[,which_ConcFlg])# input that column into input_mat1
  rm(which_ConcFlg)
  
  # input "1st_Max_Value"            
  which_col <- which(colnames(Daily_Fire_Cache)=="1st_Max_Value")
  data_vector <- Daily_Fire_Cache[,which_col]
  input_mat1[row_start:row_stop,c("1st_Max_Value")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col,data_vector)
  
  # "1st_Max_Hour"            
  which_col <- which(colnames(Daily_Fire_Cache)=="1st_Max_Hour")
  data_vector <- Daily_Fire_Cache[,which_col]
  input_mat1[row_start:row_stop,c("1st_Max_Hour")] <- as.integer(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col,data_vector)
  
  # input "PM25_Station_Name"        
  input_mat1[row_start:row_stop,c("PM25_Station_Name")] <- this_name
  
  #"Data_Source_Name_Display" 
  input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
  
  # "Data_Source_Name_Short"  
  input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short
  
  # input "Data_Source_Counter" - indicates if this is EPA data or field data, etc.
  input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
  
  # input color for plotting this data source (totally arbitrary choice of color)
  input_mat1[row_start:row_stop,c("PlottingColor")] <- "red"
  #c("darkgoldenrod","green","blue") "PlottingColor"
  
  # input source file name ("Source_File")
  input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
  
  # "Composite_of_N_rows"      
  input_mat1[row_start:row_stop,c("Composite_of_N_rows")] <- Daily_Fire_Cache[,c("N_Obs")] 
  
  # "N_Negative_Obs"   
  input_mat1[row_start:row_stop,c("N_Negative_Obs")] <- Daily_Fire_Cache[,c("N_neg")]
  
  # input air flow and flag information
  which_col <- which(colnames(Daily_Fire_Cache)==" l/m   Ave.   Air Flw")
  input_mat1[row_start:row_stop,c("l/m Ave. Air Flw")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg. l/m   Ave.   Air Flw") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("flg.AirFlw")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
  rm(which_col)
  
  # input Air Temp and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="Deg C  Av Air   Temp ")
  input_mat1[row_start:row_stop,c("Deg C Av Air Temp")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.Deg C  Av Air   Temp ") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("flg.AirTemp")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
  rm(which_col)
  
  # input Relative Humidity and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="  %     Rel   Humidty")
  input_mat1[row_start:row_stop,c("% Rel Humidty")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.  %     Rel   Humidty") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("flg.RelHumid")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
  rm(which_col)            
  
  # input Barometric Pressure and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="mbar   Barom   Press ")
  input_mat1[row_start:row_stop,c("mbar Barom Press")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.mbar   Barom   Press ") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("flg.Barom Press")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
  rm(which_col)  
  
  # input Internal Temperature and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="deg C Sensor  Int AT ")
  input_mat1[row_start:row_stop,c("deg C Sensor  Int AT")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.deg C Sensor  Int AT ") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("flg.deg C Sensor Int AT")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
  rm(which_col)    
                                  
  # input Internal Relative Humidity and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="  %   Sensor  Int RH ")
  input_mat1[row_start:row_stop,c("% Sensor Int RH")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.  %   Sensor  Int RH ") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("flg.%SensorIntRH")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
  rm(which_col) 
  
  # input Wind Speed and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)==" m/s    Wind    Speed")
  input_mat1[row_start:row_stop,c("Wind Speed m/s")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg. m/s    Wind    Speed") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("flg.WindSpeed")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
  rm(which_col) 
  
  # input Battery Voltage and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="volts Battery Voltage")
  input_mat1[row_start:row_stop,c("Battery Voltage volts")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.volts Battery Voltage") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("flg.BatteryVoltage")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
  rm(which_col) 
         
  # input Alarm and flag information                  
  which_col <- which(colnames(Daily_Fire_Cache)=="      Alarm          ")
  input_mat1[row_start:row_stop,c("Alarm")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
  rm(which_col)
  which_col <- which(colnames(Daily_Fire_Cache)=="           flg.      Alarm          ") # identify which column in Daily_Fire_Cache we are looking for 
  input_mat1[row_start:row_stop,c("flg.Alarm")] <- as.character(Daily_Fire_Cache[,which_col])# input that column into input_mat1
  rm(which_col) 

## columns of data in input_mat1 to figure out in DRI data
# "Datum"  "State_Name" "County_Name" "State_Abbrev"  "Month" "Day"  
# "Winter" "Year"  

## think about whether we need to figure out how to fill in these variables in input_mat1:
# "Parameter_Code"  "Parameter_Name" "Pollutant_Standard" "Event_Type" "AQI" "Method_Code"              "Method_Name" 
# "City_Name" "POC"                               
# "Address" "CBSA_Name"  "Date_of_Last_Change"                                      
# "flg.Site_Num"   "Type"                     "flg.Type"       
 
## think about whether any of thes variables in Daily_Fire_Cache need to be brought into input_mat1
# " GMT  Time    hh:mm "  "      Type           " , "           flg.      Type           "
# "           flg.ser # Serial  Number ", " Unk   Misc     #1   " ,"           flg. Unk   Misc     #1   "
#  " Deg   Wind    Direc "                "           flg. Deg   Wind    Direc "
  
  # if (this_file_counter==length(all_DRI_Files)){stop("on last file")}
  rm(Daily_Fire_Cache,this_Fire_Cache_data)
  # tick up the row counter
  row_start <- row_stop+1
  print(paste("Done processing ",this_source_file))
  rm(this_source_file)
} # for (this_file_counter in 1:length(all_DRI_Files)){
rm(all_DRI_Files,this_file_counter,comprehensive.header)

rm(Data_Source_Name_Display,Data_Source_Name_Short)

####### Fill in Lyman Uintah Basin data ########################
#stop('fix data_source_counter')
data_source_counter <- data_source_counter+1
#data_source_counter <- 2

Data_Source_Name_Short <- "UintahBasin"
Data_Source_Name_Display <- "Uintah Basin" # Data_Source_Name_Display

     # data_source_counter <- 3
# row_start <-   1870569
this_source_file <- "FinalPM2.5_multiyear_thruwint2017_sheet1_dates.csv" # "Source_File"
print(this_source_file)

# load data
UBdata<-read.csv(file.path(StartData.directory,this_source_file),header=TRUE) 

#row_start <- 1
# row_start <- 1870569
# row_stop <- 1870568
row_stop=row_start+dim(UBdata)[1]-1

# handle date information
new_col_number <- length(UBdata)+1 # figure out how many columns are in UBdata and then add 1
UBdata[,new_col_number] <- as.Date(UBdata[,c("Dates")],"%m/%d/%Y") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
colnames(UBdata)[new_col_number] <- "R_Dates"
rm(new_col_number)

# load file with lat/lon for Uintah Basin stations
UBLocations <- read.csv(file.path(StartData.directory,"FinalPM2.5_multiyear_thruwint2017_GISsheet.csv"),header=TRUE)

for(this_column in 6:15){  
  #print(paste("Column number = ",this_column))
  this_name=colnames(UBdata)[this_column]
  #print(this_name)
  #print(paste(this_name)) #COMMENT
  
  # input data source counter - indicates if this is EPA data or field data, etc.
  input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter #  "Data_Source_Counter"
  input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short # "Data_Source_Name_Short"
  input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
  
  # input color for plotting this data source (totally arbitrary choice of color)
  input_mat1[row_start:row_stop,c("PlottingColor")] <- "darkgoldenrod"
  #c(,"green","blue") "PlottingColor"
  
  # "State_Code"               
  # input state information
  #input_mat1[row_start:row_stop,c("State_Code")] <- 49 
  #input_mat1[row_start:row_stop,c("State_Name")] <- "Utah" # "State_Name"
  #input_mat1[row_start:row_stop,c("State_Abbrev")] <- "UT" # "State_Abbrev"
  print('check if any Uintah basin sites are in CO')
  
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
  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- UBdata[,this_column]
  
  # input source file name
  input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
  #print(UBdata[,"R_Dates"])
  #input_mat1[row_start:row_stop,c('RDates')] <- as.Date(UBdata[,c("Dates")],"%m/%d/%Y")#UBdata[,"R_Dates"]
  
  # input dates "Date_Local"
  #input_mat1[row_start:row_stop,c('RDates')] <- format(UBdata[,c("R_Dates")], "%Y-%m-%d")
  input_mat1[row_start:row_stop,c("Date_Local")] <- format(UBdata[,c("R_Dates")], "%Y-%m-%d")
  
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
    input_mat1[row_start:row_stop,c("Method_Name")] <- "24-hr fileter/gravimetric"
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
  input_mat1[row_start:row_stop,c('Winter')] <- UBdata[,"Winter."] # "Winter"
  input_mat1[row_start:row_stop,c('Year')] <- UBdata[,"year"] # "Year"
  
  # figure out how to fill in "Datum"                    
  
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
  
  # "N_Negative_Obs"
  i_start <- row_start
  i_stop <- row_stop
  for (i in i_start:i_stop) {
    if (input_mat1[i,c("PM2.5_Obs")]<0 & is.na(input_mat1[i,c("PM2.5_Obs")])==FALSE) {
      input_mat1[i,c("N_Negative_Obs")] <- 1
      #print("Negative Conc")
    } else if (input_mat1[i,c("PM2.5_Obs")]>=0 & is.na(input_mat1[i,c("PM2.5_Obs")])==FALSE) { # if (input_mat1[i,c("PM2.5_Obs")<0]) {
      input_mat1[i,c("N_Negative_Obs")] <- 0
      #print("Positive Conc")
    } else {# else if
      #print("Unknown Conc")
    } # else {
  } # for (i in row_start:row_stop) {
  rm(i,i_start,i_stop)
  
  # "InDayLatDiff"
  input_mat1[row_start:row_stop,c("InDayLatDiff")] <- 0
  
  # "InDayLonDiff"
  input_mat1[row_start:row_stop,c("InDayLonDiff")] <- 0
  
  row_start <- row_stop+1
  row_stop <- row_start+dim(UBdata)[1]-1
}

# think about whether to try to fill anything in for these columns:
#"County_Name"              "City_Name"                "CBSA_Name"                "Date_of_Last_Change"                  
# "flg.Lat" "flg.Lon"  "Type" "flg.Type"  "flg.Site_Num"
#  "flg.PM25_Obs" "l/m Ave. Air Flw" "flg.AirFlw" "Deg C Av Air Temp" "flg.AirTemp"
# "% Rel Humidty" "flg.RelHumid" "mbar Barom Press "  ",flg.,Barom,Press" "deg C Sensor  Int AT"
# "flg.deg C Sensor Int AT"  "% Sensor Int RH" "flg.%SensorIntRH" "Wind Speed m/s" "flg.WindSpeed" 
# "Battery Voltage volts" "flg.BatteryVoltage" "Alarm" "flg.Alarm"


# variables to be filled in at the end of the script
# "Month"  "Day"                     

rm(this_column,this_name,this_source_file)
rm(UBdata,UBLocations)
rm(Data_Source_Name_Display,Data_Source_Name_Short)

############################# Fill in Salt Lake City PCAPS data ############################
data_source_counter=data_source_counter+1
Data_Source_Name_Short <- "PCAPS"
Data_Source_Name_Display <- "PCAPS (Salt Lake Valley)"

# load the data file
this_source_file <- "MiniVol_data_dates.csv"
PCAPSdata<-read.csv(file.path(PCAPSData.directory,this_source_file),header=TRUE) 
row_stop <- row_start+dim(PCAPSdata)[1]-1

# handle date information
new_col_number <- length(PCAPSdata)+1
PCAPSdata[,new_col_number] <- as.Date(PCAPSdata[,c("Dates")],"%m/%d/%Y") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
colnames(PCAPSdata)[new_col_number] <- "R_Dates"

# input station names
PCAPSstations <- PCAPSdata[,c('Location')]
print(PCAPSstations)
PCAPSstationsChar <- as.character(PCAPSstations)
print(PCAPSstationsChar)
input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- PCAPSstationsChar # "PM25_Station_Name"
rm(PCAPSstations,PCAPSstationsChar)

# input state information
input_mat1[row_start:row_stop,c("State_Code")] <- 49 # "State_Code" 
input_mat1[row_start:row_stop,c("State_Name")] <- "Utah" # "State_Name"
input_mat1[row_start:row_stop,c("State_Abbrev")] <- "UT" # "State_Abbrev"

# input PM2.5 concentration
this_column <- which(colnames(PCAPSdata)=="ug.m3")
print(paste("Column number = ",this_column))
input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- PCAPSdata[,this_column] # "PM2.5_Obs"

# load file containing lat/lon info for PCAPS sites
PCAPSLocations<-read.csv(file.path(PCAPSData.directory,"PCAPS_Site_Locations.csv"),header=TRUE) 
# input lat/lon information  "PM2.5_Lat", "PM2.5_Lon" 
for(this_row in row_start:row_stop){     
  
  this_name <- input_mat1[this_row,c('PM25_Station_Name')]
  #print(this_name)
  
  if (input_mat1[this_row,c("PM2.5_Obs")]<0 & is.na(input_mat1[this_row,c("PM2.5_Obs")])==FALSE) {
    input_mat1[this_row,c("N_Negative_Obs")] <- 1
    print("Negative obs")
  } else if (input_mat1[this_row,c("PM2.5_Obs")]>=0 & is.na(input_mat1[this_row,c("PM2.5_Obs")])==FALSE){
    input_mat1[this_row,c("N_Negative_Obs")] <- 0
    print("Positive obs")
  } else {
    print("unknown conc")
  }
  
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
         
############################# Fill in data from Federal Land Managers - IMPROVE RHR III ######################
print('still need to pull in data from Federal Land Managers')
data_source_counter <- data_source_counter+1
#Data_Source_Name_Short <- "FedLndMng"
#Data_Source_Name_Display <- "Federal Land Manager"

# if re-starting from here:
# data_source_counter <- 4
# row_start <- 1898396

#this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_II_2018315132109KL0L2K_top_removed.csv" # name of file to be loaded
this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_II_88101_20183151757452922Mvw0s_top_removed.csv"
this_source_file_full <- "Federal_Land_Manager_IMPROVE_RHR_II_88101_20183151757452922Mvw0s.csv"
print(this_source_file)

# load FMLE data
FMLEdata_all_states <- read.csv(file.path(FMLE.directory,this_source_file), header = T, sep = ",",blank.lines.skip = F)
# load parameter description
FMLEdata_Parameter_MetaData <- read.csv(file.path(FMLE.directory,this_source_file_full), header = T, sep = ",",blank.lines.skip = T,nrows = 1,skip = 240)
rm(this_source_file_full)

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
input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- as.character(FMLE_StudyStates_sepCodes[,c("Dataset")]) #"Dataset" 

# "Data_Source_Name_Short"
input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- as.character(FMLE_StudyStates_sepCodes[,c("Dataset")])# "Dataset" 

# "State_Abbrev" 
input_mat1[row_start:row_stop,c("State_Abbrev")] <- as.character(FMLE_StudyStates_sepCodes[,c("State")])

# "Data_Source_Counter"      
input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter

# input color for plotting this data source (totally arbitrary choice of color)
input_mat1[row_start:row_stop,c("PlottingColor")] <- "lightsalmon4" #"blue"

# "Source_File"              
input_mat1[row_start:row_stop,c("Source_File")] <- this_source_file

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

#### Pull in new California PM2.5 data ####
data_source_counter=data_source_counter+1 # counter to distinguish between the various data sources
Data_Source_Name_Short <- "CARB"
Data_Source_Name_Display <- "CARB"

    #this_source_file <- "2008-2014_PM25_daily_averages_20180402.csv"
    this_source_file <- "2008-2014_PM25_daily_averages_20180402_all_characters.csv"
    print(this_source_file)

    # load CARB data
    CARB_data <- read.csv(file.path(CARB.directory,this_source_file), header = T, sep = ",",blank.lines.skip = F, skip = 2)
    
    row_stop <- row_start+dim(CARB_data)[1]-1 # what is the last row number in input_mat1 for inputing this block of data?
    
### fill in each column of input_mat1 ###
# Split FMLE EPACode into State_Code, County_Code and Site_Num for IMPROVE data and put them into input_mat1
N_CARB_EPACodes <- length(unique(CARB_data$AQS.Site.ID))
CARB_EPACode_header <-  c("EPACode","StateCode","CountyCode","SiteNum")
N_EPACode_columns <- length(CARB_EPACode_header) # how many columns are in header?
CARB_EPACode <- data.frame(matrix(NA,nrow=N_CARB_EPACodes,ncol=N_EPACode_columns)) # create data frame for input_mat1
names(CARB_EPACode) <- CARB_EPACode_header # assign the header to input_mat1
CARB_EPACode$EPACode <- unique(CARB_data$AQS.Site.ID)
rm(N_EPACode_columns)

# Split CARB EPACode into State_Code, County_Code and Site_Num and put them into input_mat1
for (this_row in 1:N_CARB_EPACodes) { # cycle through each row in CARB data to determine state code, county code, and site num and put into input_mat1
  this_EPACode <- as.character((CARB_EPACode[this_row,c("EPACode")])) # isolate the EPA code for this row of data
  #print(this_EPACode)
  if (is.na(this_EPACode)==TRUE) {
    CARB_EPACode[this_row,c("StateCode")] <- NA
    CARB_EPACode[this_row,c("CountyCode")] <- NA
    CARB_EPACode[this_row,c("SiteNum")] <- NA
  } else if (nchar(this_EPACode)==8) { # determine how many characters are in EPACode (leading zeros are not in the data)
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
  } # if (is.na(this_EPACode)==TRUE) {
  rm(this_EPACode)
} # for (this_row in row_start:row_stop) { # cycle through each row in CARB data to determine state code, county code, and site num and put into input_mat1
rm(this_row)

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

for (this_row in 1:dim(CARB_EPACode)[1]) { # put columns of state code, county code, and site number into CARB_StudyStates_sepCodes
  # what are the codes for this row of CARB_EPACode?
  this_code <- CARB_EPACode[this_row,c("EPACode")]
  this_state <- CARB_EPACode[this_row,c("StateCode")]
  this_county <- CARB_EPACode[this_row,c("CountyCode")]
  this_siteNum <- CARB_EPACode[this_row,c("SiteNum")]
  #print(this_code) # this row of code
  # what rows in CARB_StudyStates_sepCodes has this EPA code?
  rows_of_interest <- which(CARB_data$AQS.Site.ID==this_code)
  CARB_data[rows_of_interest,c("StateCode")] <- this_state
  CARB_data[rows_of_interest,c("CountyCode")] <- this_county
  CARB_data[rows_of_interest,c("SiteNum")] <- this_siteNum
  rm(rows_of_interest,this_code,this_state,this_county,this_siteNum)
}
rm(this_row)

# State Code
input_mat1[row_start:row_stop,c("State_Code")] <- as.character(CARB_data$StateCode)

# County Code
input_mat1[row_start:row_stop,c("County_Code")] <- as.character(CARB_data$CountyCode)

# Site Number
input_mat1[row_start:row_stop,c("Site_Num")] <- as.character(CARB_data$SiteNum)

#"PM25_Station_Name"
input_mat1[row_start:row_stop,c("PM25_Station_Name")] <- paste(CARB_data$Basin,CARB_data$Site.Name,CARB_data$Site,CARB_data$Monitor,sep = " ")

# "County_Name"
input_mat1[row_start:row_stop,c("County_Name")] <- as.character(CARB_data$County)

# input latitude and longitude ('PM2.5_Lat','PM2.5_Lon')
input_mat1[row_start:row_stop,c("PM2.5_Lat")] <- CARB_data[,c('Latitude')]
input_mat1[row_start:row_stop,c("PM2.5_Lon")] <- CARB_data[,c('Longitude')]

# "Observation_Count"
input_mat1[row_start:row_stop,c("Observation_Count")] <- CARB_data$Number.of.Observations

# "PM2.5_Obs" 
input_mat1[row_start:row_stop,c("PM2.5_Obs")] <- CARB_data$"Daily.Average..µg.m3."

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
input_mat1[row_start:row_stop,c("Units_of_Measure")] <- "µg.m3"

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

which_negative <- which(CARB_data$Daily.Average..µg.m3.<0)
CARB_data[which_negative,c("N_Negative_Obs")] <- 1 # indicate that these rows had 1 negative observation
rm(which_negative)

which_positive <- which(CARB_data$Daily.Average..µg.m3.>=0)
CARB_data[which_positive,c("N_Negative_Obs")] <- 0 # indicate that rows with positive concentrations have 0 negative valuse
rm(which_positive)

input_mat1[row_start:row_stop,c("N_Negative_Obs")] <- CARB_data$N_Negative_Obs

# InDayLatDiff and InDayLonDiff used to figure out if lat/lon observations within a day do not agree (relevant for DRI data), set to 0 for this data
input_mat1[row_start:row_stop,c("InDayLatDiff")] <- 0
input_mat1[row_start:row_stop,c("InDayLonDiff")] <- 0

# Think about whether to try to include these variables from CARB_data into input_mat1
# "Number.of.Hours"     "Notes."    

# need to figure out whether/how to fill in these variable in input_mat1 for the CARB_data
# "Parameter_Code" "POC" "Datum" "Parameter_Name" "Pollutant_Standard" "Event_Type"
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
    rm(Data_Source_Name_Display,Data_Source_Name_Short)
#### Pull in new Utah PM2.5 data ####
print('pull in new Utah PM2.5 data')
     
###################### Fill in columns derived from other columns ########
print('pick up writing code here')
# Note: 'Winter' is filled in near the end of the script

# Note: 'Year' is filled in near the end of the script
  All_dates_vec <- as.Date(input_mat1$Date_Local)
  str(All_dates_vec)
  All_Years_vec <- format(All_dates_vec,"%Y")
  input_mat1$Year <- All_Years_vec
  rm(All_Years_vec)

# Note: 'Month' is filled in near the end of the script
  All_Months_vec <- format(All_dates_vec,"%m")
  input_mat1$Month <- All_Months_vec
  rm(All_Months_vec)

# Note: 'Day' is filled in near the end of the script
  All_Days_vec <- format(All_dates_vec,"%d")
  input_mat1$Day <- All_Days_vec
  rm(All_Days_vec,All_dates_vec)

# fill in state code, "County_Code"   where it is missing

#### Save input_mat1 to csv file ####

print('figure out why it seems to be adding an extra column with row numbers at the beginning.')      

print('Check if the AQS data is not on top (use "head") and if not, figure out why')
      
print('figure out if it would be possible to set documentation so that ?input_mat1 does something')

print('figure out why RDates column is still showing up and get rid of it')

#write.csv(mycars, file='mycars.csv') #or export DF as .CSV file
write.csv(input_mat1,file = file.path(ProcessedData.directory,'combined_ML_input.csv'),row.names = FALSE)

#### Create a data frame with just lat, lon, and date ####
four_cols_w_duplicates <- input_mat1[,c("PM2.5_Lat","PM2.5_Lon","Datum","Date_Local")]
four_cols_data <- four_cols_w_duplicates[!duplicated(four_cols_w_duplicates),]
names(four_cols_data) <- c("Latitude","Longitude","Datum","Date")
write.csv(four_cols_data,file = file.path(ProcessedData.directory,'Locations_Dates_of_PM25_Obs.csv'),row.names = FALSE)
rm(four_cols_data,four_cols_w_duplicates)

#### End of file clean up ####
rm(AQSData.directory,FMLE.directory,ProcessedData.directory,StartData.directory,CARB.directory)
rm(PCAPSData.directory,USMaps.directory,FireCache.directory,uppermost.directory,working.directory)
rm(SinkFileName)
rm(data_source_counter,row_start,row_stop,voltage_threshold_lower,voltage_threshold_upper)
rm(start_study_year,stop_study_year)
rm(input_header,input_mat1)
sink()
