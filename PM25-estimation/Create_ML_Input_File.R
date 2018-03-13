# Create input file for Machine Learning estimation of PM2.5 for the western US, 2008-2014
# To clear all variables and start fresh:
# rm(list = ls())
options(warn=2) # throw an error when there's a warning and stop the code from running further

cat("INSTALL PACKAGES \n")
#install.packages(pkgs="maps")
#install.packages(pkgs="mapproj")
install.packages('ggplot2')
install.packages(pkgs="ggmap")
install.packages(pkgs="rgdal")
install.packages(pkgs="rgeos")
install.packages(pkgs="maptools")
install.packages(pkgs="dplyr")
install.packages(pkgs="tidyr")
#install.packages(pkgs="tmap")
#install.packages('leaflet')

# define directories
uppermost.directory="/home/rstudio" # on AWS
working.directory=uppermost.directory # on AWS
setwd(working.directory)
output.directory=file.path(working.directory,"Code_Outputs")
#output.directory=file.path(working.directory,"estimate-pm25","LaTeX_documentation","Code_Outputs")
ProcessedData.directory=file.path(working.directory,"Processed_Data")
StartData.directory=file.path(working.directory,"PM25_Uintah_Basin")
USMaps.directory=file.path(working.directory,"Shapefiles_for_mapping","cp_2016_us_state_500k")
PCAPSData.directory=file.path(working.directory,"PM25_PCAPS_Salt_Lake")
AQSData.directory=file.path(working.directory,"AQS_Daily_Summaries")
FMLE.directory=file.path(working.directory,"Federal_Land_Manager_Environmental_Database")
FireCache.directory=file.path(working.directory,"Fire_Cache_Smoke_DRI")
start_study_year <- 2008
stop_study_year <- 2014
#min_hourly_obs_daily <- 18 # minimum number of hourly observations required to compute a 24-hr average
# sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
SinkFileName=file.path(output.directory,"Create_ML_Input_File_sink.txt")
sink(file =SinkFileName, append = FALSE, type = c("output","message"),
     split = FALSE)
sink() #COMMENT
cat("Code and R output for Creat_ML_Input_File.R \n \n")
############################################################################
cat("Title: Create_ML_Input_File.R \n")
cat("Author: Melissa May Maestas \n")
cat("Original Date: January 23, 2018 \n")
cat("Latest Update: February 11, 2018 \n")
cat("This program reads in and PM2.5 data from several sources. \n")
############################################################################

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

######################## Start Input file for machine learning#######################
input_header= c('State_Code','County_Code','Site_Num','Parameter_Code','POC','PM2.5_Lat','PM2.5_Lon','Datum','Parameter_Name','Sample_Duration','Pollutant_Standard','Date_Local','Units_of_Measure','Event_Type','Observation_Count','Observation_Percent','PM2.5_Obs','1st_Max_Value','1st_Max_Hour','AQI','Method_Code','Method_Name','PM25_Station_Name','Address','State_Name','County_Name','City_Name','CBSA_Name','Date_of_Last_Change', # columns in AQS data
                'State_Abbrev','Winter','Year','Month','Day','Data_Source_Name_Display','Data_Source_Name_Short','Data_Source_Counter','Source_File','Composite_of_N_rows','N_Negative_Obs', # other columns to include
                "flg.Lat","flg.Lon","Type","flg.Type","flg.Site_Num","flg.PM25_Obs","l/m Ave. Air Flw", # DRI variables
                "flg.AirFlw","Deg C Av Air Temp","flg.AirTemp","% Rel Humidty","flg.RelHumid","mbar Barom Press ",",flg.,Barom,Press", # DRI variables
                "deg C Sensor  Int AT","flg.deg C Sensor Int AT","% Sensor Int RH","flg.%SensorIntRH", # DRI variables
                "Wind Speed m/s","flg.WindSpeed","Battery Voltage volts","flg.BatteryVoltage","Alarm","flg.Alarm", # DRI variables
                "InDayLatDiff","InDayLonDiff")
                
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

N_columns=length(input_header)
input_mat1=data.frame(matrix(NA,nrow=10,ncol=N_columns))
names(input_mat1)=input_header

############################## Pull in AQS data #################
data_source_counter=0 # counter to distinguish between the various data sources
Data_Source_Name_Short <- "EPA_PM25"
Data_Source_Name_Display <- "EPA PM2.5"
row_start=1 # start row counter
ParameterCode_vec <- cbind(88101,88502)

# cycle through files
for(this_year in start_study_year:stop_study_year){     # cycle through years
  print(this_year)
  for(this_ParamCode in ParameterCode_vec){ # cycle through Parameter Codes
    print(this_ParamCode)
    this_source_file <- paste('daily_',as.character(this_ParamCode),'_',as.character(this_year),'.csv',sep="")
    print(this_source_file)
    # load the AQS file
    #ThisAQSdata_step<-read.csv(file.path(AQSData.directory,this_source_file),header=TRUE) 
    ThisAQSdata<-read.csv(file.path(AQSData.directory,this_source_file),header=TRUE) 

    # isolate data in study states
    #class(ThisAQSdata$State.Code)
    # only study area states: #ThisAQSdata_StudyStates <- ThisAQSdata[which(ThisAQSdata$State.Code==4|ThisAQSdata$State.Code==6|ThisAQSdata$State.Code==8|ThisAQSdata$State.Code==16|ThisAQSdata$State.Code==30|ThisAQSdata$State.Code==32|ThisAQSdata$State.Code==35|ThisAQSdata$State.Code==41|ThisAQSdata$State.Code==49|ThisAQSdata$State.Code==53|ThisAQSdata$State.Code==56), ]
    ThisAQSdata_StudyStates <- ThisAQSdata[which(ThisAQSdata$State.Code==4|ThisAQSdata$State.Code==6|ThisAQSdata$State.Code==8|ThisAQSdata$State.Code==16|ThisAQSdata$State.Code==30|ThisAQSdata$State.Code==32|ThisAQSdata$State.Code==35|ThisAQSdata$State.Code==41|ThisAQSdata$State.Code==49|ThisAQSdata$State.Code==53|ThisAQSdata$State.Code==56|ThisAQSdata$State.Code==38|ThisAQSdata$State.Code==46|ThisAQSdata$State.Code==31|ThisAQSdata$State.Code==20|ThisAQSdata$State.Code==40|ThisAQSdata$State.Code==48), ]
    
    rm(ThisAQSdata)
    #unique(ThisAQSdata_StudyStates$State.Name)
    row_stop <- row_start+dim(ThisAQSdata_StudyStates)[1]-1
    
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
    #print(AQSVar)
    AQSVarChar <- as.character(AQSVar)
    #print(AQSVarChar)
    input_mat1[row_start:row_stop,c(this_col)] <- AQSVarChar
    rm(this_col,AQSVar,AQSVarChar)
    
    # input 'Parameter_Name' into input_mat1
    this_col_input_mat <- 'Parameter_Name'
    this_col_AQS <- 'Parameter.Name'
    AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
    #print(AQSVar)
    AQSVarChar <- as.character(AQSVar)
    #print(AQSVarChar)
    input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
    rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
    
    # input "Sample_Duration" into input_mat1
    this_col_input_mat <- "Sample_Duration"
    this_col_AQS <- 'Sample.Duration'
    AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
    #print(AQSVar)
    AQSVarChar <- as.character(AQSVar)
    #print(AQSVarChar)
    input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
    rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
    
    # input 'Pollutant_Standard' into input_mat1
    this_col_input_mat <- 'Pollutant_Standard'
    this_col_AQS <- 'Pollutant.Standard'
    AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
    #print(AQSVar)
    AQSVarChar <- as.character(AQSVar)
    #print(AQSVarChar)
    input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
    rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
    
    # input 'Date_Local' into input_mat1
    this_col_input_mat <- 'Date_Local'
    this_col_AQS <- 'Date.Local'
    AQSVar <- as.Date(ThisAQSdata_StudyStates[,c(this_col_AQS)],"%Y-%m-%d")
    #print(AQSVar)
    AQSVarChar <- format(AQSVar,"%Y-%m-%d")
    #print(AQSVarChar)
    input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
    rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
    
    # input 'Units_of_Measure' into input_mat1
    this_col_input_mat <- 'Units_of_Measure'
    this_col_AQS <- 'Units.of.Measure'
    AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
    #print(AQSVar)
    AQSVarChar <- as.character(AQSVar)
    #print(AQSVarChar)
    input_mat1[row_start:row_stop,c(this_col_input_mat)] <- AQSVarChar
    rm(this_col_input_mat,this_col_AQS,AQSVar,AQSVarChar)
    
    # input 'Event_Type' into input_mat1
    this_col_input_mat <- 'Event_Type'
    this_col_AQS <- 'Event.Type'
    AQSVar <- ThisAQSdata_StudyStates[,c(this_col_AQS)]
    #print(AQSVar)
    AQSVarChar <- as.character(AQSVar)
    #print(AQSVarChar)
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
    #print(AQSVar)
    AQSVarChar <- as.character(AQSVar)
    #print(AQSVarChar)
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

# inpout 'State_Abbrev' 
print('think about whether to move input of State_Abbrev to the end of the script')
state_rows <- which(input_mat1$State_Code==4)
input_mat1[state_rows,c("State_Abbrev")] <- "AZ"
state_rows <- which(input_mat1$State_Code==6)
input_mat1[state_rows,c("State_Abbrev")] <- "CA"
state_rows <- which(input_mat1$State_Code==8)
input_mat1[state_rows,c("State_Abbrev")] <- "CO"
state_rows <- which(input_mat1$State_Code==16)
input_mat1[state_rows,c("State_Abbrev")] <- "ID"
state_rows <- which(input_mat1$State_Code==20)
input_mat1[state_rows,c("State_Abbrev")] <- "KS"
state_rows <- which(input_mat1$State_Code==30)
input_mat1[state_rows,c("State_Abbrev")] <- "MT"
state_rows <- which(input_mat1$State_Code==31)
input_mat1[state_rows,c("State_Abbrev")] <- "NE"
state_rows <- which(input_mat1$State_Code==32)
input_mat1[state_rows,c("State_Abbrev")] <- "NV"
state_rows <- which(input_mat1$State_Code==35)
input_mat1[state_rows,c("State_Abbrev")] <- "NM"
state_rows <- which(input_mat1$State_Code==38)
input_mat1[state_rows,c("State_Abbrev")] <- "ND"
state_rows <- which(input_mat1$State_Code==40)
input_mat1[state_rows,c("State_Abbrev")] <- "OK"
state_rows <- which(input_mat1$State_Code==41)
input_mat1[state_rows,c("State_Abbrev")] <- "OR"
state_rows <- which(input_mat1$State_Code==46)
input_mat1[state_rows,c("State_Abbrev")] <- "SD"
state_rows <- which(input_mat1$State_Code==48)
input_mat1[state_rows,c("State_Abbrev")] <- "TX"
state_rows <- which(input_mat1$State_Code==49)
input_mat1[state_rows,c("State_Abbrev")] <- "UT"
state_rows <- which(input_mat1$State_Code==53)
input_mat1[state_rows,c("State_Abbrev")] <- "WA"
state_rows <- which(input_mat1$State_Code==56)
input_mat1[state_rows,c("State_Abbrev")] <- "WY"

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

############################# Pull in Fire Cache Smoke (DRI) data #################
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
     print(all_DRI_Files)

     # cycle through files
     for (this_file_counter in 1:3){#length(all_DRI_Files)){
       print(paste('this_file_counter =',this_file_counter))
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
         print('first file')
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
       print(these_dates)
       # create data frame that will have one observation per day
       N_columns_Fire_Cache=length(comprehensive.header) # number of columns
       Daily_Fire_Cache=data.frame(matrix(NA,nrow=length(these_dates),ncol=N_columns_Fire_Cache)) # create empty data frame
       names(Daily_Fire_Cache)=comprehensive.header # give new data frame a header
       rm(N_columns_Fire_Cache)
       print('still need to deal with some files having hour 20:00 data shifted a couple of columns')
       for (date_counter in 1:length(these_dates)) {
         this_date <- these_dates[date_counter]
         print(this_date)
         
         # isolate the data for this date
         find_this_data_rows <- which(this_Fire_Cache_data[,c("R_Dates")]==this_date)
         date_all_Fire_Cache_data <- this_Fire_Cache_data[find_this_data_rows,]
         rm(find_this_data_rows)
         
         # make a note of negative values
         print('need to decide whether we should just be removing the negative values and keeping the others within a 24-hr period')
         date_this_conc_data <-as.numeric(as.character(date_all_Fire_Cache_data[,c("ug/m3 Conc     RT    ")]))
         which_negative <- which(date_this_conc_data<0)
         sum_negative <- length(which_negative)
         print(paste("number of negative observations in ",this_source_file,"on",this_date,"=",sum_negative))
         #if (length(which_negative)>0){print(paste(length(which_negative),' data points are removed from ',this_name,' on ',this_date))}
         rm(which_negative)
         
         # make a note of max value and when it occurred, used for "1st_Max_Value" and "1st_Max_Hour"
         max_conc_this_day <- max(date_this_conc_data)
         #print(max_conc_this_day)
         which_max_conc <- which(date_this_conc_data==max_conc_this_day)
         #print(which_max_conc)
         when_max_conc <- date_all_Fire_Cache_data[which_max_conc,c(" GMT  Time    hh:mm ")]
         #print(when_max_conc)
         
         #find_this_data_rows_step2 <- which(date_this_conc_data>=0)
         #date_all_Fire_Cache_data_step2 <- date_all_Fire_Cache_data_step[find_this_data_rows_step2,]
         #rm(date_this_conc_data,find_this_data_rows_step2,date_all_Fire_Cache_data_step)
         
         # # rule out readings with missing longitude data
         # date_this_lon_data_step3 <-as.numeric(as.character(date_all_Fire_Cache_data_step2[,c(" Deg    GPS     Lon. ")]))
         # find_this_data_rows_step3 <- which(date_this_lon_data_step3>=-180)
         # date_all_Fire_Cache_data_step3 <- date_all_Fire_Cache_data_step2[find_this_data_rows_step3,]
         # rm(date_this_lon_data_step3,find_this_data_rows_step3,date_all_Fire_Cache_data_step2)
         # # rule out readings with negative battery voltage
         # date_this_batt_volt <-as.numeric(as.character(date_all_Fire_Cache_data_step3[,c("volts Battery Voltage")]))
         # find_this_data_rows <- which(date_this_batt_volt>=0)
         # date_all_Fire_Cache_data <- date_all_Fire_Cache_data_step3[find_this_data_rows,]
         # rm(date_this_batt_volt,date_all_Fire_Cache_data_step3)
         # #rm(date_this_conc_data,find_this_data_rows_step,date_all_Fire_Cache_data_step)
         
         # check if there are more than 24 observations on a given day ... not expected
         if (dim(date_all_Fire_Cache_data)[1]>24){#(length(find_this_data_rows)>24){
           print(this_date)
           print(this_source_file)
           stop('There appear to be more than 24 observations for this monitor')
         }
         N_obs_this_day <- dim(date_all_Fire_Cache_data)[1]
         print(paste("Number of observations in ",this_source_file,"on",this_date,"=",N_obs_this_day))
        
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
         rm(flag_col)
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
         rm(max_lon,min_lon,lon_diff)
         
         # fill in Type and corresponding flag
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
         rm(flag_col)
         
         # input monitor serial # and corresponding flag
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
          rm(flag_col)

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
             #cat("Yep, it's in there!\n");
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
           rm(flag_col)
           
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
           rm(flag_col)
           
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
             #cat("Yep, it's in there!\n");
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
             #cat("Yep, it's in there!\n");
           } #else {print('Nope, column is not here')} # if/else (" Unk   Misc     #2   " %in% colnames(date_all_Fire_Cache_data)) {
           
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
             rm(flag_col)
             #cat("Yep, it's in there!\n");
           } #else {print('Nope, column is not here')} # if/else (col_interest %in% colnames(date_all_Fire_Cache_data)){
           rm(col_interest)

           # input "deg C Sensor  Int AT " and "           flg.deg C Sensor  Int AT "
            # install deg C Sensor Int AT and corresponding flag
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
           
          # ipnut "  %   Sensor  Int RH " and "           flg.  %   Sensor  Int RH "
          # input %   Sensor  Int RH and corresponding flag
           #Daily_Fire_Cache[date_counter,c("  %   Sensor  Int RH ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("  %   Sensor  Int RH ")])))
           #Daily_Fire_Cache[date_counter,c("           flg.  %   Sensor  Int RH ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.  %   Sensor  Int RH ")])))
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
             #cat("Yep, it's in there!\n");
           } else #{print('Nope, column is not here')} # if/else ("  %   Sensor  Int RH " %in% colnames(date_all_Fire_Cache_data)) {
           
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
             rm(flag_col)
           #  cat("Yep, it's in there!\n");
           #} else {print('Nope, column is not here')}
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
             #Daily_Fire_Cache[date_counter,c("           flg. Deg   Wind    Direc ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Deg   Wind    Direc ")])))
             #cat("Yep, it's in there!\n");
           } #else {print('Nope, column is not here')} # if(" Deg   Wind    Direc " %in% colnames(date_all_Fire_Cache_data)) {
           
          # input "volts Battery Voltage" and "           flg.volts Battery Voltage"
          Daily_Fire_Cache[date_counter,c("volts Battery Voltage")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")])))
          #Daily_Fire_Cache[date_counter,c("           flg.volts Battery Voltage")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.volts Battery Voltage")])))
          # flag is sometimes non-numeric, so an average cannot be taken
          flag_col <- "           flg.volts Battery Voltage"
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
          rm(flag_col)

          # "      Alarm          "                "           flg.      Alarm          "
          # input Alarm variable and corresponding flag
           #Daily_Fire_Cache[date_counter,c("      Alarm          ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("      Alarm          ")])))
           #Daily_Fire_Cache[date_counter,c("           flg.      Alarm          ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.      Alarm          ")])))
           # Alarm column may not exist in all of these files, so only fill it in when it exists:
           if("      Alarm          " %in% colnames(date_all_Fire_Cache_data)) {
             Daily_Fire_Cache[date_counter,c("      Alarm          ")] <- NA # average wind direction calculation is slightly more complicated than a direct average, so not doing that for now
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
             #cat("Yep, it's in there!\n");
           } #else {print('Nope, column is not here')} # if("      Alarm          " %in% colnames(date_all_Fire_Cache_data)) {
           
          # input the number of negative values "N_neg"                                
          Daily_Fire_Cache[date_counter,c("N_neg")] <- sum_negative
          
          # input the number of observations "N_Obs"
          Daily_Fire_Cache[date_counter,c("N_Obs")] <- N_obs_this_day 
           
          # input max conc for day and what time it occurred
          Daily_Fire_Cache[date_counter,c("1st_Max_Value")] <- max_conc_this_day
          Daily_Fire_Cache[date_counter,c("1st_Max_Hour")] <- when_max_conc[1]#when_max_conc
          print("Note that if the maximum concentration is repeated the time of the first occurrence is recorded in 1st_Max_Hour refer")
          
           #Daily_Fire_Cache[date_counter,c("R_Dates")] <- format(unique(date_all_Fire_Cache_data[,c("R_Dates")]), "%Y-%m-%d")
           
           print('think about how best to handle flags, wind direction etc.')
           rm(this_date)
         } # for (date_counter in 1:length(these_dates))  
         rm(date_counter,these_dates,date_all_Fire_Cache_data) # clear variables
          
         # figure out what row we're on in input_mat
         row_stop <- row_start+dim(Daily_Fire_Cache)[1]-1
         
         # fill in input_mat1 with Daily_Fire_Cache data for this DRI file
         #> colnames(input_mat1)
         print(paste("input ","State_Code"," into input_mat1 for DRI data"),sep ="")
         print(paste("input ","County_Code"," into input_mat1 for DRI data"))           
         
         # put serial # in the Site_Num column of input_mat1
         print(paste("input DRI serial number into ","Site_Num" ," in input_mat1"))   
         input_mat1[row_start:row_stop,c("Site_Num")] <- as.numeric(as.character(Daily_Fire_Cache[,c("ser # Serial  Number ")]))
           
         print(paste("input ","Parameter_Code"," into input_mat1 for DRI data"))       
         print(paste("input ","POC"    ," into input_mat1 for DRI data"))                    
                        
         # input lat and lon ("PM2.5_Lat" and "PM2.5_Lon")
         which_colLat <- which(colnames(Daily_Fire_Cache)==" Deg    GPS     Lat. ")
         input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLat]))
         rm(which_colLat)
         which_colLon <- which(colnames(Daily_Fire_Cache)==" Deg    GPS     Lon. ")
         input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- as.numeric(as.character(Daily_Fire_Cache[,which_colLon]))
         rm(which_colLon)
         
         print(paste("figure out ","Datum","for DRI data"))
         
         print(paste("figure out ","Parameter_Name","for DRI data"))
         
         # input "Sample_Duration"          
         input_mat1[row_start:row_stop,c("Sample_Duration")] <- "1 HOUR"
         
         print(paste("figure out ","Pollutant_Standard","for DRI data"))
         
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
         
         print(paste("figure out ","Event_Type","for DRI data"))
         
         # input "Observation_Count" 
         input_mat1[row_start:row_stop,c("Observation_Count")] <- Daily_Fire_Cache[,c("N_Obs")]
         
         # input "Observation_Percent"     
         input_mat1[row_start:row_stop,c("Observation_Percent")] <- Daily_Fire_Cache[,c("N_Obs")]/24*100
         
         # input "PM2.5_Obs"               
         which_colConc <- which(colnames(Daily_Fire_Cache)=="ug/m3 Conc     RT    ")
         concentration_vector <- Daily_Fire_Cache[,which_colConc]
         input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- as.numeric(as.character(Daily_Fire_Cache[,which_colConc]))
         rm(which_colConc)
         
         # input "1st_Max_Value"            
         which_col <- which(colnames(Daily_Fire_Cache)=="1st_Max_Value")
         data_vector <- Daily_Fire_Cache[,which_col]
         input_mat1[row_start:row_stop,c("1st_Max_Value")] <- as.numeric(as.character(Daily_Fire_Cache[,which_col]))
         rm(which_col)
         
         "1st_Max_Hour"            
         which_col <- which(colnames(Daily_Fire_Cache)=="1st_Max_Hour")
         data_vector <- Daily_Fire_Cache[,which_col]
         input_mat1[row_start:row_stop,c("1st_Max_Hour")] <- as.integer(as.character(Daily_Fire_Cache[,which_col]))
         rm(which_col)
         
         print(paste("figure out ","AQI","for DRI data"))      
         
         print(paste("figure out ","Method_Code","for DRI data"))
         print(paste("figure out ","Method_Name","for DRI data"))
          
         # input "PM25_Station_Name"        
         input_mat1[row_start:row_stop,c("PM25_Station_Name")] <- this_name
         
         print(paste("figure out ","Address","for DRI data"))
         print(paste("figure out ","State_Name","for DRI data"))            
         print(paste("figure out ","County_Name","for DRI data"))         
         print(paste("figure out ","City_Name","for DRI data"))              
         print(paste("figure out ","CBSA_Name","for DRI data")) 
         print(paste("figure out ","Date_of_Last_Change","for DRI data"))
         print(paste("figure out ","State_Abbrev","for DRI data"))
         print(paste("figure out ","Winter","for DRI data"))
         print(paste("figure out ","Year","for DRI data"))                 
         print(paste("figure out ","Month","for DRI data"))
         print(paste("figure out ","Day","for DRI data"))
         
         #"Data_Source_Name_Display" 
         input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
         
         # "Data_Source_Name_Short"  
         input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short
         
         # input "Data_Source_Counter" - indicates if this is EPA data or field data, etc.
         input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
    
         # input source file name ("Source_File")
         input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
         
         # "Composite_of_N_rows"      
         input_mat1[row_start:row_stop,c("Composite_of_N_rows")] <- Daily_Fire_Cache[,c("N_Obs")] 
         
         # "N_Negative_Obs"   
         input_mat1[row_start:row_stop,c("N_Negative_Obs")] <- Daily_Fire_Cache[,c("N_neg")]
         
         # DRI variables to be filled in from mapping information (derive from lat/lon)
         # "State_Code","County_Code","State_Name","County_Name","City_Name","CBSA_Name","State_Abbrev"
         
        # Variables to derive from date information
        # "Winter","Year","Month","Day"                
         
         # > colnames(Daily_Fire_Cache)
         # "           flg. Deg    GPS     Lat. "
         # "           flg. Deg    GPS     Lon. "
         # [7] "      Type           "                "           flg.      Type           "
         # [9]                 "           flg.ser # Serial  Number "
         #                 "           flg.ug/m3 Conc     RT    "
         # [13] " Unk   Misc     #1   "                "           flg. Unk   Misc     #1   "
         # [15] " l/m   Ave.   Air Flw"                "           flg. l/m   Ave.   Air Flw"
         # [17] "Deg C  Av Air   Temp "                "           flg.Deg C  Av Air   Temp "
         # [19] "  %     Rel   Humidty"                "           flg.  %     Rel   Humidty"
         # [21] "mbar   Barom   Press "                "           flg.mbar   Barom   Press "
         # [23] "deg C Sensor  Int AT "                "           flg.deg C Sensor  Int AT "
         # [25] "  %   Sensor  Int RH "                "           flg.  %   Sensor  Int RH "
         # [27] " m/s    Wind    Speed"                "           flg. m/s    Wind    Speed"
         # [29] " Deg   Wind    Direc "                "           flg. Deg   Wind    Direc "
         # [31] "volts Battery Voltage"                "           flg.volts Battery Voltage"
         # [33] "      Alarm          "                "           flg.      Alarm          "
         # [37] "InDayLatDiff"                         "InDayLonDiff"                        
         # [39] " Unk   Misc     #2   "                "           flg. Unk   Misc     #2   "
         
         if (this_file_counter==length(all_DRI_Files)){stop("on last file")}
         rm(Daily_Fire_Cache,this_Fire_Cache_data)
         # tick up the row counter
         row_start <- row_stop+1
         print(paste("Done processing ",this_source_file))
         rm(this_source_file)
       } # for (this_file_counter in 1:length(all_DRI_Files)){

 

       
       
     #}
     #rm(all_DRI_Files)

####### Fill in Lyman Uintah Basin data ########################
data_source_counter <- data_source_counter+1
Data_Source_Name_Short <- "UintahBasin"
Data_Source_Name_Display <- "Uintah Basin"

     # data_source_counter <- 3
# row_start <-   1869901
     # row_stop <- 1869900
this_source_file <- "FinalPM2.5_multiyear_thruwint2017_sheet1_dates.csv"
print(this_source_file)

# load data
UBdata<-read.csv(file.path(StartData.directory,this_source_file),header=TRUE) 

# handle date information
new_col_number <- length(UBdata)+1 # figure out how many columns are in UBdata and then add 1
UBdata[,new_col_number] <- as.Date(UBdata[,c("Dates")],"%m/%d/%Y") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
colnames(UBdata)[new_col_number] <- "R_Dates"
rm(new_col_number)

# load file with lat/lon for Uintah Basin stations
UBLocations <- read.csv(file.path(StartData.directory,"FinalPM2.5_multiyear_thruwint2017_GISsheet.csv"),header=TRUE)

#row_start <- 1
row_stop=row_start+dim(UBdata)[1]-1

for(this_column in 6:15){  
  print(paste("Column number = ",this_column))
  this_name=colnames(UBdata)[this_column]
  print(this_name)
  #print(paste(this_name)) #COMMENT
  
  # input data source counter - indicates if this is EPA data or field data, etc.
  input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
  input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short
  input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
  
  # input state information
  input_mat1[row_start:row_stop,c("State_Code")] <- 49
  input_mat1[row_start:row_stop,c("State_Name")] <- "Utah"
  input_mat1[row_start:row_stop,c("State_Abbrev")] <- "UT"
  
  
  # input station names into input_mat1
  input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- this_name
  
  # input PM2.5 concentration
  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- UBdata[,this_column]
  
  # input source file name
  input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
  print(UBdata[,"R_Dates"])
  #input_mat1[row_start:row_stop,c('RDates')] <- as.Date(UBdata[,c("Dates")],"%m/%d/%Y")#UBdata[,"R_Dates"]
  
  # input dates
  input_mat1[row_start:row_stop,c('RDates')] <- format(UBdata[,c("R_Dates")], "%Y-%m-%d")

  # input lat and lon
  if(this_name=="Roosevelt..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[1,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[1,c('long')]
  } else if(this_name=="Vernal..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[2,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[2,c('long')]
  } else if(this_name=="Ouray..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[3,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[3,c('long')]
  } else if(this_name=="Red.Wash..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[4,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[4,c('long')]
  } else if(this_name=="Myton..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[5,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[5,c('long')]
  } else if(this_name=="RabbitMtn..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[6,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[6,c('long')]
  } else if(this_name=="Horsepool..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[7,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[7,c('long')]
  } else if(this_name=="Ft..Duchesne"){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[8,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[8,c('long')]
  } else if(this_name=="Randlett"){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[9,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[9,c('long')]
  } else if(this_name=="Rangely"){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[10,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[10,c('long')]
  } else {
    stop(1, call. = TRUE, domain = NULL)
    geterrmessage("Loop should not have called this path in the if-statement")
  }
  
  # input other information
  input_mat1[row_start:row_stop,c('Winter')] <- UBdata[,"Winter."]
  input_mat1[row_start:row_stop,c('Year')] <- UBdata[,"year"]
  
  row_start <- row_stop+1
  row_stop <- row_start+dim(UBdata)[1]-1
}
rm(this_column,this_name,this_source_file)
rm(UBdata,UBLocations)

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
input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- PCAPSstationsChar
rm(PCAPSstations,PCAPSstationsChar)

# load file containing lat/lon info for PCAPS sites
PCAPSLocations<-read.csv(file.path(PCAPSData.directory,"PCAPS_Site_Locations.csv"),header=TRUE) 

# input PM2.5 concentration
this_column <- which(colnames(PCAPSdata)=="ug.m3")
  print(paste("Column number = ",this_column))
  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- PCAPSdata[,this_column]
  
  # input dates
   input_mat1[row_start:row_stop,c('RDates')] <- format(PCAPSdata[,c("R_Dates")], "%Y-%m-%d")
   input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file

   # input data source counter - indicates if this is EPA data or field data, etc.
   input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
   input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short
   input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
   
   # input state information
   input_mat1[row_start:row_stop,c("State_Code")] <- 49
   input_mat1[row_start:row_stop,c("State_Name")] <- "Utah"
   input_mat1[row_start:row_stop,c("State_Abbrev")] <- "UT"
   
   # input lat/lon information  
for(this_row in row_start:row_stop){     
   
  this_name <- input_mat1[this_row,c('PM25_Station_Name')]
  print(this_name)
 
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
   
   rm(new_col_number,this_column,this_name,this_source_file,this_row) 
  row_start <- row_stop+1
  #row_stop=row_start+dim(PCAPSdata)[1]-1
rm(PCAPSdata,PCAPSLocations)#,PCAPSstationsChar,PCAPSstations)


############################# Fill in data from Federal Land Managers - IMPROVE RHR III ######################
print('still need to pull in data from Federal Land Managers')
# data_source_counter <- data_source_counter+1
# Data_Source_Name_Short <- "FedLndMng"
# Data_Source_Name_Display <- "Federal Land Manager"
# # data_source_counter <- 2
# # row_start <- 1866409
# # row_stop <-  1866408
# # 
#      this_source_file <- "Federal_Land_Manager_Env_Database_Sites_Only.csv" #paste('daily_',as.character(this_ParamCode),'_',as.character(this_year),'.csv',sep="")
# #     #this_source_file <- "Federal_Land_Manager_Env_Database_201821321512474Iw0s1t.txt" 
# #     this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_III_2018215163723451I10uur.csv"
# #     print(this_source_file)
# #     
# #     # load FMLE data
# #     FMLEdata <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 230,sep = ",",blank.lines.skip = F)
# #    
# #     # load data information (top several lines of file)
# #     #FMLEdata.summary <- read.csv(file.path(FMLE.directory,this_source_file),header = F,nrows = 44)
# #     
# #     # load the listing of data sets
# #     #FMLEdata.datasets <- read.csv(file.path(FMLE.directory,this_source_file),header = T,skip = 44 ,nrows = 6)
# #     
# #     # load the listing of all sites
# #FMLEdata.sites <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 54,nrows = 3492)
# #FMLEdata <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 54,nrows = 3492)
#      FMLEdata <- read.csv(file.path(FMLE.directory,this_source_file), header = T)
#      #ThisAQSdata <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 54,nrows = 3492)
# #     
# #     # load the Parameters data
# #     #FMLEdata.parameters <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 3550,nrows = 9)
# #     
# #     # load flag information
# #     #FMLEdata.flags <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 3571, nrows = 17)
# #     
# #     ## isolate data in study states
# #     ##class(ThisAQSdata$State.Code)
# #ThisAQSdata_StudyStates <- ThisAQSdata[which(ThisAQSdata$State.Code==4|ThisAQSdata$State.Code==6|ThisAQSdata$State.Code==8|ThisAQSdata$State.Code==16|ThisAQSdata$State.Code==30|ThisAQSdata$State.Code==32|ThisAQSdata$State.Code==35|ThisAQSdata$State.Code==41|ThisAQSdata$State.Code==49|ThisAQSdata$State.Code==53|ThisAQSdata$State.Code==56), ]
# #     #rm(ThisAQSdata)
# #     #unique(ThisAQSdata_StudyStates$State.Name)
# #     
# #     FMLEAllStates <- FMLEdata[,c("State")]
# #     FMLEAllStatesChar <- as.character(FMLEAllStates)
# #     print("Pick up writing code here and finish listing all of the states")
# #     Which_FMLE <- FMLEAllStatesChar=="UT" | FMLEAllStatesChar=="AZ"
# #     
# #     FMLE_StudyStates <- FMLEdata[which(as.character(FMLEdata$State)=="UT")]
# #     
#      row_stop <- row_start+dim(FMLEdata)[1]-1
# #     
# #     # input data source counter - indicates if this is EPA data or field data, etc.
#      input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
#      input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short
#      input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
#      
#      
#      #     
# #     # input dates
# #     new_col_number <- length(FMLEdata)+1
# #     #FMLEdata[,new_col_number] <- as.Date(FMLEdata[,c("Date.Local")],"%Y-%m-%d") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
# #     #colnames(FMLEdata)[new_col_number] <- "R_Dates"
# #     #input_mat1[row_start:row_stop,c("RDates")] <- format(FMLEdata[,c("R_Dates")],"%Y-%m-%d")
# #     #rm(new_col_number)
# #     
# #    # # input station names into input_mat1
# #   #  AQSStations <- FMLEdata[,c("Local.Site.Name")]
# #   #  #print(AQSStations)
# #   #  AQSstationsChar <- as.character(AQSStations)
# #   #  #print(AQSstationsChar)
# #   #  input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- AQSstationsChar
# #   #  rm(AQSStations,AQSstationsChar)
# #     
# #     # input lat and lon
#      input_mat1[row_start:row_stop,c("PM2.5_Lat")] <- FMLEdata[,c('Latitude')]
#      input_mat1[row_start:row_stop,c("PM2.5_Lon")] <- FMLEdata[,c('Longitude')]
# #     
# #   #  # input PM2.5 concentration
# #   #  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- FMLEdata[,c("Arithmetic.Mean")]
# #     
# #   #  # input source file name
# input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
# #     
# #    # # input parameter code and method name
# #   #  input_mat1[row_start:row_stop,c("Parameter")] <- this_ParamCode
# #   #  input_mat1[row_start:row_stop,c("Method")] <- FMLEdata[,c("Method.Name")]
# #     
# #     # update row counter
#      row_start=row_stop+1
# #     
# #     # clear variables before moving on to next iteration of loop
# #     rm(this_source_file,FMLEdata)
# # 
# # 
# # rm(ParameterCode_vec,this_year,this_ParamCode)

###################### Fill in columns derived from other columns ########
print('pick up writing code here')
# Note: 'Winter' is filled in near the end of the script

# Note: 'Year' is filled in near the end of the script

# Note: 'Month' is filled in near the end of the script

# Note: 'Day' is filled in near the end of the script

###################### Save input_mat1 to csv file

#write.csv(mycars, file='mycars.csv') #or export DF as .CSV file
write.csv(input_mat1,file = file.path(ProcessedData.directory,'combined_ML_input.csv'))

sink()

################################################

# ############################# map locations #########################
# # Resources for mapping
# # http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# 
# # map boundaries of western US states
# USmap=readOGR(dsn=file.path(USMaps.directory),layer = "cb_2016_us_state_500k")
# #USmap=readOGR(dsn="/home/rstudio/Shapefiles_for_mapping/cp_2016_us_state_500k",layer = "cb_2016_us_state_500k")
# 
# # COMMENT following lines
# head(USmap@data,n=2)
# sapply(USmap@data,class)
# USmap$ALAND_NUM <- as.numeric(as.character(USmap$ALAND))
# mean(USmap$ALAND_NUM)
# nrow(USmap)
# ncol(USmap)
# #plot(USmap)
# 
# # have R recognize state FP's as numerical values (in a new column)
# USmap$STATEFP_NUM <- as.numeric(as.character(USmap$STATEFP))
# 
# # display the State FP values and state abbreviations next to each other
# USmap@data[,c("STATEFP_NUM","STUSPS")]
# 
# # find the 11 western states included in the study
# WestUSmap=USmap@data[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==38|USmap$STATEFP_NUM==46|USmap$STATEFP_NUM==31|USmap$STATEFP_NUM==20|USmap$STATEFP_NUM==40|USmap$STATEFP_NUM==48,]
# print(WestUSmap)
# 
# # start file for map
# FigFileName=file.path(output.directory,"MapPM25_All_Obs13.pdf")
# pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
# #plot.new()
# WestUSmapGeom=USmap[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==38|USmap$STATEFP_NUM==46|USmap$STATEFP_NUM==31|USmap$STATEFP_NUM==20|USmap$STATEFP_NUM==40|USmap$STATEFP_NUM==48,]
# 
# plot(WestUSmapGeom)
# 
# # cycle through each data source (EPA and various field campaigns) and plot each in a different color
# for(this_data_source_counter in 0:data_source_counter){     
#  print(this_data_source_counter) 
# 
#   # isolate data from this data source (in loop iteration) 
#   This_data <- input_mat1[which(input_mat1$Data_Source_Counter==this_data_source_counter), ]
#   
#   # find unique locations in data https://stats.stackexchange.com/questions/6759/removing-duplicated-rows-data-frame-in-r
#   repeated_locations=This_data[,c("PM2.5_Lat","PM2.5_Lon")]
#   duplicated(repeated_locations)
#   repeated_locations[duplicated(repeated_locations), ]
#   non_repeat_locations <- repeated_locations[!duplicated(repeated_locations), ]
#   #plot(non_repeat_locations[,2],non_repeat_locations[,1])
# 
#   if(this_data_source_counter==0){
#     points(non_repeat_locations[,2],non_repeat_locations[,1],col="black",cex=.3) # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
#   } 
#   else if(this_data_source_counter==1){
#     points(non_repeat_locations[,2],non_repeat_locations[,1],col="red",cex=0.6)
#   }
#   else if(this_data_source_counter==2){
#     
#     points(non_repeat_locations[,2],non_repeat_locations[,1],col="darkgoldenrod",cex=0.8)
#   }
#   else if(this_data_source_counter==3){
#     points(non_repeat_locations[,2],non_repeat_locations[,1],col="green",cex=0.6)
#   }
#   else if(this_data_source_counter==4){
#     points(non_repeat_locations[,2],non_repeat_locations[,1],col="blue",cex=0.6)
#   }
#   else {
#     stop(1, call. = TRUE, domain = NULL)
#     geterrmessage("Loop should not have called this path in the if-statement")
#   }
#   rm(This_data)
# } # for(this_data_source_counter in 0:data_source_counter){    
# 
# 
# 
# par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
# #summary(gbmtrainonly)
# title(main = "All PM2.5 Observation Locations")
# dev.off() # stop writing to pdf file
# remove(FigFileName) # delete pdf file name variable

## find unique locations in data https://stats.stackexchange.com/questions/6759/removing-duplicated-rows-data-frame-in-r
#repeated_locations=input_mat1[,c("PM2.5_Lat","PM2.5_Lon")]
#duplicated(repeated_locations)
#repeated_locations[duplicated(repeated_locations), ]
#non_repeat_locations <- repeated_locations[!duplicated(repeated_locations), ]
#plot(non_repeat_locations[,2],non_repeat_locations[,1])


#plot(non_repeat_locations[,2],non_repeat_locations[,1])

#sel <- USmap$STATEFP_NUM==6
#plot(USmap[sel,])



## some data is hourly data, averaged into 24-hour blocks and some is just one data point for 24 hours
## need to get rid of the hourly data that doesn't have at least min_hourly_obs_daily measurements
#all_sample_durations <- unique(ThisAQSdata_step[,c("Sample.Duration")])
#print(all_sample_durations[1])
##ThisAQSdata_24HR <- ThisAQSdata_step[which(ThisAQSdata_step$Sample.Duration=="24 HOUR"|ThisAQSdata_step$Sample.Duration=="24-HR BLK AVG"),]
#ThisAQSdata_24HR <- ThisAQSdata_step[which(ThisAQSdata_step$Sample.Duration=="24-HR BLK AVG"|ThisAQSdata_step$Sample.Duration=="24 HOUR"),]
#twenty4hr_sample_durations <- unique(ThisAQSdata_24HR[,c("Sample.Duration")])
#print(twenty4hr_sample_durations)
#ThisAQSdata_1HR <- ThisAQSdata_step[which(ThisAQSdata_step$Sample.Duration=="1 HOUR"),]
#rm(all_sample_durations,twenty4hr_sample_durations)

# # check that all rows are accounted for
# if (dim(ThisAQSdata_24HR)[1]+dim(ThisAQSdata_1HR)[1]!=dim(ThisAQSdata_step)[1]){stop('check code - not all rows of data accounted for')}
# rm(ThisAQSdata_step)
# 
# # check that there are enough observations in the 1-hr data
# ThisAQSdata_1HR_enough <- ThisAQSdata_1HR[which(ThisAQSdata_1HR$Observation.Count>=min_hourly_obs_daily),]
# print(cat(dim(ThisAQSdata_1HR)[1]-dim(ThisAQSdata_1HR_enough)[1]," data points removed because there were fewer than ",min_hourly_obs_daily," observations in the daily data in ",this_source_file,"."))
# 
# # recombine the 24-hr and 1-hr data
# ThisAQSdata <- rbind(ThisAQSdata_24HR,ThisAQSdata_1HR_enough)
# rm(ThisAQSdata_24HR,ThisAQSdata_1HR,ThisAQSdata_1HR_enough)

# input dates
#new_col_number <- length(ThisAQSdata_StudyStates)+1
#ThisAQSdata_StudyStates[,new_col_number] <- as.Date(ThisAQSdata_StudyStates[,c("Date.Local")],"%Y-%m-%d") # add column at end of data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
#colnames(ThisAQSdata_StudyStates)[new_col_number] <- "R_Dates"
#input_mat1[row_start:row_stop,c("RDates")] <- format(ThisAQSdata_StudyStates[,c("R_Dates")],"%Y-%m-%d")
#rm(new_col_number)

# # input station names into input_mat1
# AQSStations <- ThisAQSdata_StudyStates[,c("Local.Site.Name")]
# #print(AQSStations)
# AQSstationsChar <- as.character(AQSStations)
# #print(AQSstationsChar)
# input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- AQSstationsChar
# rm(AQSStations,AQSstationsChar)
# 
# # input state names into input_mat1
# AQSStates <- ThisAQSdata_StudyStates[,c("State.Name")]
# #print(AQSStates)
# AQSstatesChar <- as.character(AQSStates)
# #print(AQSstatesChar)
# input_mat1[row_start:row_stop,c("State_Name")] <- AQSstatesChar
# rm(AQSStates,AQSstatesChar)

#AZ_rows <- which(input_mat1$State_Code==4)
#input_mat1[AZ_rows,c("State_Abbrev")] <- "AZ"