# Create input file for Machine Learning estimation of PM2.5 for the western US, 2008-2014

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


# To clear all variables and start fresh:
# rm(list = ls())

# define directories
uppermost.directory="/home/rstudio" # on AWS
working.directory=uppermost.directory # on AWS
setwd(working.directory)
output.directory=file.path(working.directory,"Code_Outputs")
ProcessedData.directory=file.path(working.directory,"Processed_Data")
StartData.directory=file.path(working.directory,"PM25_Uintah_Basin")
USMaps.directory=file.path(working.directory,"Shapefiles_for_mapping","cp_2016_us_state_500k")
PCAPSData.directory=file.path(working.directory,"PM25_PCAPS_Salt_Lake")
AQSData.directory=file.path(working.directory,"AQS_Daily_Summaries")
FMLE.directory=file.path(working.directory,"Federal_Land_Manager_Environmental_Database")
FireCache.directory=file.path(working.directory,"Fire_Cache_Smoke_DRI")
start_study_year <- 2008
stop_study_year <- 2014
min_hourly_obs_daily <- 18 # minimum number of hourly observations required to compute a 24-hr average
# sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
SinkFileName=file.path(output.directory,"Create_ML_Input_File_sink.txt")
sink(file =SinkFileName, append = FALSE, type = c("output","message"),
     split = FALSE)

cat("Code and R output for Process_Lyman_data.R \n \n")
############################################################################
cat("Title: Create_ML_Input_File.R \n")
cat("Author: Melissa May Maestas \n")
cat("Original Date: January 23, 2018 \n")
cat("Latest Update: February 11, 2018 \n")
cat("This program reads in and process the Uintah Basin PM2.5 data provided by Seth Lyman. \n")
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
input_header= c('ID','Parameter','Method','Winter','RDates','Year','Month','Day','PM2.5_Obs','PM2.5_Lat',
                'PM2.5_Lon','PM25_Station_Name','Source_File',
                'Data_Source_Name_Display','Data_Source_Name_Short','Data_Source_Counter','Sample_Duration','Observation_Count','State_Name','State_Abbrev')
N_columns=length(input_header)
input_mat1=data.frame(matrix(NA,nrow=10,ncol=N_columns))
names(input_mat1)=input_header

############################## Pull in AQS data #################
data_source_counter=0
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
    ThisAQSdata<-read.csv(file.path(AQSData.directory,this_source_file),header=TRUE) 
    
    # some data is hourly data, averaged into 24-hour blocks and some is just one data point for 24 hours
    # need to get rid of the hourly data that doesn't have at least min_hourly_obs_daily measurements
    print('pick up writing code here')
    ThisAQSdata_sufficient_obs <- ThisAQSdata[which(ThisAQS),]
    
    # isolate data in study states
    #class(ThisAQSdata$State.Code)
    # only study area states: #ThisAQSdata_StudyStates <- ThisAQSdata[which(ThisAQSdata$State.Code==4|ThisAQSdata$State.Code==6|ThisAQSdata$State.Code==8|ThisAQSdata$State.Code==16|ThisAQSdata$State.Code==30|ThisAQSdata$State.Code==32|ThisAQSdata$State.Code==35|ThisAQSdata$State.Code==41|ThisAQSdata$State.Code==49|ThisAQSdata$State.Code==53|ThisAQSdata$State.Code==56), ]
    ThisAQSdata_StudyStates <- ThisAQSdata[which(ThisAQSdata$State.Code==4|ThisAQSdata$State.Code==6|ThisAQSdata$State.Code==8|ThisAQSdata$State.Code==16|ThisAQSdata$State.Code==30|ThisAQSdata$State.Code==32|ThisAQSdata$State.Code==35|ThisAQSdata$State.Code==41|ThisAQSdata$State.Code==49|ThisAQSdata$State.Code==53|ThisAQSdata$State.Code==56|ThisAQSdata$State.Code==38|ThisAQSdata$State.Code==46|ThisAQSdata$State.Code==31|ThisAQSdata$State.Code==20|ThisAQSdata$State.Code==40|ThisAQSdata$State.Code==48), ]
    
    rm(ThisAQSdata)
    #unique(ThisAQSdata_StudyStates$State.Name)
    row_stop <- row_start+dim(ThisAQSdata_StudyStates)[1]-1
    
    # input data source counter - indicates if this is EPA data or field data, etc.
    input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
    input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short
    input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
    
    # check that all the data represent 24-hr measurements
    all_Sample_duration=unique(ThisAQSdata_StudyStates[,c("Sample.Duration")])
    print(all_Sample_duration)
    if (){}
    
    # input dates
    new_col_number <- length(ThisAQSdata_StudyStates)+1
    ThisAQSdata_StudyStates[,new_col_number] <- as.Date(ThisAQSdata_StudyStates[,c("Date.Local")],"%Y-%m-%d") # add column at end of data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
    colnames(ThisAQSdata_StudyStates)[new_col_number] <- "R_Dates"
    input_mat1[row_start:row_stop,c("RDates")] <- format(ThisAQSdata_StudyStates[,c("R_Dates")],"%Y-%m-%d")
    rm(new_col_number)
    
    # input station names into input_mat1
    AQSStations <- ThisAQSdata_StudyStates[,c("Local.Site.Name")]
    #print(AQSStations)
    AQSstationsChar <- as.character(AQSStations)
    #print(AQSstationsChar)
    input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- AQSstationsChar
    rm(AQSStations,AQSstationsChar)
    
    # input lat and lon
    input_mat1[row_start:row_stop,c("PM2.5_Lat")] <- ThisAQSdata_StudyStates[,c('Latitude')]
    input_mat1[row_start:row_stop,c("PM2.5_Lon")] <- ThisAQSdata_StudyStates[,c('Longitude')]
    
    # input state name
    input_mat1[row_start:row_stop,c("State_Name")] <- ThisAQSdata_StudyStates[,c('State.Name')]
    
    # input PM2.5 concentration
    input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- ThisAQSdata_StudyStates[,c("Arithmetic.Mean")]

    # input source file name
    input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
    
    # input parameter code and method name
    input_mat1[row_start:row_stop,c("Parameter")] <- this_ParamCode
    input_mat1[row_start:row_stop,c("Method")] <- ThisAQSdata_StudyStates[,c("Method.Name")]
    
    # update row counter
    row_start=row_stop+1
    
    # clear variables before moving on to next iteration of loop
    rm(this_source_file,ThisAQSdata_StudyStates)
  } # for(this_POC in POC_vec){ # cycle through POC
} # for(this_year in start_study_year:stop_study_year){     # cycle through years

rm(ParameterCode_vec,this_year,this_ParamCode)

############################# Fill in data from Federal Land Managers - IMPROVE RHR III ######################
data_source_counter <- data_source_counter+1
Data_Source_Name_Short <- "FedLndMng"
Data_Source_Name_Display <- "Federal Land Manager"
# data_source_counter <- 2
# row_start <- 1866409
# row_stop <-  1866408
# 
     this_source_file <- "Federal_Land_Manager_Env_Database_Sites_Only.csv" #paste('daily_',as.character(this_ParamCode),'_',as.character(this_year),'.csv',sep="")
#     #this_source_file <- "Federal_Land_Manager_Env_Database_201821321512474Iw0s1t.txt" 
#     this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_III_2018215163723451I10uur.csv"
#     print(this_source_file)
#     
#     # load FMLE data
#     FMLEdata <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 230,sep = ",",blank.lines.skip = F)
#    
#     # load data information (top several lines of file)
#     #FMLEdata.summary <- read.csv(file.path(FMLE.directory,this_source_file),header = F,nrows = 44)
#     
#     # load the listing of data sets
#     #FMLEdata.datasets <- read.csv(file.path(FMLE.directory,this_source_file),header = T,skip = 44 ,nrows = 6)
#     
#     # load the listing of all sites
#FMLEdata.sites <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 54,nrows = 3492)
#FMLEdata <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 54,nrows = 3492)
     FMLEdata <- read.csv(file.path(FMLE.directory,this_source_file), header = T)
     #ThisAQSdata <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 54,nrows = 3492)
#     
#     # load the Parameters data
#     #FMLEdata.parameters <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 3550,nrows = 9)
#     
#     # load flag information
#     #FMLEdata.flags <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 3571, nrows = 17)
#     
#     ## isolate data in study states
#     ##class(ThisAQSdata$State.Code)
#ThisAQSdata_StudyStates <- ThisAQSdata[which(ThisAQSdata$State.Code==4|ThisAQSdata$State.Code==6|ThisAQSdata$State.Code==8|ThisAQSdata$State.Code==16|ThisAQSdata$State.Code==30|ThisAQSdata$State.Code==32|ThisAQSdata$State.Code==35|ThisAQSdata$State.Code==41|ThisAQSdata$State.Code==49|ThisAQSdata$State.Code==53|ThisAQSdata$State.Code==56), ]
#     #rm(ThisAQSdata)
#     #unique(ThisAQSdata_StudyStates$State.Name)
#     
#     FMLEAllStates <- FMLEdata[,c("State")]
#     FMLEAllStatesChar <- as.character(FMLEAllStates)
#     print("Pick up writing code here and finish listing all of the states")
#     Which_FMLE <- FMLEAllStatesChar=="UT" | FMLEAllStatesChar=="AZ"
#     
#     FMLE_StudyStates <- FMLEdata[which(as.character(FMLEdata$State)=="UT")]
#     
     row_stop <- row_start+dim(FMLEdata)[1]-1
#     
#     # input data source counter - indicates if this is EPA data or field data, etc.
     input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
     input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short
     input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
     
     
     #     
#     # input dates
#     new_col_number <- length(FMLEdata)+1
#     #FMLEdata[,new_col_number] <- as.Date(FMLEdata[,c("Date.Local")],"%Y-%m-%d") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
#     #colnames(FMLEdata)[new_col_number] <- "R_Dates"
#     #input_mat1[row_start:row_stop,c("RDates")] <- format(FMLEdata[,c("R_Dates")],"%Y-%m-%d")
#     #rm(new_col_number)
#     
#    # # input station names into input_mat1
#   #  AQSStations <- FMLEdata[,c("Local.Site.Name")]
#   #  #print(AQSStations)
#   #  AQSstationsChar <- as.character(AQSStations)
#   #  #print(AQSstationsChar)
#   #  input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- AQSstationsChar
#   #  rm(AQSStations,AQSstationsChar)
#     
#     # input lat and lon
     input_mat1[row_start:row_stop,c("PM2.5_Lat")] <- FMLEdata[,c('Latitude')]
     input_mat1[row_start:row_stop,c("PM2.5_Lon")] <- FMLEdata[,c('Longitude')]
#     
#   #  # input PM2.5 concentration
#   #  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- FMLEdata[,c("Arithmetic.Mean")]
#     
#   #  # input source file name
input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
#     
#    # # input parameter code and method name
#   #  input_mat1[row_start:row_stop,c("Parameter")] <- this_ParamCode
#   #  input_mat1[row_start:row_stop,c("Method")] <- FMLEdata[,c("Method.Name")]
#     
#     # update row counter
     row_start=row_stop+1
#     
#     # clear variables before moving on to next iteration of loop
#     rm(this_source_file,FMLEdata)
# 
# 
# rm(ParameterCode_vec,this_year,this_ParamCode)

############################# Pull in Fire Cache Smoke (DRI) data #################
     # https://stat.ethz.ch/R-manual/R-devel/library/base/html/list.files.html
     # increase dummy counter by 1 (used for differentiating data sources by color in map)
     
     data_source_counter <- data_source_counter+1
     Data_Source_Name_Short <- "FireCacheDRI"
     Data_Source_Name_Display <- "Fire Cache Smoke Monitor (DRI)"
     
     # these lines for running code skipping AQS data above
     #data_source_counter <- 3
     #row_start <- 1
     #row_stop <- 0
     
     # what files are in the FireCache.directory?
     all_DRI_Files <- list.files(path = file.path(FireCache.directory,"."), pattern = NULL, all.files = FALSE,
                                 full.names = FALSE, recursive = FALSE,
                                 ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
     print(all_DRI_Files)
     # row_start <- 1864583
     # row_stop <-  1864582
     for (this_file_counter in 1:length(all_DRI_Files)){
       print('this_file_counter')
       print(this_file_counter)
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
       #flag_counter <- 0
       for(this_col in 1:dim(three_header_rows)[2]){
         #this_col_header <- paste(three_header_rows[1,this_col],three_header_rows[2,this_col],three_header_rows[3,this_col],sep = " ",strip.white = 1)
         
         part1 <- as.character(three_header_rows[1,this_col])
         print(part1)
         part2 <- as.character(three_header_rows[2,this_col])
         print(part2)
         part3 <- as.character(three_header_rows[3,this_col])
         print(part3)
         if (part3==' flg'){
           #flag_counter <- flag_counter+1
           #part3 <- paste(' flg.',flag_counter,sep = "")
           part3 <- paste(' flg.',one_row_header[this_col-1],sep = "")
         }
         #this_col_header <- paste(three_header_rows[1,this_col],three_header_rows[2,this_col],three_header_rows[3,this_col])
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
         comprehensive.header <- colnames(this_Fire_Cache_data_step)
       } else if (this_file_counter>1){
         print(paste('this_file_counter is ',this_file_counter))
         this_file_header <- colnames(this_Fire_Cache_data_step)
         print(this_file_header)
         for (this_col in 1:length(this_file_header)) {
           print(paste('this_col = ',this_col))
           this_col_header <- this_file_header[this_col]
           print(this_col_header)
           which_col <- which(comprehensive.header==this_col_header)
           print(paste('this_col (',this_col,') matches column ',which_col,' in comprehensive.header')) 
           if (length(which_col)!=1){
             print('adding new column header that was not in previous files:')
             this_col_header
             new_col_number <- length(comprehensive.header)+1
             comprehensive.header[new_col_number] <- this_col_header
             rm(new_col_number)
             #stop('headers do not match. write code to accomodate')
             
           } # if (length(which_col)!=1)
           rm(which_col)
         } # for (this_col in 1:length(this_file_header)) {
       } # else if (this_file_counter>1){
       
       
       # The header is (sometimes/always?) repeated further down in the data. These rows need to be found and removed.
       row_restart_header <- which(this_Fire_Cache_data_step[,1]==":          ")
       
       if (length(row_restart_header)==0){this_Fire_Cache_data <- this_Fire_Cache_data_step
       } else {
         for (header_repeat_counter in 1:length(row_restart_header)) {
           if (header_repeat_counter==length(row_restart_header)) {
             
             part1 <- this_Fire_Cache_data_step[1:row_restart_header-1,]
             part2_rowstart <- row_restart_header+3
             part2_rowstop <- as.numeric(dim(this_Fire_Cache_data_step)[1])
             part2 <- this_Fire_Cache_data_step[part2_rowstart:part2_rowstop,]
             
             this_Fire_Cache_data <- rbind(part1,part2)
             rm(part1,part2,part2_rowstart,part2_rowstop)
           } else {
             stop('expand code')
           } # else
         } # for
         rm(header_repeat_counter)
       }
       rm(this_Fire_Cache_data_step,row_restart_header)
       
       #  # figure out what row we're on in input_mat
       #  row_stop <- row_start+dim(this_Fire_Cache_data)[1]-1
       
       # handle date information
       new_col_number <- length(this_Fire_Cache_data)+1 # figure out how many columns are in UBdata and then add 1
       this_Fire_Cache_data[,new_col_number] <- as.Date(this_Fire_Cache_data[,1],"%m/%d/%Y") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
       colnames(this_Fire_Cache_data)[new_col_number] <- "R_Dates"
       rm(new_col_number)
       
       #### take 24-hr averages
       # on what days does this monitor have data?
       these_dates <- unique(this_Fire_Cache_data[,c("R_Dates")])
       print(these_dates)
       # create data frame that will have one observation per day
       #N_columns_Fire_Cache=length(colnames(this_Fire_Cache_data)) # number of columns
       N_columns_Fire_Cache=length(comprehensive.header) # number of columns
       Daily_Fire_Cache=data.frame(matrix(NA,nrow=length(these_dates),ncol=N_columns_Fire_Cache)) # create empty data frame
       names(Daily_Fire_Cache)=comprehensive.header # give new data frame a header
       rm(N_columns_Fire_Cache)
       print('still need to deal with some files having hour 20:00 data shifted a couple of columns')
       for (date_counter in 1:length(these_dates)) {
         this_date <- these_dates[date_counter]
         print(this_date)
         
         # isolate the data for this date
         find_this_data_rows_step <- which(this_Fire_Cache_data[,c("R_Dates")]==this_date)
         date_all_Fire_Cache_data_step <- this_Fire_Cache_data[find_this_data_rows_step,]
         rm(find_this_data_rows_step)
         # rule out readings with negative PM2.5 concentrations
         date_this_conc_data <-as.numeric(as.character(date_all_Fire_Cache_data_step[,c("ug/m3 Conc     RT    ")]))
         find_this_data_rows_step2 <- which(date_this_conc_data>=0)
         date_all_Fire_Cache_data_step2 <- date_all_Fire_Cache_data_step[find_this_data_rows_step2,]
         rm(date_this_conc_data,find_this_data_rows_step2,date_all_Fire_Cache_data_step)
         # rule out readings with missing longitude data
         date_this_lon_data_step3 <-as.numeric(as.character(date_all_Fire_Cache_data_step2[,c(" Deg    GPS     Lon. ")]))
         find_this_data_rows_step3 <- which(date_this_lon_data_step3>=-180)
         date_all_Fire_Cache_data_step3 <- date_all_Fire_Cache_data_step2[find_this_data_rows_step3,]
         rm(date_this_lon_data_step3,find_this_data_rows_step3,date_all_Fire_Cache_data_step2)
         # rule out readings with negative battery voltage
         date_this_batt_volt <-as.numeric(as.character(date_all_Fire_Cache_data_step3[,c("volts Battery Voltage")]))
         find_this_data_rows <- which(date_this_batt_volt>=0)
         date_all_Fire_Cache_data <- date_all_Fire_Cache_data_step3[find_this_data_rows,]
         rm(date_this_batt_volt,date_all_Fire_Cache_data_step3)
         #rm(date_this_conc_data,find_this_data_rows_step,date_all_Fire_Cache_data_step)
         
         # check if there are more than 24 observations on a given day ... not expected
         if (length(find_this_data_rows)>24){
           print('There appear to be more than 24 observations for this monitor')
           print(this_date)
           print(this_source_file)
           error()
         }
         # check if there are at least min_hourly_obs_daily hourly observations, otherwise a daily value won't be computed
         if (length(find_this_data_rows)>=min_hourly_obs_daily){
           
           Daily_Fire_Cache[date_counter,c(" Deg    GPS     Lat. ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lat. ")])))
           Daily_Fire_Cache[date_counter,c("           flg. Deg    GPS     Lat. ")] <- unique(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Deg    GPS     Lat. ")])))
           
           if (mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))>0){
             Daily_Fire_Cache[date_counter,c(" Deg    GPS     Lon. ")] <- (-1)*mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))
             print('longitude value was positive, so it was multiplied by -1 to make it negative')
           } else {
             Daily_Fire_Cache[date_counter,c(" Deg    GPS     Lon. ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Deg    GPS     Lon. ")])))
           }
           
           Daily_Fire_Cache[date_counter,c("           flg. Deg    GPS     Lon. ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Deg    GPS     Lon. ")])))
           Daily_Fire_Cache[date_counter,c("      Type           ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("      Type           ")])))
           Daily_Fire_Cache[date_counter,c("           flg.      Type           ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.      Type           ")])))
           
           # input monitor serial # and corresponding flag
           Daily_Fire_Cache[date_counter,c("ser # Serial  Number ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("ser # Serial  Number ")])))
           # flag for serial # is sometimes non-numeric, so an average cannot be taken
           all_serial_flags <- unique(date_all_Fire_Cache_data[,c("           flg.ser # Serial  Number ")]) # what are all the flags on this day?
           print(all_serial_flags)
           if (length(all_serial_flags)==1){ # there is only 1 flag, so it can be put in directly
             Daily_Fire_Cache[date_counter,c("           flg.ser # Serial  Number ")] <- unique(as.character(date_all_Fire_Cache_data[,c("           flg.ser # Serial  Number ")]))
           } else {# there are multiple flags and they need to be stitched together
             combine_flags <- all_serial_flags[1] # get the first flag
             print(combine_flags)
             for (flag_counter in 2:length(all_serial_flags)) { # loop through the other flags and stitch them together
               combine_flags <- paste(combine_flags,all_serial_flags[flag_counter],sep = " ")
               print(combine_flags)
             }
             Daily_Fire_Cache[date_counter,c("           flg.ser # Serial  Number ")] <- combine_flags # input the flags
             rm(flag_counter,combine_flags) # clear variables
           }
           
           # input concentration in corresponding flag
           Daily_Fire_Cache[date_counter,c("ug/m3 Conc     RT    ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("ug/m3 Conc     RT    ")])))
           Daily_Fire_Cache[date_counter,c("           flg.ug/m3 Conc     RT    ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.ug/m3 Conc     RT    ")])))
           
           # Misc # 1 column does not exist in all of these files, so only fill it in when it exists:
           if(" Unk   Misc     #1   " %in% colnames(date_all_Fire_Cache_data))
           {
             Daily_Fire_Cache[date_counter,c(" Unk   Misc     #1   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Unk   Misc     #1   ")])))
             # flag for Unk Misc #1 is sometimes non-numeric, so an average cannot be taken
             all_flags <- unique(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #1   ")]) # what are all the flags on this day?
             print(all_flags)
             if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
               # Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #1   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #1   ")])))
               Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #1   ")] <- unique(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #1   ")])
               
             } else {# there are multiple flags and they need to be stitched together
               combine_flags <- all_flags[1] # get the first flag
               print(combine_flags)
               for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
                 combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
                 print(combine_flags)
               } # for
               Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #1   ")] <- combine_flags # input the flags
               rm(flag_counter,combine_flags) # clear variables
             } # else
             
             #Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #1   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #1   ")])))
             cat("Yep, it's in there!\n");
           } else {print('Nope, column is not here')}
           
           
           Daily_Fire_Cache[date_counter,c(" l/m   Ave.   Air Flw")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" l/m   Ave.   Air Flw")])))
           Daily_Fire_Cache[date_counter,c("           flg. l/m   Ave.   Air Flw")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. l/m   Ave.   Air Flw")])))
           Daily_Fire_Cache[date_counter,c("Deg C  Av Air   Temp ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("Deg C  Av Air   Temp ")])))
           Daily_Fire_Cache[date_counter,c("           flg.Deg C  Av Air   Temp ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.Deg C  Av Air   Temp ")])))
           
           
           # input % Rel Humidity and corresponding flag
           #Daily_Fire_Cache[date_counter,c("  %     Rel   Humidty")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("  %     Rel   Humidty")])))
           #Daily_Fire_Cache[date_counter,c("           flg.  %     Rel   Humidty")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.  %     Rel   Humidty")])))
           # % Rel Humidity column may not exist in all of these files, so only fill it in when it exists:
           if("  %     Rel   Humidty" %in% colnames(date_all_Fire_Cache_data)) {
             Daily_Fire_Cache[date_counter,c("  %     Rel   Humidty")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("  %     Rel   Humidty")])))
             # flag for Wind Direc is sometimes non-numeric, so an average cannot be taken
             all_flags <- unique(date_all_Fire_Cache_data[,c("           flg.  %     Rel   Humidty")]) # what are all the flags on this day?
             print(all_flags)
             if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
               Daily_Fire_Cache[date_counter,c("           flg.  %     Rel   Humidty")] <- unique(date_all_Fire_Cache_data[,c("           flg.  %     Rel   Humidty")])
             } else {# there are multiple flags and they need to be stitched together
               combine_flags <- all_flags[1] # get the first flag
               print(combine_flags)
               for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
                 combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
                 print(combine_flags)
               } # for
               Daily_Fire_Cache[date_counter,c("           flg.  %     Rel   Humidty")] <- combine_flags # input the flags
               rm(flag_counter,combine_flags) # clear variables
             } # else
             cat("Yep, it's in there!\n");
           } else {print('Nope, column is not here')}      
           
           
           
           
           
           # # Misc # 2 column does not exist in all of these files, so only fill it in when it exists:
           #if(" Unk   Misc     #2   " %in% colnames(date_all_Fire_Cache_data))
           #{
           #  Daily_Fire_Cache[date_counter,c(" Unk   Misc     #2   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Unk   Misc     #2   ")])))
           #  Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #2   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #2   ")])))
           #  cat("Yep, it's in there!\n");
           #} else {print('Nope, column is not here')}
           
           
           # Misc # 2 column does not exist in all of these files, so only fill it in when it exists:
           if(" Unk   Misc     #2   " %in% colnames(date_all_Fire_Cache_data))
           {
             Daily_Fire_Cache[date_counter,c(" Unk   Misc     #2   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" Unk   Misc     #2   ")])))
             # flag for Unk Misc #2 is sometimes non-numeric, so an average cannot be taken
             all_flags <- unique(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #2   ")]) # what are all the flags on this day?
             print(all_flags)
             if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
               #Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #2   ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #2   ")])))
               Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #2   ")] <- unique(date_all_Fire_Cache_data[,c("           flg. Unk   Misc     #2   ")])
               
             } else {# there are multiple flags and they need to be stitched together
               combine_flags <- all_flags[1] # get the first flag
               print(combine_flags)
               for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
                 combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
                 print(combine_flags)
               } # for
               Daily_Fire_Cache[date_counter,c("           flg. Unk   Misc     #2   ")] <- combine_flags # input the flags
               rm(flag_counter,combine_flags) # clear variables
             } # else
             cat("Yep, it's in there!\n");
           } else {print('Nope, column is not here')}
           
           # install deg C Sensor Int AT and corresponding flag
           Daily_Fire_Cache[date_counter,c("deg C Sensor  Int AT ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("deg C Sensor  Int AT ")])))
           #Daily_Fire_Cache[date_counter,c("           flg.deg C Sensor  Int AT ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.deg C Sensor  Int AT ")])))
           # flag for deg C Sensor  Int AT is sometimes non-numeric, so an average cannot be taken
           all_flags <- unique(date_all_Fire_Cache_data[,c("           flg.deg C Sensor  Int AT ")]) # what are all the flags on this day?
           print(all_flags)
           if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
             Daily_Fire_Cache[date_counter,c("           flg.deg C Sensor  Int AT ")] <- unique(date_all_Fire_Cache_data[,c("           flg.deg C Sensor  Int AT ")])
           } else {# there are multiple flags and they need to be stitched together
             combine_flags <- all_flags[1] # get the first flag
             print(combine_flags)
             for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
               combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
               print(combine_flags)
             } # for
             Daily_Fire_Cache[date_counter,c("           flg.deg C Sensor  Int AT ")] <- combine_flags # input the flags
             rm(flag_counter,combine_flags) # clear variables
           } # else
           
           # input %   Sensor  Int RH and corresponding flag
           #Daily_Fire_Cache[date_counter,c("  %   Sensor  Int RH ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("  %   Sensor  Int RH ")])))
           #Daily_Fire_Cache[date_counter,c("           flg.  %   Sensor  Int RH ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.  %   Sensor  Int RH ")])))
           # %   Sensor  Int RH column may not exist in all of these files, so only fill it in when it exists:
           if("  %   Sensor  Int RH " %in% colnames(date_all_Fire_Cache_data)) {
             Daily_Fire_Cache[date_counter,c("  %   Sensor  Int RH ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("  %   Sensor  Int RH ")])))
             # flag for Wind Direc is sometimes non-numeric, so an average cannot be taken
             all_flags <- unique(date_all_Fire_Cache_data[,c("           flg.  %   Sensor  Int RH ")]) # what are all the flags on this day?
             print(all_flags)
             if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
               Daily_Fire_Cache[date_counter,c("           flg.  %   Sensor  Int RH ")] <- unique(date_all_Fire_Cache_data[,c("           flg.  %   Sensor  Int RH ")])
             } else {# there are multiple flags and they need to be stitched together
               combine_flags <- all_flags[1] # get the first flag
               print(combine_flags)
               for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
                 combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
                 print(combine_flags)
               } # for
               Daily_Fire_Cache[date_counter,c("           flg.  %   Sensor  Int RH ")] <- combine_flags # input the flags
               rm(flag_counter,combine_flags) # clear variables
             } # else
             cat("Yep, it's in there!\n");
           } else {print('Nope, column is not here')} 
           
           
           
           
           Daily_Fire_Cache[date_counter,c(" m/s    Wind    Speed")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c(" m/s    Wind    Speed")])))
           Daily_Fire_Cache[date_counter,c("           flg. m/s    Wind    Speed")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. m/s    Wind    Speed")])))
           
           # input Wind direction and corresponding flag
           #Daily_Fire_Cache[date_counter,c(" Deg   Wind    Direc ")] <- NA
           #Daily_Fire_Cache[date_counter,c("           flg. Deg   Wind    Direc ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Deg   Wind    Direc ")])))
           # Wind direction column may not exist in all of these files, so only fill it in when it exists:
           if(" Deg   Wind    Direc " %in% colnames(date_all_Fire_Cache_data)) {
             Daily_Fire_Cache[date_counter,c(" Deg   Wind    Direc ")] <- NA # average wind direction calculation is slightly more complicated than a direct average, so not doing that for now
             # flag for Wind Direc is sometimes non-numeric, so an average cannot be taken
             all_flags <- unique(date_all_Fire_Cache_data[,c("           flg. Deg   Wind    Direc ")]) # what are all the flags on this day?
             print(all_flags)
             if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
               Daily_Fire_Cache[date_counter,c("           flg. Deg   Wind    Direc ")] <- unique(date_all_Fire_Cache_data[,c("           flg. Deg   Wind    Direc ")])
             } else {# there are multiple flags and they need to be stitched together
               combine_flags <- all_flags[1] # get the first flag
               print(combine_flags)
               for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
                 combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
                 print(combine_flags)
               } # for
               Daily_Fire_Cache[date_counter,c("           flg. Deg   Wind    Direc ")] <- combine_flags # input the flags
               rm(flag_counter,combine_flags) # clear variables
             } # else
             #Daily_Fire_Cache[date_counter,c("           flg. Deg   Wind    Direc ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg. Deg   Wind    Direc ")])))
             cat("Yep, it's in there!\n");
           } else {print('Nope, column is not here')}
           
           Daily_Fire_Cache[date_counter,c("volts Battery Voltage")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("volts Battery Voltage")])))
           Daily_Fire_Cache[date_counter,c("           flg.volts Battery Voltage")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.volts Battery Voltage")])))
           
           # input Alarm variable and corresponding flag
           #Daily_Fire_Cache[date_counter,c("      Alarm          ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("      Alarm          ")])))
           #Daily_Fire_Cache[date_counter,c("           flg.      Alarm          ")] <- mean(as.numeric(as.character(date_all_Fire_Cache_data[,c("           flg.      Alarm          ")])))
           # Alarm column may not exist in all of these files, so only fill it in when it exists:
           if("      Alarm          " %in% colnames(date_all_Fire_Cache_data)) {
             Daily_Fire_Cache[date_counter,c("      Alarm          ")] <- NA # average wind direction calculation is slightly more complicated than a direct average, so not doing that for now
             # flag for Wind Direc is sometimes non-numeric, so an average cannot be taken
             all_flags <- unique(date_all_Fire_Cache_data[,c("           flg.      Alarm          ")]) # what are all the flags on this day?
             print(all_flags)
             if (length(all_flags)==1){ # there is only 1 flag, so it can be put in directly
               Daily_Fire_Cache[date_counter,c("           flg.      Alarm          ")] <- unique(date_all_Fire_Cache_data[,c("           flg.      Alarm          ")])
             } else {# there are multiple flags and they need to be stitched together
               combine_flags <- all_flags[1] # get the first flag
               print(combine_flags)
               for (flag_counter in 2:length(all_flags)) { # loop through the other flags and stitch them together
                 combine_flags <- paste(combine_flags,all_flags[flag_counter],sep = " ")
                 print(combine_flags)
               } # for
               Daily_Fire_Cache[date_counter,c("           flg.      Alarm          ")] <- combine_flags # input the flags
               rm(flag_counter,combine_flags) # clear variables
             } # else
             cat("Yep, it's in there!\n");
           } else {print('Nope, column is not here')}      
           
           
           
           
           Daily_Fire_Cache[date_counter,c("R_Dates")] <- format(unique(date_all_Fire_Cache_data[,c("R_Dates")]), "%Y-%m-%d")
           
           print('think about how best to handle flags, wind direction etc.')
           
         } # if   
         
         rm(this_date,date_all_Fire_Cache_data)
       } # for
       rm(these_dates,date_counter) # clear variables
       
       # remove the rows from Daily_Fire_Cache that don't have any data
       rows_have_data <- complete.cases(Daily_Fire_Cache[,c("R_Dates")])
       Daily_Fire_Cache_no_miss <- Daily_Fire_Cache[rows_have_data,]
       rm(Daily_Fire_Cache,rows_have_data)
       
       # figure out what row we're on in input_mat
       row_stop <- row_start+dim(Daily_Fire_Cache_no_miss)[1]-1
       
       # input dates
       input_mat1[row_start:row_stop,c("RDates")] <- Daily_Fire_Cache_no_miss[,c("R_Dates")]
       
       # input PM2.5 concentration
       which_colConc <- which(colnames(Daily_Fire_Cache_no_miss)=="ug/m3 Conc     RT    ")
       concentration_vector <- Daily_Fire_Cache_no_miss[,which_colConc]
       input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- as.numeric(as.character(Daily_Fire_Cache_no_miss[,which_colConc]))
       rm(which_colConc)
       
       # input data source counter - indicates if this is EPA data or field data, etc.
       input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
       input_mat1[row_start:row_stop,c("Data_Source_Name_Short")] <- Data_Source_Name_Short
       input_mat1[row_start:row_stop,c("Data_Source_Name_Display")] <- Data_Source_Name_Display
       
       
       # input station names into input_mat1
       input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- this_name
       
       # input source file name
       input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
       
       # input lat and lon
       which_colLat <- which(colnames(Daily_Fire_Cache_no_miss)==" Deg    GPS     Lat. ")
       input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- as.numeric(as.character(Daily_Fire_Cache_no_miss[,which_colLat]))
       rm(which_colLat)
       which_colLon <- which(colnames(Daily_Fire_Cache_no_miss)==" Deg    GPS     Lon. ")
       input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- as.numeric(as.character(Daily_Fire_Cache_no_miss[,which_colLon]))
       rm(which_colLon)
       
       # tick up the row counter
       row_start <- row_stop+1
       print(this_source_file)
       rm(this_source_file,this_Fire_Cache_data)
       
     }
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

############################# map locations #########################
# Resources for mapping
# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

# map boundaries of western US states
USmap=readOGR(dsn=file.path(USMaps.directory),layer = "cb_2016_us_state_500k")
#USmap=readOGR(dsn="/home/rstudio/Shapefiles_for_mapping/cp_2016_us_state_500k",layer = "cb_2016_us_state_500k")

# COMMENT following lines
head(USmap@data,n=2)
sapply(USmap@data,class)
USmap$ALAND_NUM <- as.numeric(as.character(USmap$ALAND))
mean(USmap$ALAND_NUM)
nrow(USmap)
ncol(USmap)
#plot(USmap)

# have R recognize state FP's as numerical values (in a new column)
USmap$STATEFP_NUM <- as.numeric(as.character(USmap$STATEFP))

# display the State FP values and state abbreviations next to each other
USmap@data[,c("STATEFP_NUM","STUSPS")]

# find the 11 western states included in the study
WestUSmap=USmap@data[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==38|USmap$STATEFP_NUM==46|USmap$STATEFP_NUM==31|USmap$STATEFP_NUM==20|USmap$STATEFP_NUM==40|USmap$STATEFP_NUM==48,]
print(WestUSmap)

# start file for map
FigFileName=file.path(output.directory,"MapPM25_All_Obs13.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
#plot.new()
WestUSmapGeom=USmap[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==38|USmap$STATEFP_NUM==46|USmap$STATEFP_NUM==31|USmap$STATEFP_NUM==20|USmap$STATEFP_NUM==40|USmap$STATEFP_NUM==48,]

plot(WestUSmapGeom)

# cycle through each data source (EPA and various field campaigns) and plot each in a different color
for(this_data_source_counter in 0:data_source_counter){     
 print(this_data_source_counter) 

  # isolate data from this data source (in loop iteration) 
  This_data <- input_mat1[which(input_mat1$Data_Source_Counter==this_data_source_counter), ]
  
  # find unique locations in data https://stats.stackexchange.com/questions/6759/removing-duplicated-rows-data-frame-in-r
  repeated_locations=This_data[,c("PM2.5_Lat","PM2.5_Lon")]
  duplicated(repeated_locations)
  repeated_locations[duplicated(repeated_locations), ]
  non_repeat_locations <- repeated_locations[!duplicated(repeated_locations), ]
  #plot(non_repeat_locations[,2],non_repeat_locations[,1])

  if(this_data_source_counter==0){
    points(non_repeat_locations[,2],non_repeat_locations[,1],col="black",cex=.3) # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
  } 
  else if(this_data_source_counter==1){
    points(non_repeat_locations[,2],non_repeat_locations[,1],col="red",cex=0.6)
  }
  else if(this_data_source_counter==2){
    
    points(non_repeat_locations[,2],non_repeat_locations[,1],col="darkgoldenrod",cex=0.8)
  }
  else if(this_data_source_counter==3){
    points(non_repeat_locations[,2],non_repeat_locations[,1],col="green",cex=0.6)
  }
  else if(this_data_source_counter==4){
    points(non_repeat_locations[,2],non_repeat_locations[,1],col="blue",cex=0.6)
  }
  else {
    stop(1, call. = TRUE, domain = NULL)
    geterrmessage("Loop should not have called this path in the if-statement")
  }
  rm(This_data)
} # for(this_data_source_counter in 0:data_source_counter){    



par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
#summary(gbmtrainonly)
title(main = "All PM2.5 Observation Locations")
dev.off() # stop writing to pdf file
remove(FigFileName) # delete pdf file name variable

## find unique locations in data https://stats.stackexchange.com/questions/6759/removing-duplicated-rows-data-frame-in-r
#repeated_locations=input_mat1[,c("PM2.5_Lat","PM2.5_Lon")]
#duplicated(repeated_locations)
#repeated_locations[duplicated(repeated_locations), ]
#non_repeat_locations <- repeated_locations[!duplicated(repeated_locations), ]
#plot(non_repeat_locations[,2],non_repeat_locations[,1])


#plot(non_repeat_locations[,2],non_repeat_locations[,1])

#sel <- USmap$STATEFP_NUM==6
#plot(USmap[sel,])

###################### Save input_mat1 to csv file

#write.csv(mycars, file='mycars.csv') #or export DF as .CSV file
write.csv(input_mat1,file = file.path(ProcessedData.directory,'combined_ML_input.csv'))

sink()

################################################

#for (year in UBdata[c(2010,2011,2012,2013,2014,2015)]){
#  print(paste("The year is", year))
#}
#which( colnames(df)=="b" )
# 
# input_mat1=matrix(NA, 4, 0)
# 
# m2 <- cbind(1, 1:4)
# colnames(m2, do.NULL = FALSE)
# colnames(m2) <- c("x","Y")
# 
# input_mat1=data.frame()
# 
# 
# 
# data.frame(..., row.names = NULL, check.rows = FALSE,
#            check.names = TRUE, fix.empty.names = TRUE,
#            stringsAsFactors = default.stringsAsFactors())
# 
# 
# col_headings <- c('heading1','heading2', ....,'heading_c')
# names(your_dataframe) <- col_headings
# 
# input_header= c('ID','POC','Parameter','Method','Winter','Year','Month','Day','PM2.5_Obs','PM2.5_Lat','PM2.5_Lon','PM25_Station_Name')
# input_mat1=data.frame(row.names=input_header)
# 
# try_mat=data.frame(matrix(NA, nrow = 2, ncol = 3))