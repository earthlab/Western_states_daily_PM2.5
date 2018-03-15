rm(list = ls())
###### Clean input file for Machine Learning estimation of PM2.5 for the western US, 2008-2014 ######
# Create_ML_Input_File.R >> compiles the various PM2.5 data sources into data frame called input_mat1 which mimics Colleen's AllforCaret.csv, but for the western US. 

# To clear all variables and start fresh:
# rm(list = ls())

options(warn=2) # throw an error when there's a warning and stop the code from running further

#### define directories and constants ####
uppermost.directory="/home/rstudio" 
working.directory=uppermost.directory 
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

##### Create Sink output file ####
# sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
SinkFileName=file.path(output.directory,"Clean_ML_Input_File_sink.txt")
sink(file =SinkFileName, append = FALSE, type = c("output","message"),
     split = FALSE)
#sink() #COMMENT
cat("output for Clean_ML_Input_File.R \n \n")

#### Set thresholds for cleaning data #####
min_hourly_obs_daily <- 18/24*100 #18 # minimum number of hourly observations required to compute a 24-hr average
#### Load input_mat1 and do basic description ####
this_source_file <- 'combined_ML_input.csv'
input_mat1<-read.csv(file.path(ProcessedData.directory,this_source_file),header=TRUE) # load data file
print(paste(this_source_file,' has ',dim(input_mat1)[1],' rows of data and ',dim(input_mat1)[2],' columns.',sep = ""))
N_obs_original <- dim(input_mat1)[1]
#### Remove Negative Concentrations ####
which_negative <- which(input_mat1[,c("PM2.5_Obs")]<0)
which_positive <- which(input_mat1[,c("PM2.5_Obs")]>=0)
which_NA <- which(is.na(input_mat1$PM2.5_Obs))
input_mat_step1 <- input_mat1[which_positive,] 
if (N_obs_original!=length(which_negative)+length(which_positive)+length(which_NA)) {stop('stop on line 45: number of rows does not add up.')} # check that things add up
print(paste(length(which_negative)," rows of data are removed because PM2.5 concentrations are negative",sep = ""))
print(paste(length(which_NA)," rows of data are removed because PM2.5 concentrations are NA",sep = ""))
print(paste(dim(input_mat_step1)[1]," rows of data remain.",sep = ""))
rm(which_negative,which_positive,which_NA,input_mat1)
N_obs_check <- dim(input_mat_step1)[1]
#### Remove rows that are composites of hourly data without at least 18/24 observations ####
# separate and describe data by hourly vs daily data (hourly data has already been turned into 24-hr averages)
which_daily <- which(input_mat_step1[,c("Sample_Duration")]!="1 HOUR") # find the rows that were daily (24-hr) data
input_mat_daily <- input_mat_step1[which_daily,] # create data frame of just daily (24 hr) data
print(paste(dim(input_mat_daily)[1]," rows of data are daily data",sep = ""))
which_NA <- which(is.na(input_mat_step1$Sample_Duration)) # find the rows that have sample duration uknown
#input_mat_sample_duration_NA <- input_mat_step1[which_NA,]
print(paste(length(which_NA)," rows of data are removed because they have unknown sample duration.",sep = ""))
which_hourly <- which(input_mat_step1[,c("Sample_Duration")]=="1 HOUR") # find the rows that were from hourly data
input_mat_hourly <- input_mat_step1[which_hourly,] # create data frame of just the hourly data
print(paste(dim(input_mat_hourly)[1]," rows of data are hourly data",sep = ""))
if (N_obs_check!=length(which_hourly)+length(which_daily)+length(which_NA)) {stop('stop on line 56: number of rows does not add up.')} # check that things add up
rm(which_hourly,which_daily,which_NA,input_mat_step1,N_obs_check) # clear variables
# figure out which rows of hourly data have enough observations
N_obs_check <- dim(input_mat_hourly)[1] # how many rows are in input_mat_hourly
which_NA <- which(is.na(input_mat_hourly$Observation_Percent)) # which rows have unknown number of obs in hourly data
print(paste(length(which_NA)," rows of data are removed because the number of observations for hourly data is NA",sep = ""))
which_hourly_insuff <- which(input_mat_hourly$Observation_Percent<min_hourly_obs_daily) # which rows do not have enough hourly data
print(paste(length(which_hourly_insuff)," rows of data are removed there were not at least ",min_hourly_obs_daily,"% of expected observations for hourly data",sep = ""))
which_hourly_suff <- which(input_mat_hourly$Observation_Percent>=min_hourly_obs_daily) # which rows have enough hourly data
input_mat_hourly_suff <- input_mat_hourly[which_hourly_suff,] # data matrix with enough hourly data in every row
print(paste(dim(input_mat_hourly_suff)[1]," rows of hourly data remain.",sep = ""))
if (N_obs_check!=length(which_hourly_insuff)+length(which_hourly_suff)+length(which_NA)) {stop('stop on line 56: number of rows does not add up.')} # check that things add up
rm(which_hourly_insuff,which_hourly_suff,which_NA,input_mat_hourly) # clear variables
# recombine hourly and daily data
input_mat_step1 <- rbind(input_mat_daily,input_mat_hourly_suff)
print(paste(dim(input_mat_step1)[1]," rows of data remain",sep = ""))
rm(input_mat_daily,input_mat_hourly_suff)

#### Save cleaned file to .csv ####
input_mat2 <- input_mat_step1 # re-name data frame
write.csv(input_mat2,file = file.path(ProcessedData.directory,'cleaned_ML_input.csv'))

#######Clean code and move it above this line ###########################################
####  AQS data #### 

# Fire Cache data
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