# Process_PM25_data_step2.R - Clean input file for Machine Learning estimation of PM2.5 for the western US, 2008-2014
#clean PM2.5 data (get rid of negative concentrations, etc.)

#### Clear variables and sinks; define working directory ####
rm(list  =  ls())
options(warn  =  2) # throw an error when there's a warning and stop the code from running further
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which  =  dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
working.directory  <-  "/home/rstudio"
setwd(working.directory) # set working directory

#### Source functions I've written ####
# not sure if this one causes problems: #source(file.path(writingcode.directory,"Reconcile_multi_LatLon_one_site_function.R"))
#source(file.path(writingcode.directory,"Replace_LatLonDatum_for_NA_UKNOWN_function.R"))
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))

##### Create Sink output file ####
processed_data_version <- define_study_constants.fn("processed_data_version")
this_source_file <- paste("PM25_Step1_part_",processed_data_version,".csv",sep = "") # define file name
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")
file_sub_label <- paste("PM25_Step2_part_",processed_data_version,sep = "")
SinkFileName=file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,"_sink.txt",sep = ""))
sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE)
#sink() #COMMENT
cat("output for Process_PM25_data_step2.R \n \n")
cat("Source file:")
cat(this_source_file)
#### Define constants and set thresholds for cleaning data #####
start_study_date <- as.Date(define_study_constants.fn("start_date"),format = "%Y-%m-%d") #as.Date("2008-01-01",format = "%Y-%m-%d")
stop_study_date <- as.Date(define_study_constants.fn("end_date"),format = "%Y-%m-%d")#as.Date("2014-12-31",format = "%Y-%m-%d")
min_hourly_obs_daily <- define_study_constants.fn("min_hourly_obs_daily") #18/24*100 # minimum percent of hourly observations required to compute a 24-hr average
voltage_threshold_upper <- define_study_constants.fn("voltage_threshold_upper")  #17 # should match value set in step1
voltage_threshold_lower <- define_study_constants.fn("voltage_threshold_lower")  #11 # should match value set in step1
# bounds that just have about 78 km east of Colorado 
North_Edge <- define_study_constants.fn("North_Edge") #50
South_Edge <- define_study_constants.fn("South_Edge") #25
West_Edge <- define_study_constants.fn("West_Edge")  #-126
East_Edge <- define_study_constants.fn("East_Edge")  #-101 # about 78 km east of eastern edge of Colorado

# load data file
input_mat1 <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,this_source_file),header=TRUE)
input_mat1 <- input_mat_change_data_classes.fn(input_mat1)

print(paste(this_source_file,' has ',dim(input_mat1)[1],' rows of data and ',
            dim(input_mat1)[2],' columns.',sep = ""))
N_obs_original <- dim(input_mat1)[1]
print("summary(input_mat1)")
summary(input_mat1) # give summary of current state of data
print("file names still included")
unique(input_mat1$Source_File)

# replace sites with "UNKOWN" datum with NAD27 per Colleen's advice
print("summary of datum information:")
summary(input_mat1$Datum)
which_datum_unk <- which(input_mat1$Datum == "UNKNOWN")
print("UNKNOWN datum's are from this data source:")
unique(input_mat1[which_datum_unk, c("Data_Source_Name_Display")])
input_mat1[which_datum_unk, c("Datum")] <- "NAD27"
print(paste(length(which_datum_unk)," PM2.5 observations with UNKNOWN datum were replaced with NAD27",sep = ""))
print("summary of datum information:")
summary(input_mat1$Datum)

#### Remove Negative Concentrations ####
print("remove negative concentrations and create input_mat_step1")
input_mat_step1 <- remove_data_outside_range.fn(df_in = input_mat1, column_of_interest = "PM2.5_Obs", upper_limit = NA, lower_limit = 0, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE)
rm(input_mat1)
print(paste(dim(input_mat_step1)[1]," rows of data remain.",sep = ""))
print("summary(input_mat_step1)")
summary(input_mat_step1) # give summary of current state of data
print("file names still included")
unique(input_mat_step1$Source_File)

# remove data where the concentrations are positive, but negative concentrations were used in its calculation (hourly data)
print("remove data where the concentrations are positive, but negative concentrations were used in its calculation (hourly data)")
input_mat_step2 <- remove_data_outside_range.fn(df_in = input_mat_step1, column_of_interest = "N_Negative_Obs", upper_limit = 0, lower_limit = 0, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE)
rm(input_mat_step1)
print("summary(input_mat_step2)")
summary(input_mat_step2) # give summary of current state of data
print("file names still included")
unique(input_mat_step2$Source_File)

#### Remove rows that are composites of hourly data without at least 18/24 observations ####
# separate and describe data by hourly vs daily data (hourly data has already been turned into 24-hr averages)
which_daily <- which(input_mat_step2[,c("Sample_Duration")]!="1 HOUR") # find the rows that were daily (24-hr) data
input_mat_daily <- input_mat_step2[which_daily,] # create data frame of just daily (24 hr) data
print(paste(dim(input_mat_daily)[1]," rows of data are daily data",sep = ""))

which_hourly <- which(input_mat_step2[,c("Sample_Duration")]=="1 HOUR") # find the rows that were from hourly data
input_mat_hourly <- input_mat_step2[which_hourly,] # create data frame of just the hourly data
print(paste(dim(input_mat_hourly)[1]," rows of data are hourly data",sep = ""))
input_mat_hourly_clean <- remove_data_outside_range.fn(df_in = input_mat_hourly, column_of_interest = "Observation_Percent", upper_limit = NA, lower_limit = min_hourly_obs_daily, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE)
rm(input_mat_hourly)
input_mat_step3 <- rbind(input_mat_daily,input_mat_hourly_clean) # recombine hourly and daily data
print(paste(dim(input_mat_step3)[1]," rows of data remain",sep = ""))
rm(input_mat_daily,input_mat_hourly_clean)
summary(input_mat_step3) # give summary of current state of data
print("file names still included")
unique(input_mat_step3$Source_File)
rm(input_mat_step2)

#### Remove rows of DRI data with voltage flags and no flow ####
which_non_DRI <- which(input_mat_step3[,c("Data_Source_Name_Short")]!="FireCacheDRI") # find the rows that were DRI data
non_DRI <- input_mat_step3[which_non_DRI,]
rm(which_non_DRI)

which_DRI <- which(input_mat_step3[,c("Data_Source_Name_Short")]=="FireCacheDRI") # find the rows that were DRI data
DRI_only_data_not_clean <- input_mat_step3[which_DRI,] # isolate DRI data
rm(which_DRI)
# of the DRI data, remove those with flags for voltage
DRI_only_voltage_clean_step <- remove_data_not_matching_string.fn(df_in = DRI_only_data_not_clean, column_of_interest = "flg.BatteryVoltage", specified_string = "0 0", remove_NAs = TRUE)
rm(DRI_only_data_not_clean)

DRI_only_voltage_clean <- remove_data_outside_range.fn(df_in = DRI_only_voltage_clean_step, column_of_interest = "l.m.Ave..Air.Flw", upper_limit = NA, lower_limit = 0, include_upper_limit = TRUE, include_lower_limit = FALSE, remove_NAs = TRUE, verbose = TRUE) 
print("Think about whether a minimum value of flow should be set (higher than zero)")
input_mat_step4 <- rbind(non_DRI,DRI_only_voltage_clean) # put DRI and non-DRI data back together
rm(non_DRI,DRI_only_voltage_clean,input_mat_step3)
summary(input_mat_step4)
print("file names still included")
unique(input_mat_step4$Source_File)

if (max(input_mat_step4$Battery.Voltage.volts, na.rm = TRUE) > voltage_threshold_upper) { # make sure voltages out of range are gone
  rm(input_mat_step4)
  stop("check data and code, all high voltage data should have been removed")
} # if (max(input_mat_step4$Battery.Voltage.volts, na.rm = TRUE) > voltage_threshold_upper) { # make sure voltages out of range are gone

if (min(input_mat_step4$Battery.Voltage.volts, na.rm = TRUE) < voltage_threshold_lower) { # make sure voltages out of range are gone
  rm(input_mat_step4)
  stop("check data and code, all high voltage data should have been removed")
} # if (max(input_mat_step4$Battery.Voltage.volts, na.rm = TRUE) > voltage_threshold_upper) { # make sure voltages out of range are gone
  
#### Remove data from Fire_Cache_Smoke_DRI_Smoke_NCFS_E_BAM_N1.csv ####
# June 6, 2014 24-hr average PM\textsubscript{2.5} concentration from monitor ``Smoke NCFS E-BAM \#1'' 
#(Fire_Cache_Smoke_DRI_Smoke_NCFS_E_BAM_N1.csv) is 24,203 ug/m3. There's nothing apparent wrong with the 
#hourly data, however, this is the only day of data that made it through the other quality checks from 
#this data file. This suggests that this monitor is suspect, and will be removed. 

input_mat_step5 <- remove_data_matching_string.fn(df_in = input_mat_step4, column_of_interest = "Source_File", specified_string = "Fire_Cache_Smoke_DRI_Smoke_NCFS_E_BAM_N1.csv", remove_NAs = TRUE) 
rm(input_mat_step4)
summary(input_mat_step5)
print("file names still included")
unique(input_mat_step5$Source_File)

#### Remove data points outside geographic area ####
# bounding box that includes full extra row of states: 
# NW corner 50,-126
# SW corner 25, -126
# NE corner 50, -93 # 
# SE corner 25,-93

# bounds that just have about 78 km east of Colorado 
#North_Edge <- 50
#South_Edge <- 25
#West_Edge <- -126
#East_Edge <- -101 # about 78 km east of eastern edge of Colorado
print(paste("Remove data that is outside this range: ",South_Edge," - ",North_Edge," Degrees North and ",West_Edge," - ",East_Edge," degrees in Longitude",sep = ""))
input_mat_step6 <- remove_data_outside_range.fn(df_in = input_mat_step5, column_of_interest = "PM2.5_Lat", upper_limit = North_Edge, lower_limit = South_Edge, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE) 
rm(input_mat_step5)
input_mat_step7 <- remove_data_outside_range.fn(df_in = input_mat_step6, column_of_interest = "PM2.5_Lon", upper_limit = East_Edge, lower_limit = West_Edge, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE) 
rm(input_mat_step6)
summary(input_mat_step7)
print("file names still included")
unique(input_mat_step7$Source_File)
#rm(North_Edge,South_Edge,West_Edge,East_Edge)

#### Remove data outside the study period (2008-2014) ####
input_mat_step8 <- remove_data_outside_range.fn(df_in = input_mat_step7, column_of_interest = "Date_Local", upper_limit = stop_study_date, lower_limit = start_study_date, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE) 
rm(input_mat_step7)
print("summary of data kept, which is during the study period:")
summary(input_mat_step8)
print("file names still included")
unique(input_mat_step8$Source_File)

#### remove data with Event_Type == "Excluded", keeping NAs ####
print("remove data with Event_Type == 'Excluded', keeping NAs")
input_mat_step9 <- remove_data_matching_string.fn(df_in = input_mat_step8, column_of_interest = "Event_Type", specified_string = "Excluded", remove_NAs = FALSE)
rm(input_mat_step8)
print("summary of data kept:")
summary(input_mat_step9)
print("file names still included")
unique(input_mat_step9$Source_File)

#### Put in error messages to write more code should certain conditions be met ####
which_date_NA <- which(is.na(input_mat_step9$Date_Local))
if (length(which_date_NA)>0) {stop("figure out why some data has unknown date information")}
rm(which_date_NA)

#### Notes about data ####
print('consider merging "24-HR BLK AVG" and "24 HOUR" data together in Sample Duration variable')
print('figure out why Observation percent has a max value of 200% - assuming this is already an average of multiple monitors at a given site')
which_Obs_Perc_gt100 <- which(input_mat_step9$Observation_Percent>100)
#length(which_Obs_Perc_gt100)
Obs_Perc_gt100_data <- input_mat_step9[which_Obs_Perc_gt100,]
print(paste(length(which_Obs_Perc_gt100)," rows of data have more than 100% of the anticipated observations."))
which_ObsPerc_hourly <- which(Obs_Perc_gt100_data$Sample_Duration=="1 HOUR")
print(paste(length(which_ObsPerc_hourly)," of these rows are from hourly data",sep = ""))
print("Data with more than 100% of anticipated observations come from these data source(s)")
print(unique(Obs_Perc_gt100_data$Data_Source_Name_Short))
rm(which_Obs_Perc_gt100,Obs_Perc_gt100_data,which_ObsPerc_hourly)

#### More Cleaning of the Data ####
print('try using "subset()" function for some of these:')
print('think about making cuts on any unrealistic air temperatures for DRI data')

print('need to convert missing values that have a -9999 etc to NA value')
print('look at flag info for Federal Land Manager data and see if any other cuts should be made')
print('make quality cuts on InDayLatDiff and InDayLonDiff')

#### Save cleaned file to .csv ####
input_mat2 <- input_mat_step9 # re-name data frame
rm(input_mat_step9)
print("summary of the data output by Clean_ML_Input_File.R:")
summary(input_mat2) # give summary of current state of data
print("file names still included")
unique(input_mat2$Source_File)
write.csv(input_mat2,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,'.csv',sep = "")),row.names = FALSE)

#### Create a data frame with just lat, lon, and date ####
four_cols_w_duplicates <- input_mat2[,c("PM2.5_Lat","PM2.5_Lon","Datum","Date_Local")]
four_cols_data <- four_cols_w_duplicates[!duplicated(four_cols_w_duplicates),]
names(four_cols_data) <- c("Latitude","Longitude","Datum","Date")
#write.csv(four_cols_data,file = file.path(ProcessedData.directory,paste('Locations_Dates_of_PM25_Obs_from_clean_script_',Sys.Date(),'_part',processed_data_version,'.csv',sep = "")),row.names = FALSE)
write.csv(four_cols_data,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,'_Locations_Dates','.csv',sep = "")),row.names = FALSE)
rm(four_cols_data,four_cols_w_duplicates)

#### Create a data frame with just lat, and lon ####
three_cols_w_duplicates <- input_mat2[,c("PM2.5_Lat","PM2.5_Lon","Datum")]
three_cols_data <- three_cols_w_duplicates[!duplicated(three_cols_w_duplicates),]
names(three_cols_data) <- c("Latitude","Longitude","Datum")
write.csv(three_cols_data,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,'_Locations','.csv',sep = "")),row.names = FALSE)
rm(three_cols_data,three_cols_w_duplicates)

#### End of file clean up ####
sink()
#rm(input_mat2)
#rm(uppermost.directory,output.directory)
#rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
#rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory)
#rm(writingcode.directory,computer_system,PythonProcessedData.directory)
rm(min_hourly_obs_daily,N_obs_original,SinkFileName,start_study_date,stop_study_date,this_source_file)
rm(voltage_threshold_upper,voltage_threshold_lower,North_Edge,South_Edge,West_Edge,East_Edge)
rm(file_sub_label,processed_data_version,sub_folder,which_datum_unk,working.directory)
