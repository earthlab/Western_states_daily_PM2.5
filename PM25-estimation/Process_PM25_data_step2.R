# Process_PM25_data_step2.R - Clean input file for Machine Learning estimation of PM2.5 for the western US, 2008-2018
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
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("General_functions.directory"),"merging_data_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))

##### Create Sink output file ####
processed_data_version <- define_study_constants.fn("processed_data_version")
this_source_file <- paste("PM25_Step1_part_",processed_data_version,".csv",sep = "") # define file name
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")
file_sub_label <- paste("PM25_Step2_part_",processed_data_version,sep = "")
SinkFileName=file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,"_sink.txt",sep = ""))
sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE)

cat("R output for Process_PM25_data_step2.R \n \n")
cat("Title: Process_PM25_data_step2.R \n")
cat("Author: Melissa May Maestas, PhD \n")
cat("Latest Update: July 2, 2019 \n")
cat(paste("Script ran and this text file created ",Sys.time()," \n",sep = ""))
cat("This program reads in cleans the PM2.5 data compiled in Process_PM25_data_step1.R. \n \n")
cat("Source file: \n")
cat(this_source_file)
cat("\n \n")

#### Define constants and set thresholds for cleaning data #####
cat("Constants Defined: \n")
start_study_date <- as.Date(define_study_constants.fn("start_date"),format = "%Y-%m-%d") #as.Date("2008-01-01",format = "%Y-%m-%d")
print_name_value.fn(this_var_name = "start_study_date",this_var_value = start_study_date,this_var_units = NA)
stop_study_date <- as.Date(define_study_constants.fn("end_date"),format = "%Y-%m-%d")#as.Date("2014-12-31",format = "%Y-%m-%d")
print_name_value.fn(this_var_name = "stop_study_date",this_var_value = stop_study_date,this_var_units = NA)
min_hourly_obs_daily <- define_study_constants.fn("min_hourly_obs_daily") #18/24*100 # minimum percent of hourly observations required to compute a 24-hr average
print_name_value.fn(this_var_name = "min_hourly_obs_daily",this_var_value = min_hourly_obs_daily,this_var_units = "Percent")
voltage_threshold_upper <- define_study_constants.fn("voltage_threshold_upper")  #17 # should match value set in step1
print_name_value.fn(this_var_name = "voltage_threshold_upper",this_var_value = voltage_threshold_upper,this_var_units = "V")
voltage_threshold_lower <- define_study_constants.fn("voltage_threshold_lower")  #11 # should match value set in step1
print_name_value.fn(this_var_name = "voltage_threshold_lower",this_var_value = voltage_threshold_lower,this_var_units = "V")
CARB_Mobile_flow_threshold_upper <- define_study_constants.fn("CARB_Mobile_flow_threshold_upper")
print_name_value.fn(this_var_name = "CARB_Mobile_flow_threshold_upper",this_var_value = CARB_Mobile_flow_threshold_upper,this_var_units = "L/min")
CARB_Mobile_flow_threshold_lower <- define_study_constants.fn("CARB_Mobile_flow_threshold_lower")
print_name_value.fn(this_var_name = "CARB_Mobile_flow_threshold_lower",this_var_value = CARB_Mobile_flow_threshold_lower,this_var_units = "L/min")
DRI_flow_threshold_upper <- define_study_constants.fn("DRI_flow_threshold_upper")
print_name_value.fn(this_var_name = "DRI_flow_threshold_upper",this_var_value = DRI_flow_threshold_upper,this_var_units = "L/min")
DRI_flow_threshold_lower <- define_study_constants.fn("DRI_flow_threshold_lower")
print_name_value.fn(this_var_name = "DRI_flow_threshold_lower",this_var_value = DRI_flow_threshold_lower,this_var_units = "L/min")
RHi_threshold_upper <- define_study_constants.fn("RHi_threshold_upper")
print_name_value.fn(this_var_name = "RHi_threshold_upper",this_var_value = RHi_threshold_upper,this_var_units = "%")
# bounds that just have about 78 km east of Colorado 
North_Edge <- define_study_constants.fn("North_Edge")
print_name_value.fn(this_var_name = "North_Edge",this_var_value = North_Edge,this_var_units = "degrees latitude")
South_Edge <- define_study_constants.fn("South_Edge")
print_name_value.fn(this_var_name = "South_Edge",this_var_value = South_Edge,this_var_units = "degrees latitude")
West_Edge <- define_study_constants.fn("West_Edge")
print_name_value.fn(this_var_name = "West_Edge",this_var_value = West_Edge,this_var_units = "degrees longitude")
East_Edge <- define_study_constants.fn("East_Edge")
print_name_value.fn(this_var_name = "East_Edge",this_var_value = East_Edge,this_var_units = "degrees longitude")
allowed_in_day_LatLon_variation <- define_study_constants.fn("allowed_in_day_LatLon_variation")
print_name_value.fn(this_var_name = "allowed_in_day_LatLon_variation",this_var_value = allowed_in_day_LatLon_variation,this_var_units = "degrees")
cat("\n") # add extra space so output report is neater

# load data file
input_mat1 <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,this_source_file),header=TRUE)
input_mat1 <- input_mat_change_data_classes.fn(input_mat1)

print(paste(this_source_file,' has ',dim(input_mat1)[1],' rows of data and ',
            dim(input_mat1)[2],' columns.',sep = ""))
N_obs_original <- dim(input_mat1)[1]
print("summary(input_mat1) prior to any quality cuts")
summary(input_mat1) # give summary of current state of data
cat("\n") # add extra space so output report is neater
print("file names in data:")
unique(input_mat1$Source_File)
cat("\n") # add extra space so output report is neater

# replace sites with "UNKOWN" datum with NAD27 per Colleen's advice
print("summary of datum information:")
summary(input_mat1$Datum)
which_datum_unk <- which(input_mat1$Datum == "UNKNOWN")
print("UNKNOWN datum's are from this data source:")
unique(input_mat1[which_datum_unk, c("Data_Source_Name_Display")])
input_mat1[which_datum_unk, c("Datum")] <- "NAD27"
print(paste(length(which_datum_unk)," PM2.5 observations with UNKNOWN datum were replaced with NAD27 per advice from Dr. Colleen Reid",sep = ""))
rm(which_datum_unk)
print("summary of datum information:")
summary(input_mat1$Datum)
cat("\n") # add extra space so output report is neater

#### Remove Negative Concentrations ####
print("remove negative concentrations and create input_mat_step1")
split_df_list <- remove_data_outside_range.fn(df_in = input_mat1, column_of_interest = "PM2.5_Obs", upper_limit = NA, lower_limit = 0, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE, reason_removed = "Remove negative and NA PM2.5")
input_mat_step1 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step1)[1], part_B = dim(removing_mat)[1]) 
rm(input_mat1,split_df_list)
print(paste(dim(input_mat_step1)[1]," rows of data remain.",sep = ""))
print("summary(input_mat_step1)")
summary(input_mat_step1) # give summary of current state of data
cat("\n") # add extra space so output report is neater
print("file names still included")
unique(input_mat_step1$Source_File)
print("Summary for data removed for negative concentrations: \n")
summary(removing_mat)
Aggregate_removed_data <- removing_mat
rm(removing_mat)

# remove data where the concentrations are positive, but negative concentrations were used in its calculation (hourly data)
print("remove data where the concentrations are positive, but negative concentrations were used in its calculation (hourly data)")
split_df_list <- remove_data_outside_range.fn(df_in = input_mat_step1, column_of_interest = "N_Negative_Obs", upper_limit = 0, lower_limit = 0, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE, reason_removed = "concentrations are positive, but negative concentrations were used in its calculation (hourly data)")
input_mat_step2 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
rm(input_mat_step1)
print("summary(input_mat_step2)")
summary(input_mat_step2) # give summary of current state of data
print("file names still included")
unique(input_mat_step2$Source_File)
print("Summary for removed data where the concentrations are positive, but negative concentrations were used in its calculation (hourly data): \n")
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
rm(removing_mat)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step2)[1], part_B = dim(Aggregate_removed_data)[1]) 

#### Remove rows that are composites of hourly data without at least 18/24 observations ####
print(paste("remove hourly data that doesn't have at least ",min_hourly_obs_daily,"percent of expected observations in a day"))
# separate and describe data by hourly vs daily data (hourly data has already been turned into 24-hr averages)
which_daily <- which(input_mat_step2[,c("Sample_Duration")]!="1 HOUR") # find the rows that were daily (24-hr) data
input_mat_daily <- input_mat_step2[which_daily,] # create data frame of just daily (24 hr) data
print(paste(dim(input_mat_daily)[1]," rows of data are daily data",sep = ""))
rm(which_daily)
which_hourly <- which(input_mat_step2[,c("Sample_Duration")]=="1 HOUR") # find the rows that were from hourly data
input_mat_hourly <- input_mat_step2[which_hourly,] # create data frame of just the hourly data
print(paste(dim(input_mat_hourly)[1]," rows of data are hourly data",sep = ""))
split_df_list <- remove_data_outside_range.fn(df_in = input_mat_hourly, column_of_interest = "Observation_Percent", upper_limit = NA, lower_limit = min_hourly_obs_daily, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE, reason_removed = paste("require ",min_hourly_obs_daily," percent of hourly obs"))
rm(input_mat_hourly, which_hourly)
input_mat_hourly_clean <- split_df_list[[1]]
removing_mat <- split_df_list[[2]] 
rm(split_df_list)
input_mat_step3 <- rbind(input_mat_daily,input_mat_hourly_clean) # recombine hourly and daily data
print(paste(dim(input_mat_step3)[1]," rows of data remain:",sep = ""))
rm(input_mat_daily,input_mat_hourly_clean)
summary(input_mat_step3) # give summary of current state of data
print("file names still included")
unique(input_mat_step3$Source_File)
rm(input_mat_step2)
print(paste("Summary for removed data that are composites of hourly data without at least",min_hourly_obs_daily,"percent of observations: \n"))
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
rm(removing_mat)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step3)[1], part_B = dim(Aggregate_removed_data)[1]) 
print("\n ---------------------------------------- \n \n")

#### Remove rows of DRI and CARB Mobile data with voltage flags and no flow ####
print("Remove data with voltage flags (relevant for DRI and CARB Mobile data)")
# separate DRI and non-DRI data
which_non_DRI_non_CARBMobile <- which(input_mat_step3[,c("Data_Source_Name_Short")]!="FireCacheDRI" & input_mat_step3[,c("Data_Source_Name_Short")]!="CARBMobile") # find the rows that were neither DRI nor CARB Mobile data
non_DRI_non_CARBMobile <- input_mat_step3[which_non_DRI_non_CARBMobile,]
#non_DRI <- input_mat_step3[which_non_DRI,]
rm(which_non_DRI_non_CARBMobile)
which_DRI <- which(input_mat_step3[,c("Data_Source_Name_Short")]=="FireCacheDRI") # find the rows that were DRI or CARB Mobile data
DRI_only_data_not_clean <- input_mat_step3[which_DRI,] # isolate DRI data
rm(which_DRI)
which_CARBMobile <- which(input_mat_step3[,c("Data_Source_Name_Short")]=="CARBMobile") # find the rows that were DRI or CARB Mobile data
CARBMobile_only_data_not_clean <- input_mat_step3[which_CARBMobile,] # isolate DRI data
rm(which_CARBMobile)

# For the DRI data, remove those with flags for voltage
split_df_list <- remove_data_not_matching_string.fn(df_in = DRI_only_data_not_clean, column_of_interest = "flg.BatteryVoltage", specified_string = "0 0", remove_NAs = TRUE, reason_removed = "Battery Voltage flags")
rm(DRI_only_data_not_clean)
DRI_only_voltage_clean_step <- split_df_list[[1]]
removing_mat <- split_df_list[[2]] 
rm(split_df_list)
print("Summary for removed data that had bad voltage flags (DRI data only):")
summary(removing_mat)
removing_mat_volt_flags_DRI <- removing_mat
rm(removing_mat)

# For the CARB Mobile data, remove those with flags for voltage
split_df_list <- remove_data_not_matching_string.fn(df_in = CARBMobile_only_data_not_clean, column_of_interest = "flg.BatteryVoltage", specified_string = "0 0", remove_NAs = TRUE, reason_removed = "Battery Voltage flags")
rm(CARBMobile_only_data_not_clean)
CARBMobile_only_voltage_clean_step <- split_df_list[[1]]
removing_mat <- split_df_list[[2]] 
rm(split_df_list)
print("Summary for removed data that had bad voltage flags (CARB Mobile data only):")
summary(removing_mat)
removing_mat_volt_flags_CARBMobile <- removing_mat
rm(removing_mat)

# For the remaining DRI data, remove rows with flags for flow
# the flags indicate if the flow is more than 2% different from 2.0 L/min (thresholds set in general_project_functions.R)
print(paste("remove DRI data that is more than 2% different from 2.0 L/min (2% range:",DRI_flow_threshold_lower,"-",DRI_flow_threshold_upper," L/min)"))
DRI_only_voltage_clean_step$flg.AirFlw <- as.character(DRI_only_voltage_clean_step$flg.AirFlw)
split_df_list <- remove_data_not_matching_string.fn(df_in = DRI_only_voltage_clean_step, column_of_interest = "flg.AirFlw", specified_string = "0 0", remove_NAs = TRUE, reason_removed = "Flow flags - DRI")
DRI_only_voltage_clean <- split_df_list[[1]]
removing_mat <- split_df_list[[2]] 
rm(split_df_list, DRI_only_voltage_clean_step)
print("summary of DRI data removed for having flow data point outside of set thresholds:")
print(summary(removing_mat))
removing_mat_flow_DRI <- removing_mat
rm(removing_mat)

# For the remaining CARB Mobile data, remove rows with flags for flow
# the flags indicate if the flow is more than 2% different from 16.7 L/min (thresholds set in general_project_functions.R)
print(paste("remove data that is more than 2% different from 16.7 L/min (2% range:",CARB_Mobile_flow_threshold_lower,"-",CARB_Mobile_flow_threshold_upper," L/min)"))
CARBMobile_only_voltage_clean_step$flg.AirFlw <- as.character(CARBMobile_only_voltage_clean_step$flg.AirFlw)
split_df_list <- remove_data_not_matching_string.fn(df_in = CARBMobile_only_voltage_clean_step, column_of_interest = "flg.AirFlw", specified_string = "OK", remove_NAs = TRUE, reason_removed = "Flow flags")
CARBMobile_only_voltage_clean <- split_df_list[[1]]
removing_mat <- split_df_list[[2]] 
rm(split_df_list, CARBMobile_only_voltage_clean_step)
print("summary of CARB Mobile data removed for having flow data point outside of set thresholds:")
print(summary(removing_mat))
removing_mat_flow_CARBMobile <- removing_mat
rm(removing_mat)

# For the remaining DRI data, remove rows with flags for RH (relative humidity)
# the flags indicate if the flow is more than 45% (thresholds set in general_project_functions.R)
#print(paste("Remove DRI data with relative humidity (RHi) greater than",RHi_threshold_upper,"%"))
print(paste("Remove DRI data with internal relative humidity greater than or equal to",RHi_threshold_upper,"%"))
#DRI_only_voltage_clean$flg.RelHumid<- as.character(DRI_only_voltage_clean$flg.RelHumid)
DRI_only_voltage_clean$flg..SensorIntRH<- as.character(DRI_only_voltage_clean$flg..SensorIntRH)
#split_df_list <- remove_data_not_matching_string.fn(df_in = DRI_only_voltage_clean, column_of_interest = "flg.RelHumid", specified_string = "0 0", remove_NAs = FALSE, reason_removed = "Relative humidity flags - DRI")
split_df_list <- remove_data_not_matching_string.fn(df_in = DRI_only_voltage_clean, column_of_interest = "flg..SensorIntRH", specified_string = "0 0", remove_NAs = FALSE, reason_removed = "Internal Relative humidity flags - DRI")
DRI_only_RH_clean <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
rm(split_df_list, DRI_only_voltage_clean)
print("summary of DRI data removed for having RH data point outside of set thresholds:")
print(summary(removing_mat))
removing_mat_RH_DRI <- removing_mat
rm(removing_mat)

# For the remaining CARB Mobile data, remove rows with flags for RHi (relative humidity)
# the flags indicate if the flow is more than 45% (thresholds set in general_project_functions.R)
print(paste("Remove CARB Mobile data with relative humidity (RHi) greater than or equal to",RHi_threshold_upper,"%"))
CARBMobile_only_voltage_clean$flg.RelHumid<- as.character(CARBMobile_only_voltage_clean$flg.RelHumid)
split_df_list <- remove_data_not_matching_string.fn(df_in = CARBMobile_only_voltage_clean, column_of_interest = "flg.RelHumid", specified_string = "OK", remove_NAs = TRUE, reason_removed = "Relative humidity flags")
CARBMobile_only_RH_clean <- split_df_list[[1]]
removing_mat <- split_df_list[[2]] 
rm(split_df_list, CARBMobile_only_voltage_clean)
print("summary of CARB Mobile data removed for having RH data point outside of set thresholds:")
print(summary(removing_mat))
removing_mat_RH_CARB_Mobile <- removing_mat
rm(removing_mat)

#input_mat_step4 <- rbind(non_DRI,DRI_only_voltage_clean) # put DRI and non-DRI data back together
#input_mat_step4 <- rbind(non_DRI,DRI_only_RH_clean) # put DRI and non-DRI data back together
input_mat_step4 <- rbind(non_DRI_non_CARBMobile,DRI_only_RH_clean,CARBMobile_only_RH_clean) # put DRI and non-DRI data back together
#rm(non_DRI,DRI_only_voltage_clean,input_mat_step3)
rm(non_DRI_non_CARBMobile,DRI_only_RH_clean,CARBMobile_only_RH_clean,input_mat_step3)
print("Summary of data remaining (input_mat_step4):")
print(summary(input_mat_step4))
print("file names still included")
unique(input_mat_step4$Source_File)
#print("Summary for removed data that flow outside of thresholds (DRI and CARB Mobile data only):")
#summary(removing_mat)
#removing_mat_zero_flow <- removing_mat
#rm(removing_mat)

#Aggregate_removed_data <- rbind(removing_mat_zero_flow,removing_mat_volt_flags,Aggregate_removed_data)
Aggregate_removed_data <- rbind(removing_mat_volt_flags_DRI,removing_mat_volt_flags_CARBMobile,
                                removing_mat_flow_DRI, removing_mat_flow_CARBMobile,
                                removing_mat_RH_DRI,removing_mat_RH_CARB_Mobile,
                                Aggregate_removed_data)
#rm(removing_mat_volt_flags,removing_mat_zero_flow)
rm(removing_mat_volt_flags_DRI,removing_mat_volt_flags_CARBMobile,
   removing_mat_flow_DRI, removing_mat_flow_CARBMobile,
   removing_mat_RH_DRI,removing_mat_RH_CARB_Mobile)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step4)[1], part_B = dim(Aggregate_removed_data)[1]) 

if (max(input_mat_step4$Battery.Voltage.volts, na.rm = TRUE) > voltage_threshold_upper) { # make sure voltages out of range are gone
  rm(input_mat_step4)
  stop("check data and code, all high voltage data should have been removed")
} # if (max(input_mat_step4$Battery.Voltage.volts, na.rm = TRUE) > voltage_threshold_upper) { # make sure voltages out of range are gone

if (min(input_mat_step4$Battery.Voltage.volts, na.rm = TRUE) < voltage_threshold_lower) { # make sure voltages out of range are gone
  rm(input_mat_step4)
  stop("check data and code, all low voltage data should have been removed")
} # if (max(input_mat_step4$Battery.Voltage.volts, na.rm = TRUE) > voltage_threshold_upper) { # make sure voltages out of range are gone
  
if (max(input_mat_step4$l.m.Ave..Air.Flw, na.rm = TRUE) > CARB_Mobile_flow_threshold_upper) { # make sure voltages out of range are gone
  rm(input_mat_step4)
  stop("check data and code, all high flow data should have been removed")
} # if (max(input_mat_step4$Battery.Voltage.volts, na.rm = TRUE) > voltage_threshold_upper) { # make sure voltages out of range are gone

#print("*** finish fixing flow QA checks for DRI data")
#if (min(input_mat_step4$l.m.Ave..Air.Flw, na.rm = TRUE) < DRI_flow_threshold_lower) { # make sure voltages out of range are gone
#  rm(input_mat_step4)
#  stop("check data and code, all low voltage data should have been removed")
#} # if (max(input_mat_step4$Battery.Voltage.volts, na.rm = TRUE) > voltage_threshold_upper) { # make sure voltages out of range are gone

#print("*** finish fixing RH QA checks for DRI data")
# if (max(input_mat_step4$X..Rel.Humidty, na.rm = TRUE) > RHi_threshold_upper) { # make sure voltages out of range are gone
#   rm(input_mat_step4)
#   stop("check data and code, all high RH data should have been removed")
# } # if (max(input_mat_step4$Battery.Voltage.volts, na.rm = TRUE) > voltage_threshold_upper) { # make sure voltages out of range are gone

#### Remove data from Fire_Cache_Smoke_DRI_Smoke_NCFS_E_BAM_N1.csv ####
print("June 6, 2014 24-hr average PM\textsubscript{2.5} concentration from monitor ``Smoke NCFS E-BAM #1'' ")
print("(Fire_Cache_Smoke_DRI_Smoke_NCFS_E_BAM_N1.csv) is 24,203 ug/m3. There's nothing apparent wrong with the")
print("hourly data, however, only 4 (out of about 41) days of data that made it through the other quality checks from")
print("this data file. This suggests that this monitor is suspect, and will be removed.")

split_df_list <- remove_data_matching_string.fn(df_in = input_mat_step4, column_of_interest = "Source_File", specified_string = "Fire_Cache_Smoke_DRI_Smoke_NCFS_E_BAM_N1.csv", remove_NAs = TRUE, reason_removed = "Removing all data from NCFS E BAM N1") 
input_mat_step5 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
print("summary of data removed (removed all data from the NCFA E BAM N1 site):")
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step5)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step4,split_df_list,removing_mat)
print("summary(input_mat_step5)")
summary(input_mat_step5)
print("file names still included")
unique(input_mat_step5$Source_File)

#### Remove data points outside geographic area ####
# bounding box set in general_project_functions.R
print(paste("Remove data that is outside this range: ",South_Edge," - ",North_Edge," Degrees North and ",West_Edge," - ",East_Edge," degrees in Longitude",sep = ""))
split_df_list <- remove_data_outside_range.fn(df_in = input_mat_step5, column_of_interest = "PM2.5_Lat", upper_limit = North_Edge, lower_limit = South_Edge, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE, reason_removed = "Remove data outside North/South Geographic Boundary") 

input_mat_step6 <- split_df_list[[1]]
removing_mat_NS <- split_df_list[[2]]
rm(input_mat_step5,split_df_list)

split_df_list <- remove_data_outside_range.fn(df_in = input_mat_step6, column_of_interest = "PM2.5_Lon", upper_limit = East_Edge, lower_limit = West_Edge, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE) 
input_mat_step7 <- split_df_list[[1]]
removing_mat_EW <- split_df_list[[2]]
rm(input_mat_step6,split_df_list)

remove_out_geog <- rbind(removing_mat_NS,removing_mat_EW)
rm(removing_mat_NS,removing_mat_EW)
print("Summary of data removed for being outside geographic boundary:")
print(summary(remove_out_geog))
print("summary of data remaining (input_mat_step7):")
print(summary(input_mat_step7))
print("file names still included")
print(unique(input_mat_step7$Source_File))

Aggregate_removed_data <- rbind(remove_out_geog,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step7)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(remove_out_geog)

#### Remove data outside the study period (defined in general_project_functions.R) ####
split_df_list <- remove_data_outside_range.fn(df_in = input_mat_step7, column_of_interest = "Date_Local", upper_limit = stop_study_date, lower_limit = start_study_date, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE, reason_removed = "Outside study period") 
input_mat_step8 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
print(paste("summary of data removed for being outside the study period,",start_study_date,"-",stop_study_date,":"))
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step8)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step7,removing_mat,split_df_list)
print("summary of data kept, which is during the study period:")
summary(input_mat_step8)
print("file names still included")
unique(input_mat_step8$Source_File)

#### remove data with Event_Type == "Excluded", keeping NAs ####
print("remove data with Event_Type == 'Excluded', keeping NAs")
split_df_list <- remove_data_matching_string.fn(df_in = input_mat_step8, column_of_interest = "Event_Type", specified_string = "Excluded", remove_NAs = FALSE, reason_removed = "Remove Event Type 'Excluded'")
input_mat_step9 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
rm(split_df_list)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step9)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step8)
print("summary of data removed for having Event_Type == 'Excluded' (NA's are kept):")
print(summary(removing_mat))
print("summary of data kept (input_mat_step9):")
summary(input_mat_step9)
print("file names still included")
unique(input_mat_step9$Source_File)

# Remove data based on having too much variation in Lat/lon observations within a day
split_df_list <- remove_data_outside_range.fn(df_in = input_mat_step9, column_of_interest = "InDayLatDiff", upper_limit = allowed_in_day_LatLon_variation, lower_limit = 0, include_upper_limit = FALSE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE, reason_removed = paste("In-day Latitude variation greater than",allowed_in_day_LatLon_variation)) 
input_mat_step10 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
print(paste("summary of data removed for ","In-day Latitude variation greater than ",allowed_in_day_LatLon_variation,":",sep = ""))
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step10)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step9,removing_mat,split_df_list)
print("summary of data kept after removing data with too much in-day latitude variation:")
summary(input_mat_step10)
print("file names still included")
unique(input_mat_step10$Source_File)

split_df_list <- remove_data_outside_range.fn(df_in = input_mat_step10, column_of_interest = "InDayLonDiff", upper_limit = allowed_in_day_LatLon_variation, lower_limit = 0, include_upper_limit = FALSE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE, reason_removed = paste("In-day Longitude variation greater than",allowed_in_day_LatLon_variation)) 
input_mat_step11 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
print(paste("summary of data removed for ","In-day Longitude variation greater than ",allowed_in_day_LatLon_variation,":",sep = ""))
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step11)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step10,removing_mat,split_df_list)
print("summary of data kept after removing data with too much in-day longitude variation:")
summary(input_mat_step11)
print("file names still included")
unique(input_mat_step11$Source_File)

#### look at how many decimal places are in location information ####
#print("Look at how many decimal places are in location information")
#sort(unique(unlist(lapply(input_mat_step11$PM2.5_Lat, decimalplaces))))
#which_0_dec <- which(unlist(lapply(input_mat_step11$PM2.5_Lat, decimalplaces)) == 0)
#lat and/or lon: which_1_or_2_dec <- unique(which(unlist(lapply(input_mat_step11$PM2.5_Lat, decimalplaces)) <= 2 | unlist(lapply(input_mat_step11$PM2.5_Lon, decimalplaces)) <= 2)) # identify data points with 1 or 2 decimal places in lat or lon info

#-------------- Latitude *AND* longitude have too few decimals
which_1_or_2_dec <- unique(which(unlist(lapply(input_mat_step11$PM2.5_Lat, decimalplaces)) <= 2 & unlist(lapply(input_mat_step11$PM2.5_Lon, decimalplaces)) <= 2)) # identify data points with 1 or 2 decimal places in lat AND lon info

One_or_Two_dec <- input_mat_step11[which_1_or_2_dec, ] # isolate observations with one or two decimal places for lat or lon
print(paste(length(which_1_or_2_dec),"observations have only 1 or 2 decimal places for both Latitude and Longitude. ")) #Summary of these data:"))
rm(which_1_or_2_dec,One_or_Two_dec)

#### Remove data from Fire_Cache_Smoke_USFS_R2-265.csv between October 2016 - May 2017 due to unrealistic concentration behavior ####
# some data has what seems to be unrealistically high PM2.5 concentrations
print("The USFS R2-265 monitor exhibits unrealistic concentration behavior between October 2016 - May 2017. ")
print("The concentrations only seem to vary in increments of 1000 ug/m3 with hourly concentrations of 65,000,000 ug/m3.")
print("The data from this time period for this monitor are being removed.")

split_df_list <- remove_data_2_criteria.fn(df_in = input_mat_step11, column_of_interest1 = "Source_File", upper_limit1 = "Fire_Cache_Smoke_USFS_R2-265.csv", remove_NAs1 = TRUE, column_of_interest2 = "Date_Local", upper_limit2 = as.Date("2017-05-31",format = "%Y-%m-%d"), lower_limit2 = as.Date("2016-10-01",format = "%Y-%m-%d"), remove_NAs2 = TRUE, reason_removed = "unrealistic values for this monitor during this time frame")
input_mat_step12 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
print("summary of data removed (removed all data from the USFS R2-265 site from October 2016 - May 2017):")
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step12)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step11,split_df_list,removing_mat)
print("summary(input_mat_step12)")
summary(input_mat_step12)
print("file names still included")
unique(input_mat_step12$Source_File)

#### Remove data from Fire_Cache_Smoke_USFS_R2-264.csv between October 2016 - October 2017 due to unrealistic concentration behavior ####
# some data has what seems to be unrealistically high PM2.5 concentrations
print("The USFS R2-264 monitor exhibits unrealistic concentration behavior between October 2016 - October 2017. ")
print("The concentrations only seem to vary in increments of 1000 ug/m3.")
print("The data from this time period for this monitor are being removed.")
split_df_list <- remove_data_2_criteria.fn(df_in = input_mat_step12, column_of_interest1 = "Source_File", upper_limit1 = "Fire_Cache_Smoke_USFS_R2-264.csv", remove_NAs1 = TRUE, column_of_interest2 = "Date_Local", upper_limit2 = as.Date("2017-10-31",format = "%Y-%m-%d"), lower_limit2 = as.Date("2016-10-01",format = "%Y-%m-%d"), remove_NAs2 = TRUE, reason_removed = "unrealistic concentration behavior for this monitor during this time frame")
input_mat_step13 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
print("summary of data removed (removed all data from the USFS R2-265 site from October 2016 - October 2017):")
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step13)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step12,split_df_list,removing_mat)
print("summary(input_mat_step13)")
summary(input_mat_step13)
print("file names still included")
unique(input_mat_step13$Source_File)

#### Remove data from Fire_Cache_Smoke_DRI_FWS_Smoke_N1.csv between February 11, 2017 - February 14, 2017 due to unrealistic concentration behavior ####
# some data has what seems to be unrealistically high PM2.5 concentrations
print("The FWS Smoke #1 monitor exhibits unrealistic concentration behavior between February 11, 2017 - February 14, 2017. ")
print("The concentrations only seem to vary in increments of 1000 ug/m3.")
print("The data from this time period for this monitor are being removed.")
split_df_list <- remove_data_2_criteria.fn(df_in = input_mat_step13, column_of_interest1 = "Source_File", upper_limit1 = "Fire_Cache_Smoke_DRI_FWS_Smoke_N1.csv", remove_NAs1 = TRUE, column_of_interest2 = "Date_Local", upper_limit2 = as.Date("2017-02-14",format = "%Y-%m-%d"), lower_limit2 = as.Date("2017-01-11",format = "%Y-%m-%d"), remove_NAs2 = TRUE, reason_removed = "unrealistic concentration behavior for this monitor during this time frame")
input_mat_step14 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
print("summary of data removed (removed all data from the FWS Smoke #1 site from February 11, 2017 - February 14, 2017):")
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step14)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step13,split_df_list,removing_mat)
print("summary(input_mat_step14)")
summary(input_mat_step14)
print("file names still included")
unique(input_mat_step14$Source_File)

#### Remove data from Fire_Cache_Smoke_DRI_Smoke_N22.csv on June 15, 2012 due to unrealistic concentration behavior ####
# some data has what seems to be unrealistically high PM2.5 concentrations
print("The Smoke #22 monitor exhibits unrealistic concentration behavior on June 15, 2012. ")
print("The concentrations only seem to vary in increments of 1000 ug/m3.")
print("The data from this time period for this monitor are being removed.")
split_df_list <- remove_data_2_criteria.fn(df_in = input_mat_step14, column_of_interest1 = "Source_File", upper_limit1 = "Fire_Cache_Smoke_DRI_Smoke_N22.csv", remove_NAs1 = TRUE, column_of_interest2 = "Date_Local", upper_limit2 = as.Date("2012-06-15",format = "%Y-%m-%d"), lower_limit2 = as.Date("2012-06-15",format = "%Y-%m-%d"), remove_NAs2 = TRUE, reason_removed = "unrealistic concentration behavior for this monitor during this time frame")
input_mat_step15 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
print("summary of data removed (removed all data from the Smoke #22 site from June 2015):")
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step15)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step14,split_df_list,removing_mat)
print("summary(input_mat_step15)")
summary(input_mat_step15)
print("file names still included")
unique(input_mat_step15$Source_File)

#### Remove data from Fire_Cache_Smoke_USFS_R2-69.csv from August 31, 2016 - September 5, 2016 due to unrealistic concentration behavior ####
# some data has what seems to be unrealistically high PM2.5 concentrations
print("The Smoke USFS R2-69 monitor exhibits unrealistic concentration behavior on during August 31, 2016 - September 5, 2016. ")
print("The concentrations sometimes vary by more than 10000 ug/m3 from one hour to the next.")
print("The data from this time period for this monitor are being removed.")
split_df_list <- remove_data_2_criteria.fn(df_in = input_mat_step15, column_of_interest1 = "Source_File", upper_limit1 = "Fire_Cache_Smoke_USFS_R2-69.csv", remove_NAs1 = TRUE, column_of_interest2 = "Date_Local", upper_limit2 = as.Date("2016-09-05",format = "%Y-%m-%d"), lower_limit2 = as.Date("2016-08-31",format = "%Y-%m-%d"), remove_NAs2 = TRUE, reason_removed = "unrealistic concentration behavior for this monitor during this time frame")
input_mat_step16 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
print("summary of data removed (removed all data from the Smoke USFS R2-69 site from August 31, 2016 - September 5, 2016):")
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step16)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step15,split_df_list,removing_mat)
print("summary(input_mat_step16)")
summary(input_mat_step16)
print("file names still included")
unique(input_mat_step16$Source_File)

#### Remove data from Fire_Cache_Smoke_USFS_R1-307.csv from May 4, 2015 - May 19, 2015 due to unrealistic concentration behavior ####
# some data has what seems to be unrealistically high PM2.5 concentrations
print("The Smoke USFS R1-307 monitor exhibits unrealistic concentration behavior during May 4, 2015 - May 19, 2015. ")
print("The concentrations are often exactly 65535 ug/m3 for several hours at a time.")
print("The data from this time period for this monitor are being removed.")
split_df_list <- remove_data_2_criteria.fn(df_in = input_mat_step16, column_of_interest1 = "Source_File", upper_limit1 = "Fire_Cache_Smoke_USFS_R1-307.csv", remove_NAs1 = TRUE, column_of_interest2 = "Date_Local", upper_limit2 = as.Date("2015-05-19",format = "%Y-%m-%d"), lower_limit2 = as.Date("2015-05-04",format = "%Y-%m-%d"), remove_NAs2 = TRUE, reason_removed = "unrealistic concentration behavior for this monitor during this time frame")
input_mat_step17 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
print("summary of data removed (removed all data from the Smoke USFS R1-307 site from  May 4, 2015 - May 19, 2015):")
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step17)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step16,split_df_list,removing_mat)
print("summary(input_mat_step17)")
summary(input_mat_step17)
print("file names still included")
unique(input_mat_step17$Source_File)

#### Remove data from Fire_Cache_Smoke_DRI_Smoke_N216.csv from May 15 - June 17, 2018 due to unrealistic concentration behavior ####
# some data has what seems to be unrealistically high PM2.5 concentrations
print("The Smoke #216 monitor exhibits unrealistic concentration behavior during May 15 - June 17, 2018. ")
print("The concentrations are often exactly 851 ug/m3 for several hours at a time.")
print("The data from this time period for this monitor are being removed.")
split_df_list <- remove_data_2_criteria.fn(df_in = input_mat_step17, column_of_interest1 = "Source_File", upper_limit1 = "Fire_Cache_Smoke_DRI_Smoke_N216.csv", remove_NAs1 = TRUE, column_of_interest2 = "Date_Local", upper_limit2 = as.Date("2018-06-17",format = "%Y-%m-%d"), lower_limit2 = as.Date("2018-05-06",format = "%Y-%m-%d"), remove_NAs2 = TRUE, reason_removed = "unrealistic concentration behavior for this monitor during this time frame")
input_mat_step18 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
print("summary of data removed (removed all data from the Smoke #216 site from  May 15 - June 17, 2018):")
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step18)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step17,split_df_list,removing_mat)
print("summary(input_mat_step18)")
summary(input_mat_step18)
print("file names still included")
unique(input_mat_step18$Source_File)

#### Remove data from Fire_Cache_Smoke_USFS_R2-922.csv from July 26, 2016- July 28, 2016 due to unrealistic concentration behavior ####
# some data has what seems to be unrealistically high PM2.5 concentrations
print("The Smoke USFS R2-922 monitor exhibits unrealistic concentration behavior during July 26, 2016- July 28, 2016.")
print("The concentrations vary by thousands of ug/m3 from one hour to the next.")
print("The data from this time period for this monitor are being removed.")
split_df_list <- remove_data_2_criteria.fn(df_in = input_mat_step18, column_of_interest1 = "Source_File", upper_limit1 = "Fire_Cache_Smoke_USFS_R2-922.csv", remove_NAs1 = TRUE, column_of_interest2 = "Date_Local", upper_limit2 = as.Date("2016-07-28",format = "%Y-%m-%d"), lower_limit2 = as.Date("2016-07-26",format = "%Y-%m-%d"), remove_NAs2 = TRUE, reason_removed = "unrealistic concentration behavior for this monitor during this time frame")
input_mat_step19 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
print("summary of data removed (removed all data from the Smoke USFS R2-922 site from  July 26, 2016- July 28, 2016):")
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step19)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step18,split_df_list,removing_mat)
print("summary(input_mat_step19)")
summary(input_mat_step19)
print("file names still included")
unique(input_mat_step19$Source_File)

#### Remove data from 'ARB 1027 2018 Historical Data.csv' from August 2-9 2018 due to unrealistic concentration behavior ####
print("The Smoke ARB 1027 monitor exhibits unrealistic concentration behavior during August 2-9, 2018.")
print("Starting late on August 2, 2018, the concentration starts to have a constant (very high) value through August 9, 2018.")
print("The data from this time period for this monitor are being removed.")
split_df_list <- remove_data_2_criteria.fn(df_in = input_mat_step19, column_of_interest1 = "Source_File", upper_limit1 = "ARB 1027 2018 Historical Data.csv", remove_NAs1 = TRUE, column_of_interest2 = "Date_Local", upper_limit2 = as.Date("2018-08-09",format = "%Y-%m-%d"), lower_limit2 = as.Date("2018-08-02",format = "%Y-%m-%d"), remove_NAs2 = TRUE, reason_removed = "unrealistic concentration behavior (constant values) for this monitor during this time frame")
input_mat_step20 <- split_df_list[[1]]
removing_mat <- split_df_list[[2]]
print("summary of data removed (removed all data from the ARB 1027 monitor from August 2-9, 2018):")
summary(removing_mat)
Aggregate_removed_data <- rbind(removing_mat,Aggregate_removed_data)
checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step20)[1], part_B = dim(Aggregate_removed_data)[1]) 
rm(input_mat_step19,split_df_list,removing_mat)
print("summary(input_mat_step20)")
summary(input_mat_step20)
print("file names still included")
unique(input_mat_step20$Source_File)

#### Make notes about data with very high concentrations ####

which_above_1000ugm3 <- which(input_mat_step20$PM2.5_Obs > 1000)
PM25_above1000 <- input_mat_step20[which_above_1000ugm3, ]
print("Noting data with concentrations above 1000 ug/m3")
print(PM25_above1000[ ,c("PM2.5_Obs","Date_Local","PM25_Station_Name","Data_Source_Name_Short","Source_File")])
rm(which_above_1000ugm3,PM25_above1000)
#### Put in error messages to write more code should certain conditions be met ####
which_date_NA <- which(is.na(input_mat_step20$Date_Local))
if (length(which_date_NA)>0) {stop("figure out why some data has unknown date information")}
rm(which_date_NA)

#### Notes about data ####
print('consider merging "24-HR BLK AVG" and "24 HOUR" data together in Sample Duration variable')
print('figure out why Observation percent has a max value of 200% - assuming this is already an average of multiple monitors at a given site')
which_Obs_Perc_gt100 <- which(input_mat_step20$Observation_Percent>100)
#length(which_Obs_Perc_gt100)
Obs_Perc_gt100_data <- input_mat_step20[which_Obs_Perc_gt100,]
print(paste(length(which_Obs_Perc_gt100)," rows of data have more than 100% of the anticipated observations."))
which_ObsPerc_hourly <- which(Obs_Perc_gt100_data$Sample_Duration=="1 HOUR")
print(paste(length(which_ObsPerc_hourly)," of these rows are from hourly data",sep = ""))
print("Data with more than 100% of anticipated observations come from these data source(s)")
print(unique(Obs_Perc_gt100_data$Data_Source_Name_Short))
rm(which_Obs_Perc_gt100,Obs_Perc_gt100_data,which_ObsPerc_hourly)

#### More Cleaning of the Data ####
#print('try using "subset()" function for some of these:')
print('think about making cuts on any unrealistic air temperatures for DRI data')
print('need to convert missing values that have a -9999 etc to NA value')
print('look at flag info for Federal Land Manager data and see if any other cuts should be made')

checksum.fn(N_original = N_obs_original, part_A = dim(input_mat_step20)[1], part_B = dim(Aggregate_removed_data)[1]) # check the number of rows

#### Save discarded data file to .csv ####
print("summary of the data discarded in Process_PM25_data_step2.R:")
summary(Aggregate_removed_data) # give summary of current state of data
write.csv(Aggregate_removed_data,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste("Data_Removed_in_",file_sub_label,'.csv',sep = "")),row.names = FALSE)
rm(Aggregate_removed_data)

#### Save cleaned file to .csv ####
input_mat2 <- input_mat_step20 # re-name data frame
rm(input_mat_step20)
print("summary of the data output by Process_PM25_data_step2.R:")
summary(input_mat2) # give summary of current state of data
print("unique flow values and data source (CARB Mobile should be near 16.7 and DRI should be near 2 L/min)")
print(input_mat2[!duplicated(input_mat2[,c("l.m.Ave..Air.Flw", "flg.AirFlw","Data_Source_Name_Short")]),c("l.m.Ave..Air.Flw", "flg.AirFlw","Data_Source_Name_Short")])
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
Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("PM2.5_Lat","PM2.5_Lon","Datum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = input_mat2)
if (length(Check_data)>0) {stop("***check_4_NAs.fn found questionable data. Investigate.***")}
if (class(input_mat2$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
rm(Check_data)
#rm(input_mat2)
#rm(uppermost.directory,output.directory)
#rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
#rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory)
#rm(writingcode.directory,computer_system,PythonProcessedData.directory)
rm(min_hourly_obs_daily,N_obs_original,SinkFileName,start_study_date,stop_study_date,this_source_file)
rm(voltage_threshold_upper,voltage_threshold_lower,North_Edge,South_Edge,West_Edge,East_Edge,allowed_in_day_LatLon_variation)
rm(file_sub_label,processed_data_version,sub_folder,working.directory)
rm(input_mat2)
print(paste("Process_PM25_data_step2.R completed at",Sys.time(),sep = " "))
