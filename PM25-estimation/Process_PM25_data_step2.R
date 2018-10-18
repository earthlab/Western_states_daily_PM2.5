# Process_PM25_data_step2.R - clean PM2.5 data (get rid of negative concentrations, etc.)

print("run Define_directories.R before this script") 

# Clean input file for Machine Learning estimation of PM2.5 for the western US, 2008-2014
#file_sub_label <- paste("PM25_Step1_",Sys.Date(),"_part_",processed_data_version,"_Sources_Merged",sep = "")
this_source_file_name <- "PM25_Step1_2018-10-15_part_a_Sources_Merged" #'combined_ML_input2018-10-15_part_a.csv' # define file name

#### Source functions I've written ####
source(file.path(writingcode.directory,"Reconcile_multi_LatLon_one_site_function.R"))
source(file.path(writingcode.directory,"Replace_LatLonDatum_for_NA_UKNOWN_function.R"))

#### define constants ####
start_study_date <- as.Date("2008-01-01",format = "%Y-%m-%d")
stop_study_date <- as.Date("2014-12-31",format = "%Y-%m-%d")
#Set in Define_directories.R # processed_data_version <- "b" # Do not go earlier in the alphabet than what is currently set

##### Create Sink output file ####
# sink command sends R output to a file. 
# Don't try to open file until R has closed it at end of script.
# https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
file_sub_label <- paste("PM25_Step2_",Sys.Date(),"_part_",processed_data_version,"_Cleaned",sep = "")
SinkFileName=file.path(ProcessedData.directory,paste(file_sub_label,"_sink.txt",sep = ""))
#SinkFileName=file.path(ProcessedData.directory,paste("Clean_ML_Input_File_sink_",Sys.Date(),'_part_',processed_data_version,".txt",sep = "")) # file name
sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE)
#sink() #COMMENT
cat("output for Clean_ML_Input_File.R \n \n")
cat("Source file:")
cat(this_source_file_name)
#### Set thresholds for cleaning data #####
# minimum percent of hourly observations required to compute a 24-hr average
min_hourly_obs_daily <- 18/24*100 

#### Load input_mat1 ####
print("Load data that was created in Create_ML_Input_File.R")
#this_source_file <- 'combined_ML_input.csv' # define file name
#this_source_file <- 'combined_ML_input2018-10-15_part_a.csv' # define file name
this_source_file <- paste(this_source_file_name,".csv",sep = "")
# load data file
input_mat1 <- read.csv(file.path(ProcessedData.directory,this_source_file),header=TRUE)
#class(input_mat1)
#class(input_mat1$Date_Local)

print(paste(this_source_file,' has ',dim(input_mat1)[1],' rows of data and ',
            dim(input_mat1)[2],' columns.',sep = ""))
N_obs_original <- dim(input_mat1)[1]
print("summary(input_mat1)")
summary(input_mat1) # give summary of current state of data
print("file names still included")
unique(input_mat1$Source_File)
#### Remove Negative Concentrations ####
print("remove negative concentrations and create input_mat_step1")
# remove data with concentrations that are negative
which_negative <- which(input_mat1[,c("PM2.5_Obs")]<0)
which_positive <- which(input_mat1[,c("PM2.5_Obs")]>=0)
which_NA <- which(is.na(input_mat1$PM2.5_Obs))
input_mat_step1 <- input_mat1[which_positive,] 
if (N_obs_original!=length(which_negative)+length(which_positive)+length(which_NA)) {stop('stop on line 45: number of rows does not add up.')} # check that things add up
print(paste(length(which_negative)," rows of data are removed because PM2.5 concentrations are negative",sep = ""))
print(paste(length(which_NA)," rows of data are removed because PM2.5 concentrations are NA",sep = ""))
print(paste(dim(input_mat_step1)[1]," rows of data remain.",sep = ""))
rm(which_negative,which_positive,which_NA,input_mat1)
#N_obs_check <- dim(input_mat_step1)[1]
print("summary(input_mat_step1)")
summary(input_mat_step1) # give summary of current state of data
print("file names still included")
unique(input_mat_step1$Source_File)
# remove data where the concentrations are positive, but negative concentrations were used in its calculation (hourly data)
print("remove data where the concentrations are positive, but negative concentrations were used in its calculation (hourly data)")
which_N_neg <- which(input_mat_step1[,c("N_Negative_Obs")]>0)
which_no_neg <- which(input_mat_step1[,c("N_Negative_Obs")]==0)

which_NA <- which(is.na(input_mat_step1[,c("N_Negative_Obs")]))
if (length(which_NA)>0) {
  print("Some N_Negative_Obs data not filled in. Go back to create file and fix.")
  print(unique(input_mat_step1[which_NA,c("Data_Source_Name_Short")]))
  missing_neg_info <- input_mat_step1[which_NA,]
  stop("Go back to create file and fix")
  } # error message - there should be any NA's for N_Negative_Obs column at this point

input_mat_step2 <- input_mat_step1[which_no_neg,]
rm(input_mat_step1,which_N_neg,which_no_neg)
print("summary(input_mat_step2)")
summary(input_mat_step2) # give summary of current state of data
print("file names still included")
unique(input_mat_step2$Source_File)
N_obs_check <- dim(input_mat_step2)[1]
#### Remove rows that are composites of hourly data without at least 18/24 observations ####
# separate and describe data by hourly vs daily data (hourly data has already been turned into 24-hr averages)
which_daily <- which(input_mat_step2[,c("Sample_Duration")]!="1 HOUR") # find the rows that were daily (24-hr) data
input_mat_daily <- input_mat_step2[which_daily,] # create data frame of just daily (24 hr) data
print(paste(dim(input_mat_daily)[1]," rows of data are daily data",sep = ""))

#summary(input_mat_daily)
#which_low_obs_perc <- which(input_mat_daily$Observation_Percent<5)
#length(which_low_obs_perc)
#low_obs_perc <- input_mat_daily[which_low_obs_perc,]

which_NA <- which(is.na(input_mat_step2$Sample_Duration)) # find the rows that have sample duration uknown
#input_mat_sample_duration_NA <- input_mat_step2[which_NA,]
print(paste(length(which_NA)," rows of data are removed because they have unknown sample duration.",sep = ""))
which_hourly <- which(input_mat_step2[,c("Sample_Duration")]=="1 HOUR") # find the rows that were from hourly data
input_mat_hourly <- input_mat_step2[which_hourly,] # create data frame of just the hourly data
print(paste(dim(input_mat_hourly)[1]," rows of data are hourly data",sep = ""))
if (N_obs_check!=length(which_hourly)+length(which_daily)+length(which_NA)) {stop('stop on line 56: number of rows does not add up.')} # check that things add up
rm(which_hourly,which_daily,which_NA,input_mat_step2,N_obs_check) # clear variables
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
rm(which_hourly_insuff,which_hourly_suff,which_NA,input_mat_hourly,N_obs_check) # clear variables
# recombine hourly and daily data
input_mat_step3 <- rbind(input_mat_daily,input_mat_hourly_suff)
print(paste(dim(input_mat_step3)[1]," rows of data remain",sep = ""))
rm(input_mat_daily,input_mat_hourly_suff)
summary(input_mat_step3) # give summary of current state of data
print("file names still included")
unique(input_mat_step3$Source_File)
#### Remove rows of DRI data with voltage flags ####
N_obs_check <- dim(input_mat_step3)[1] # how many rows are in input_mat_hourly
which_DRI <- which(input_mat_step3[,c("Data_Source_Name_Short")]=="FireCacheDRI") # find the rows that were DRI data
DRI_only_data_not_clean <- input_mat_step3[which_DRI,] # isolate DRI data

which_non_DRI <- which(input_mat_step3[,c("Data_Source_Name_Short")]!="FireCacheDRI") # find the rows that were DRI data
non_DRI <- input_mat_step3[which_non_DRI,]

# of the DRI data, remove those with flags for voltage
which_flag_0 <- which(DRI_only_data_not_clean[,c("flg.BatteryVoltage")]=="0")
DRI_only_voltage_clean <- DRI_only_data_not_clean[which_flag_0,]

which_flag_volt <- which(DRI_only_data_not_clean[,c("flg.BatteryVoltage")]!="0")
DRI_voltage_flagged <- DRI_only_data_not_clean[which_flag_volt,]

print(paste(length(which_flag_volt)," rows of data are removed because either the Battery voltage had a flag or was outside the thresholds set in Create_ML_Input_File.R",sep = ""))

if (N_obs_check!=length(which_non_DRI)+length(which_flag_0)+length(which_flag_volt)) {stop('stop on line 105: number of rows does not add up.')} # check that things add up

input_mat_step4 <- rbind(non_DRI,DRI_only_voltage_clean)
rm(which_DRI,DRI_only_data_not_clean,which_non_DRI,non_DRI,which_flag_0,DRI_only_voltage_clean,which_flag_volt,DRI_voltage_flagged)
rm(input_mat_step3,N_obs_check)
summary(input_mat_step4)
print("file names still included")
unique(input_mat_step4$Source_File)
#### Remove data from Fire_Cache_Smoke_DRI_Smoke_NCFS_E_BAM_N1.csv ####
# June 6, 2014 24-hr average PM\textsubscript{2.5} concentration from monitor ``Smoke NCFS E-BAM \#1'' 
#(Fire_Cache_Smoke_DRI_Smoke_NCFS_E_BAM_N1.csv) is 24,203 ug/m3. There's nothing apparent wrong with the 
#hourly data, however, this is the only day of data that made it through the other quality checks from 
#this data file. This suggests that this monitor is suspect, and will be removed. 

which_this_file <- which(input_mat_step4$Source_File=="Fire_Cache_Smoke_DRI_Smoke_NCFS_E_BAM_N1.csv")
if (length(which_this_file)>1) {stop("Check code and data - only expecting to remove 1 file")}
which_not_this_file <- which(input_mat_step4$Source_File!="Fire_Cache_Smoke_DRI_Smoke_NCFS_E_BAM_N1.csv")

input_mat_step5 <- input_mat_step4[which_not_this_file,]
rm(which_this_file,which_not_this_file,input_mat_step4)
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
North_Edge <- 50
South_Edge <- 25
West_Edge <- -126
East_Edge <- -101 # about 78 km east of eastern edge of Colorado
print(paste("Remove data that is outside this range: ",South_Edge," - ",North_Edge," Degrees North and ",West_Edge," - ",East_Edge," degrees in Longitude",sep = ""))
#which_lats_keep <- which(input_mat_step5$PM2.5_Lat>=25 & input_mat_step5$PM2.5_Lat<= 50)
#which_lats_keep <- which(input_mat_step5$PM2.5_Lat>=South_Edge & input_mat_step5$PM2.5_Lat<= North_Edge)
which_lats_keep <- which(as.numeric(as.character(input_mat_step5$PM2.5_Lat))>=South_Edge & as.numeric(as.character(input_mat_step5$PM2.5_Lat))<= North_Edge)
which_lats_part <- which(as.numeric(as.character(input_mat_step5$PM2.5_Lat))< South_Edge | as.numeric(as.character(input_mat_step5$PM2.5_Lat))> North_Edge | is.na(input_mat_step5$PM2.5_Lat))
if (length(which_lats_keep)+length(which_lats_part)!=dim(input_mat_step5)[1]){stop("Number of rows did not add up when making quality cuts on latitude")}
print("summary of data removed because the latitude was either out of the study area or NA")
summary(input_mat_step5[which_lats_part,])
input_mat_step6 <- input_mat_step5[which_lats_keep,]
rm(which_lats_keep,which_lats_part,input_mat_step5)
#which_lon_keep <- which(input_mat_step6$PM2.5_Lon>=West_Edge & input_mat_step6$PM2.5_Lon<= East_Edge)
#which_lon_part <- which(input_mat_step6$PM2.5_Lon< West_Edge | input_mat_step6$PM2.5_Lon> East_Edge)
which_lon_keep <- which(as.numeric(as.character(input_mat_step6$PM2.5_Lon))>=West_Edge & as.numeric(as.character(input_mat_step6$PM2.5_Lon))<= East_Edge)
which_lon_part <- which(as.numeric(as.character(input_mat_step6$PM2.5_Lon))< West_Edge | as.numeric(as.character(input_mat_step6$PM2.5_Lon))> East_Edge | is.na(input_mat_step6$PM2.5_Lon))
if (length(which_lon_keep)+length(which_lon_part)!=dim(input_mat_step6)[1]){stop("Number of rows did not add up when making quality cuts on longitude")}
print("summary of data removed because the longitudes were either out of the study area or NA")
summary(input_mat_step6[which_lon_part,])
input_mat_step7 <- input_mat_step6[which_lon_keep,]
rm(which_lon_keep,which_lon_part,input_mat_step6)
summary(input_mat_step7)
print("file names still included")
unique(input_mat_step7$Source_File)
rm(North_Edge,South_Edge,West_Edge,East_Edge)

#### Remove rows of data with no flow (applies to DRI data) ####
which_0_flow <- which(input_mat_step7$l.m.Ave..Air.Flw<=0)
no_flow_data <- input_mat_step7[which_0_flow,]
print("summary of data removed for no flow data (relevant for DRI Fire Cache data)")
summary(no_flow_data)
which_w_flow <- which(input_mat_step7$l.m.Ave..Air.Flw>0 | is.na(input_mat_step7$l.m.Ave..Air.Flw)) # keep data that either has positive flow or unknown flow (only DRI data has any flow info)
print(paste("Remove ",length(which_0_flow)," rows of data that have 0 l/m or negative flow.",sep = ""))
print("All of the removed data are from this data source(s)")
print(unique(no_flow_data$Data_Source_Name_Short))
print("Think about whether a minimum value of flow should be set (higher than zero)")
if (length(which_0_flow)+length(which_w_flow)!= dim(input_mat_step7)[1]) {stop("Number of rows does not add up when removing data with no flow")}
input_mat_step8 <- input_mat_step7[which_w_flow,]
rm(which_0_flow,no_flow_data,which_w_flow,input_mat_step7)

#### Remove data outside the study period (2008-2014) ####
#which_times_keep <- which(input_mat_step8$Date_Local>=start_study_date & input_mat_step8$Date_Local<= stop_study_date)
#which_times_remove <- which(input_mat_step8$Date_Local> stop_study_date)
which_times_keep <- which(as.Date(input_mat_step8$Date_Local,format = "%Y-%m-%d")>=start_study_date & as.Date(input_mat_step8$Date_Local,format = "%Y-%m-%d")<= stop_study_date)
which_times_remove <- which(as.Date(input_mat_step8$Date_Local,format = "%Y-%m-%d") < start_study_date | as.Date(input_mat_step8$Date_Local,format = "%Y-%m-%d")> stop_study_date)
if (length(which_times_keep) + length(which_times_remove) != dim(input_mat_step8)[1]) {stop("Number of rows not adding up when removing data outside the study area. Check data and code.")}
#min(input_mat_step8[which_times_remove,c("Date_Local")])
data_outside_time_frame_removed <- input_mat_step8[which_times_remove,]
print("summary of data removed due to being outside the study period:")
summary(data_outside_time_frame_removed)
rm(which_times_remove,data_outside_time_frame_removed)
input_mat_step9 <- input_mat_step8[which_times_keep,]
rm(which_times_keep,input_mat_step8)
print("summary of data kept, which is during the study period:")
summary(input_mat_step9)
print("file names still included")
unique(input_mat_step9$Source_File)

#### Fill in datums that are NA or "Unknown" from aqs_monitors.csv (only sites with EPA code) ####

# identify rows with known state code, county code, and site num, which together comprise the EPA code
which_known_EPA_Code <- which(!is.na(input_mat_step9$State_Code) & !is.na(input_mat_step9$County_Code) & !is.na(input_mat_step9$Site_Num) & !is.na(input_mat_step9$Parameter_Code) & !is.na(input_mat_step9$POC))
print(paste(length(which_known_EPA_Code)/dim(input_mat_step9)[1]*100,"% of rows in input_mat_step9 have known EPA codes",sep = ""))
which_unknown_EPA_Code <- which(is.na(input_mat_step9$State_Code) | is.na(input_mat_step9$County_Code) | is.na(input_mat_step9$Site_Num) | is.na(input_mat_step9$Parameter_Code) | is.na(input_mat_step9$POC))
print(paste(length(which_unknown_EPA_Code)/dim(input_mat_step9)[1]*100,"% of rows in input_mat_step9 have unknown EPA codes",sep = ""))
if (length(which_known_EPA_Code)+length(which_unknown_EPA_Code) != dim(input_mat_step9)[1]) {stop("Number of rows not adding up")} # check that number of rows makes sense

# create new data frames separating known and unknown EPA codes
known_EPA_Code_data <- input_mat_step9[which_known_EPA_Code,]
unknown_EPA_Code_data <- input_mat_step9[which_unknown_EPA_Code,]
rm(input_mat_step9,which_known_EPA_Code,which_unknown_EPA_Code)

# Function to provide data frame of meta data
All_sites_meta <- Reconcile_multi_LatLon_one_site.fn(this_station,this_station_data)
  
# Function to replace Unknown/NA datums and corresponding lat/lon based on output of previous function  
known_EPA_Code_data_new <- Replace_LatLonDatum_for_NA_UKNOWN.fn(known_EPA_Code_data,All_sites_meta)
rm(All_sites_meta)

# merge known_EPA_Code_data and unknown_EPA_Code_data back together
input_mat_step10 <- rbind(unknown_EPA_Code_data,known_EPA_Code_data_new)
rm(known_EPA_Code_data,known_EPA_Code_data_new,unknown_EPA_Code_data)

#### Remove data with unknown datums (e.g., WGS84, NAD83, etc) ####
which_known_datum <- which(!is.na(input_mat_step10$Datum))
which_unknown_datum <- which(is.na(input_mat_step10$Datum))
if (length(which_known_datum)+length(which_unknown_datum)!=dim(input_mat_step10)[1]) {stop("number of rows does not add up when removing unknown datums. check code and data.")}
data_unknown_datums <- input_mat_step10[which_unknown_datum,]
print("summary of data removed due to unknown datums")
summary(data_unknown_datums)
rm(which_unknown_datum,data_unknown_datums)
input_mat_step11 <- input_mat_step10[which_known_datum,]
print("summary of data kept, which has datum information:")
summary(input_mat_step11)
print("file names still included")
unique(input_mat_step11$Source_File)
rm(which_known_datum,input_mat_step10)

#### remove data with Event_Type == "Excluded" ####
#unique(input_mat_step11$Event_Type)
which_keep_not_excluded_event_type <- which(input_mat_step11$Event_Type != "Excluded" | is.na(input_mat_step11$Event_Type))
which_remove_excluded_event_type <- which(input_mat_step11$Event_Type == "Excluded")
#which_event_type_NA <- which(is.na(input_mat_step11$Event_Type))
#event_type
if (length(which_keep_not_excluded_event_type)+length(which_remove_excluded_event_type) != dim(input_mat_step11)[1]) {stop("number of rows does not add up when removing event_type = excluded. Check code and data")}
excluded_events <- input_mat_step11[which_remove_excluded_event_type,]
print("summary of data removed due to being 'excluded events'")
summary(excluded_events)
input_mat_step12 <- input_mat_step11[which_keep_not_excluded_event_type,]
rm(input_mat_step11)
for (i_row in 1:dim(excluded_events)[1]) {
  # what is the station and date for this row of excluded data?
  this_state_code <- excluded_events[i_row,c("State_Code")]
  this_county_code <- excluded_events[i_row,c("County_Code")]
  this_site_code <- excluded_events[i_row,c("Site_Num")]
  this_date <- as.Date(excluded_events[i_row,c("Date_Local")],format = "%Y-%m-%d")
  #print(paste("site ",this_state_code,"-",this_county_code,"-",this_site_code," on ",this_date,sep = ""))  
  # see if there is a corresponding data point in the kept data at this station on this date?
  which_in_kept <- which(input_mat_step12$State_Code==this_state_code & input_mat_step12$County_Code == this_county_code & input_mat_step12$Site_Num == this_site_code & as.Date(input_mat_step12$Date_Local,format = "%Y-%m-%d") == this_date)
  #print(which_in_kept)
  if (length(which_in_kept)==0) {stop("It appears there is no corresponding data to the data point removed for being 'Excluded' Event type")}
  rm(this_state_code,this_county_code,this_site_code,this_date,which_in_kept)
} # for (i_row in 1:dim(excluded_events)[1]) {
rm(i_row,which_keep_not_excluded_event_type,which_remove_excluded_event_type,excluded_events)
#### Put in error messages to write more code should certain conditions be met ####
which_date_NA <- which(is.na(input_mat_step12$Date_Local))
if (length(which_date_NA)>0) {stop("figure out why some data has unknown date information")}
rm(which_date_NA)
#### Notes about data ####
print('why are some of the Site_Num values not integers? - because the serial number from the DRI data was put into the SiteNum slot. If the serial number was unknown, some have -9999')
# find negative site numbers
#which_neg_site_num <- which(input_mat_step8$Site_Num<0)
#neg_site_num_data <- input_mat_step8[which_neg_site_num,]

print('consider merging "24-HR BLK AVG" and "24 HOUR" data together in Sample Duration variable')

print('figure out why Observation percent has a max value of 200% - assuming this is already an average of multiple monitors at a given site')
which_Obs_Perc_gt100 <- which(input_mat_step12$Observation_Percent>100)
#length(which_Obs_Perc_gt100)
Obs_Perc_gt100_data <- input_mat_step12[which_Obs_Perc_gt100,]
print(paste(length(which_Obs_Perc_gt100)," rows of data have more than 100% of the anticipated observations."))
which_ObsPerc_hourly <- which(Obs_Perc_gt100_data$Sample_Duration=="1 HOUR")
print(paste(length(which_ObsPerc_hourly)," of these rows are from hourly data",sep = ""))
print("Data with more than 100% of anticipated observations come from these data source(s)")
print(unique(Obs_Perc_gt100_data$Data_Source_Name_Short))
rm(which_Obs_Perc_gt100,Obs_Perc_gt100_data,which_ObsPerc_hourly)

print('why are some of the Site_Num values not integers?')

#### More Cleaning of the Data ####
print('try using "subset()" function for some of these:')
print('think about making cuts on any unrealistic air temperatures for DRI data')

print('need to convert missing values that have a -9999 etc to NA value')
print('look at flag info for Federal Land Manager data and see if any other cuts should be made')
print('make quality cuts on InDayLatDiff and InDayLonDiff')

#### Save cleaned file to .csv ####
input_mat2 <- input_mat_step12 # re-name data frame
rm(input_mat_step12)
print("summary of the data output by Clean_ML_Input_File.R:")
summary(input_mat2) # give summary of current state of data
print("file names still included")
unique(input_mat2$Source_File)
#write.csv(input_mat2,file = file.path(ProcessedData.directory,paste('cleaned_ML_input_',Sys.Date(),'_part_',processed_data_version,'.csv',sep = "")),row.names = FALSE)

write.csv(input_mat2,file = file.path(ProcessedData.directory,paste(file_sub_label,'.csv',sep = "")),row.names = FALSE)

#### Create a data frame with just lat, lon, and date ####
four_cols_w_duplicates <- input_mat2[,c("PM2.5_Lat","PM2.5_Lon","Datum","Date_Local")]
four_cols_data <- four_cols_w_duplicates[!duplicated(four_cols_w_duplicates),]
names(four_cols_data) <- c("Latitude","Longitude","Datum","Date")
#write.csv(four_cols_data,file = file.path(ProcessedData.directory,paste('Locations_Dates_of_PM25_Obs_from_clean_script_',Sys.Date(),'_part',processed_data_version,'.csv',sep = "")),row.names = FALSE)
write.csv(four_cols_data,file = file.path(ProcessedData.directory,paste(file_sub_label,'_Locations_Dates','.csv',sep = "")),row.names = FALSE)

rm(four_cols_data,four_cols_w_duplicates)

#### Create a data frame with just lat, and lon ####
three_cols_w_duplicates <- input_mat2[,c("PM2.5_Lat","PM2.5_Lon","Datum")]
three_cols_data <- three_cols_w_duplicates[!duplicated(three_cols_w_duplicates),]
names(three_cols_data) <- c("Latitude","Longitude","Datum")
#write.csv(three_cols_data,file = file.path(ProcessedData.directory,paste('Locations_PM25_Obs_from_clean_script_',Sys.Date(),'_part_',processed_data_version,'.csv',sep = "")),row.names = FALSE)
write.csv(three_cols_data,file = file.path(ProcessedData.directory,paste(file_sub_label,'_Locations','.csv',sep = "")),row.names = FALSE)

rm(three_cols_data,three_cols_w_duplicates)

#### End of file clean up ####
sink()
rm(input_mat2)
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory)
rm(writingcode.directory,computer_system,PythonProcessedData.directory)
rm(min_hourly_obs_daily,N_obs_original,SinkFileName,start_study_date,stop_study_date,this_source_file)
