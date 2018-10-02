# process PM2.5 data step 1: combine the various PM2.5 data sources
 
print("run Define_directories.R before this script") 

# start timer for code
start_code_timer <- proc.time()
print(paste("Start Process_PM25_data_step1.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"process_PM25_parallal_wrapper_function.R"))
source(file.path(writingcode.directory,"process_PM25_EPA_data_source_function.R"))
source(file.path(writingcode.directory,"process_PM25_Fire_Cache_data_source_function.R"))
source(file.path(writingcode.directory,"Fire_Cache_specific_functions.R"))
source(file.path(writingcode.directory,"separate_character_vec_at_comma_function.R"))
source(file.path(writingcode.directory,"State_Abbrev_Definitions_function.R"))

#### define constants and variables needed for all R workers ####
n_data_sets <- 1#3 # change to higher number as more code is written
start_study_year <- 2008
stop_study_year <- 2014
voltage_threshold_upper <- 17
voltage_threshold_lower <- 11

input_header <-  c('PM2.5_Obs','PM2.5_Lat','PM2.5_Lon','Datum','Date_Local','Year','Month','Day','State_Code','County_Code','Site_Num','Parameter_Code','POC','Parameter_Name','Sample_Duration','Pollutant_Standard','Units_of_Measure','Event_Type','Observation_Count','Observation_Percent','1st_Max_Value','1st_Max_Hour','AQI','Method_Code','Method_Name','PM25_Station_Name','Address','State_Name','County_Name','City_Name','CBSA_Name','Date_of_Last_Change', # columns in AQS data
                   'State_Abbrev','Winter','Data_Source_Name_Display','Data_Source_Name_Short','Data_Source_Counter','Source_File','Composite_of_N_rows','N_Negative_Obs', # other columns to include
                   "flg.Lat","flg.Lon","Type","flg.Type","flg.Site_Num","flg.PM25_Obs","l/m Ave. Air Flw", # DRI variables
                   "flg.AirFlw","Deg C Av Air Temp","flg.AirTemp","% Rel Humidty","flg.RelHumid","mbar Barom Press","flg.Barom Press", # DRI variables
                   "deg C Sensor  Int AT","flg.deg C Sensor Int AT","% Sensor Int RH","flg.%SensorIntRH", # DRI variables
                   "Wind Speed m/s","flg.WindSpeed","Battery Voltage volts","flg.BatteryVoltage","Alarm","flg.Alarm", # DRI variables
                   "InDayLatDiff","InDayLonDiff","PlottingColor")

#### Run the parallel loop ####
n_cores <- detectCores() - 1 # Calculate the number of cores
print(paste(n_cores,"cores available for parallel processing",sep = " "))

# Initiate cluster
this_cluster <- makeCluster(n_cores)

# export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
clusterExport(cl = this_cluster, varlist = c("start_study_year","stop_study_year","voltage_threshold_upper","voltage_threshold_lower","input_header",
                                             "AQSData.directory",
                                             "process_PM25_EPA_data_source.fn","separate_character_vec_at_comma.fn","State_Abbrev_Definitions.fn",
                                             "Fire_Cache_consolidate_file_header.fn"), envir = .GlobalEnv)

# send necessary libraries to each parallel worker
#clusterEvalQ(cl = this_cluster, library(rNOMADS)) # copy this line and call function again if another library is needed

# run function loop_NAM_run_times.parallel.fn in parallel
par_output <- parLapply(this_cluster,X = 1:n_data_sets, fun = process_PM25_parallal_wrapper.fn,
                        input_header = input_header, ProcessedData.directory = ProcessedData.directory,
                        AQSData.directory = AQSData.directory)

# End use of parallel computing #
stopCluster(this_cluster)
rm(this_cluster, n_cores)

#### concatinate the output from each iteration ####

# first data set
#input_mat1 <- par_output[[1]]
# subsequent data sets  

input_mat1 <- do.call("rbind", par_output)

#### Save input_mat1 to csv file ####

write.csv(input_mat1,file = file.path(ProcessedData.directory,paste('combined_ML_input',Sys.Date(),'.csv',sep = "")),row.names = FALSE)

#### Clear variables ####
rm(n_data_sets, start_study_year, stop_study_year, voltage_threshold_upper, voltage_threshold_lower, input_header)
rm(par_output, input_mat1)

#### End of file cleanup
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
rm(writingcode.directory,computer_system,NAM.directory,PythonProcessedData.directory)

print(paste("Process_PM25_data_step1.R completed at",Sys.time(),sep = " "))
# stop the timer
proc.time() - start_code_timer
rm(start_code_timer)
