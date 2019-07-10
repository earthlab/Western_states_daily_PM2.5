# Process NAM data

print("run Define_directories.R before this script") 

# start timer for code
start_code_timer <- proc.time()
print(paste("Start Process_PM25_data_step1.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(rNOMADS)
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

#### Call Load Functions that I created ####
source(file.path(NAM_Code.directory,"NAM_processing_functions.R"))
#source(file.path(NAM_Code.directory,"extract_NAM_data_function.R"))
#source(file.path(writingcode.directory,"grb1to2_conversion_prep_function.R"))
  #source(file.path(writingcode.directory,"which_type_of_grib_file_function.R"))
  #source(file.path(writingcode.directory,"convert_grib1to2_function.R"))

function_list <- c("grb1to2_conversion_prep.fn", "extract_NAM_data.fn", "which_type_of_grib_file.fn", "convert_grib1to2.fn")
directories_vector <- c("ProcessedData.directory")
#### Set up code for running in parallel ####
# Calculate the number of cores
#no_cores <- detectCores() - 1
# Initiate cluster
#cl <- makeCluster(no_cores)

#### Run function so that grib1>2 conversion will work ####
grb1to2_conversion_prep.fn()

#### define constants ####
forecast_times <- 00 # reanalysis - anything else would be a forecast
# Select which model to use
Model_in_use_abbrev <-  "namanl" # NAM Analysis
NAM_processed_data_version <- "bc"
sub_folder <- paste("NAM_data_part_",NAM_processed_data_version,sep = "")
output_file_name_sub <- paste("NAM_Step2_part_",NAM_processed_data_version,sep = "")

#### Load list of meteorology variables of interest ####
this_source_file <- paste("MeteoVariablesNAM.csv")
MeteoVarsMultiType <- read.csv(file.path(NAM_Code.directory,this_source_file))
rm(this_source_file)

#### Load _wNextDay data ####
this_location_date_file <- "NAM_Step1_part_bc_Locations_Dates_wNextDay" #'Locations_Dates_of_PM25_Obs_DeDuplicate'
PM25DateLoc <- read.csv(file.path(ProcessedData.directory,sub_folder,paste(this_location_date_file,".csv",sep = "")))
PM25DateLoc$Date <- as.Date(PM25DateLoc$Date, format = "%Y-%m-%d") # recognize date column as dates

# find the first and last dates in the data
study_start_date <- min(PM25DateLoc$Date) #as.Date("20080101",format="%Y%m%d") # first date in study period
print(study_start_date)
study_stop_date  <- as.Date("20080104",format="%Y%m%d") #max(PM25DateLoc$Date) #as.Date("20080101",format="%Y%m%d") # last date in study period
print(study_stop_date)

# export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
clusterExport(cl = this_cluster, varlist = c(function_list, directories_vector), envir = .GlobalEnv)


# put in a function so it could run both where we have monitors and where we
# want to predict, just uses different input files
print("fix code so it actually runs in parallel")
#### Cycle through all .grb files for processing ####
date_vec <- seq(as.Date(study_start_date), as.Date(study_stop_date), by="days")
print(date_vec)
#date_i = 1:length(date_vec)
for(date_i in 1:2) {
extract_NAM_data.fn(date_i = 1, date_vec = date_vec, ProcessedData.directory = ProcessedData.directory, PM25DateLoc = PM25DateLoc, output_file_name_sub = output_file_name_sub, MeteoVarsMultiType = MeteoVarsMultiType, 
                                study_start_date = study_start_date, study_stop_date = study_stop_date, forecast_times = forecast_times, 
                                Model_in_use_abbrev = Model_in_use_abbrev)
}
#extract_NAM_data.fn(ProcessedData.directory = ProcessedData.directory, PM25DateLoc = PM25DateLoc, output_file_name_sub = output_file_name_sub, MeteoVarsMultiType = MeteoVarsMultiType, 
#                    study_start_date = study_start_date, study_stop_date = study_stop_date, forecast_times = forecast_times, 
#                    Model_in_use_abbrev = Model_in_use_abbrev)

#### Clear Variables
rm(study_start_date,study_stop_date,this_location_date_file,forecast_times,Model_in_use_abbrev)
rm(MeteoVarsMultiType)

# #### End of file cleanup
# rm(uppermost.directory,output.directory)
# rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
# rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
# rm(writingcode.directory,computer_system,NAM.directory,PythonProcessedData.directory)

#### End use of parallel computing ####
#stopCluster(cl)
