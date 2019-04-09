# Process NAM data using parallel resources

#print("run Define_directories.R before this script") 
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

# start timer for code
start_code_timer <- proc.time()
print(paste("Start Process_NAM_data_step2_parallel.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(rNOMADS)
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/rNOMADS/rNOMADS_2.4.0.tar.gz" # https://support.rstudio.com/hc/en-us/articles/219949047-Installing-older-versions-of-packages
#install.packages(packageurl, repos=NULL, type="source")
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

#### Source functions I've written ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("NAM_Code.directory"),"NAM_processing_functions.R"))
source(file.path(define_file_paths.fn("NAM_Code.directory"),"extract_NAM_data_parallel_function.R"))
source(file.path(define_file_paths.fn("NAM_Code.directory"),"define_project_bounds_function.R"))
source(file.path(define_file_paths.fn("NAM_Code.directory"),"loop_NAM_run_times.parallel_function.R"))
source(file.path("estimate-pm25","General_Project_Functions","merging_data_functions.R"))

#source(file.path(writingcode.directory,"grb1to2_conversion_prep_function.R"))
#source(file.path(writingcode.directory,"which_type_of_grib_file_function.R"))
#source(file.path(writingcode.directory,"convert_grib1to2_function.R"))

#### Run function so that grib1>2 conversion will work ####
grb1to2_conversion_prep.fn()

#### define constants ####
#study_start_date <- as.Date("20080101",format="%Y%m%d") # first date in study period
study_start_date_step <- as.Date(define_study_constants.fn("start_date"))
study_start_date <- format(study_start_date_step,"%Y%m%d")
print(study_start_date)
#rm(study_start_date_step)
#study_stop_date  <- as.Date("20181231",format="%Y%m%d") # last date in study period
study_stop_date_step <- as.Date(define_study_constants.fn("end_date"))
study_stop_date <- format(study_stop_date_step,"%Y%m%d")
print(study_stop_date)
#rm(study_stop_date_step)
print("Update Batch Date")
batch_date <- as.Date("2019-04-05") # used to know whether data has already been ran. This
print(batch_date)

Date_vector <- seq(study_start_date_step,study_stop_date_step, by = "day") # vector of all dates for which meteo data will be extracted
n_days <- length(Date_vector)
#day_counter <- 1:n_days
forecast_times <- 00 # reanalysis - anything else would be a forecast
with_pause <- 0 # 1 = pause, 0=no pause
NAM.directory <- define_file_paths.fn("NAM.directory")
uppermost.directory <- define_file_paths.fn("uppermost.directory")
ProcessedData.directory <- define_file_paths.fn("ProcessedData.directory")

# Select which model to use
Model_in_use_abbrev <-  "namanl" # NAM Analysis
#fix#NAM_processed_data_version <- "bc"
#fix#sub_folder <- paste("NAM_data_part_",NAM_processed_data_version,sep = "")
#fix#output_file_name_sub <- paste("NAM_Step2_part_",NAM_processed_data_version,sep = "")
NAM_folder <- "NAM_data"
input_sub_folder <- "NAM_Step1"
output_sub_folder <- "NAM_Step2"

#### Load list of meteorology variables of interest ####
this_source_file <- paste("MeteoVariablesNAM.csv")
MeteoVarsMultiType <- read.csv(file.path(define_file_paths.fn("NAM_Code.directory"),this_source_file))
rm(this_source_file)

#### Load Date/Locations of PM2.5 Obs ####
#this_location_date_file <- 'Locations_Dates_of_PM25_Obs_DeDuplicate'
#this_location_date_file <- "NAM_Step1_part_bc_Locations_Dates_wNextDay"
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/list.files.html
input_files <- list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,"."), pattern = NULL, all.files = FALSE,
                            full.names = FALSE, recursive = FALSE,
                            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
print(input_files)

#### Load _wNextDay data ####
lapply_output <- lapply(1:length(input_files), function(x) { # start lapply function
  this_location_date_file <- input_files[x] # get file name
  PM25DateLoc <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,paste(this_location_date_file,sep = "")))
  PM25DateLoc$Date <- as.Date(PM25DateLoc$Date) # recognize date column as dates
  PM25DateLoc <- replace_column_names.fn(df_in = PM25DateLoc, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
  PM25DateLoc <- replace_column_names.fn(df_in = PM25DateLoc, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
  Dates_Locations <- PM25DateLoc[ ,c("Latitude","Longitude","Date")]
  
  return(Dates_Locations) # return names of files processed
  }) # end lapply function
Merged_Dates_Locations <- do.call("rbind", lapply_output) #concatinate the output from each iteration
rm(input_files,input_sub_folder)
#Merged_Dates_Locations <- Merged_Dates_Locations_step[!duplicated(Merged_Dates_Locations_step), ]

#### Run the parallel loop ####
# Calculate the number of cores
n_cores <- detectCores() - 1 # 2 #
print(paste(n_cores,"cores available for parallel processing",sep = " "))

# Initiate cluster
this_cluster <- makeCluster(n_cores)

# export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
clusterExport(cl = this_cluster, varlist = c("extract_NAM_data.parallel.fn","which_type_of_grib_file.fn",
                                             "convert_grib1to2.fn","define_project_bounds.fn",
                                             "Merged_Dates_Locations","NAM.directory","output_sub_folder","uppermost.directory","with_pause",
                                             "ProcessedData.directory","batch_date",
                                             "Date_vector","MeteoVarsMultiType","forecast_times","Model_in_use_abbrev",
                                             "NAM_folder","output_sub_folder"), envir = .GlobalEnv)

# send necessary librarys to each parallel worker
clusterEvalQ(cl = this_cluster, library(rNOMADS)) # copy this line and call function again if another library is needed
#clusterEvalQ(cl = this_cluster, library(audio)) # copy this line and call function again if another library is needed

# # run function loop_NAM_run_times.parallel.fn in parallel
par_out <- parLapply(cl = this_cluster,X = 1:n_days, fun = loop_NAM_run_times.parallel.fn)
#par_out <- parLapply(this_cluster,X = 1:n_days, fun = loop_NAM_run_times.parallel.fn,
#      Date_vector = Date_vector,
#      ProcessedData.directory=define_file_paths.fn("ProcessedData.directory"), #this_location_date_file=this_location_date_file,
#      MeteoVarsMultiType = MeteoVarsMultiType, forecast_times = 00,
#      PM25DateLoc_time = Merged_Dates_Locations, Model_in_use_abbrev =  "namanl", NAM_folder = NAM_folder, sub_folder = output_sub_folder)



# End use of parallel computing #
stopCluster(this_cluster)
rm(this_cluster)

#### Serial version of code ####
for (day_counter in 500:n_days) {
loop_NAM_run_times.parallel.fn(day_counter)#, Date_vector = Date_vector,
                               #ProcessedData.directory=define_file_paths.fn("ProcessedData.directory"),
                               #this_location_date_file=this_location_date_file,
                               #MeteoVarsMultiType = MeteoVarsMultiType, forecast_times = 00,
                               #PM25DateLoc_time = PM25DateLoc, Model_in_use_abbrev =  "namanl", sub_folder = sub_folder)
}

#### Clear variables ####
rm(study_start_date, study_stop_date, forecast_times, Model_in_use_abbrev)
rm(PM25DateLoc,this_location_date_file,MeteoVarsMultiType)

#### End of file cleanup
#rm(uppermost.directory,output.directory)
#rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
#rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
#rm(writingcode.directory,computer_system,NAM.directory,PythonProcessedData.directory)

print(paste("Process_NAM_data_step2_parallel.R completed at",Sys.time(),sep = " "))
# stop the timer
proc.time() - start_code_timer
