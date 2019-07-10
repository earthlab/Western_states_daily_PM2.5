# Process NAM data Step 2 using parallel resources
# this step downloads NAM files, extracts the relevant data, and deletes the original file
# the batch date will need to be updated if new dates/locations have been added to the PM25 data or there are
# new locations to predict to

#### Clear variables and sinks; define working directory ####
rm(list  =  ls()) # clear all variables
options(warn  =  2) # throw an error when there's a warning and stop the code from running further
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which  =  dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) { # make sure it isn't outputting to any sink files
  sink()
} # while (sink.number()>0) {
working.directory  <-  "/home/rstudio" # define working directory
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
source(file.path(define_file_paths.fn("NAM_Code.directory"),"rNOMADS_MMM_edited.R"))
source(file.path("estimate-pm25","General_Project_Functions","merging_data_functions.R"))

grb1to2_conversion_prep.fn() # Run function so that grib1>2 conversion will work

#### define constants ####
study_start_date_step <- as.Date(define_study_constants.fn("start_date")) # first date in study period
study_start_date <- format(study_start_date_step,"%Y%m%d") # change date format
print(study_start_date) # print to screen
study_stop_date_step <- as.Date(define_study_constants.fn("end_date"))+1 # last date in study period - and then add one day to account for time zone differences
study_stop_date <- format(study_stop_date_step,"%Y%m%d") # change date format
print(study_stop_date) # print to screen
print("***Update Batch Date if processing new dates/locations for PM2.5 data or new locations of interest for PM25 predictions***")
batch_date <- define_study_constants.fn("NAM_batch_date") #as.Date("2019-07-10") #as.Date("2019-04-05") # used to know whether data has already been ran. This
print(batch_date)

Date_vector <- seq(study_start_date_step,study_stop_date_step, by = "day") # vector of all dates for which meteo data will be extracted
n_days <- length(Date_vector) # how many days need to be processed?
forecast_times <- 00 # reanalysis - anything else would be a forecast
with_pause <- 0 # 1 = pause, 0=no pause - this is mostly obsolete - I've tried to remove all pause commands, but leaving this in case I missed any
NAM.directory <- define_file_paths.fn("NAM.directory")
uppermost.directory <- define_file_paths.fn("uppermost.directory")
ProcessedData.directory <- define_file_paths.fn("ProcessedData.directory")
Prediction_Locations_folder <- "PM25_Locations_Dates"
Prediction_Locations_file <- "West_prediction_locations.csv"

# Select which model to use
Model_in_use_abbrev <-  "namanl" # NAM Analysis
NAM_folder <- "NAM_data"
input_sub_folder <- "NAM_Step1"
output_sub_folder <- "NAM_Step2"
# create NAM_Step2 folder if it doesn't already exist
if(dir.exists(file.path(define_file_paths.fn("ProcessedData.directory"),"NAM_data","NAM_Step2")) == FALSE) { # create directory if it doesn't already exist
  dir.create(file.path(define_file_paths.fn("ProcessedData.directory"),"NAM_data","NAM_Step2"))
} # if(exists(file.path(define_file_paths.fn("ProcessedData.directory"),"NAM_data","NAM_Step2")) == FALSE) { # create directory if it doesn't already exist

# create folder for NAM files that are downloaded and then deleted
if(dir.exists(file.path(uppermost.directory,"NAM_data_orig")) == FALSE) { # create directory if it doesn't already exist
  dir.create(file.path(uppermost.directory,"NAM_data_orig"))
} # if(exists(file.path(uppermost.directory,"NAM_data_orig")) == FALSE) { # create directory if it doesn't already exist

# start report for files/dates with issues
ReportFileName=file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,paste("Rgenerated_Report_NAM_Step2_issues_batch",batch_date,".csv",sep = "")) # name of file for latex code images
sink(file = ReportFileName, append = FALSE) # create file
write.table(paste("day_counter","this_model.date","this_model.run","Issue.Message",sep = ","),row.names = FALSE, col.names = FALSE, sep = "", quote = FALSE) # create header in file
sink() # close file (it will be opened again later in code)

#### Load list of meteorology variables of interest ####
this_source_file <- paste("MeteoVariablesNAM.csv")
MeteoVarsMultiType <- read.csv(file.path(define_file_paths.fn("NAM_Code.directory"),this_source_file))
rm(this_source_file)

#### Load Date/Locations of PM2.5 Obs ####
# include all files located in the NAM_Step1 folder
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
Merged_Dates_Locations_step <- do.call("rbind", lapply_output) #concatinate the output from each iteration
rm(input_files,input_sub_folder)
#Merged_Dates_Locations <- Merged_Dates_Locations_step[!duplicated(Merged_Dates_Locations_step), ]

# load the locations-only files - where we want PM25 predictions - these will be expanded to all dates in study period (and next day)
Prediction_Locations_step <- read.csv(file.path(ProcessedData.directory,Prediction_Locations_folder,Prediction_Locations_file))
Prediction_Locations_step <- replace_column_names.fn(Prediction_Locations_step,old_col_name = "Lat", new_col_name = "Latitude")
Prediction_Locations_step <- replace_column_names.fn(Prediction_Locations_step,old_col_name = "Lon", new_col_name = "Longitude")

# get the PM25 locations/dates and the prediction locations to have the same headers
goal_header <- unique(c(names(Merged_Dates_Locations_step), names(Prediction_Locations_step)))
Merged_Dates_Locations <- harmonize_column_names.fn(df_in = Merged_Dates_Locations_step, goal_header = goal_header)
Prediction_Locations <- harmonize_column_names.fn(df_in = Prediction_Locations_step, goal_header = goal_header)
rm(Merged_Dates_Locations_step, Prediction_Locations_step)

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
                                             "NAM_folder","output_sub_folder","CheckNOMADSArchive_MMM","ArchiveGribGrab_MMM","ReportFileName",
                                             "Prediction_Locations","harmonize_column_names.fn"), envir = .GlobalEnv)

# send necessary librarys to each parallel worker
clusterEvalQ(cl = this_cluster, library(rNOMADS)) # copy this line and call function again if another library is needed

# # run function loop_NAM_run_times.parallel.fn in parallel
#n_days <- 3
#X = 1:n_days
#X <- 1217#3070#1929#1217
#X <- 328
#day_counter <- 328
# 1191:1220
par_out <- parLapply(cl = this_cluster,X = 1, fun = loop_NAM_run_times.parallel.fn)
#par_out <- parLapply(cl = this_cluster,X = 1:n_days, fun = loop_NAM_run_times.parallel.fn) #UNCOMMENT

# End use of parallel computing #
stopCluster(this_cluster)
rm(this_cluster)

# #### Serial version of code ####
# for (day_counter in 1215:1220) {
#   print(paste("day_counter =",day_counter))
# loop_NAM_run_times.parallel.fn(day_counter)
# }

#### Clear variables ####
rm(study_start_date, study_stop_date, forecast_times, Model_in_use_abbrev)
rm(MeteoVarsMultiType) # PM25DateLoc,this_location_date_file,

#### End of file cleanup
#rm(uppermost.directory,output.directory)
#rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
#rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
#rm(writingcode.directory,computer_system,NAM.directory,PythonProcessedData.directory)

print(paste("Process_NAM_data_step2_parallel.R completed at",Sys.time(),sep = " "))
# stop the timer
proc.time() - start_code_timer
