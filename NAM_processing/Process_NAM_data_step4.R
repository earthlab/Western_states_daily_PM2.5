# Process_NAM_data_step4.R calculate daily values combining the 4 timesteps for each day

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

#### Libraries ####
library(parallel)
library(lubridate) # https://cran.r-project.org/web/packages/lubridate/lubridate.pdf
library(lutz) # look up time zone https://cran.r-project.org/web/packages/lutz/index.html 

#### Define Constants ####
NAM_folder <- "NAM_data" # define folder for NAM data
input_sub_folder <- "NAM_Step3" # define location of input files
output_sub_folder <- "NAM_Step4" # define location for output files
output_file_name <- paste("NAM_Step4_processed_",Sys.Date(),sep = "") # define name of output file

#### Load and Process Data ####
# determine which file from step 3 is most recent
file_name_pattern <- "\\.csv$" # only looking for .csv files (don't want to pick up the sub-folder)
this_file_list <- list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,"."), pattern = file_name_pattern, all.files = FALSE,
                                  full.names = FALSE, recursive = FALSE,
                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE) # get list of all .csv file in this folder
print(paste("There are ",length(this_file_list),"files for NAM Step 3 data")) # optional output statement
date_list <- unlist(lapply(this_file_list, function(x){ # start lapply and start defining function used in lapply
  processed_date <- substr(x,nchar(x)-13,nchar(x)-4) # identify the time stamp for the file in this iteration
  return(processed_date) # return the new file name so a new list of files can be created
}))
recent_processed_date <- max(as.Date(date_list)) # which date is the most recent file
which_recent_file <- which(date_list == recent_processed_date) # locate the file name for the most recent file
recent_file_name <- this_file_list[which_recent_file] # most recent file name
print(paste(recent_file_name,"is the most recent file and will be used"))
# load the data created in step 3, which has all of the observations for the 4 timesteps per day in one data frame
NAM_data_step <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,recent_file_name)) # open data file
NAM_data_step$Date <- as.Date(NAM_data_step$Date)
all_dates_UTC <- unique(NAM_data_step$Date)
rm(file_name_pattern,this_file_list,date_list,recent_processed_date,which_recent_file,recent_file_name) # clear variables

#### Set up for parallel processing ####
n_cores <- detectCores() - 1 # Calculate the number of cores
print(paste(n_cores,"cores available for parallel processing",sep = " "))
this_cluster <- makeCluster(n_cores) # # Initiate cluster
clusterExport(cl = this_cluster, varlist = c("NAM_data_step","all_dates_UTC"), envir = .GlobalEnv) # export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
clusterEvalQ(cl = this_cluster, library(lubridate)) # copy this line and call function again if another library is needed
clusterEvalQ(cl = this_cluster, library(lutz)) # copy this line and call function again if another library is needed

#### Parallel Code ####
#serial version: NAM_data$TimeZone <- tz_lookup_coords(lat = NAM_data$Latitude, lon = NAM_data$Longitude, method = "accurate")
par_output <- parLapply(this_cluster,X = 1:length(all_dates_UTC), fun = function(x){ # call parallel function
  # isolate all data for this date
  which_this_date <- which(NAM_data_step$Date == all_dates_UTC[x])
  NAM_data_date <- NAM_data_step[which_this_date, ]
  
  ## add a column indicating the time in the relevant time zone - see page 58-72
  NAM_data_date$TimeZone <- tz_lookup_coords(lat = NAM_data_date$Latitude, lon = NAM_data_date$Longitude, method = "accurate")
  
  # create a vector of the UTC time stamps
  UTC_date_vec <- unlist(lapply(1:dim(NAM_data_date)[1], function(x){ # start lapply and start defining function used in lapply
    this_date <- as.character(NAM_data_date[x,c("Date")])
    this_UTC_time <- as.character(NAM_data_date[x,"Time.UTC"])
     if (nchar(this_UTC_time)==1) {
       this_UTC_time <- paste("0",this_UTC_time,sep = "")
     }
    date_stamp_UTC <- paste(this_date," ",this_UTC_time,":00:00",sep = "")
    #print(this_date)
    #print(this_UTC_time)
    #print(date_stamp_UTC)
    return(date_stamp_UTC) # return the new file name so a new list of files can be created
  }))#, ProcessedData.directory,sub_folder)
  NAM_data_date$UTC.Date.Time <- ymd_hms(UTC_date_vec, tz = "UTC") ## add a column giving the time stamp in UTC
  rm(UTC_date_vec) # clear variable

  # find the local time for each row of data
  Local_time_vec <- unlist(lapply(1:dim(NAM_data_date)[1],function(x) {
    this_time <- NAM_data_date[x,"UTC.Date.Time"]
    this_tz <- NAM_data_date[x,"TimeZone"]
    this_local_time <- with_tz(this_time,this_tz)
    this_local_time_char <- as.character(this_local_time)
    #print(this_time)
    #print(this_tz)
    #print(this_local_time)
    #print(this_local_time_char)
    return(this_local_time_char)
  }))
  
  ## add column indicating the date/time in the relevant time zone - see page 58-72
  NAM_data_date$Local.Date.Time <- as_datetime(Local_time_vec)#, "%Y-%m-%d HH:MM:SS")
  NAM_data_date$Local.Date <- as.Date(Local_time_vec,"%Y-%m-%d")
  rm(Local_time_vec) # clear variable
  
  # # create a vector of the UTC time stamps
  #this_date <- as.character(NAM_data[x,c("Date")])
  #this_UTC_time <- as.character(NAM_data[x,"Time.UTC"])
  #if (nchar(this_UTC_time)==1) {
  #  this_UTC_time <- paste("0",this_UTC_time,sep = "")
  #}
  #date_stamp_UTC <- paste(this_date," ",this_UTC_time,":00:00",sep = "")
  #NAM_data_date$UTC.Date.Time <- ymd_hms(date_stamp_UTC, tz = "UTC") ## add a column giving the time stamp in UTC
  #rm(date_stamp_UTC)

  return(NAM_data_date) # output from function
}) # end parallel function

#### concatinate the output from each iteration ####
NAM_data <- do.call("rbind", par_output)
print(unique(NAM_data$TimeZone))
# if needed, see page 14 of https://cran.r-project.org/web/packages/lubridate/lubridate.pdf for info about a daylight savings time indicator

#### End use of parallel computing #####
stopCluster(this_cluster) # stop the cluster

# # create a vector of the UTC time stamps
# UTC_date_vec <- unlist(lapply(1:dim(NAM_data)[1], function(x){ # start lapply and start defining function used in lapply
#   this_date <- as.character(NAM_data[x,c("Date")])
#   this_UTC_time <- as.character(NAM_data[x,"Time.UTC"])
#    if (nchar(this_UTC_time)==1) {
#      this_UTC_time <- paste("0",this_UTC_time,sep = "")
#    }
#   date_stamp_UTC <- paste(this_date," ",this_UTC_time,":00:00",sep = "")
#   #print(this_date)
#   #print(this_UTC_time)
#   #print(date_stamp_UTC)
#   return(date_stamp_UTC) # return the new file name so a new list of files can be created
# }))#, ProcessedData.directory,sub_folder)
# NAM_data$UTC.Date.Time <- ymd_hms(UTC_date_vec, tz = "UTC") ## add a column giving the time stamp in UTC
# rm(UTC_date_vec) # clear variable

# # find the local time for each row of data
# Local_time_vec <- unlist(lapply(1:dim(NAM_data)[1],function(x) {
# this_time <- NAM_data[x,"UTC.Date.Time"]
# this_tz <- NAM_data[x,"TimeZone"]
# this_local_time <- with_tz(this_time,this_tz)
# this_local_time_char <- as.character(this_local_time)
# #print(this_time)
# #print(this_tz)
# #print(this_local_time)
# #print(this_local_time_char)
# return(this_local_time_char)
# }))

# ## add column indicating the date/time in the relevant time zone - see page 58-72
# NAM_data$Local.Date.Time <- as_datetime(Local_time_vec)#, "%Y-%m-%d HH:MM:SS")
# NAM_data$Local.Date <- as.Date(Local_time_vec,"%Y-%m-%d")
# rm(Local_time_vec) # clear variable

# write step 4 data to csv file
write.csv(NAM_data,file = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,paste(output_file_name,".csv",sep = "")),row.names = FALSE) # write data to file

# clear variables
rm(NAM_data,NAM_folder,input_sub_folder,output_sub_folder,output_file_name,working.directory)
