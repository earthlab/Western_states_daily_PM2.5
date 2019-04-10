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
  #print(processed_date)
  #class(processed_date)
  return(processed_date) # return the new file name so a new list of files can be created
}))
recent_processed_date <- max(as.Date(date_list))
which_recent_file <- which(date_list == recent_processed_date)
recent_file_name <- this_file_list[which_recent_file]
print(paste(recent_file_name,"is the most recent file and will be used"))
# load the data created in step 3, which has all of the observations for the 4 timesteps per day in one data frame
NAM_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,recent_file_name)) # open data file
rm(file_name_pattern,this_file_list,date_list,recent_processed_date,which_recent_file,recent_file_name)

## add a column indicating the time in the relevant time zone - see page 58-72
#NAM_data$TimeZone <- NA
# use sf option for lutz
#point_interest <- NAM_Step3[1, c("Lat","Lon")]
#tz_lookup_coords(lat = point_interest$Lat, lon = point_interest$Lon, method = "accurate")
NAM_data$TimeZone <- tz_lookup_coords(lat = NAM_data$Latitude, lon = NAM_data$Longitude, method = "accurate")
print(unique(NAM_data$TimeZone))
# if needed, see page 14 of https://cran.r-project.org/web/packages/lubridate/lubridate.pdf for info about a daylight savings time indicator?

## add a column giving the time stamp in UTC
UTC_date_vec <- unlist(lapply(1:dim(NAM_data)[1], function(x){ # start lapply and start defining function used in lapply
this_date <- as.character(NAM_data[x,c("Date")])
this_UTC_time <- as.character(NAM_data[x,"Time.UTC"])
   if (nchar(this_UTC_time)==1) {
     this_UTC_time <- paste("0",this_UTC_time,sep = "")
   }
  date_stamp_UTC <- paste(this_date," ",this_UTC_time,":00:00",sep = "")
  #print(this_date)
  #print(this_UTC_time)
  #print(date_stamp_UTC)
  return(date_stamp_UTC) # return the new file name so a new list of files can be created
}))#, ProcessedData.directory,sub_folder)
NAM_data$UTC.Date.Time <- ymd_hms(UTC_date_vec, tz = "UTC")

# find the local time for each row of data
Local_time_vec <- unlist(lapply(1:dim(NAM_data)[1],function(x) {
this_time <- NAM_data[x,"UTC.Date.Time"]
this_tz <- NAM_data[x,"TimeZone"]
this_local_time <- with_tz(this_time,this_tz)
this_local_time_char <- as.character(this_local_time)
#print(this_time)
#print(this_tz)
#print(this_local_time)
#print(this_local_time_char)
return(this_local_time_char)
}))

## add a column indicating the time in the relevant time zone - see page 58-72
NAM_data$Local.Date.Time <- as_datetime(Local_time_vec)#, "%Y-%m-%d HH:MM:SS")
NAM_data$Local.Date <- as.Date(Local_time_vec,"%Y-%m-%d")

# write step 4 data to csv file
write.csv(NAM_data,file = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,paste(output_file_name,".csv",sep = "")),row.names = FALSE) # write data to file

#### Start Obsolete Code ####

# 
# NAM_data$UTC.Date.Time <- ymd_hms(UTC_date_vec, tz = "UTC")

# #print(Local_time_vec)
# #unlist(Local_time_vec)
# 
# Local_time_vec_unlisted <- NA 
# for (x in 1:length(Local_time_vec)) {
#   Local_time_vec_unlisted[x] <- Local_time_vec[[x]]
# }
# print(Local_time_vec_unlisted)
# 
# this_time <- NAM_data[1,"UTC.Date.Time"]
# print(this_time)
# this_tz <- NAM_data[1,"TimeZone"]
# print(this_tz)
# this_local_time <- with_tz(this_time,this_tz)
# print(this_local_time)
# 
# x <- ymd_hms("2009-08-07 00:00:01", tz = "America/New_York")
# print(with_tz(x,"GMT"))
# x <- ymd_hms("2009-08-07 00:00:01", tz = "UTC")
# 
# ## add a column indicating the time in the relevant time zone - see page 58-72
# NAM_data$Local.Date.Time <- with_tz(NAM_data$UTC.Date.Time, NAM_data$TimeZone)
# print(with_tz(x,"GMT"))

#"2009-08-07 00:00:01"
# add a column indicating the time stamp in UTC
#UTC_date_vec <- unlist(lapply(NAM_data$Date, function(x){ # start lapply and start defining function used in lapply
#  #date_stamp_UTC <- paste(x)
#  date_stamp_UTC <- paste(x," ",,sep = "")
#  return(date_stamp_UTC) # return the new file name so a new list of files can be created
#}))#, ProcessedData.directory,sub_folder)

#NAM_data$UTC.Date.Time <- paste()
#NAM_data$UTC.Date.Time <- ymd_h(paste(NAM_data$Date,NAM_data$Time.UTC), tz = "UTC")
#NAM_data$UTC.Date.Time <- as.Date(NAM_data$UTC.Date.Time)

#UTC_date_vec <- unlist(lapply(1:dim(NAM_data)[1]), function(row_i){ # start definition of anonymous function
#  date_stamp_UTC <- paste(NAM_data[row_i,c("Date")])
#  #this_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,file_name)) # load Step 2 file
#  #this_n_cols <- dim(this_data)[2] # determine the number of columns
#  #rm(this_data) # clear variable
#  return(date_stamp_UTC) # output from function
#}) # n_cols <- unlist(lapply(1:length(this_file_list_step), function(file_i){

# ## add a column indicating the time in the relevant time zone - see page 58-72
# #NAM_data$Local.Date.Time <- with_tz(time = NAM_data$UTC.Date.Time, tz = NAM_data$TimeZone)
# #print(unique(NAM_data$Local.Date.Time))
# 
# x <- ymd_hms("2009-08-07 00:00:01", tz = "America/New_York")
# x <- ymd_hms("2009-08-07 00:00:01", tz = "UTC")
# x <- ymd_hms("2009-08-07 00:00:01", tz = "UTC")
# 
# print(with_tz(x,"GMT"))

# #NAM_data$Local.Date <- date(NAM_data$Local.Date.Time)
# #1:dim(NAM_data)[1]
# for (x in 1:dim(NAM_data)[1]) {
#   print(paste("x =",x,"of",dim(NAM_data)[1]))
#   print(paste(NAM_data[x,c("UTC.Date.Time")],NAM_data[x,c("TimeZone")]))
#   NAM_data[x,c("Local.Date.Time")] <- with_tz(time = NAM_data[x,c("UTC.Date.Time")], tzone = NAM_data[x,c("TimeZone")])
#   print(NAM_data[x,c("Local.Date.Time")])
# }

# # write step 4 data to csv file
# write.csv(NAM_data,file = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,paste(output_file_name,".csv",sep = "")),row.names = FALSE) # write data to file
