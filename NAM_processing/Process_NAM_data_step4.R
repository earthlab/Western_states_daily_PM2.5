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
#part_number <- "bc"
#NAM_processed_data_version <- "bc"
#sub_folder <- paste("NAM_data_part_",NAM_processed_data_version,sep = "") # subfolder withing ProcessedData.directory
NAM_folder <- "NAM_data" # define folder for NAM data
input_sub_folder <- "NAM_Step3" # define location of input files
output_sub_folder <- "NAM_Step4" # define location for output files
output_file_name <- paste("NAM_Step4_processed_",Sys.Date(),sep = "") # define name of output file

#### Load and Process Data ####
# determine which file from step 3 is most recent
#this_file_list_step <- list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,"."), pattern = NULL, all.files = FALSE,
#                                  full.names = FALSE, recursive = FALSE,
#                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
file_name_pattern <- "\\.csv$"
this_file_list <- list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,"."), pattern = file_name_pattern, all.files = FALSE,
                                  full.names = FALSE, recursive = FALSE,
                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
print(paste("There are ",length(this_file_list),"files for NAM Step 3 data")) # optional output statement
date_list <- unlist(lapply(this_file_list, function(x){ # start lapply and start defining function used in lapply
  #processed_date <- as.Date(substr(x,nchar(x)-13,nchar(x)-4),"%Y-%m-%d") # identify the time stamp for the file in this iteration
  processed_date <- substr(x,nchar(x)-13,nchar(x)-4) # identify the time stamp for the file in this iteration
  #print(processed_date)
  class(processed_date)
  return(processed_date) # return the new file name so a new list of files can be created
}))
#date_list <- as.Date(date_list)
recent_processed_date <- max(as.Date(date_list))
which_recent_file <- which(date_list == recent_processed_date)
recent_file_name <- this_file_list[which_recent_file]
print(paste(recent_file_name,"is the most recent file and will be used"))
# load the data created in step 3, which has all of the observations for the 4 timesteps per day in one data frame
#NAM_Step3 <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste("NAM_Step3_part_",part_number,".csv",sep = ""))) # open data file
NAM_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,recent_file_name)) # open data file

#Step3_NAM_data[1,c("Time.UTC")]
#as.character(Step3_NAM_data[1,c("Date")])
#ymd_h(paste(Step3_NAM_data[3007767,c("Date")],Step3_NAM_data[3007767,c("Time.UTC")]), tz = "UTC")

#NAM_data$TimeZone <- NA
# use sf option for lutz
#point_interest <- NAM_Step3[1, c("Lat","Lon")]
#tz_lookup_coords(lat = point_interest$Lat, lon = point_interest$Lon, method = "accurate")
NAM_data$TimeZone <- tz_lookup_coords(lat = NAM_data$Latitude, lon = NAM_data$Longitude, method = "accurate")
print(unique(NAM_data$TimeZone))
# if needed, see page 14 of https://cran.r-project.org/web/packages/lubridate/lubridate.pdf for info about a daylight savings time indicator?

# add a column indicating the time stamp in UTC
UTC_date_vec <- unlist(lapply(1:dim(NAM_data)[1], function(x){ # start lapply and start defining function used in lapply
  this_date <- as.character(NAM_data[x,c("Date")])
  this_UTC_time <- as.character(NAM_data[x,"Time.UTC"])
  if (nchar(this_UTC_time)==1) {
    this_UTC_time <- paste("0",this_UTC_time,sep = "")
  }
  #print(this_date)
  #print(this_UTC_time)
  date_stamp_UTC <- paste(this_date," ",this_UTC_time,":00:00",sep = "")
  #print(date_stamp_UTC)
  return(date_stamp_UTC) # return the new file name so a new list of files can be created
}))#, ProcessedData.directory,sub_folder)

NAM_data$UTC.Date.Time <- yymd

x <- ymd_hms("2009-08-07 00:00:01", tz = "UTC")

print(with_tz(x,"GMT"))

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




## add a column indicating the time in the relevant time zone - see page 58-72
#NAM_data$Local.Date.Time <- with_tz(time = NAM_data$UTC.Date.Time, tz = NAM_data$TimeZone)
#print(unique(NAM_data$Local.Date.Time))

x <- ymd_hms("2009-08-07 00:00:01", tz = "America/New_York")
x <- ymd_hms("2009-08-07 00:00:01", tz = "UTC")
x <- ymd_hms("2009-08-07 00:00:01", tz = "UTC")

print(with_tz(x,"GMT"))



#NAM_data$Local.Date <- date(NAM_data$Local.Date.Time)
#1:dim(NAM_data)[1]
for (x in 1:dim(NAM_data)[1]) {
  print(paste("x =",x,"of",dim(NAM_data)[1]))
  print(paste(NAM_data[x,c("UTC.Date.Time")],NAM_data[x,c("TimeZone")]))
  NAM_data[x,c("Local.Date.Time")] <- with_tz(time = NAM_data[x,c("UTC.Date.Time")], tzone = NAM_data[x,c("TimeZone")])
  print(NAM_data[x,c("Local.Date.Time")])
}

# write step 4 data to csv file
write.csv(NAM_data,file = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,paste(output_file_name,".csv",sep = "")),row.names = FALSE) # write data to file
