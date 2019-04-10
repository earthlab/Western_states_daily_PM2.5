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
print(paste("There are ",length(this_file_list_step),"files for NAM Step 3 data")) # optional output statement
date_list <- unlist(lapply(this_file_list, function(x){ # start lapply and start defining function used in lapply
  #processed_date <- as.Date(substr(x,nchar(x)-13,nchar(x)-4),"%Y-%m-%d") # identify the time stamp for the file in this iteration
  processed_date <- substr(x,nchar(x)-13,nchar(x)-4) # identify the time stamp for the file in this iteration
  print(processed_date)
  class(processed_date)
  return(processed_date) # return the new file name so a new list of files can be created
}))
#date_list <- as.Date(date_list)
recent_processed_date <- max(as.Date(date_list))
which_recent_file <- which(date_list == recent_processed_date)
recent_file_name <- this_file_list[which_recent_file]

# load the data created in step 3, which has all of the observations for the 4 timesteps per day in one data frame
#NAM_Step3 <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste("NAM_Step3_part_",part_number,".csv",sep = ""))) # open data file
NAM_Step3 <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,recent_file_name)) # open data file

#Step3_NAM_data[1,c("Time.UTC")]
#as.character(Step3_NAM_data[1,c("Date")])
#ymd_h(paste(Step3_NAM_data[3007767,c("Date")],Step3_NAM_data[3007767,c("Time.UTC")]), tz = "UTC")

# add a column indicating the time stamp in UTC
NAM_Step3$UTC.Date.Time <- ymd_h(paste(NAM_Step3$Date,NAM_Step3$Time.UTC), tz = "UTC")

## add a column indicating the time zone for each observation
##unique_locations <- unique(NAM_Step3[,c("Lat","Lon")])
#unique_locations <- unique(round(NAM_Step3[,c("Latitude","Longitude")],5))
#unique_locations$TimeZone <- NA
#unique_locations$TimeZone <- tz_lookup_coords(lat = unique_locations$Lat, lon = unique_locations$Lon, method = "accurate")
  
NAM_Step3$TimeZone <- NA
# use sf option for lutz
#point_interest <- NAM_Step3[1, c("Lat","Lon")]
#tz_lookup_coords(lat = point_interest$Lat, lon = point_interest$Lon, method = "accurate")
NAM_Step3$TimeZone <- tz_lookup_coords(lat = NAM_Step3$Latitude, lon = NAM_Step3$Longitude, method = "accurate")

#NAM_Step4 <- unlist(lapply(1:dim(NAM_Step3)[1], function(x){
#  stop("fill in code")
#}))


# do we need a daylight savings time indicator? - see page 14

# add a column indicating the time in the relevant time zone - see page 58-72
NAM_Step3$Local.Date.Time <- with_tz(time = NAM_Step3$UTC.Date.Time, tzone = NAM_Step3$TimeZone)
NAM_Step3$Local.Date <- date(NAM_Step3$Local.Date.Time)

# write step 4 data to csv file