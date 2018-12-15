# CountyCentroid_CreateLatLonDateFiles.R - create data frame of the dates/locations of county centroids (places we want to predict PM2.5)
rm(list  =  ls())
options(warn  =  2) # throw an error when there's a warning and stop the code from running further
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which  =  dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
working.directory  <-  "/home/rstudio"
setwd(working.directory)

#print("Run Define_directories.R before running this file.")

#### Call Packages (Library) ####
library(rgdal)
library(geosphere)

#### Call Load Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_proj_functions.R"))
#source(file.path(writingcode.directory,"State_Abbrev_Definitions_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"State_Abbrev_Definitions_function.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"Plotting_and_LaTex_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"ML_processing_functions.R"))
source(file.path(define_file_paths.fn("LocationsInterest_Code.directory"),"Spatial_functions.R"))

#### define constants and variables needed for all R workers ####
#processed_data_version <- "c" # locations for predictions
processed_data_version <- "CountyCentroid"
study_states_abbrev <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
this_datum <- "NAD83"
#sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")
sub_folder <- processed_data_version

# load map shape file
WestCountymapGeom <- load_County_Boundaries.fn(USMaps.directory, study_states_abbrev)

# County Centroids file
county_centroids_for_coord <- create_centroid_df.fn(WestCountymapGeom)

# fill in Easting/Northing
county_centroids <- fill_in_EastingNorthing_from_LatLon.fn(county_centroids_for_coord) 

# write centroids to file
#write.csv(county_centroids,file = file.path(ProcessedData.directory,sub_folder,paste(sub_folder,'_Locations_part_',processed_data_version,'.csv',sep = "")),row.names = FALSE)
write.csv(county_centroids,file = file.path(ProcessedData.directory,sub_folder,paste(sub_folder,'_Locations','.csv',sep = "")),row.names = FALSE)

# create a vector of dates
start_date <- define_study_constants.fn("start_date")
end_date <- define_study_constants.fn("end_date")
#start_date <- "2008-01-01"
#end_date <- "2018-12-31"
date_vec <- seq(as.Date(start_date), as.Date(end_date), by="days")

# create data frame for all locations/dates
date_place <- expand_date_location.fn(locations_of_interest = county_centroids, date_vec = date_vec, this_datum = this_datum)
#write.csv(date_place,file = file.path(ProcessedData.directory,sub_folder,paste(file_sub_label,'_Locations_Dates_part_',processed_data_version,"_",start_date,"to",end_date,'.csv',sep = "")),row.names = FALSE)
write.csv(date_place,file = file.path(ProcessedData.directory,sub_folder,paste(sub_folder,'_Locations_Dates',"_",start_date,"to",end_date,'.csv',sep = "")),row.names = FALSE)
