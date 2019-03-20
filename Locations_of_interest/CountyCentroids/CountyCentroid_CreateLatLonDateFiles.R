# CountyCentroid_CreateLatLonDateFiles.R - create data frame of the dates/locations of county centroids (places we want to predict PM2.5)

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

#### Call Packages (Library) ####
library(rgdal)
library(geosphere)

#### Call Load Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"State_Abbrev_Definitions_function.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"Plotting_and_LaTex_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"ML_processing_functions.R"))
source(file.path(define_file_paths.fn("LocationsInterest_Code.directory"),"Spatial_functions.R"))

#### define constants ####
study_states_abbrev <- define_study_constants.fn("study_states_abbrev") # get the study area (state abbreviations)
this_datum <- define_study_constants.fn("study_datum") # get the datum used for the study
processed_data_version <- "CountyCentroid" # previously "c" # define the name of this data
sub_folder <- processed_data_version # define sub-folder name
start_date <- "2008-01-01"#"2015-01-01"#define_study_constants.fn("start_date") # get the start date for the study
end_date <- "2014-12-31"#define_study_constants.fn("end_date") # get the end date for the study

#### Find county centroids and write locations to csv file ####
WestCountymapGeom <- load_County_Boundaries.fn(define_file_paths.fn("USMaps.directory"), study_states_abbrev) # load map shape file
county_centroids_for_coord <- create_centroid_df.fn(WestCountymapGeom) # create county centroids object
county_centroids <- fill_in_EastingNorthing_from_LatLon.fn(county_centroids_for_coord) # fill in Easting/Northing
write.csv(county_centroids,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(sub_folder,'_Locations','.csv',sep = "")),row.names = FALSE) # write centroids to file
rm(WestCountymapGeom,county_centroids_for_coord, study_states_abbrev) # clear variables

#### Expand locations to all dates in study period ####
date_vec <- seq(as.Date(start_date), as.Date(end_date), by="days") # create a vector of dates for every day during the starting period for every centroid
date_place <- expand_date_location.fn(locations_of_interest = county_centroids, date_vec = date_vec, this_datum = this_datum) # create data frame for all locations/dates
write.csv(date_place,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(sub_folder,'_Locations_Dates',"_",start_date,"to",end_date,'.csv',sep = "")),row.names = FALSE) # write the dates/locations df to csv
rm(date_vec,date_place, county_centroids, this_datum, processed_data_version, sub_folder, start_date, end_date) # clear variables
