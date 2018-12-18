# Process_PM25_data_step3.R - reproject locations to all have the same datum

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
source(file.path(define_file_paths.fn("writingcode.directory"),"reprojection_functions.R"))

#### Reproject data ####
# get names of folders and files 
processed_data_version <- define_study_constants.fn("processed_data_version")
this_source_file_loc <- paste("PM25_Step2_part_",processed_data_version,"_Locations.csv",sep = "") # define file name
print(this_source_file_loc)
this_source_file_loc_date <- paste("PM25_Step2_part_",processed_data_version,"_Locations_Dates.csv",sep = "") # define file name
print(this_source_file_loc_date)
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")
print(sub_folder)

reproject_monitors.fn(this_source_file_loc = this_source_file_loc, this_source_file_loc_date = this_source_file_loc_date, sub_folder = sub_folder) # reproject location and location/date files and print to csv

# put the reprojected locations into the full input_mat1
this_source_file <- paste("PM25_Step2_part_",processed_data_version,".csv",sep = "") # define file name

reprojected_into_input_mat1.fn(ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), sub_folder = sub_folder, this_source_file = this_source_file, this_source_file_loc = this_source_file_loc)
