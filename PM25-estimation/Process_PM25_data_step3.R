# Process_PM25_data_step3.R - reproject locations to all have the same datum

print("run Define_directories.R before this script") 

# Load Functions that I created #
source(file.path(writingcode.directory,"reprojection_functions.R"))

# List of current and previously processed file names
#this_source_file <- "PM25_Step2_2018-10-15_part_a_Cleaned_Locations.csv"
this_source_file_loc <- paste("PM25_Step2_part_",processed_data_version,"_Locations.csv",sep = "") # define file name
this_source_file_loc_date <- paste("PM25_Step2_part_",processed_data_version,"_Locations_Dates.csv",sep = "") # define file name

reproject_monitors.fn(this_source_file_loc, this_source_file_loc_date) # reproject location and location/date files and print to csv
