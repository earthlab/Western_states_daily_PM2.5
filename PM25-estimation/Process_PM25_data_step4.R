# Process_PM25_data_step4.R - handle the separate data parts with regard to locations

print("run Define_directories.R before this script") 

# load libraries
library(daff)

# Load Functions that I created #
source(file.path(writingcode.directory,"Deduplication_functions.R"))
source(file.path(ML_Code.directory,"Plotting_and_LaTex_functions.R"))
source(file.path(writingcode.directory,"State_Abbrev_Definitions_function.R"))
# Constants
study_states_abbrev <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")


# Load part a
sub_folder_name <- "PM25_data_part_a"
file_name_loc <- "Projected_locations_part_a.csv"
part_a_loc <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc), stringsAsFactors = FALSE) # load data
file_name_loc_dates <- "Projected_locations_with_dates_part_a.csv"
part_a_loc_dates <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc), stringsAsFactors = FALSE) # load data
rm(sub_folder_name,file_name_loc,file_name_loc_dates)

# Load part b
sub_folder_name <- "PM25_data_part_b"
file_name_loc <- "PM25_Step3_part_b_Locations_Projected.csv"
part_b_loc <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc), stringsAsFactors = FALSE) # load data
file_name_loc_dates <- "PM25_Step3_part_b_Locations_Dates_Projected.csv"
part_b_loc_dates <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc), stringsAsFactors = FALSE) # load data
rm(sub_folder_name,file_name_loc,file_name_loc_dates)

# Take the difference in locations from part b and a
old_df <- part_a_loc 
new_df <- part_b_loc
cols_interest <- c("Lon","Lat")#names(old_df)

map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev)

points(new_df$Lon,new_df$Lat,col="red") # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/

points(old_df$Lon,old_df$Lat,col="blue") # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/


only_in_new <- rows.in.a1.that.are.not.in.a2(a1 = new_df[ ,cols_interest],a2 = old_df[ ,cols_interest])

only_in_old <- rows.in.a1.that.are.not.in.a2(a2 = new_df[ ,cols_interest],a1 = old_df[ ,cols_interest])


# List of current and previously processed file names
#this_source_file <- "PM25_Step2_2018-10-15_part_a_Cleaned_Locations.csv"
#this_source_file_loc <- paste("PM25_Step2_part_",processed_data_version,"_Locations.csv",sep = "") # define file name
#this_source_file_loc_date <- paste("PM25_Step2_part_",processed_data_version,"_Locations_Dates.csv",sep = "") # define file name

#reproject_monitors.fn(this_source_file_loc, this_source_file_loc_date) # reproject location and location/date files and print to csv
