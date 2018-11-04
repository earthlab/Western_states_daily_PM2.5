# Process_PM25_data_step4.R - handle the separate data parts with regard to locations

print("run Define_directories.R before this script") 

# load libraries
#library(daff)
library(ggplot2)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)
library(maps)
library(geosphere)

# Load Functions that I created #
source(file.path(writingcode.directory,"Deduplication_functions.R"))
source(file.path(ML_Code.directory,"Plotting_and_LaTex_functions.R"))
source(file.path(writingcode.directory,"State_Abbrev_Definitions_function.R"))
source(file.path(writingcode.directory,"reprojection_functions.R"))
# Constants
study_states_abbrev <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")

# Load part b
sub_folder_name <- "PM25_data_part_b"
file_name_loc <- "PM25_Step3_part_b_Locations_Projected.csv"
part_b_loc <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc), stringsAsFactors = FALSE) # load data
file_name_loc_dates <- "PM25_Step3_part_b_Locations_Dates_Projected.csv"
part_b_loc_dates <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc_dates), stringsAsFactors = FALSE) # load data

# Load part c
sub_folder_name <- "PM25_data_part_c"
file_name_loc <- "CountyGeometricCentroids_Locations_part_c.csv"
part_c_loc <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc), stringsAsFactors = FALSE) # load data
file_name_loc_dates <- "CountyGeometricCentroids_Locations_Dates_part_c_2008-01-01to2008-12-31.csv"
part_c_loc_dates <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc_dates), stringsAsFactors = FALSE) # load data

# merge parts  b, & c - locations only
part_bc_loc_step <- rbind(part_b_loc[ , c("Lat", "Lon", "Datum","Easting","Northing")], part_c_loc[ , c("Lat", "Lon", "Datum","Easting","Northing")])
if (dim(part_b_loc)[1]+dim(part_c_loc)[1] != dim(part_bc_loc_step)[1]) {stop("check code and data")} # check number of rows
part_bc_loc <- part_bc_loc_step[!duplicated(part_bc_loc_step),]
write.csv(part_bc_loc,file = file.path(ProcessedData.directory,"PM25_Step4_part_bc_Locations.csv"),row.names = FALSE)

# merge parts b & c - locations and dates
part_bc_loc_date_step <- rbind(part_b_loc_dates[ , c("Lat", "Lon", "Datum","Easting","Northing","Date")], part_c_loc_dates[ , c("Lat", "Lon", "Datum","Easting","Northing","Date")])
if (dim(part_b_loc_dates)[1]+dim(part_c_loc_dates)[1] != dim(part_bc_loc_date_step)[1]) {stop("check code and data")} # check number of rows
part_bc_col_date <- part_bc_loc_date_step[!duplicated(part_bc_loc_date_step),]
write.csv(part_bc_col_date,file = file.path(ProcessedData.directory,"PM25_Step4_part_bc_Locations_Dates.csv"),row.names = FALSE)

print("finish code to take differences between dataframes")
print("put part d into Process_PM25_data_step4.R")

map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev)
points(part_b_loc$Lon,part_b_loc$Lat,col="green") # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
points(part_c_loc$Longitude,part_c_loc$Latitude,col="blue") # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
