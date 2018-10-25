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

# Load part a
#sub_folder_name <- "PM25_data_part_a"
#file_name_loc <- "Projected_locations_part_a.csv"
#part_a_loc <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc), stringsAsFactors = FALSE) # load data
#file_name_loc_dates <- "Projected_locations_with_dates_part_a.csv"
#part_a_loc_dates <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc_dates), stringsAsFactors = FALSE) # load data
#rm(sub_folder_name,file_name_loc,file_name_loc_dates)

# Load part b
sub_folder_name <- "PM25_data_part_b"
file_name_loc <- "PM25_Step3_part_b_Locations_Projected.csv"
part_b_loc <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc), stringsAsFactors = FALSE) # load data
file_name_loc_dates <- "PM25_Step3_part_b_Locations_Dates_Projected.csv"
part_b_loc_dates <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc_dates), stringsAsFactors = FALSE) # load data

# Take the difference in locations from part b and a
#old_df <- part_a_loc 
#new_df <- part_b_loc
#only_new_df <- df_only_in_new.fn(old_df, new_df)
print("finish code to take differences between dataframes")
#new_file_name <- "PM25_Step3_part_b_minus_a_Locations_Projected"
#write.csv(only_in_new, file = file.path(ProcessedData.directory,sub_folder_name,paste(new_file_name,'.csv',sep = "")),row.names = FALSE)
#rm(sub_folder_name,file_name_loc,file_name_loc_dates)

# Load part c
sub_folder_name <- "PM25_data_part_c"
file_name_loc <- "CountyGeometricCentroids_Locations_part_c.csv"
part_c_loc <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc), stringsAsFactors = FALSE) # load data
file_name_loc_dates <- "CountyGeometricCentroids_Locations_Dates_part_c_2008-01-01to2008-12-31.csv"
part_c_loc_dates <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc_dates), stringsAsFactors = FALSE) # load data

# # get columns in part_a that are consistently named with parts b-c
# part_a_loc$NewDatum <- unique(part_b_loc$Datum) # put the new datum into part a
# new_header <- c("Lat", "Lon", "Datum", "Easting", "Northing")
# part_a_few_col <- data.frame(matrix(NA,nrow=dim(part_a_loc)[1],ncol=length(new_header))) # create data frame for input_mat1
# names(part_a_few_col) <- new_header # assign the header to full_mat_a1.that.are.not.in.a2
# part_a_few_col$Datum <- part_a_loc$NewDatum
# part_a_few_col$Lat <- part_a_loc$Lat
# part_a_few_col$Lon <- part_a_loc$Lon
# part_a_few_col$Easting <- part_a_loc$Easting
# part_a_few_col$Northing <- part_a_loc$Northing
# # and for df with dates:
# part_a_loc_dates$NewDatum <- unique(part_b_loc$Datum) # put the new datum into part a
# new_header <- c("Lat", "Lon", "Datum", "Easting", "Northing", "Date") #c("Lat", "Lon", "Datum", "Easting", "Northing")
# part_a_few_col_date <- data.frame(matrix(NA,nrow=dim(part_a_loc_dates)[1],ncol=length(new_header))) # create data frame for input_mat1
# names(part_a_few_col_date) <- new_header # assign the header to full_mat_a1.that.are.not.in.a2
# part_a_few_col_date$Datum <- part_a_loc_dates$NewDatum
# part_a_few_col_date$Lat <- part_a_loc_dates$Lat
# part_a_few_col_date$Lon <- part_a_loc_dates$Lon
# part_a_few_col_date$Easting <- part_a_loc_dates$Easting
# part_a_few_col_date$Northing <- part_a_loc_dates$Northing
# part_a_few_col_date$Date <- part_a_loc_dates$Date
# rm(new_header)

# # merge parts a, b, & c - locations only
# part_abc_loc_step <- rbind(part_a_few_col[ , c("Lat", "Lon", "Datum","Easting","Northing")], part_b_loc[ , c("Lat", "Lon", "Datum","Easting","Northing")], part_c_loc[ , c("Lat", "Lon", "Datum","Easting","Northing")])
# if (dim(part_a_loc)[1]+dim(part_b_loc)[1]+dim(part_c_loc)[1] != dim(part_abc_loc_step)[1]) {stop("check code and data")} # check number of rows
# part_abc_loc <- part_abc_loc_step[!duplicated(part_abc_loc_step),]
# write.csv(part_abc_loc,file = file.path(ProcessedData.directory,"PM25_Step4_part_abc_Locations.csv"),row.names = FALSE)

# merge parts  b, & c - locations only
part_bc_loc_step <- rbind(part_b_loc[ , c("Lat", "Lon", "Datum","Easting","Northing")], part_c_loc[ , c("Lat", "Lon", "Datum","Easting","Northing")])
if (dim(part_b_loc)[1]+dim(part_c_loc)[1] != dim(part_bc_loc_step)[1]) {stop("check code and data")} # check number of rows
part_bc_loc <- part_bc_loc_step[!duplicated(part_bc_loc_step),]
write.csv(part_bc_loc,file = file.path(ProcessedData.directory,"PM25_Step4_part_bc_Locations.csv"),row.names = FALSE)

# # merge parts a, b, & c - locations and dates
# part_abc_loc_date_step <- rbind(part_a_few_col_date[ , c("Lat", "Lon", "Datum","Easting","Northing")], part_b_loc_dates[ , c("Lat", "Lon", "Datum","Easting","Northing")], part_c_loc_dates[ , c("Lat", "Lon", "Datum","Easting","Northing")])
# if (dim(part_a_loc_dates)[1]+dim(part_b_loc_dates)[1]+dim(part_c_loc_dates)[1] != dim(part_abc_loc_date_step)[1]) {stop("check code and data")} # check number of rows
# part_abc_col_date <- part_abc_loc_date_step[!duplicated(part_abc_loc_date_step),]
# write.csv(part_a_few_col_date,file = file.path(ProcessedData.directory,"PM25_Step4_part_abc_Locations_Dates.csv"),row.names = FALSE)

# merge parts a, b, & c - locations and dates
part_bc_loc_date_step <- rbind(part_b_loc_dates[ , c("Lat", "Lon", "Datum","Easting","Northing","Date")], part_c_loc_dates[ , c("Lat", "Lon", "Datum","Easting","Northing","Date")])
if (dim(part_b_loc_dates)[1]+dim(part_c_loc_dates)[1] != dim(part_bc_loc_date_step)[1]) {stop("check code and data")} # check number of rows
part_bc_col_date <- part_bc_loc_date_step[!duplicated(part_bc_loc_date_step),]
write.csv(part_bc_col_date,file = file.path(ProcessedData.directory,"PM25_Step4_part_bc_Locations_Dates.csv"),row.names = FALSE)

map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev)
#points(part_a_loc$Lon,part_a_loc$Lat,col="red") # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
points(part_b_loc$Lon,part_b_loc$Lat,col="green") # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
points(part_c_loc$Longitude,part_c_loc$Latitude,col="blue") # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/


# List of current and previously processed file names
#this_source_file <- "PM25_Step2_2018-10-15_part_a_Cleaned_Locations.csv"
#this_source_file_loc <- paste("PM25_Step2_part_",processed_data_version,"_Locations.csv",sep = "") # define file name
#this_source_file_loc_date <- paste("PM25_Step2_part_",processed_data_version,"_Locations_Dates.csv",sep = "") # define file name

#reproject_monitors.fn(this_source_file_loc, this_source_file_loc_date) # reproject location and location/date files and print to csv
