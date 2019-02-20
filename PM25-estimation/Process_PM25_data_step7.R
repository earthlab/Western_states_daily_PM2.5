# Process_PM25_data_step4.R - handle the separate data parts with regard to locations

#### record of various parts ####
# part a: early version created while writing code. Disregard
# part b: first batch of PM2.5 data that was used to exctract predictor data, years 2008-2014
# part c: county centroids, 2008-2014. This work flow has now been moved to the "Locations_of_interest" folder.
# part d: second batch of PM2.5 data, adds EPA AQS data for 2014-2018

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
library(dplyr)

#### Source functions I've written ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"Deduplication_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"Plotting_and_LaTex_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"State_Abbrev_Definitions_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"reprojection_functions.R"))
source(file.path(define_file_paths.fn("General_functions.directory"),"merging_data_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))

#### Define constants #####
study_states_abbrev <- define_study_constants.fn("study_states_abbrev") # c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
ProcessedData.directory <-  define_file_paths.fn("ProcessedData.directory")
#drop_cols <- c("old_lon","old_lat","old_Datum","Easting","Northing","Datum") # list extraneous columns
drop_cols <- c("")
#round_lat_lon_digits <- define_study_constants.fn("round_lat_lon_digits")
#round_lat_lon_digits <- 4
#### Load locations/dates by batch ####

# 4 digits
part_b_loc_4_digits <- PM25_lat_lon_part.fn(this_part = "b", Locations_Only = TRUE, round_lat_lon_digits = 4) # Load part b locations, round to 4 digits
part_b_loc_dates_4_digits <- PM25_lat_lon_part.fn(this_part = "b", Locations_Only = FALSE, round_lat_lon_digits = 4) # Load part b locations & dates, round to 4 digits
part_d_loc_4_digits <- PM25_lat_lon_part.fn(this_part = "d", Locations_Only = TRUE, round_lat_lon_digits = 4) # Load part b locations, round to 4 digits
part_d_loc_dates_4_digits <- PM25_lat_lon_part.fn(this_part = "d", Locations_Only = FALSE, round_lat_lon_digits = 4) # Load part b locations & dates, round to 4 digits

part_d_not_in_b_loc_4_digits <- anti_join(part_d_loc_4_digits, part_b_loc_4_digits, by=c("Lat","Lon"))
part_b_not_in_d_loc_4_digits <- anti_join(part_b_loc_4_digits, part_d_loc_4_digits, by=c("Lat","Lon"))


# 5 digits
part_b_loc_5_digits <- PM25_lat_lon_part.fn(this_part = "b", Locations_Only = TRUE, round_lat_lon_digits = 5) # Load part b locations, round to 5 digits
part_b_loc_dates_5_digits <- PM25_lat_lon_part.fn(this_part = "b", Locations_Only = FALSE, round_lat_lon_digits = 5) # Load part b locations & dates, round to 5 digits
part_d_loc_5_digits <- PM25_lat_lon_part.fn(this_part = "d", Locations_Only = TRUE, round_lat_lon_digits = 5) # Load part b locations, round to 5 digits
part_d_loc_dates_5_digits <- PM25_lat_lon_part.fn(this_part = "d", Locations_Only = FALSE, round_lat_lon_digits = 5) # Load part b locations & dates, round to 5 digits

part_d_not_in_b_loc_5_digits <- anti_join(part_d_loc_5_digits, part_b_loc_5_digits, by=c("Lat","Lon"))
part_b_not_in_d_loc_5_digits <- anti_join(part_b_loc_5_digits, part_d_loc_5_digits, by=c("Lat","Lon"))


#this_part <- "b"
#sub_folder_name <- "PM25_data_part_b"
#file_name_loc <- "PM25_Step3_part_b_Locations_Projected.csv"
#part_b_loc_step <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc), stringsAsFactors = FALSE) # load data
#part_b_loc <- part_b_loc_step[ , !(names(part_b_loc_step) %in% drop_cols)] # remove extraneous columns
#part_b_loc$Latitude <- part_b_loc$Lat
#part_b_loc$Longitude <- part_b_loc$Lon
#part_b_loc$Lat <- round(part_b_loc$Lat, round_lat_lon_digits)
#part_b_loc$Lon <- round(part_b_loc$Lon, round_lat_lon_digits)
#part_b_loc <- PM25_lat_lon_part.fn(this_part = "b", Locations_Only = TRUE, round_lat_lon_digits = 4)

# file_name_loc_dates <- "PM25_Step3_part_b_Locations_Dates_Projected.csv"
# part_b_loc_dates_step <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc_dates), stringsAsFactors = FALSE) # load data
# part_b_loc_dates <- part_b_loc_dates_step[ , !(names(part_b_loc_dates_step) %in% drop_cols)] # remove extraneous columns
# part_b_loc_dates$Latitude <- part_b_loc_dates$Lat
# part_b_loc_dates$Longitude <- part_b_loc_dates$Lon
# part_b_loc_dates$Lat <- round(part_b_loc_dates$Lat, round_lat_lon_digits)
# part_b_loc_dates$Lon <- round(part_b_loc_dates$Lon, round_lat_lon_digits)
# part_b_loc_dates$Date <- as.Date(part_b_loc_dates$Date, format = "%Y-%m-%d")
# rm(part_b_loc_step, part_b_loc_dates_step)
# rm(sub_folder_name,file_name_loc, file_name_loc_dates)

# # Load part d
# sub_folder_name <- "PM25_data_part_d"
# file_name_loc <- "PM25_Step3_part_d_Locations_Projected.csv"
# part_d_loc_step <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc), stringsAsFactors = FALSE) # load data
# part_d_loc <- part_d_loc_step[ , !(names(part_d_loc_step) %in% drop_cols)] # remove extraneous columns
# part_d_loc$Latitude <- part_d_loc$Lat
# part_d_loc$Longitude <- part_d_loc$Lon
# part_d_loc$Lat <- round(part_d_loc$Lat, round_lat_lon_digits)
# part_d_loc$Lon <- round(part_d_loc$Lon, round_lat_lon_digits)
# file_name_loc_dates <- "PM25_Step3_part_d_Locations_Dates_Projected.csv"
# part_d_loc_dates_step <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc_dates), stringsAsFactors = FALSE) # load data
# part_d_loc_dates <- part_d_loc_dates_step[ , !(names(part_d_loc_dates_step) %in% drop_cols)] # remove extraneous columns
# part_d_loc_dates$Latitude <- part_d_loc_dates$Lat
# part_d_loc_dates$Longitude <- part_d_loc_dates$Lon
# part_d_loc_dates$Lat <- round(part_d_loc_dates$Lat, round_lat_lon_digits)
# part_d_loc_dates$Lon <- round(part_d_loc_dates$Lon, round_lat_lon_digits)
# part_d_loc_dates$Date <- as.Date(part_d_loc_dates$Date, format = "%Y-%m-%d")
# rm(part_d_loc_step, part_d_loc_dates_step)
# rm(sub_folder_name,file_name_loc, file_name_loc_dates)

#### find the rows that are in part d that are not in part b ####
# match on 4 digits in lat/lon, but keep all digits when writing to file
part_d_not_in_b_loc <- anti_join(part_d_loc, part_b_loc, by=c("Lat","Lon"))
part_d_not_in_b_full_digits <- part_d_not_in_b_loc[ , !(names(part_d_not_in_b_loc) %in% c("Lat","Lon"))] # remove extraneous columns
part_d_not_in_b_full_digits <- replace_column_names.fn(df_in = part_d_not_in_b_full_digits, old_col_name = "Latitude", new_col_name = "Lat") # replace "Latitude" with "Lat" 
part_d_not_in_b_full_digits <- replace_column_names.fn(df_in = part_d_not_in_b_full_digits, old_col_name = "Longitude", new_col_name = "Lon") # replace "Latitude" with "Lat" 
write.csv(part_d_not_in_b_full_digits,file = file.path(ProcessedData.directory,"PM25_Step4_part_d_not_in_b_Locations.csv"),row.names = FALSE)
#rm(part_b_loc,part_d_loc)
#rm(part_d_not_in_b_loc,part_d_not_in_b_full_digits)

 part_d_not_in_b_loc_dates <- anti_join(part_d_loc_dates,part_b_loc_dates, by = c("Lat","Lon"))
 part_d_not_in_b_dates_full_digits <- part_d_not_in_b_loc_dates[ , !(names(part_d_not_in_b_loc_dates) %in% c("Lat","Lon"))] # remove extraneous columns
 part_d_not_in_b_dates_full_digits <- replace_column_names.fn(df_in =  part_d_not_in_b_dates_full_digits, old_col_name = "Latitude", new_col_name = "Lat") # replace "Latitude" with "Lat" 
 part_d_not_in_b_dates_full_digits <- replace_column_names.fn(df_in =  part_d_not_in_b_dates_full_digits, old_col_name = "Longitude", new_col_name = "Lon") # replace "Latitude" with "Lat" 
write.csv(part_d_not_in_b_dates_full_digits,file = file.path(ProcessedData.directory,"PM25_Step4_part_d_not_in_b_Locations_Dates.csv"),row.names = FALSE)

# looking at what is in part b but not part d:
#part_b_not_in_d_loc <- anti_join(part_b_loc, part_d_loc, by=c(colnames(part_d_loc)))
#part_b_not_in_d_loc_dates <- anti_join(part_b_loc_dates,part_d_loc_dates, by = c(colnames(part_d_loc_dates)))

# plot locations (image not saved)
map_county_base_layer.fn(define_file_paths.fn("CountyMaps.directory"), study_states_abbrev)
points(part_d_not_in_b_loc$Lon,part_d_not_in_b_loc$Lat,col = "red", pch = 17)
#points(part_b_not_in_d_loc$Lon,part_b_not_in_d_loc$Lat,col = "orange", pch = "#")
points(part_b_loc$Lon,part_b_loc$Lat,col="green") # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
points(part_d_loc$Lon,part_d_loc$Lat,col="blue") # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/

