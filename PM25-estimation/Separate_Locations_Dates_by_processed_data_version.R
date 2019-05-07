# Separate_Locations_Dates_by_processed_data_version.R - handle the separate data parts with regard to locations

#### record of various parts ####
# part a: early version created while writing code. Disregard
# part b: first batch of PM2.5 data that was used to exctract predictor data, years 2008-2014
# part c: county centroids, 2008-2014. This work flow has now been moved to the "Locations_of_interest" folder.
# part d: second batch of PM2.5 data, adds EPA AQS data for 2014-2018
# part e: updates IMPROVE and Fire Cache data to include 2008-2018 (whatever portion of that was available for download)

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
drop_cols <- c("old_lon","old_lat","old_Datum","Easting","Northing","Datum") # list extraneous columns
sub_folder <- paste("PM25_data_part_",define_study_constants.fn("processed_data_version"),sep = "")

#### Load locations/dates by batch ####
# load part b locations and date files
part_b_loc_rounded_maybe_w_dup <- PM25_lat_lon_part.fn(this_part = "b", Locations_Only = TRUE, round_lat_lon_digits = define_study_constants.fn("round_lat_lon_digits"), drop_cols = drop_cols) # Load part b locations, round to specified digits
part_b_loc_rounded <- part_b_loc_rounded_maybe_w_dup[!duplicated(part_b_loc_rounded_maybe_w_dup), ]
rm(part_b_loc_rounded_maybe_w_dup)
part_b_date_loc_rounded_maybe_w_dup <- PM25_lat_lon_part.fn(this_part = "b", Locations_Only = FALSE, round_lat_lon_digits = define_study_constants.fn("round_lat_lon_digits"), drop_cols = drop_cols) # Load part b dates and locations, round to specified digits
part_b_date_loc_rounded_maybe_w_dup$Date <- as.Date(part_b_date_loc_rounded_maybe_w_dup$Date, format = "%Y-%m-%d")
part_b_date_loc_rounded <- part_b_date_loc_rounded_maybe_w_dup[!duplicated(part_b_date_loc_rounded_maybe_w_dup), ]
rm(part_b_date_loc_rounded_maybe_w_dup)

# load part d locations and date files
part_d_loc_rounded_maybe_w_dup <- PM25_lat_lon_part.fn(this_part = "d", Locations_Only = TRUE, round_lat_lon_digits = define_study_constants.fn("round_lat_lon_digits"), drop_cols = drop_cols) # Load part d locations, round to specified digits
part_d_loc_rounded <- part_d_loc_rounded_maybe_w_dup[!duplicated(part_d_loc_rounded_maybe_w_dup), ]
rm(part_d_loc_rounded_maybe_w_dup)
part_d_date_loc_rounded_maybe_w_dup <- PM25_lat_lon_part.fn(this_part = "d", Locations_Only = FALSE, round_lat_lon_digits = define_study_constants.fn("round_lat_lon_digits"), drop_cols = drop_cols) # Load part d dates and locations, round to specified digits
part_d_date_loc_rounded_maybe_w_dup$Date <- as.Date(part_d_date_loc_rounded_maybe_w_dup$Date, format = "%Y-%m-%d")
part_d_date_loc_rounded <- part_d_date_loc_rounded_maybe_w_dup[!duplicated(part_d_date_loc_rounded_maybe_w_dup), ]
rm(part_d_date_loc_rounded_maybe_w_dup)

# load part e locations and date files
part_e_loc_rounded_maybe_w_dup <- PM25_lat_lon_part.fn(this_part = "e", Locations_Only = TRUE, round_lat_lon_digits = define_study_constants.fn("round_lat_lon_digits"), drop_cols = drop_cols) # Load part e locations, round to specified digits
part_e_loc_rounded <- part_e_loc_rounded_maybe_w_dup[!duplicated(part_e_loc_rounded_maybe_w_dup), ]
rm(part_e_loc_rounded_maybe_w_dup)
part_e_date_loc_rounded_maybe_w_dup <- PM25_lat_lon_part.fn(this_part = "e", Locations_Only = FALSE, round_lat_lon_digits = define_study_constants.fn("round_lat_lon_digits"), drop_cols = drop_cols) # Load part e dates and locations, round to specified digits
part_e_date_loc_rounded_maybe_w_dup$Date <- as.Date(part_e_date_loc_rounded_maybe_w_dup$Date, format = "%Y-%m-%d")
part_e_date_loc_rounded <- part_e_date_loc_rounded_maybe_w_dup[!duplicated(part_e_date_loc_rounded_maybe_w_dup), ]
rm(part_e_date_loc_rounded_maybe_w_dup)

##### Find the data that is in the newest version, but not b ####
#part_e_not_in_b_loc <- anti_join(part_e_loc_rounded, part_b_loc_rounded, by=c("Lat","Lon"))
part_e_not_in_b_loc <- anti_merge_time_static_data.fn(Newer_data = part_e_loc_rounded, Older_data = part_b_loc_rounded, by_vars = c("Lat","Lon"), round_digits = define_study_constants.fn("round_lat_lon_digits"))#,latitude_col_s = "Lat",longitude_col_s = "Lon")

part_e_not_in_b_date_loc <- anti_join(part_e_date_loc_rounded, part_b_date_loc_rounded, by=c("Lat","Lon"))

##### Find the data that is in the newest version, but not d ####
part_e_not_in_d_loc <- anti_join(part_e_loc_rounded, part_d_loc_rounded, by=c("Lat","Lon"))
part_e_not_in_d_date_loc <- anti_join(part_e_date_loc_rounded, part_d_date_loc_rounded, by=c("Lat","Lon"))

##### Combine all previous versions so that they can be separated from current version ####
part_bd_loc_rounded_step1 <- rbind(part_b_loc_rounded,part_d_loc_rounded) # combine locations from previous parts
part_bd_loc_rounded <- part_bd_loc_rounded_step1[!duplicated(part_bd_loc_rounded_step1), ] # get rid of duplicates
rm(part_bd_loc_rounded_step1) # clear variable

part_bd_date_loc_rounded_step1 <- rbind(part_b_date_loc_rounded,part_d_date_loc_rounded) # combine dates/locations from previous parts
part_bd_date_loc_rounded <- part_bd_date_loc_rounded_step1[!duplicated(part_bd_date_loc_rounded_step1), ] # get rid of duplicates
rm(part_bd_date_loc_rounded_step1)

##### Find the data that is only in the newest version ####
part_e_not_in_bd_loc <- anti_join(part_e_loc_rounded, part_bd_loc_rounded, by=c("Lat","Lon"))
part_e_not_in_bd_date_loc <- anti_join(part_e_date_loc_rounded, part_bd_date_loc_rounded, by=c("Lat","Lon"))

#### Save files ####
# locations file:
write.csv(part_e_not_in_b_loc,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste('Part_e_not_in_b_Locations','.csv',sep = "")),row.names = FALSE)
# dates/locations file:
write.csv(part_e_not_in_b_date_loc,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste('Part_e_not_in_b_Locations_Dates','.csv',sep = "")),row.names = FALSE)

# locations file:
write.csv(part_e_not_in_bd_loc,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste('Part_e_not_in_bd_Locations','.csv',sep = "")),row.names = FALSE)
# dates/locations file:
write.csv(part_e_not_in_bd_date_loc,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste('Part_e_not_in_bd_Locations_Dates','.csv',sep = "")),row.names = FALSE)

#### plot locations ####
this_df_list <- list("part_b" = part_b_loc_rounded, "part_d" = part_d_loc_rounded, "part_e" = part_e_loc_rounded)
color_list <- list("part_b" = "grey", "part_d" = "black", "part_e" = "red")
symbol_list <- list("part_b" = 20, "part_d" = 1, "part_e" = 6)
symbol_size_list <- list("part_b" = 1, "part_d" = 1.10, "part_e" = 1.5)
legend_list <- list("part_b" = "part_b", "part_d" = "part_d", "part_e" = "part_e")
file_sub_label <- paste("PM25_obs_locations_by_data_version",sep = "") # file partial name,
LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
if (file.exists(LatexFileName)) {file.remove(LatexFileName)} # Delete file if it exists
plot_name_extension <- "Obs_Locations_versions_bde"
title_string <- "Observation Locations by data version"
fig_caption <- "Monitor locations by data version (b, d and e)"
map_data_locations_by_set.fn(this_df_list = this_df_list, legend_list = legend_list, color_list = color_list, symbol_list = symbol_list, symbol_size_list = symbol_size_list, Latitude_var_name = "Lat", Longitude_var_name = "Lon", output.directory = define_file_paths.fn("output.directory"), file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev = study_states_abbrev, title_string = title_string, ClearPage = FALSE, LatexFileName = LatexFileName, fig_caption = fig_caption)
