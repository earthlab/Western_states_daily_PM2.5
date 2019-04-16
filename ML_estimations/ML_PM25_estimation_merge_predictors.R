# ML_PM25_estimation_merge_predictors.R - merge the various predictor variables together with the monitor data 
  # or dates/locations of interest

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

# start timer for code
start_code_timer <- proc.time()
print(paste("Start ML_PM25_estimation_merge_predictors.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
library(plyr)
library(lubridate)

#### Call Load Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
General_fn_list <- c("define_file_paths.fn","define_study_constants.fn","replace_character_in_string.fn",
                     "decimalplaces","print_name_value.fn","checksum.fn","check_4_NAs.fn")
source(file.path("estimate-pm25","General_Project_Functions","merging_data_functions.R"))
Merging_fn_list <- c("merge_predictors.fn","replace_column_names.fn","merge_time_varying_data.fn",
                     "merge_time_static_data.fn","merge_Fire_MODIS_data.fn","merge_Highways_data.fn","merge_GASP_data.fn","merge_MAIAC_data.fn",
                     "merge_NED_data.fn","merge_NLCD_data.fn","merge_NAM_data.fn","%!in%","average_slight_LatLon_variations.fn","determine_date_format.fn")
source(file.path(define_file_paths.fn("ML_Code.directory"),"ML_merge_predictors_parallal_wrapper_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))
input_mat_functions <- c("input_mat_change_data_classes.fn", "input_mat_extract_year_from_date.fn",
                         "input_mat_extract_month_from_date.fn", "input_mat_extract_day_from_date.fn",
                         "fancy_which.fn", "subset_data_frame_via_vector.fn", "EPA_codes_2_components_no_hyphens.fn",
                         "remove_data_outside_range.fn")
# create vector with directories that will be needed in parallel functions
ProcessedData.directory <- define_file_paths.fn("ProcessedData.directory")
output.directory <- define_file_paths.fn("output.directory")
output.directory.short <- define_file_paths.fn("output.directory.short")
USMaps.directory <- define_file_paths.fn("USMaps.directory")
directories_vector <- c("ProcessedData.directory", "output.directory", "output.directory.short", "USMaps.directory")

#### define constants and variables needed for all R workers ####
processed_data_version <- define_study_constants.fn("processed_data_version")
#fire_MODIS_25km_file_name <- c("fire_modis_part_bc_25km_final.csv","fire_modis_part_d_25km_extract_final.csv")
fire_MODIS_25km_file_name <- c("fire_modis_part_d_25km_extract_final.csv")
#fire_MODIS_50km_file_name  <- c("fire_modis_part_bc_50km_final.csv","fire_modis_part_d_50km_extract_final.csv")
fire_MODIS_50km_file_name  <- c("fire_modis_part_d_50km_extract_final.csv")
#fire_MODIS_100km_file_name  <- c("fire_modis_part_bc_100km_final.csv","fire_modis_part_d_100km_extract_final.csv")
fire_MODIS_100km_file_name  <- c("fire_modis_part_d_100km_extract_final.csv")
#fire_MODIS_500km_file_name  <- c("fire_modis_part_bc_500km_extract_final.csv","fire_modis_part_d_500km_extract_final.csv")
fire_MODIS_500km_file_name  <- c("fire_modis_part_d_500km_extract_final.csv")

GASP_file_name <- c("GASP_extracted_part_b.csv","GASP_extracted_part_c.csv","GASP_extracted_part_b_2012-2014.csv")
Highways_file_name <- c("Highways_part_e.csv")# files b and c have dates and later files do not. c("Highways_part_b.csv", "Highways_part_c.csv", "Highways_part_e.csv")
MAIAC_file_name <- c("MAIAC_extracted_part_b.csv", "MAIAC_extracted_part_c.csv","MAIAC_extracted_part_e_minus_b_done.csv")
#NAM_file_name <- NA#c("NAM_Step3_part_bc.csv") #,"NAM_Step3_part_bc.csv")
NDVI_file_name <- c("ndvi_mod13a3_part_bc_extract.csv","ndvi_mod13a3_part_d_extract.csv","ndvi_mod13a3_part_e_minus_bc_extract.csv")
NED_file_name <- c("ned_part_bc_extract.csv","ned_part_d_extract.csv","ned_part_e_not_in_bd_extract.csv")#c("ned_extract.csv","ned_extract.csv")
NLCD_1km_file_name <- c("nlcd_1km_part_bc_extract.csv","nlcd_part_d_1km_extract.csv","nlcd_part_e_not_bd_1km_extract.csv")
NLCD_5km_file_name <- c("nlcd_5km_part_bc_extract.csv","nlcd_part_d_5km_extract.csv","nlcd_part_e_not_bd_5km_extract.csv")
NLCD_10km_file_name <- c("nlcd_10km_part_bc_extract.csv","nlcd_part_d_10km_extract.csv","nlcd_part_e_not_bd_10km_extract.csv")

# determine which NAM file is the most recent
NAM_folder <- "NAM_data" # define folder for NAM data
input_sub_folder <- "NAM_Step5" # define location of input files
file_name_pattern <- "\\.csv$" # only looking for .csv files (don't want to pick up the sub-folder)
this_file_list <- list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,"."), pattern = file_name_pattern, all.files = FALSE,
                             full.names = FALSE, recursive = FALSE,
                             ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE) # get list of all .csv file in this folder
print(paste("There are ",length(this_file_list),"files for NAM Step 4 data")) # optional output statement
date_list <- unlist(lapply(this_file_list, function(x){ # start lapply and start defining function used in lapply
  processed_date <- substr(x,nchar(x)-13,nchar(x)-4) # identify the time stamp for the file in this iteration
  return(processed_date) # return the new file name so a new list of files can be created
}))
recent_processed_date <- max(as.Date(date_list)) # which date is the most recent file
which_recent_file <- which(date_list == recent_processed_date) # locate the file name for the most recent file
recent_file_name <- this_file_list[which_recent_file] # most recent file name
print(paste(recent_file_name,"is the most recent file and will be used"))
NAM_file_name <- recent_file_name
rm(NAM_folder,input_sub_folder,file_name_pattern,this_file_list,date_list,recent_processed_date,which_recent_file,recent_file_name)

predictor_sub_folder <- "PredictorVariablesExtractedToDatesLocations"

file_paths_to_merge_to <- c(paste("PM25_data_part_",processed_data_version,sep = ""),paste("PM25_data_part_",processed_data_version,sep = ""),"CountyCentroid")
#files_to_merge_to <- c(paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input",sep = ""),paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_prioritize_24hr_obs_ML_input",sep = ""), "CountyGeometricCentroids_Locations_Dates_part_c_2008-01-01to2008-12-31")
files_to_merge_to <- c(paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input",sep = ""),paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_prioritize_24hr_obs_ML_input",sep = ""), "CountyCentroid_Locations_Dates_2008-01-01to2018-12-31")

n_data_sets <- length(files_to_merge_to)
all_files_list <- c("fire_MODIS_25km_file_name","fire_MODIS_50km_file_name","fire_MODIS_100km_file_name","fire_MODIS_500km_file_name",
                    "GASP_file_name", "Highways_file_name", "MAIAC_file_name","NAM_file_name","NDVI_file_name","NED_file_name",
                    "NLCD_1km_file_name","NLCD_5km_file_name","NLCD_10km_file_name",
                    "predictor_sub_folder","files_to_merge_to","file_paths_to_merge_to")
print("make sure the file names and paths match")

#### Loop through data sets for processing ####
n_data_sets <- 1 # REMOVE
for (data_set_counter in 1:n_data_sets) {
  print(paste("Starting data set #",data_set_counter))
  ML_merge_predictors_parallal_wrapper.fn(data_set_counter,General_fn_list,Merging_fn_list,directories_vector,input_mat_functions)#,Merging_fn_list,input_mat_functions)
}

#stop("check that it's working - add days of week as input columns, see pages 12-13 of https://cran.r-project.org/web/packages/lubridate/lubridate.pdf")
#stop("also consider decimal_date")

#### Clear variables ####
#rm()

#### End of file cleanup
#rm()

print(paste("ML_PM25_estimation_step0.R completed at",Sys.time(),sep = " "))
# stop the timer
proc.time() - start_code_timer
rm(start_code_timer)
