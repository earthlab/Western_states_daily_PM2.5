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
library(stringr)
library(sp)
library(maps)
library(maptools)

#### Load Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
General_fn_list <- c("define_file_paths.fn","define_study_constants.fn","replace_character_in_string.fn",
                     "decimalplaces","print_name_value.fn","checksum.fn","check_4_NAs.fn")
source(file.path("estimate-pm25","General_Project_Functions","merging_data_functions.R"))
Merging_fn_list <- c("merge_predictors.fn","replace_column_names.fn","merge_time_varying_data.fn",
                     "merge_time_static_data.fn","merge_Fire_MODIS_data.fn","merge_Highways_data.fn","merge_GASP_data.fn","merge_MAIAC_data.fn",
                     "merge_NED_data.fn","merge_NLCD_data.fn","merge_NAM_data.fn","%!in%","average_slight_LatLon_variations.fn","determine_date_format.fn",
                     "merge_NDVI_data.fn", "add_season_indicator_columns.fn","DOY","latlong2state","merge_population_data.fn") # "YMD"
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

#### Define path and file names of Predictors to be merged (except NAM data) ####
predictor_sub_folder <- "PredictorVariablesExtractedToDatesLocations"
processed_data_version <- define_study_constants.fn("processed_data_version")
# # Ellen sent these updated file names on 9/17/2019:
# fire_MODIS_25km_file_name <- c("fire_modis_part_f_wLags_25km_extract_final.csv","fire_modis_part_g_25km_extract_final.csv") 
# fire_MODIS_50km_file_name  <- c( "fire_modis_part_f_wLags_50km_extract_final.csv"  ,"fire_modis_part_g_50km_extract_final.csv") 
# fire_MODIS_100km_file_name  <- c( "fire_modis_part_f_wLags_100km_extract_final.csv"  ,"fire_modis_part_g_100km_extract_final.csv") 
# fire_MODIS_500km_file_name  <- c( "fire_modis_part_f_wLags_500km_extract_final.csv"  ,"fire_modis_part_g_500km_extract_final.csv") 
# Highways_file_name <- c("Highways_part_e.csv","Highways_part_f_minus_e.csv","Highways_part_g.csv")
# MAIAC_file_name <- c("MAIAC_extracted_part_b.csv", "MAIAC_extracted_part_e_2014_JD-1-through-278.csv","MAIAC_extracted_part_e_not_in_b.csv","MAIAC_extracted_part_f_minus_e.csv","MAIAC_extracted_part_g.csv")
# NDVI_file_name <- c("ndvi_mod13a3_part_e_extract.csv","ndvi_mod13a3_part_f_minus_e_extract.csv", "ndvi_mod13a3_part_g_subset1_latlon.csv",  "ndvi_mod13a3_part_g_subset2_latlon.csv",  "ndvi_mod13a3_part_g_subset3_latlon.csv") 
# NED_file_name <- c("ned_part_bc_extract.csv","ned_part_e_not_in_b_extract.csv","ned_part_f_minus_e_extract.csv","ned_part_g_extract.csv") 
# NLCD_1km_file_name <- c("nlcd_1km_part_bc_extract.csv","nlcd_part_e_not_b_1km_extract.csv","nlcd_part_f_minus_e_1km_extract.csv","nlcd_part_g_1km_extract.csv") 
# NLCD_5km_file_name <- c("nlcd_5km_part_bc_extract.csv","nlcd_part_e_not_b_5km_extract.csv","nlcd_part_f_minus_e_5km_extract.csv","nlcd_part_g_5km_extract.csv") 
# NLCD_10km_file_name <- c("nlcd_10km_part_bc_extract.csv","nlcd_part_e_not_b_10km_extract.csv","nlcd_part_f_minus_e_10km_extract.csv","nlcd_part_g_10km_extract.csv") 
# Pop_density_file_name <- c("Pop_density_part_f.csv","Pop_density_part_g.csv")

#### Define information needed for merging in NAM data ####
NAM_folder <- "NAM_data" # define folder for NAM data
NAM_sub_folder <- "NAM_Step5" # define location of input files
NAM_sub_sub_folder <- paste("NAM_Step5_batch",define_study_constants.fn("NAM_batch_date"),sep = "")

#### Define files with dates/locations (and PM2.5 observations) to which the predictors will be merged ####
print("Consider merging the Census Tract (CT) files together, then merge with predictors")
file_paths_to_merge_to <- c(paste("PM25_data_part_",processed_data_version,sep = ""),
                            paste("PM25_data_part_",processed_data_version,sep = ""),
                            "PM25_Locations_Dates",
                            "PM25_Locations_Dates",
                            "PM25_Locations_Dates",
                            "PM25_Locations_Dates",
                            "PM25_Locations_Dates")
                            #"CountyCentroid")
files_to_merge_to <- c(paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input",sep = ""),
                       paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_prioritize_24hr_obs_ML_input",sep = ""),
                       "West_county_prediction_locations_dates",
                       "West_zips_prediction_locations_dates",
                       "West_CT_subset1_prediction_locations_dates",
                       "West_CT_subset2_prediction_locations_dates",
                       "West_CT_subset3_prediction_locations_dates")
output_sub_folder <- "ML_input_files"
output_sub_sub_folders <- c(paste("ML_input_part_",processed_data_version,"_Intermediary_Files_aves",sep = ""), 
                            paste("ML_input_part_",processed_data_version,"_Intermediary_Files_prioritize_24hr_obs",sep = ""),
                            "ML_input_files_CountyCentroid_Intermediary_Files",
                            "ML_input_files_ZIPs_Intermediary_Files",
                            "ML_input_files_CT_subset1_Intermediary_Files",
                            "ML_input_files_CT_subset1_Intermediary_Files",
                            "ML_input_files_CT_subset1_Intermediary_Files")
n_data_sets <- length(files_to_merge_to)
all_files_list <- c("fire_MODIS_25km_file_name","fire_MODIS_50km_file_name","fire_MODIS_100km_file_name","fire_MODIS_500km_file_name",
                    "Highways_file_name", "MAIAC_file_name","NDVI_file_name","NED_file_name",
                    "NLCD_1km_file_name","NLCD_5km_file_name","NLCD_10km_file_name","Pop_density_file_name",
                    "predictor_sub_folder","files_to_merge_to","file_paths_to_merge_to","NAM_folder","NAM_sub_folder","NAM_sub_sub_folder")
print("make sure the file names and paths match")

#### Loop through data sets for processing ####
#data_set_counter <- 1 # REMOVE
for (data_set_counter in 1:n_data_sets) { # cycle through the data sets for which predictors should be merged
  
  #### Start sink file ####
  SinkFileName=file.path(define_file_paths.fn("ProcessedData.directory"),output_sub_folder,paste(file_to_merge_to[data_set_counter],"_sink.txt",sep = ""))
  sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE)
  
  
  if (substr(files_to_merge_to[data_set_counter],1,15) == "PM25_Step4_part") {
    print("merging predictors onto PM2.5 data")
    # Ellen sent these updated file names on 9/17/2019:
    fire_MODIS_25km_file_name <- c("fire_modis_part_f_wLags_25km_extract_final.csv") 
    fire_MODIS_50km_file_name  <- c( "fire_modis_part_f_wLags_50km_extract_final.csv") 
    fire_MODIS_100km_file_name  <- c( "fire_modis_part_f_wLags_100km_extract_final.csv") 
    fire_MODIS_500km_file_name  <- c( "fire_modis_part_f_wLags_500km_extract_final.csv") 
    Highways_file_name <- c("Highways_part_e.csv","Highways_part_f_minus_e.csv")
    MAIAC_file_name <- c("MAIAC_extracted_part_b.csv", "MAIAC_extracted_part_e_2014_JD-1-through-278.csv","MAIAC_extracted_part_e_not_in_b.csv","MAIAC_extracted_part_f_minus_e.csv")
    NDVI_file_name <- c("ndvi_mod13a3_part_e_extract.csv","ndvi_mod13a3_part_f_minus_e_extract.csv") 
    NED_file_name <- c("ned_part_bc_extract.csv","ned_part_e_not_in_b_extract.csv","ned_part_f_minus_e_extract.csv") 
    NLCD_1km_file_name <- c("nlcd_1km_part_bc_extract.csv","nlcd_part_e_not_b_1km_extract.csv","nlcd_part_f_minus_e_1km_extract.csv") 
    NLCD_5km_file_name <- c("nlcd_5km_part_bc_extract.csv","nlcd_part_e_not_b_5km_extract.csv","nlcd_part_f_minus_e_5km_extract.csv") 
    NLCD_10km_file_name <- c("nlcd_10km_part_bc_extract.csv","nlcd_part_e_not_b_10km_extract.csv","nlcd_part_f_minus_e_10km_extract.csv") 
    Pop_density_file_name <- c("Pop_density_part_f.csv")
  } else {
    print("merging predictors for locations/dates of interest")
    # Ellen sent these updated file names on 9/17/2019:
    fire_MODIS_25km_file_name <- c("fire_modis_part_g_25km_extract_final.csv") 
    fire_MODIS_50km_file_name  <- c("fire_modis_part_g_50km_extract_final.csv") 
    fire_MODIS_100km_file_name  <- c("fire_modis_part_g_100km_extract_final.csv") 
    fire_MODIS_500km_file_name  <- c("fire_modis_part_g_500km_extract_final.csv") 
    Highways_file_name <- c("Highways_part_g.csv")
    MAIAC_file_name <- c("MAIAC_extracted_part_g.csv")
    NDVI_file_name <- c("ndvi_mod13a3_part_g_subset1_latlon.csv",  "ndvi_mod13a3_part_g_subset2_latlon.csv",  "ndvi_mod13a3_part_g_subset3_latlon.csv") 
    NED_file_name <- c("ned_part_g_extract.csv") 
    NLCD_1km_file_name <- c("nlcd_part_g_1km_extract.csv") 
    NLCD_5km_file_name <- c("nlcd_part_g_5km_extract.csv") 
    NLCD_10km_file_name <- c("nlcd_part_g_10km_extract.csv") 
    Pop_density_file_name <- c("Pop_density_part_g.csv")
  }
  print(paste("Starting data set #",data_set_counter,"-",files_to_merge_to[data_set_counter]))
  # create output folder if it doesn't already exist
  if(dir.exists(file.path(define_file_paths.fn("ProcessedData.directory"),output_sub_folder)) == FALSE) { # create directory if it doesn't already exist
    dir.create(file.path(define_file_paths.fn("ProcessedData.directory"),output_sub_folder))
  } # if(exists(file.path(define_file_paths.fn("ProcessedData.directory"),"NAM_data","NAM_Step3")) == FALSE) { # create directory if it doesn't already exist
  # create output sub folder if it doesn't already exist
  if(dir.exists(file.path(define_file_paths.fn("ProcessedData.directory"),output_sub_folder,output_sub_sub_folders[data_set_counter])) == FALSE) { # create directory if it doesn't already exist
    dir.create(file.path(define_file_paths.fn("ProcessedData.directory"),output_sub_folder,output_sub_sub_folders[data_set_counter]))
  } #if(dir.exists(file.path(define_file_paths.fn("ProcessedData.directory"),output_sub_sub_folder)) == FALSE) { # create directory if it doesn't already exist
  print(paste("start running ML_merge_predictors_parallal_wrapper.fn for",output_sub_sub_folders[data_set_counter]))
  ML_merge_predictors_parallal_wrapper.fn(data_set_counter,General_fn_list,Merging_fn_list,directories_vector,input_mat_functions,processed_data_version,output_sub_sub_folders)#,Merging_fn_list,input_mat_functions)
  print(paste("finished running ML_merge_predictors_parallal_wrapper.fn for",output_sub_sub_folders[data_set_counter]))
  
  # Merge all of the files that could have data for this date into one data frame
  print("get list of all output files")
  list_daily_ML_files = list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),output_sub_folder,output_sub_sub_folders[data_set_counter]), pattern = "csv$", full.names = TRUE) # list all files
  print("open all files")
  full_data_list = lapply(list_daily_ML_files, "read.csv") # read all files
  print("merge all files")
  Merged_input_file <- do.call("rbind",full_data_list) # merge files into one data frame
  rm(full_data_list,list_daily_ML_files)
  print("start writing data to file")
  this_source_file <- files_to_merge_to[data_set_counter] # get name of file to be merged to
  # define path and file name for output
  if (substr(this_source_file,(nchar(this_source_file)-8),nchar(this_source_file)) == "_ML_input") {
    ML_input_file_name_output_step <- substr(this_source_file,1,(nchar(this_source_file)-9))
  } else {
    ML_input_file_name_output_step <- this_source_file
  }
  ML_input_file_name_output <- paste("ML_input_",ML_input_file_name_output_step,sep = "")
  write.csv(Merged_input_file,file = file.path(ProcessedData.directory,output_sub_folder,paste(ML_input_file_name_output,'.csv',sep = "")),row.names = FALSE) # Write csv file
  print(paste("finished writing",ML_input_file_name_output,"to file"))
  
  sink()
} # for (data_set_counter in 1:n_data_sets) { # cycle through the data sets for which predictors should be merged

#### Clear variables ####
#rm()

#### End of file cleanup
#rm()

print(paste("ML_PM25_estimation_merge_predictors.R completed at",Sys.time(),sep = " "))
# stop the timer
proc.time() - start_code_timer
rm(start_code_timer)
