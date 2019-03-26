# ML_PM25_estimation_step0.R - merge the various predictor variables together with the monitor data 
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

#### Call Load Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path("estimate-pm25","General_Project_Functions","merging_data_functions.R"))
Merging_fn_list <- c("merge_predictors.fn","merge_GASP_data.fn","replace_column_names.fn","merge_time_varying_data.fn")
source(file.path(define_file_paths.fn("ML_Code.directory"),"ML_merge_predictors_parallal_wrapper_function.R"))
#source(file.path(define_file_paths.fn("ML_Code.directory"),"ML_processing_functions.R"))
#ML_processing_fn_list <- c("ML_input_report.fn", "ML_run_report.fn", "ML_plot_model.fn", "compare_multiple_models.fn", "merge_predictors.fn",
#                           "merge_time_varying_data.fn", "merge_time_static_data.fn", "merge_Highways_data.fn", "merge_GASP_data.fn", "merge_MAIAC_data.fn", "merge_NAM_data.fn", "merge_NED_data.fn", "merge_NLCD_data.fn")
#source(file.path(define_file_paths.fn("ML_Code.directory"),"Plotting_and_LaTex_functions.R"))
#Plotting_and_LaTex_fn_list <- c("Plot_to_ImageFile.fn", "Plot_and_latex.fn", "LaTex_code_4_figure.fn", "LaTex_code_start_subsection.fn")
#source(file.path(define_file_paths.fn("writingcode.directory"),"State_Abbrev_Definitions_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))
input_mat_functions <- c("input_mat_change_data_classes.fn", "input_mat_extract_year_from_date.fn",
                         "input_mat_extract_month_from_date.fn", "input_mat_extract_day_from_date.fn",
                         "fancy_which.fn", "subset_data_frame_via_vector.fn", "EPA_codes_2_components_no_hyphens.fn",
                         "remove_data_outside_range.fn")
#state_functions <- c("State_Abbrev_Definitions.fn","StateCode2StateName.fn","fill_in_StateNames_from_Code.fn")
# create vector with directories that will be needed in parallel functions
ProcessedData.directory <- define_file_paths.fn("ProcessedData.directory")
output.directory <- define_file_paths.fn("output.directory")
output.directory.short <- define_file_paths.fn("output.directory.short")
USMaps.directory <- define_file_paths.fn("USMaps.directory")
directories_vector <- c("ProcessedData.directory", "output.directory", "output.directory.short", "USMaps.directory")

#### define constants and variables needed for all R workers ####
processed_data_version <- define_study_constants.fn("processed_data_version")
#n_task_sets <- 2 # change to higher number as more code is written
#set_seed <- 42 # set seed for reproducible results
#set_seed <- 272 # same as Colleen's
fire_MODIS_25km_file_name <- c("fire_modis_part_bc_25km_final.csv","fire_modis_part_d_25km_extract_final.csv")
fire_MODIS_50km_file_name  <- c("fire_modis_part_bc_50km_final.csv","fire_modis_part_d_50km_extract_final.csv")
fire_MODIS_100km_file_name  <- c("fire_modis_part_bc_100km_final.csv","fire_modis_part_d_100km_extract_final.csv")
fire_MODIS_500km_file_name  <- c("fire_modis_part_bc_500km_final.csv","fire_modis_part_d_500km_extract_final.csv")
GASP_file_name <- c("GASP_extracted_part_b.csv","GASP_extracted_part_c.csv","GASP_extracted_part_b_2012-2014.csv")
Highways_file_name <- c("Highways_part_b.csv", "Highways_part_c.csv", "Highways_part_e.csv")
MAIAC_file_name <- c("MAIAC_extracted_part_b.csv", "MAIAC_extracted_part_c.csv","MAIAC_extracted_part_e_minus_b_done.csv")
NAM_file_name <- c("NAM_Step3_part_bc.csv") #,"NAM_Step3_part_bc.csv")
NDVI_file_name <- c("ndvi_mod13a3_part_bc_extract.csv","ndvi_mod13a3_part_d_extract.csv","ndvi_mod13a3_part_e_minus_bc_extract.csv")
NED_file_name <- c("ned_part_bc_extract.csv","ned_part_d_extract.csv","ned_part_e_not_in_bd_extract.csv")#c("ned_extract.csv","ned_extract.csv")
#NLCD_file_name <- c("nlcd_extract.csv","nlcd_extract.csv")
NLCD_1km_file_name <- c("nlcd_1km_part_bc_extract.csv","nlcd_part_d_1km_extract.csv","nlcd_part_e_not_bd_1km_extract.csv")
NLCD_5km_file_name <- c("nlcd_5km_part_bc_extract.csv","nlcd_5km_part_d_extract.csv","nlcd_5km_part_e_not_in_bd_extract.csv")
NLCD_10km_file_name <- c("nlcd_10km_part_bc_extract.csv","nlcd_10km_part_d_extract.csv","nlcd_10km_part_e_not_in_bd_extract.csv")

predictor_sub_folder <- "PredictorVariablesExtractedToDatesLocations"

#file_paths_to_merge_to <- c("PM25_data_part_d","PM25_data_part_c")
file_paths_to_merge_to <- c(paste("PM25_data_part_",processed_data_version,sep = ""),paste("PM25_data_part_",processed_data_version,sep = ""),"CountyCentroid")

#files_to_merge_to <- c("PM25_Step3_part_b_Projected", "CountyGeometricCentroids_Locations_Dates_part_c_2008-01-01to2008-12-31")
#files_to_merge_to <- c("PM25_Step5_part_d_de_duplicated_aves_ML_input", "CountyGeometricCentroids_Locations_Dates_part_c_2008-01-01to2008-12-31")
#files_to_merge_to <- c("PM25_Step5_part_d_de_duplicated_aves_ML_input", "CountyGeometricCentroids_Locations_Dates_part_c_2008-01-01to2008-12-31")
# without .csv at the end
files_to_merge_to <- c(paste("TEST1-1000-PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input",sep = ""),paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_prioritize_24hr_obs_ML_input",sep = ""), "CountyGeometricCentroids_Locations_Dates_part_c_2008-01-01to2008-12-31")
n_data_sets <- length(files_to_merge_to)
all_files_list <- c("fire_MODIS_25km_file_name","fire_MODIS_50km_file_name","fire_MODIS_100km_file_name","fire_MODIS_500km_file_name",
                    "GASP_file_name", "Highways_file_name", "MAIAC_file_name","NAM_file_name","NDVI_file_name","NED_file_name",
                    "NLCD_1km_file_name","NLCD_5km_file_name","NLCD_10km_file_name",
                    "predictor_sub_folder","files_to_merge_to","file_paths_to_merge_to")
print("make sure the file names and paths match")

#### Run the parallel loop ####
#n_cores <- detectCores() - 1 # Calculate the number of cores
#print(paste(n_cores,"cores available for parallel processing",sep = " "))

# Initiate cluster
#this_cluster <- makeCluster(n_cores)

# export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
#clusterExport(cl = this_cluster, varlist = c("processed_data_version",directories_vector,
#                                             Plotting_and_LaTex_fn_list, ML_processing_fn_list, input_mat_functions,
#                                             all_files_list), envir = .GlobalEnv)

## send necessary libraries to each parallel worker
#clusterEvalQ(cl = this_cluster, library(plyr)) # copy this line and call function again if another library is needed

# run function loop_NAM_run_times.parallel.fn in parallel
# X = 1:n_data_sets
#par_output <- parLapply(this_cluster, X = 1:n_data_sets, fun = ML_merge_predictors_parallal_wrapper.fn)#,

for (data_set_counter in 1:n_data_sets) {
  ML_merge_predictors_parallal_wrapper.fn(data_set_counter)
}

#input_mat <- par_output[[1]]

stop("add days of week as input columns, see pages 12-13 of https://cran.r-project.org/web/packages/lubridate/lubridate.pdf")
stop("also consider decimal_date")

# # End use of parallel computing #
# stopCluster(this_cluster)
# rm(this_cluster, n_cores)

#### Clear variables ####
#rm(n_data_sets, start_study_year, stop_study_year, voltage_threshold_upper, voltage_threshold_lower, input_header)
#rm(par_output, input_mat1)

#### End of file cleanup
#rm(uppermost.directory,output.directory)
#rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
#rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory) 
#rm(writingcode.directory,computer_system,NAM.directory,PythonProcessedData.directory)

print(paste("ML_PM25_estimation_step0.R completed at",Sys.time(),sep = " "))
# stop the timer
proc.time() - start_code_timer
rm(start_code_timer)
