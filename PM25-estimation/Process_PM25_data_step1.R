# process PM2.5 data step 1: combine the various PM2.5 data sources
 
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

#### start timer for code ####
start_code_timer <- proc.time()
print(paste("Start Process_PM25_data_step1.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
library(measurements)
library(dismo)
library(rgdal)
library(raster)
library(lubridate)

#### Load Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path("estimate-pm25","General_Project_Functions","merging_data_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"process_PM25_parallal_wrapper_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"process_PM25_EPA_data_source_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"process_PM25_CARB_Mobile_data_source_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"CARB_Mobile_specific_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"process_PM25_Fire_Cache_data_source_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"Fire_Cache_specific_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"separate_character_vec_at_comma_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"State_Abbrev_Definitions_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"process_PM25_Lyman_Uintah_Basin_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"process_PM25_PCAPS_data_source_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"process_PM25_IMPROVE_data_source_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"process_PM25_CARB_data_source_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"process_PM25_UDEQ_data_source_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"separate_character_vec_at_comma_function.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"Plotting_and_LaTex_functions.R"))
general_functions <- c("define_study_constants.fn")
merging_functions <- c("replace_column_names.fn","determine_date_format.fn","%!in%")
Fire_cache_specific_functions <- c("Fire_Cache_consolidate_file_header.fn","Fire_Cache_comprehensive_header.fn",
                                   "Fire_Cache_remove_repeat_headers.fn", "Fire_Cache_change_data_classes.fn",
                                   "Fire_Cache_negative_longitudes.fn",
                                   "Fire_Cache_daily_averages.fn", "Fire_Cache_1_day_1_col_w_flag.fn",
                                   "Fire_Cache_1_day_ave.fn", "Fire_Cache_1_file_to_small_input_mat.fn")
input_mat_functions <- c("input_mat_change_data_classes.fn", "input_mat_extract_year_from_date.fn",
                         "input_mat_extract_month_from_date.fn", "input_mat_extract_day_from_date.fn",
                         "fancy_which.fn", "subset_data_frame_via_vector.fn", "EPA_codes_2_components_no_hyphens.fn")
state_functions <- c("State_Abbrev_Definitions.fn","StateCode2StateName.fn","fill_in_StateNames_from_Code.fn","StateAbbrev2StateCode.fn")
Uintah_basin_functions <- c("process_PM25_Lyman_Uintah_data_source.fn", "fill_in_UB_stations_input_mat.fn")
PCAPS_functions <- c("process_PM25_PCAPS_data_source.fn", "PCAPS_gather_lat_lon.fn")
IMPROVE_functions <- c("process_PM25_IMPROVE_data_source.fn", "fill_in_FMLE_code_components.fn")
CARB_functions <- c("process_PM25_CARB_data_source.fn", "compile_all_CARB_location_info.fn","merge_recent_CARB_files.fn")
CARB_Mobile_functions <- c("process_PM25_CARB_Mobile_data_source.fn")
CARB_Mobile_specific_functions <- c("drag_values_to_next_value.fn","CARB_Mobile_change_data_classes.fn","CARB_Mobile_daily_averages.fn",
                                    "make_unique_hours_obs.fn")
UDEQ_functions <- c("process_PM25_UDEQ_data_source.fn","merge_recent_UTDEQ_files.fn")
Plotting_functions <- c("Plot_to_ImageFile.fn", "Plot_to_ImageFile_TopOnly.fn", "Plot_to_ImageFile_BottomOnly.fn","LaTex_code_4_figure.fn",
                        "LaTex_code_start_subsection.fn","LaTex_code_start_subsubsection.fn", "LaTex_code_start_section.fn",
                        "Plot_and_latex.fn","load_State_Boundaries.fn","map_base_layer.fn","load_County_Boundaries.fn","map_county_base_layer.fn",
                        "df_report.fn","df_map_subset_days.fn","df_map_monthly_summary.fn","monthly_map_summary_all_yrs.fn",
                        "cut_point_legend_text.fn","map_point_values.fn","top_bottom_dates.fn","color_by_conc.fn","large_df_report.fn",
                        "replace_character_in_string.fn","map_data_locations.fn")

#### define constants and variables needed for all R workers ####
n_data_sets <- 10 # change to higher number as more code is written
start_study_year <- input_mat_extract_year_from_date.fn(define_study_constants.fn("start_date")) #2008
stop_study_year <- input_mat_extract_year_from_date.fn(define_study_constants.fn("end_date")) #2018#2014
voltage_threshold_upper <- define_study_constants.fn("voltage_threshold_upper") # 17
voltage_threshold_lower <- define_study_constants.fn("voltage_threshold_lower") #11
processed_data_version <- define_study_constants.fn("processed_data_version")
study_states_abbrev <- define_study_constants.fn("study_states_abbrev") #c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")

input_header <-  c('PM2.5_Obs','PM2.5_Lat','PM2.5_Lon','Datum','Date_Local','Year','Month','Day','State_Code','County_Code',
                   'Site_Num','Parameter_Code','POC','Parameter_Name','Sample_Duration','Pollutant_Standard','Units_of_Measure',
                   'Event_Type','Observation_Count','Observation_Percent','1st_Max_Value','1st_Max_Hour','AQI','Method_Code',
                   'Method_Name','PM25_Station_Name','Address','State_Name','County_Name','City_Name','CBSA_Name',
                   'Date_of_Last_Change', # columns in AQS data
                   'State_Abbrev','Winter','Data_Source_Name_Display','Data_Source_Name_Short','Data_Source_Counter',
                   'Source_File','Composite_of_N_rows','N_Negative_Obs', # other columns to include
                   "flg.Lat","flg.Lon","Type","flg.Type","flg.Site_Num","flg.PM25_Obs","l/m Ave. Air Flw", # DRI variables
                   "flg.AirFlw","Deg C Av Air Temp","flg.AirTemp","% Rel Humidty","flg.RelHumid","mbar Barom Press","flg.Barom Press", # DRI variables
                   "deg C Sensor  Int AT","flg.deg C Sensor Int AT","% Sensor Int RH","flg.%SensorIntRH", # DRI variables
                   "Wind Speed m/s","flg.WindSpeed","Battery Voltage volts","flg.BatteryVoltage","Alarm","flg.Alarm", # DRI variables
                   "InDayLatDiff","InDayLonDiff","PlottingColor","SerialNumber","VoltageFlag","FlowFlag","RHiFlag")

#### Run the parallel loop ####
n_cores <- detectCores() - 1 # Calculate the number of cores
print(paste(n_cores,"cores available for parallel processing",sep = " "))

# Initiate cluster
this_cluster <- makeCluster(n_cores)

# export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
clusterExport(cl = this_cluster, varlist = c("start_study_year","stop_study_year","voltage_threshold_upper","voltage_threshold_lower","input_header",
                                             "processed_data_version","study_states_abbrev",
                                             "process_PM25_EPA_data_source.fn","separate_character_vec_at_comma.fn",state_functions,
                                             "process_PM25_Fire_Cache_data_source.fn",general_functions, merging_functions, Fire_cache_specific_functions, input_mat_functions,
                                             Uintah_basin_functions, PCAPS_functions, IMPROVE_functions, "separate_character_vec_at_comma.fn",
                                             CARB_functions,CARB_Mobile_functions,CARB_Mobile_specific_functions,UDEQ_functions,"is_there_a_space.fn","sub_folder","define_file_paths.fn",
                                             Plotting_functions), envir = .GlobalEnv)

# send necessary libraries to each parallel worker
clusterEvalQ(cl = this_cluster, library(dismo)) # copy this line and call function again if another library is needed
clusterEvalQ(cl = this_cluster, library(rgdal)) # copy this line and call function again if another library is needed
clusterEvalQ(cl = this_cluster, library(raster)) # copy this line and call function again if another library is needed
clusterEvalQ(cl = this_cluster, library(measurements)) # copy this line and call function again if another library is needed

# run function loop_NAM_run_times.parallel.fn in parallel
# X = 1:n_data_sets
par_output <- parLapply(this_cluster, X = c(2,10), fun = process_PM25_parallal_wrapper.fn)

# End use of parallel computing #
stopCluster(this_cluster)
rm(this_cluster, n_cores)

#### concatinate the output from each iteration ####
input_mat1 <- do.call("rbind", par_output)

#### Save input_mat1 to csv file ####
file_sub_label <- paste("PM25_Step1_part_",processed_data_version,sep = "")
write.csv(input_mat1,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,'.csv',sep = "")),row.names = FALSE)

#### Clear variables ####
rm(n_data_sets, start_study_year, stop_study_year, voltage_threshold_upper, voltage_threshold_lower, input_header)
rm(par_output, input_mat1)
rm(working.directory)
rm(Fire_cache_specific_functions,input_mat_functions,state_functions, Uintah_basin_functions,
   PCAPS_functions,IMPROVE_functions,CARB_functions,UDEQ_functions)
rm(processed_data_version,study_states_abbrev,sub_folder,file_sub_label)

print(paste("Process_PM25_data_step1.R completed at",Sys.time(),sep = " "))
# stop the timer
proc.time() - start_code_timer
rm(start_code_timer)
