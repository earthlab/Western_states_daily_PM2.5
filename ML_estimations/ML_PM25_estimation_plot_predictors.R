# ML_PM25_estimation_plot_predictors.R
# plot input merged input file

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
print(paste("Start ML_PM25_estimation_plot_predictors.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
#library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
library(plyr)
library(rgdal)
library(geosphere)

#### Call Load Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"ML_merge_predictors_parallal_wrapper_function.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"ML_processing_functions.R"))
ML_processing_fn_list <- c("ML_input_report.fn", "ML_run_report.fn", "ML_plot_model.fn", "compare_multiple_models.fn", "merge_predictors.fn",
                           "merge_time_varying_data.fn", "merge_time_static_data.fn", "merge_Highways_data.fn", "merge_GASP_data.fn", "merge_MAIAC_data.fn", "merge_NAM_data.fn", "merge_NED_data.fn", "merge_NLCD_data.fn")
source(file.path(define_file_paths.fn("ML_Code.directory"),"Plotting_and_LaTex_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"Mapping_functions.R"))
Plotting_and_LaTex_fn_list <- c("Plot_to_ImageFile.fn", "Plot_and_latex.fn", "LaTex_code_4_figure.fn", "LaTex_code_start_subsection.fn")
source(file.path(define_file_paths.fn("writingcode.directory"),"State_Abbrev_Definitions_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))
source(file.path("estimate-pm25","General_Project_Functions","merging_data_functions.R"))

#### For new data ####
# Define columns to keep 
all_predictor_variables <- c("PM2.5_Obs","Latitude","Longitude",                     
                              "NewDatum","Date","Year" ,                         
                              "Month","Day","X25km_fire_count_lag0days"  ,   
                              "X50km_fire_count_lag0days","X100km_fire_count_lag0days","X500km_fire_count_lag0days",    
                              "X25km_fire_count_lag1days","X50km_fire_count_lag1days","X100km_fire_count_lag1days"  ,  
                              "X500km_fire_count_lag1days","X25km_fire_count_lag2days","X50km_fire_count_lag2days",     
                              "X100km_fire_count_lag2days","X500km_fire_count_lag2days","X25km_fire_count_lag3days" ,    
                              "X50km_fire_count_lag3days","X100km_fire_count_lag3days","X500km_fire_count_lag3days" ,   
                              "X25km_fire_count_lag4days","X50km_fire_count_lag4days","X100km_fire_count_lag4days" ,   
                              "X500km_fire_count_lag4days","X25km_fire_count_lag5days","X50km_fire_count_lag5days" ,    
                              "X100km_fire_count_lag5days","X500km_fire_count_lag5days","X25km_fire_count_lag6days",     
                              "X50km_fire_count_lag6days","X100km_fire_count_lag6days","X500km_fire_count_lag6days" ,   
                              "X25km_fire_count_lag7days","X50km_fire_count_lag7days","X100km_fire_count_lag7days"  ,  
                              "X500km_fire_count_lag7days","GASP_AOD","A_100",                         
                              "C_100","Both_100","A_250"       ,                  
                              "C_250","Both_250","A_500"     ,                    
                              "C_500","Both_500","A_1000"  ,                      
                              "C_1000","Both_1000","MAIAC_AOD",                     
                              "elevation","HPBL.surface","TMP.2.m.above.ground"  ,        
                              "RH.2.m.above.ground","DPT.2.m.above.ground","APCP.surface" ,                 
                              "WEASD.surface","SNOWC.surface","UGRD.10.m.above.ground",        
                              "VGRD.10.m.above.ground","PRMSL.mean.sea.level","PRES.surface",                  
                              "DZDT.850.mb","DZDT.700.mb","TimeZone",                      
                              "NLCD_1km_percent_urban_buffer","NLCD_5km_percent_urban_buffer","NLCD_10km_percent_urban_buffer",
                              "DayOfWeek","DecimalDatewYear","DecimalDate")
    #c("Date","Latitude","Longitude", "A_100" , "C_100","Both_100", "A_250","C_250","Both_250","A_500",               
                         #"C_500","Both_500","A_1000","C_1000","Both_1000","GASP_AOD", # taking GASP AOD out since it's only available for a very limited time
                         #"MAIAC_AOD",          
                         #"HPBL.surface","TMP.2.m.above.ground","RH.2.m.above.ground", "DPT.2.m.above.ground","APCP.surface","WEASD.surface", 
                         #"SNOWC.surface","UGRD.10.m.above.ground","VGRD.10.m.above.ground", "PRMSL.mean.sea.level", "PRES.surface","DZDT.850.mb",      
                         #"DZDT.700.mb", "elevation","NLCD","Year","Month","Day", "DayOfWeek","DecimalDatewYear","DecimalDate")

numerical_predictor_variables <- c("PM2.5_Obs","Latitude","Longitude",                     
                                   "Date","Year" ,                         
                                   "Month","Day","X25km_fire_count_lag0days"  ,   
                                   "X50km_fire_count_lag0days","X100km_fire_count_lag0days","X500km_fire_count_lag0days",    
                                   "X25km_fire_count_lag1days","X50km_fire_count_lag1days","X100km_fire_count_lag1days"  ,  
                                   "X500km_fire_count_lag1days","X25km_fire_count_lag2days","X50km_fire_count_lag2days",     
                                   "X100km_fire_count_lag2days","X500km_fire_count_lag2days","X25km_fire_count_lag3days" ,    
                                   "X50km_fire_count_lag3days","X100km_fire_count_lag3days","X500km_fire_count_lag3days" ,   
                                   "X25km_fire_count_lag4days","X50km_fire_count_lag4days","X100km_fire_count_lag4days" ,   
                                   "X500km_fire_count_lag4days","X25km_fire_count_lag5days","X50km_fire_count_lag5days" ,    
                                   "X100km_fire_count_lag5days","X500km_fire_count_lag5days","X25km_fire_count_lag6days",     
                                   "X50km_fire_count_lag6days","X100km_fire_count_lag6days","X500km_fire_count_lag6days" ,   
                                   "X25km_fire_count_lag7days","X50km_fire_count_lag7days","X100km_fire_count_lag7days"  ,  
                                   "X500km_fire_count_lag7days","GASP_AOD","A_100",                         
                                   "C_100","Both_100","A_250"       ,                  
                                   "C_250","Both_250","A_500"     ,                    
                                   "C_500","Both_500","A_1000"  ,                      
                                   "C_1000","Both_1000","MAIAC_AOD",                     
                                   "elevation","HPBL.surface","TMP.2.m.above.ground"  ,        
                                   "RH.2.m.above.ground","DPT.2.m.above.ground","APCP.surface" ,                 
                                   "WEASD.surface","SNOWC.surface","UGRD.10.m.above.ground",        
                                   "VGRD.10.m.above.ground","PRMSL.mean.sea.level","PRES.surface",                  
                                   "DZDT.850.mb","DZDT.700.mb",                      
                                   "NLCD_1km_percent_urban_buffer","NLCD_5km_percent_urban_buffer","NLCD_10km_percent_urban_buffer",
                                   "DayOfWeek","DecimalDatewYear","DecimalDate")

#predictor_variables_step <- c("Date","Latitude","Longitude","MAIAC_AOD")#,"TMP.2.m.above.ground","elevation") # COMMENT
dynamic_predictors_step <- c("PM2.5_Obs","X25km_fire_count_lag0days"  ,   
                             "X50km_fire_count_lag0days","X100km_fire_count_lag0days","X500km_fire_count_lag0days",    
                             "X25km_fire_count_lag1days","X50km_fire_count_lag1days","X100km_fire_count_lag1days"  ,  
                             "X500km_fire_count_lag1days","X25km_fire_count_lag2days","X50km_fire_count_lag2days",     
                             "X100km_fire_count_lag2days","X500km_fire_count_lag2days","X25km_fire_count_lag3days" ,    
                             "X50km_fire_count_lag3days","X100km_fire_count_lag3days","X500km_fire_count_lag3days" ,   
                             "X25km_fire_count_lag4days","X50km_fire_count_lag4days","X100km_fire_count_lag4days" ,   
                             "X500km_fire_count_lag4days","X25km_fire_count_lag5days","X50km_fire_count_lag5days" ,    
                             "X100km_fire_count_lag5days","X500km_fire_count_lag5days","X25km_fire_count_lag6days",     
                             "X50km_fire_count_lag6days","X100km_fire_count_lag6days","X500km_fire_count_lag6days" ,   
                             "X25km_fire_count_lag7days","X50km_fire_count_lag7days","X100km_fire_count_lag7days"  ,  
                             "X500km_fire_count_lag7days","GASP_AOD","MAIAC_AOD",                     
                             "HPBL.surface","TMP.2.m.above.ground"  ,        
                             "RH.2.m.above.ground","DPT.2.m.above.ground","APCP.surface" ,                 
                             "WEASD.surface","SNOWC.surface","UGRD.10.m.above.ground",        
                             "VGRD.10.m.above.ground","PRMSL.mean.sea.level","PRES.surface",                  
                             "DZDT.850.mb","DZDT.700.mb",                      
                             "NLCD_1km_percent_urban_buffer","NLCD_5km_percent_urban_buffer","NLCD_10km_percent_urban_buffer")
    #c("GASP_AOD", "MAIAC_AOD","HPBL.surface","TMP.2.m.above.ground","RH.2.m.above.ground", "DPT.2.m.above.ground","APCP.surface","WEASD.surface", 
                       #"SNOWC.surface","UGRD.10.m.above.ground","VGRD.10.m.above.ground", "PRMSL.mean.sea.level", "PRES.surface","DZDT.850.mb",      
                       #"DZDT.700.mb", "NLCD")

meta_variables <- c("Date","Latitude","Longitude", "Year","Month","Day","DayOfWeek","DecimalDatewYear","DecimalDate")

#predictor_variables <- c("Date","")
study_states_abbrev <- define_study_constants.fn("study_states_abbrev")  #c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
this_datum <- "NAD83"
print(numerical_predictor_variables)
col_name_interest <- "PM2.5_Obs" #"logpm25"
processed_data_version <- define_study_constants.fn("processed_data_version")

# Load input file
#file_paths_to_merge_to <- "ML_input_files" #c(paste("ML_input_PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input",processed_data_version,sep = ""),paste("PM25_data_part_",processed_data_version,sep = ""),"CountyCentroid")
#ML_input_files <- c(paste("ML_input_PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input",sep = ""),paste("ML_input_PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_prioritize_24hr_obs_ML_input",sep = ""), "ML_input_CountyCentroid_Locations_Dates_2008-01-01to2018-12-31")
ML_input_files <- c(paste("ML_input_PM25_Step4_part_",processed_data_version,"_de_duplicated_aves",sep = ""),paste("ML_input_PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_prioritize_24hr_obs",sep = ""), "ML_input_CountyCentroid_Locations_Dates_2008-01-01to2018-12-31")
#ML_input_files <- "ML_input_PM25_Step4_part_e_de_duplicated_aves2008-18_noNAM" # COMMENT
#this_source_file <- "ML_input_PM25_Step4_part_e_de_duplicated_aves_prioritize_24hr_obs_ML_input.csv"#"ML_input_PM25_Step5_part_d_de_duplicated_aves_ML_input.csv"
sub_folder <- "ML_input_files"
#file_i <- 1
for (file_i in 1:1) { # cycle through files to make plots
#for (file_i in 1:length(ML_input_files)) { # cycle through files to make plots

this_source_file_step <- paste(ML_input_files[file_i],"_compiled_",sep = "")
print(this_source_file_step)
this_file_path <- file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder)
recent_file_name <- determine_recent_file.fn(file_pattern_before_date = this_source_file_step,file_pattern_after_date = "",file_suffix = ".csv", file_path = this_file_path)
recent_file_name_no_suffix <- substr(recent_file_name,1,nchar(recent_file_name)-4)
  
#Full_PM25_obs_extra_cols_and_NA<-read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(this_source_file,".csv",sep = "")),header=TRUE) # load the file
Full_PM25_obs_extra_cols_and_NA<-read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,recent_file_name),header=TRUE) # load the file

# Get rid of extra columns and rows with NA
#which_predictors_present <- which(predictor_variables_step %in% colnames(Full_PM25_obs_extra_cols_and_NA))
which_predictors_present <- which(numerical_predictor_variables %in% colnames(Full_PM25_obs_extra_cols_and_NA))
#predictor_variables <- predictor_variables_step[which_predictors_present]
predictor_variables <- numerical_predictor_variables[which_predictors_present]
print(predictor_variables)

which_non_meta <- which(predictor_variables %!in% meta_variables)
non_meta_predictors <- predictor_variables[which_non_meta]
print(non_meta_predictors)

#which_dynamic <- which(predictor_variables %in% dynamic_predictors_step)
which_dynamic <- which(dynamic_predictors_step %in% predictor_variables)
dynamic_predictors <- dynamic_predictors_step[which_dynamic]
print(dynamic_predictors)

Full_PM25_obs_w_NA <- Full_PM25_obs_extra_cols_and_NA[ ,c(col_name_interest,predictor_variables)]
rm(Full_PM25_obs_extra_cols_and_NA)

# Set classes of columns
Full_PM25_obs_w_NA$Date <- as.Date(Full_PM25_obs_w_NA$Date,"%Y-%m-%d") # recognize dates as dates: 'Date_Local' 

#### create reports for full data - including incomplete rows ####
# define first part of .tex file names to be output
#file_sub_label <- paste("Report_",this_source_file,"wNAs",sep = "") # file partial name, decide whether to include date in file name
file_sub_label <- paste("Report_",recent_file_name_no_suffix,"wNAs",sep = "") # file partial name, decide whether to include date in file name
print(file_sub_label)
title_string_starter <- "ML Inputs (with NAs)" # will be used at beginning of title for plots


large_df_report.fn(df_in = Full_PM25_obs_w_NA, file_sub_label = file_sub_label, title_string_starter = title_string_starter, 
                   col_name_interest = col_name_interest, predictor_variables = predictor_variables, 
                   non_meta_predictors = non_meta_predictors, dynamic_predictors = dynamic_predictors)

#LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"MapCountySpecDaysImages.tex",sep = "")) # Start file for latex code images
# map_value_by_region.fn(Region = "County", RegionMaps.directory = define_file_paths.fn("CountyMaps.directory"), 
#                        df_in = Full_PM25_obs_w_NA, start_date = "2008-07-11", end_date = "2008-07-11",#"2008-08-11", 
#                        Date_col = "Date", Lat_col = "Latitude", Lon_col = "Longitude", Var_col = "PM2.5_Obs", 
#                        Cut_points_set = FALSE, cut_point_scale = Var_col,study_states_abbrev,
#                        output.directory = define_file_paths.fn("output.directory"),file_sub_label = file_sub_label,
#                        LatexFileName = LatexFileName,title_string_starter = title_string_starter)#, plot_name_extension)

# #### create reports for data that only includes complete rows
# Full_PM25_obs <- Full_PM25_obs_w_NA[complete.cases(Full_PM25_obs_w_NA), ] # get rid of any rows that have NAs
# rm(Full_PM25_obs_w_NA)
# # define first part of .tex file names to be output
# file_sub_label <- paste("Report_",this_source_file,sep = "") # file partial name, decide whether to include date in file name
# print(file_sub_label)
# print("create report with plots/maps about the input data, consider removing any columns that have nearly constant values")
# title_string_starter <- "ML Inputs" # will be used at beginning of title for plots
# large_df_report.fn(df_in = Full_PM25_obs, file_sub_label = file_sub_label, title_string_starter = title_string_starter,
#                    col_name_interest = col_name_interest, predictor_variables = predictor_variables,
#                    non_meta_predictors = non_meta_predictors)


} # for (file_i in 1:length(ML_input_files)) { # cycle through files to make plots
  