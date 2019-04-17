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
Plotting_and_LaTex_fn_list <- c("Plot_to_ImageFile.fn", "Plot_and_latex.fn", "LaTex_code_4_figure.fn", "LaTex_code_start_subsection.fn")
source(file.path(define_file_paths.fn("writingcode.directory"),"State_Abbrev_Definitions_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))
source(file.path("estimate-pm25","General_Project_Functions","merging_data_functions.R"))

#### For new data ####
# Define columns to keep 
predictor_variables_step <- c("Date","Latitude","Longitude", "A_100" , "C_100","Both_100", "A_250","C_250","Both_250","A_500",               
                         "C_500","Both_500","A_1000","C_1000","Both_1000",#"GASP_AOD", # taking GASP AOD out since it's only available for a very limited time
                         "MAIAC_AOD",          
                         "HPBL.surface","TMP.2.m.above.ground","RH.2.m.above.ground", "DPT.2.m.above.ground","APCP.surface","WEASD.surface", 
                         "SNOWC.surface","UGRD.10.m.above.ground","VGRD.10.m.above.ground", "PRMSL.mean.sea.level", "PRES.surface","DZDT.850.mb",      
                         "DZDT.700.mb", "elevation","NLCD","Year","Month","Day", "DayOfWeek","DecimalDatewYear","DecimalDate")

meta_variables <- c("Date","Latitude","Longitude")

#predictor_variables <- c("Date","")
study_states_abbrev <- define_study_constants.fn("study_states_abbrev")  #c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
this_datum <- "NAD83"
print(predictor_variables_step)
col_name_interest <- "PM2.5_Obs" #"logpm25"
processed_data_version <- define_study_constants.fn("processed_data_version")

# Load input file
#file_paths_to_merge_to <- "ML_input_files" #c(paste("ML_input_PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input",processed_data_version,sep = ""),paste("PM25_data_part_",processed_data_version,sep = ""),"CountyCentroid")
#ML_input_files <- c(paste("ML_input_PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input",sep = ""),paste("ML_input_PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_prioritize_24hr_obs_ML_input",sep = ""), "ML_input_CountyCentroid_Locations_Dates_2008-01-01to2018-12-31")
ML_input_files <- c(paste("ML_input_PM25_Step4_part_",processed_data_version,"_de_duplicated_aves",sep = ""),paste("ML_input_PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_prioritize_24hr_obs",sep = ""), "ML_input_CountyCentroid_Locations_Dates_2008-01-01to2018-12-31")

#this_source_file <- "ML_input_PM25_Step4_part_e_de_duplicated_aves_prioritize_24hr_obs_ML_input.csv"#"ML_input_PM25_Step5_part_d_de_duplicated_aves_ML_input.csv"
sub_folder <- "ML_input_files"
#file_i <- 1
for (file_i in 1:1) { # cycle through files to make plots
#for (file_i in 1:length(ML_input_files)) { # cycle through files to make plots
this_source_file <- ML_input_files[file_i]
print(this_source_file)
Full_PM25_obs_extra_cols_and_NA<-read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(this_source_file,".csv",sep = "")),header=TRUE) # load the file
# Get rid of extra columns and rows with NA
which_predictors_present <- which(predictor_variables_step %in% colnames(Full_PM25_obs_extra_cols_and_NA))
predictor_variables <- predictor_variables_step[which_predictors_present]
print(predictor_variables)
which_non_meta <- which(predictor_variables %!in% meta_variables)
non_meta_predictors <- predictor_variables[which_non_meta]
print(non_meta_predictors)

Full_PM25_obs_w_NA <- Full_PM25_obs_extra_cols_and_NA[ ,c(col_name_interest,predictor_variables)]
rm(Full_PM25_obs_extra_cols_and_NA)

# Set classes of columns
Full_PM25_obs_w_NA$Date <- as.Date(Full_PM25_obs_w_NA$Date,"%Y-%m-%d") # recognize dates as dates: 'Date_Local' 

#### create reports for full data - including incomplete rows ####
# define first part of .tex file names to be output
file_sub_label <- paste("Report_",this_source_file,"wNAs",sep = "") # file partial name, decide whether to include date in file name
print(file_sub_label)
title_string_starter <- "ML Inputs (with NAs)" # will be used at beginning of title for plots
large_df_report.fn(df_in = Full_PM25_obs_w_NA, file_sub_label = file_sub_label, title_string_starter = title_string_starter, 
                   col_name_interest = col_name_interest, predictor_variables = predictor_variables, 
                   non_meta_predictors = non_meta_predictors)

#### create reports for data that only includes complete rows
Full_PM25_obs <- Full_PM25_obs_w_NA[complete.cases(Full_PM25_obs_w_NA), ] # get rid of any rows that have NAs
rm(Full_PM25_obs_w_NA)
# define first part of .tex file names to be output
file_sub_label <- paste("Report_",this_source_file,sep = "") # file partial name, decide whether to include date in file name
print(file_sub_label)
print("create report with plots/maps about the input data, consider removing any columns that have nearly constant values")
title_string_starter <- "ML Inputs" # will be used at beginning of title for plots
large_df_report.fn(df_in = Full_PM25_obs, file_sub_label = file_sub_label, title_string_starter = title_string_starter, 
                   col_name_interest = col_name_interest, predictor_variables = predictor_variables, 
                   non_meta_predictors = non_meta_predictors)

} # for (file_i in 1:length(ML_input_files)) { # cycle through files to make plots
  
# Obsolete code
# # plot predictor_variables against date
# LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"TimeSeriesImages.tex",sep = "")) # Start file for latex code images
# title_string_partial <- "ML Inputs Time Series" # used in plot titles and subsection name
# LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = FALSE) # start subsection for latex code
# sink.number()
# df_report.fn(df = Full_PM25_obs, cols_interest = c(col_name_interest,predictor_variables), x_axis_var = "Date", output.directory = define_file_paths.fn("output.directory"),
#              output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black",
#              LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg")
# 
# # plot predictor variables against PM2.5
# print(file_sub_label)
# LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"PredictorVPM25Images.tex",sep = "")) # Start file for latex code images
# title_string_partial <- "ML Inputs Plot against PM2.5" # used in plot titles and subsection name
# LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = FALSE) # start subsection for latex code
# df_report.fn(df = Full_PM25_obs, cols_interest = c(predictor_variables), x_axis_var = col_name_interest, output.directory = define_file_paths.fn("output.directory"),
#              output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black",
#              LatexFileName = LatexFileName, SinkFileName = NA)
# 
# # plot maps of data for a few specific days
# LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"MapSpecDaysImages.tex",sep = "")) # Start file for latex code images
# title_string_partial <- "ML Inputs Map subset of days" # used in plot titles and subsection name
# LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = FALSE) # start subsection for latex code
# dates_of_interest <- top_bottom_dates.fn(Full_PM25_ob) # find the days with the overall highest and lowest max concentrations
# df_map_subset_days.fn(this_df = Full_PM25_obs, cols_interest = c(col_name_interest, non_meta_predictors), dates_of_interest = dates_of_interest, output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black", LatexFileName = LatexFileName, SinkFileName = SinkFileName, image_format = "jpg",study_states_abbrev = study_states_abbrev,this_datum = this_datum)
# 
# # plot maps of monthly medians
# LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"MapMonthlySummariesImages.tex",sep = "")) # Start file for latex code images
# title_string_partial <- "ML Inputs Map monthly medians" # used in plot titles and subsection name
# LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = FALSE) # start subsection for latex code
# df_map_monthly_summary.fn(this_df = Full_PM25_obs, cols_interest = c(col_name_interest, non_meta_predictors), output.directory = define_file_paths.fn("output.directory"), output.directory.short = define_file_paths.fn("output.directory.short"), file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black", LatexFileName = LatexFileName, SinkFileName = SinkFileName, image_format = "jpg",study_states_abbrev,this_datum)

#SinkFileName=file.path(define_file_paths.fn("ProcessedData.directory"),paste(file_sub_label,".txt",sep = "")) # file name
#sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # start output to text file


