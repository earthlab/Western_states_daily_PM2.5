# ML_PM25_estimation_step0a.R
# plot input merged input file

#### Call Packages (Library) ####
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
library(plyr)
library(rgdal)
library(geosphere)

#### Call Load Functions that I created ####
source(file.path(ML_Code.directory,"ML_merge_predictors_parallal_wrapper_function.R"))
source(file.path(ML_Code.directory,"ML_processing_functions.R"))
ML_processing_fn_list <- c("ML_input_report.fn", "ML_run_report.fn", "ML_plot_model.fn", "compare_multiple_models.fn", "merge_predictors.fn",
                           "merge_time_varying_data.fn", "merge_time_static_data.fn", "merge_Highways_data.fn", "merge_GASP_data.fn", "merge_MAIAC_data.fn", "merge_NAM_data.fn", "merge_NED_data.fn", "merge_NLCD_data.fn")
source(file.path(ML_Code.directory,"Plotting_and_LaTex_functions.R"))
Plotting_and_LaTex_fn_list <- c("Plot_to_ImageFile.fn", "Plot_and_latex.fn", "LaTex_code_4_figure.fn", "LaTex_code_start_subsection.fn")
source(file.path(writingcode.directory,"State_Abbrev_Definitions_function.R"))
source(file.path(writingcode.directory,"input_mat_functions.R"))


#### For new data ####
# Define columns to keep 
predictor_variables <- c("Date","Latitude","Longitude", "A_100" , "C_100","Both_100", "A_250","C_250","Both_250","A_500",               
                         "C_500","Both_500","A_1000","C_1000","Both_1000","AOD","MAIAC_AOD",          
                         "HPBL.surface","TMP.2.m.above.ground","RH.2.m.above.ground", "DPT.2.m.above.ground","APCP.surface","WEASD.surface", 
                         "SNOWC.surface","UGRD.10.m.above.ground","VGRD.10.m.above.ground", "PRMSL.mean.sea.level", "PRES.surface","DZDT.850.mb",      
                         "DZDT.700.mb", "elevation","NLCD")

#predictor_variables <- c("Date","")

print(predictor_variables)
col_name_interest <- "PM2.5_Obs" #"logpm25"
# Load input file
this_source_file <- "ML_input_PM25_Step5_part_d_de_duplicated_aves_ML_input.csv"
sub_folder <- "ML_input_files"
Full_PM25_obs_extra_cols_and_NA<-read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file),header=TRUE) # load the AQS file
# Get rid of extra columns and rows with NA
Full_PM25_obs_w_NA <- Full_PM25_obs_extra_cols_and_NA[ ,c(col_name_interest,predictor_variables)]
rm(Full_PM25_obs_extra_cols_and_NA)
Full_PM25_obs <- Full_PM25_obs_w_NA
#Full_PM25_obs <- Full_PM25_obs_w_NA[complete.cases(Full_PM25_obs_w_NA), ]
print("comment line 38 and uncomment line 39 to remove rows with any NA values")

# Set classes of columns
Full_PM25_obs$Date <- as.Date(Full_PM25_obs$Date,"%Y-%m-%d") # recognize dates as dates: 'Date_Local' 

##### create report ####
#with plots/maps about the input data, consider removing any columns that have nearly constant values
print("create report with plots/maps about the input data, consider removing any columns that have nearly constant values")

file_sub_label <- paste("ML_input_report_",substr(this_source_file, 1, (nchar(this_source_file)-4)),sep = "") # file partial name, decide whether to include date in file name
print(file_sub_label)

## set up documentation files/variables
LatexFileName=file.path(output.directory,paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images

# plot predictor_variables against date
title_string_partial <- "ML Inputs Time Series" # used in plot titles and subsection name
LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = FALSE) # start subsection for latex code
sink.number()
df_report.fn(df = Full_PM25_obs, cols_interest = c(col_name_interest,predictor_variables), x_axis_var = "Date", output.directory = output.directory,
             output.directory.short = output.directory.short, file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black",
             LatexFileName = LatexFileName, SinkFileName = NA, image_format = "jpg")

# plot predictor variables against PM2.5
title_string_partial <- "ML Inputs Plot against PM2.5" # used in plot titles and subsection name
LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = TRUE) # start subsection for latex code
df_report.fn(df = Full_PM25_obs, cols_interest = c(predictor_variables), x_axis_var = col_name_interest, output.directory = output.directory,
             output.directory.short = output.directory.short, file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black",
             LatexFileName = LatexFileName, SinkFileName = NA)

# plot maps of data for a few specific days
study_states_abbrev <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
this_datum <- "NAD83"
title_string_partial <- "ML Inputs Map subset of days" # used in plot titles and subsection name
LaTex_code_start_subsection.fn(LatexFileName, title_string = title_string_partial, append_option = TRUE) # start subsection for latex code

dates_of_interest <- top_bottom_dates.fn(Full_PM25_ob) # find the days with the overall highest and lowest max concentrations
df_map_subset_days.fn(df = Full_PM25_obs, cols_interest = c(col_name_interest, predictor_variables[4:length(predictor_variables)]), dates_of_interest = dates_of_interest, output.directory = output.directory, output.directory.short = output.directory.short, file_sub_label = file_sub_label, title_string_partial = title_string_partial, plot_color = "black", LatexFileName = LatexFileName, SinkFileName = SinkFileName, image_format = "jpg",study_states_abbrev = study_states_abbrev,this_datum = this_datum)



SinkFileName=file.path(ProcessedData.directory,paste(file_sub_label,".txt",sep = "")) # file name
sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # start output to text file

