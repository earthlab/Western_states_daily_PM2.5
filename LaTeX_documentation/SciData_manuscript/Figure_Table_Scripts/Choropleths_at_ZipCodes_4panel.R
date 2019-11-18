# Choropleths_at_ZipCodes_4panel.R
# choropleths at zip code level - 4-panel

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

#### Load Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"Plotting_and_LaTex_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"Mapping_functions.R"))

#### Define file path ####
output.directory <- file.path("/home/rstudio","estimate-pm25","LaTeX_documentation","SciData_manuscript")

#### Load data file ####
full_df <- read.csv(file.path("/home","rstudio","Processed_Data","ML_input_files","ML_input_PM25_Step4_part_f_de_duplicated_aves.csv"),header=TRUE) # load the file

#### Process the data ####
# Set classes of columns
full_df$Date <- as.Date(full_df$Date,"%Y-%m-%d") # recognize dates as dates: 'Date_Local' 
df_step <- full_df[ , c("Date",col_name_interest,"Latitude","Longitude")]
df_in <- na.rm(df_step)

#### Define file names ####
Top_latex_file_name <- file.path(define_file_paths.fn("output.directory"),"Report_top_template.tex")
Main_latex_file_name <- file.path(output.directory,paste("Rgenerated_Fig2_choropleth_4-panel_main.tex",sep = ""))
Bottom_latex_file_name <- file.path(define_file_paths.fn("output.directory"),"Report_bottom_template.tex")
Report_latex_file_name <- file.path(output.directory,paste("Rgenerated_Fig2_choropleth_4-panel.tex",sep = ""))
file_sub_label <- "Fig2_choropleth_4-panel"
LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"MapCountyMonthlyImages.tex",sep = "")) # Start file for latex code images

#### Define Variables ####
col_name_interest <- "PM2.5_Obs"
study_states_abbrev = define_study_constants.fn("study_states_abbrev")

### Create Figure ####

if (sink.number()>0) {sink()} # get stop any lingering sinks
if (file.exists(LatexFileName) == TRUE) {file.remove(LatexFileName)}
print("map county monthly median values- PM2.5 only")
map_spec_days_value_by_region.fn(Region = "County", RegionMaps.directory = define_file_paths.fn("CountyMaps.directory"),
                                 df_in = df_in, dates_of_interest = NA, Date_col = "Date",
                                 Lat_col = "Latitude", Lon_col = "Longitude", Var_col = col_name_interest,
                                 Cut_points_set = TRUE, cut_point_scale = col_name_interest, study_states_abbrev = study_states_abbrev,
                                 output.directory = output.directory,file_sub_label = file_sub_label,
                                 LatexFileName = LatexFileName,title_string_starter = title_string_starter, Time_aggregation = "Monthly")


