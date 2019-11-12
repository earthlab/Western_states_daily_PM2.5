# Map_Monitor_Locations.R - create Figure 1 of manuscript

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

### Load data file ###
full_df <- read.csv(file.path("/home","rstudio","Processed_Data","ML_input_files","ML_input_PM25_Step4_part_f_de_duplicated_aves.csv"),header=TRUE) # load the file
LatLon_df_w_repeats <- full_df[ ,c("Latitude","Longitude")] # isolate lat/lon columns
this_df <- LatLon_df_w_repeats[!duplicated(LatLon_df_w_repeats),] # remove repeats
rm(full_df,LatLon_df_w_repeats)

### Create Figure ####

output.directory <- file.path("/home/rstudio","estimate-pm25","LaTeX_documentation","SciData_manuscript")
file_sub_label <- "Monitor"
plot_name_extension = "Locations" 
study_states_abbrev = define_study_constants.fn("study_states_abbrev")
this_datum <- define_study_constants.fn("study_datum")
title_string <- "Observation Locations"
ClearPage = FALSE
LatexFileName <- file.path("/home/rstudio","estimate-pm25","LaTeX_documentation","SciData_manuscript","Fig1MonitorLocations.tex")
if (exists(LatexFileName)) {
  file.remove(LatexFileName) # delete file if an old version is there
}


  FigFileName <- Plot_to_ImageFile_TopOnly.fn(output.directory, file_sub_label, plot_name_extension = plot_name_extension) # start image file
  WestCountymapGeom <- map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev) # create map of counties
  plot_color <- "blue"
  points(this_df$Longitude,this_df$Latitude,pch = 19,col=plot_color)#, 
  Plot_to_ImageFile_BottomOnly.fn(FigFileName = FigFileName, title_string = title_string) # finish image file
  LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = title_string, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short, image_format = "jpg", ClearPage = ClearPage)

#### Code below this line probably obsolete ####
  #source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
  #source(file.path("estimate-pm25","General_Project_Functions","merging_data_functions.R"))
  
  #csv_file_name_w_path <- file.path("home","rstudio","Processed_Data","ML_input_files","ML_input_PM25_Step4_part_f_de_duplicated_aves.csv")
  #full_df <- read.csv(csv_file_name_w_path)
  
  #processed_data_version <- define_study_constants.fn("processed_data_version")
  
  #sub_folder <- "ML_input_files"
  #ML_input_files <- c(paste("ML_input_PM25_Step4_part_",processed_data_version,"_de_duplicated_aves",sep = ""),paste("ML_input_PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_prioritize_24hr_obs",sep = ""))#, "ML_input_CountyCentroid_Locations_Dates_2008-01-01to2018-12-31")
  #file_i <- 1
  #full_df <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(ML_input_files[file_i],".csv",sep = "")),header=TRUE) # load the file
  #full_df <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),"ML_input_files",paste(ML_input_files[file_i],".csv",sep = "")),header=TRUE) # load the file
  #full_df <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),"ML_input_files",paste("ML_input_PM25_Step4_part_",processed_data_version,"_de_duplicated_aves.csv",sep = "")),header=TRUE) # load the file
  #full_df <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),"ML_input_files","ML_input_PM25_Step4_part_f_de_duplicated_aves.csv"),header=TRUE) # load the file
  
  #map_point_values.fn <- function(this_df, var_interest, cut_point_scale = "PM2.5_Obs", output.directory, file_sub_label, plot_name_extension = plot_name_extension, study_states_abbrev,this_datum, title_string, ClearPage = FALSE, Cut_points_set = FALSE, color_cut_points = NA, color_vec = NA, LatexFileName) { # plot points of observations on map and color points by concentration
  #} # end of map_point_values.fn function
  
