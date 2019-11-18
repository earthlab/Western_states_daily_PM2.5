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
caption_string <- "Locations of PM\\textsubscript{2.5} monitoring stations that had at least one observation during our study period (2008-2018)."
ClearPage = FALSE
LatexFileName <- file.path("/home/rstudio","estimate-pm25","LaTeX_documentation","SciData_manuscript","Fig1MonitorLocations.tex")
#if (exists(LatexFileName)) {
#  file.remove(LatexFileName) # delete file if an old version is there
#}

if (file.exists(LatexFileName) == TRUE) {file.remove(LatexFileName)}
image_file_type = "pdf"

  FigFileName <- Plot_to_ImageFile_TopOnly.fn(output.directory, file_sub_label, plot_name_extension = plot_name_extension,image_file_type) # start image file
  WestCountymapGeom <- map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev) # create map of counties
  plot_color <- "blue"
  points(this_df$Longitude,this_df$Latitude,pch = 19,col=plot_color)#, 
  Plot_to_ImageFile_BottomOnly.fn(FigFileName = FigFileName, title_string = title_string) # finish image file
  LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = caption_string, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short, image_format = image_file_type, ClearPage = ClearPage)
  
#### Code below this line probably obsolete ####
  #LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = title_string, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short, image_format = "jpg", ClearPage = ClearPage)
  