# CountyCentroid_PlotLocations.R - Plot data created in CountyCentroid_CreateLatLonDateFiles.R

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

#### Call Packages (Library) ####
library(rgdal)
library(geosphere)

#### Call Load Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"State_Abbrev_Definitions_function.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"Plotting_and_LaTex_functions.R"))
source(file.path(define_file_paths.fn("ML_Code.directory"),"ML_processing_functions.R"))

#### define constants ####
study_states_abbrev <- define_study_constants.fn("study_states_abbrev") # get the study area (state abbreviations)
this_datum <- define_study_constants.fn("study_datum") # get the datum used for the study
processed_data_version <- "CountyCentroid" # previously "c" # define the name of this data
sub_folder <- processed_data_version #paste("PM25_data_part_",processed_data_version,sep = "")
start_date <- define_study_constants.fn("start_date") # get the start date for the study
end_date <- define_study_constants.fn("end_date") # get the end date for the study

#### set up documentation files/variables ####
file_sub_label <- "CountyGeometricCentroids" # file partial name, decide whether to include date in file name
title_string <- "Geometric Centroids of Counties" # used in figure titles, etc
plot_name_extension <-  "MapLocations" # part of file name for images
LatexFileName=file.path(define_file_paths.fn("output.directory"),paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
LaTex_code_start_subsection.fn(LatexFileName, title_string, append_option = FALSE) # start subsection for latex code
FigFileName <- Plot_to_ImageFile_TopOnly.fn(define_file_paths.fn("output.directory"), file_sub_label, plot_name_extension = plot_name_extension) # start image file

# Load data 
centroids_file_name <- file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(sub_folder,'_Locations','.csv',sep = ""))
county_centroids<-read.csv(centroids_file_name,header=TRUE) # load file

#### make plots/maps ####
WestCountymapGeom <- map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev) # create map of counties
rm(WestCountymapGeom) # clear variable
points(county_centroids$Lon,county_centroids$Lat,col="blue") # plot centroids on map
# resource: # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
Plot_to_ImageFile_BottomOnly.fn(FigFileName = FigFileName, title_string = title_string) # finish image file
LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = title_string, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short) # output latex code for image

#### output summary to sink file ####
file_sub_label <- paste(processed_data_version,"_Locations_File_Summary",sep = "")
SinkFileName=file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,".txt",sep = ""))
sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
cat("Summary of file \n")
cat(paste(centroids_file_name,"\n \n",sep = " "))
summary(county_centroids)
#sink()
rm(county_centroids, centroids_file_name) # clear variables

# create summary for locations/dates file
centroids_dates_file_name <- file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(sub_folder,'_Locations_Dates',"_",start_date,"to",end_date,'.csv',sep = ""))
centroids_dates <- read.csv(centroids_dates_file_name,header=TRUE) # load file
centroids_dates$Date <- as.Date(centroids_dates$Date,"%Y-%m-%d")
cat("\n \n ---- \n \n")
cat("Summary of file \n")
cat(paste(centroids_dates_file_name,"\n \n",sep = " "))
summary(centroids_dates)
sink()
rm(SinkFileName)
rm(centroids_dates_file_name,centroids_dates,sub_folder, start_date, end_date, processed_data_version)
rm(file_sub_label,title_string,plot_name_extension,LatexFileName,FigFileName)
rm(study_states_abbrev,this_datum)
