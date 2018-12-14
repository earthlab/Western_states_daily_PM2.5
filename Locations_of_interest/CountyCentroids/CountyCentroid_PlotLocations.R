# CountyCentroid_PlotLocations.R - Plot data created in CountyCentroid_CreateLatLonDateFiles.R

#### Call Packages (Library) ####
library(rgdal)
library(geosphere)

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"State_Abbrev_Definitions_function.R"))
source(file.path(ML_Code.directory,"Plotting_and_LaTex_functions.R"))
source(file.path(ML_Code.directory,"ML_processing_functions.R"))

processed_data_version <- "CountyCentroid"
study_states_abbrev <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
this_datum <- "NAD83"
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")

## set up documentation files/variables
print("set up code to input into subfolder directly")
file_sub_label <- "CountyGeometricCentroids" # file partial name, decide whether to include date in file name
title_string <- "Geometric Centroids of Counties" # used in figure titles, etc
plot_name_extension <-  "MapLocations"
LatexFileName=file.path(output.directory,paste("Rgenerated_",file_sub_label,"Images.tex",sep = "")) # Start file for latex code images
LaTex_code_start_subsection.fn(LatexFileName, title_string, append_option = FALSE) # start subsection for latex code
#SinkFileName=file.path(ProcessedData.directory,paste(file_sub_label,".txt",sep = "")) # file name
#sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # start output to text file

LaTex_code_start_subsection.fn(LatexFileName, title_string, append_option = FALSE) 

FigFileName <- Plot_to_ImageFile_TopOnly.fn(output.directory, file_sub_label, plot_name_extension = plot_name_extension) # start image file
# create map of counties
WestCountymapGeom <- map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev)

# plot centroids on map
#points(county_centroids$Longitude,county_centroids$Latitude,col="blue") # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
points(county_centroids$Lon,county_centroids$Lat,col="blue") # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
Plot_to_ImageFile_BottomOnly.fn(FigFileName = FigFileName, title_string = title_string) # finish image file


LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = title_string, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short)

