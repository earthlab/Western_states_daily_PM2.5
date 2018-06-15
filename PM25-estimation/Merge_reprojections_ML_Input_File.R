# Merge_reprojections_ML_Input_File.R
# Replace the Lat/Lon/Datum information and add Northing/Easting info from reprojections of locations in 
# PM2.5 data with USA Contiguous Albers Equal Area Conic (ESRI:102003) information from file Ellen created with ____.py.

#### Define Constants ####
Reprojected_datum <- "ESRI102003"

#### Source functions I've written ####

#### Load the cleaned data (input_mat2) ####
print("Load data that was created in Clean_ML_Input_File.R")
this_source_file <- 'cleaned_ML_input.csv'
input_mat2<-read.csv(file.path(ProcessedData.directory,this_source_file),header=TRUE) # load data file
rm(this_source_file)
#class(input_mat1)
#class(input_mat1$Date_Local)

# load the reprojection information
this_source_file <- "Final_monitors.csv"
link_projections <- read.csv(file.path(PythonProcessedData.directory,this_source_file),header=TRUE)

# replace the location information in input_mat3 (write a function)
