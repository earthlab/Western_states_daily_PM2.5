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
#class(input_mat1)
#class(input_mat1$Date_Local)

#### create new input_mat3 for the new location information to go into ####
input_header <-  c(colnames(input_mat2),'Northing','Easting')
print(input_header) 
N_columns <- length(input_header) # how many columns are in header?
mat_nrow=dim(input_mat2)[1]
input_mat3 <- data.frame(matrix(NA,nrow=mat_nrow,ncol=N_columns)) # create data frame for input_mat1
names(input_mat3) <- input_header # assign the header to input_mat1
rm(N_columns,mat_nrow)

# load the reprojection information
link_projections <- read.csv(file.path(ProcessedData.directory)

# replace the location information in input_mat3 (write a function)