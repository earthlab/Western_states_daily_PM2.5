# Merge_reprojections_ML_Input_File.R
# Replace the Lat/Lon/Datum information and add Northing/Easting info from reprojections of locations in 
# PM2.5 data with USA Contiguous Albers Equal Area Conic (ESRI:102003) information from file Ellen created with ____.py.

#### Define Constants ####
Reprojected_datum <- "ESRI102003"
given_digits <- 0.00000001
this_source_file <- paste("PM25_Step2_part_",processed_data_version,".csv",sep = "") # define file name
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")

#### Source functions I've written ####
source(file.path(writingcode.directory,"check_digit_uniqueness_function.R"))
source(file.path(writingcode.directory,"Replace_LatLon_Reprojected_function.R"))

##### Create Sink output file ####
# sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
#SinkFileName=file.path(ProcessedData.directory,"Merge_reprojections_ML_Input_File_sink.txt")
#sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
#sink() #COMMENT
#cat("output for Merge_reprojections_ML_Input_File.R \n \n")
file_sub_label <- paste("PM25_Step5_part_",processed_data_version,sep = "")
SinkFileName=file.path(ProcessedData.directory,sub_folder,paste(file_sub_label,"_sink.txt",sep = ""))
sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE)
cat("output for Process_PM25_data_step5.R \n \n")
cat("Source file:")
cat(this_source_file)

#### Load the cleaned data (input_mat2) ####
print("Load data that was created in Step 2 (steps 3-4 only handled' location info)") #Clean_ML_Input_File.R")
#this_source_file <- 'cleaned_ML_input.csv'
input_mat2<-read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file),header=TRUE) # load data file
rm(this_source_file)
#class(input_mat1)
#class(input_mat1$Date_Local)

# load the reprojection information
#this_source_file <- "Final_monitors.csv"
#link_projections <- read.csv(file.path(PythonProcessedData.directory,this_source_file),header=TRUE)
#rm(this_source_file)
file_name_loc <- paste("PM25_Step3_part_",processed_data_version,"_Locations_Projected.csv",sep = "")
link_projections <- read.csv(file.path(ProcessedData.directory,sub_folder,file_name_loc), stringsAsFactors = FALSE) # load data
#file_name_loc_dates <- "PM25_Step3_part_b_Locations_Dates_Projected.csv"
#part_b_loc_dates <- read.csv(file.path(ProcessedData.directory,sub_folder_name,file_name_loc_dates), stringsAsFactors = FALSE) # load data

# check that the number of digits in given_digits is sufficiently long to distinguish between values in link_projections
check_digit_uniqueness.fn(link_projections,given_digits) 

# replace the location information in input_mat3 (write a function)
input_mat3 <- Replace_LatLon_Reprojected.fn(input_mat2,link_projections,Reprojected_datum,given_digits)
rm(input_mat2)

#### Save cleaned file to .csv ####
print("summary of the data output by Merge_reprojections_ML_Input_File.R:")
summary(input_mat3) # give summary of current state of data
print("file names still included")
unique(input_mat3$Source_File)
write.csv(input_mat3,file = file.path(ProcessedData.directory,'reprojected_ML_input.csv'),row.names = FALSE)

#### Create a data frame with just lat, lon, and date ####
four_cols_w_duplicates <- input_mat3[,c("PM2.5_Lat","PM2.5_Lon","Datum","Date_Local")]
four_cols_data <- four_cols_w_duplicates[!duplicated(four_cols_w_duplicates),]
names(four_cols_data) <- c("Latitude","Longitude","Datum","Date")
write.csv(four_cols_data,file = file.path(ProcessedData.directory,'Locations_Dates_of_PM25_Obs_from_merge_script.csv'),row.names = FALSE)
rm(four_cols_data,four_cols_w_duplicates)

#### Create a data frame with just lat, and lon ####
three_cols_w_duplicates <- input_mat3[,c("PM2.5_Lat","PM2.5_Lon","Datum")]
three_cols_data <- three_cols_w_duplicates[!duplicated(three_cols_w_duplicates),]
names(three_cols_data) <- c("Latitude","Longitude","Datum")
write.csv(three_cols_data,file = file.path(ProcessedData.directory,'Locations_PM25_Obs_from_merge_script.csv'),row.names = FALSE)
rm(three_cols_data,three_cols_w_duplicates)

#### End of file clean up ####
sink()
rm(link_projections,input_mat3)
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
rm(writingcode.directory,computer_system,NARR.directory,PythonProcessedData.directory)
rm(SinkFileName,Reprojected_datum,given_digits)
