# Process_PM25_data_step3.R - reproject locations to all have the same datum

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

#### Source functions I've written ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"reprojection_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))

#### Reproject data ####
# get names of folders and files 
processed_data_version <- define_study_constants.fn("processed_data_version")
this_source_file_loc <- paste("PM25_Step2_part_",processed_data_version,"_Locations.csv",sep = "") # define file name
print(this_source_file_loc)
this_source_file_loc_date <- paste("PM25_Step2_part_",processed_data_version,"_Locations_Dates.csv",sep = "") # define file name
print(this_source_file_loc_date)
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")
print(sub_folder)

# call function to reproject locations all to the same datum
reproject_monitors.fn(this_source_file_loc = this_source_file_loc, this_source_file_loc_date = this_source_file_loc_date, sub_folder = sub_folder,
                      Round_LatLon_decimals = TRUE, Round_N_decimal_places = define_study_constants.fn("round_lat_lon_digits")) # reproject location and location/date files and print to csv

# put the locations (converted to NAD83) into the full input_mat1
this_source_file <- paste("PM25_Step2_part_",processed_data_version,".csv",sep = "") # define file name
reprojected_into_input_mat1.fn(ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), sub_folder = sub_folder, this_source_file = this_source_file, this_source_file_loc = this_source_file_loc)

#### check that no NAs were introduced into Lat, Lon, and date columns - this section could be commented if everything is working #### 
Locations_file <- paste("PM25_Step3_part_",processed_data_version,"_Locations_NAD83.csv",sep = "") # define file name
Locations_input_mat <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,Locations_file),header=TRUE, stringsAsFactors=FALSE) # read step 3 locations file
Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lon","Lat","Datum"), input_data = Locations_input_mat)
if (length(Check_data)>0) {stop("***check_4_NAs.fn found questionable data. Investigate.***")}
rm(Check_data)#write.csv(input_mat2, file = file.path(ProcessedData.directory,sub_folder,paste(new_file_name,'_Projected','.csv',sep = "")),row.names = FALSE)
rm(Locations_file,Locations_input_mat)

Locations_Dates_file <- paste("PM25_Step3_part_",processed_data_version,"_Locations_Dates_NAD83.csv",sep = "") # define file name
Locations_Dates_input_mat <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,Locations_Dates_file),header=TRUE, stringsAsFactors=FALSE) # read step 3 locations file
Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lon","Lat","Datum","Date"), input_data = Locations_Dates_input_mat)
if (length(Check_data)>0) {stop("***check_4_NAs.fn found questionable data. Investigate.***")}
rm(Check_data)#write.csv(input_mat2, file = file.path(ProcessedData.directory,sub_folder,paste(new_file_name,'_Projected','.csv',sep = "")),row.names = FALSE)
rm(Locations_Dates_file,Locations_Dates_input_mat)

this_source_file <- paste("PM25_Step3_part_",processed_data_version,"_NAD83.csv",sep = "") # define file name
input_mat <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,this_source_file),header=TRUE, stringsAsFactors=FALSE) # read step 3 full data file
input_mat <- input_mat_change_data_classes.fn(input_mat) # set variable classes
Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs" ,"Date_Local","Year","Month","Day"), input_data = input_mat)
if (length(Check_data)>0) {stop("***check_4_NAs.fn found questionable data. Investigate.***")}
if (class(input_mat$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
rm(Check_data)
rm(this_source_file,input_mat)
