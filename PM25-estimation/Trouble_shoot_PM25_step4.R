
# define data of interest
day_interest <- as.Date("2008-01-01")
State_Code_interest <- 6
County_Code_interest <- 41

this_source_file <- paste("PM25_Step1_part_",processed_data_version,".csv",sep = "") # define file name
Step1_data <- read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file),header=TRUE, stringsAsFactors=FALSE) # read step 3 full data file
Step1_data <- input_mat_change_data_classes.fn(Step1_data) # set variable classes

# find data of interest
which_data <- which(Step1_data$Date_Local == day_interest & Step1_data$State_Code == State_Code_interest & Step1_data$County_Code == County_Code_interest)
Step1_data_interest <- Step1_data[which_data, ]
write.csv(Step1_data_interest,file = file.path(ProcessedData.directory,sub_folder,paste('PM25_Step1_part_',processed_data_version,'_example_duplicates.csv',sep = "")),row.names = FALSE) # Write csv file

######## 
lat_interest <- this_lat #37.74307
lon_interest <- this_lon #-122.1699

# find it in the data it came from
which_this_data <- which(Locations_input_mat3$Lat== lat_interest & Locations_input_mat3$Lon == lon_interest)

which_this_location <- which(input_mat3$Lat == this_lat)# & input_mat3$Lon == this_lon) # find the rows of data with this location
this_lat_data <- input_mat3[which_this_location, ]

which_this_location <- which(input_mat3$Lon == this_lon)# & input_mat3$Lon == this_lon) # find the rows of data with this location
this_lon_data <- input_mat3[which_this_location, ]

which_this_location <- which(input_mat3$Lat == this_lat & input_mat3$Lon == this_lon) # find the rows of data with this location

which_this_location_try2 <- which(input_mat3$Lat == this_lat & round(input_mat3$Lon,4) == round(this_lon,4)) # find the rows of data with this location


which_this_location <- which(round(input_mat3$Lat,5) == round(this_lat,5) & round(input_mat3$Lon,5) == round(this_lon,5)) # find the rows of data with this location
length(which_this_location)

which_this_location <- which(round(input_mat3$Lat,5) == round(this_lat,5) & round(input_mat3$Lon,4) == round(this_lon,4)) # find the rows of data with this location
length(which_this_location)

which_this_location <- which(round(input_mat3$Lat,5) == round(this_lat,5) & round(input_mat3$Lon,3) == round(this_lon,3)) # find the rows of data with this location
length(which_this_location)

which_this_location <- which(round(input_mat3$Lat,5) == round(this_lat,5) & round(input_mat3$Lon,2) == round(this_lon,2)) # find the rows of data with this location
length(which_this_location)

which_this_location <- which(round(input_mat3$Lat,5) == round(this_lat,5) & round(input_mat3$Lon,1) == round(this_lon,1)) # find the rows of data with this location
length(which_this_location)

which_this_location <- which(round(input_mat3$Lat,4) == round(this_lat,4) & round(input_mat3$Lon,4) == round(this_lon,4)) # find the rows of data with this location
length(which_this_location)

which_this_location <- which(round(input_mat3$Lat,4) == round(this_lat,4) & round(input_mat3$Lon,3) == round(this_lon,3)) # find the rows of data with this location
length(which_this_location)

which_this_location <- which(round(input_mat3$Lat,3) == round(this_lat,3) & round(input_mat3$Lon,4) == round(this_lon,4)) # find the rows of data with this location
length(which_this_location)

which_this_location <- which(round(input_mat3$Lat,4) == round(this_lat,4) & round(input_mat3$Lon,2) == round(this_lon,2)) # find the rows of data with this location
length(which_this_location)

which_this_location <- which(round(input_mat3$Lat,4) == round(this_lat,4) & round(input_mat3$Lon,2) == round(this_lon,2)) # find the rows of data with this location

this_loc_data <- input_mat3[which_this_location, ]
unique(this_loc_data$PM25_Station_Name)
unique(this_loc_data$Source_File)

#which_this_data <- which(round(Locations_input_mat3$Lat,given_digits)== round(lat_interest,given_digits) & round(Locations_input_mat3$Lon,given_digits) == round(lon_interest,given_digits))

this_source_file <- paste("PM25_Step3_part_",processed_data_version,"_Locations_NAD83_include_old_projection",".csv",sep = "") # define file name
Step3_Loc_data <- read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file),header=TRUE, stringsAsFactors=FALSE) # read step 3 full data file
# find data of interest
which_data <- which(Step3_Loc_data$Lat == lat_interest & Step3_Loc_data$Lon == lon_interest)
Step1_data_interest <- Step1_data[which_data, ]


Step1_data <- read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file),header=TRUE, stringsAsFactors=FALSE) # read step 3 full data file
Step1_data <- input_mat_change_data_classes.fn(Step1_data) # set variable classes
# find data of interest
which_data <- which(Step1_data$Lat == lat_interest & )
Step1_data_interest <- Step1_data[which_data, ]
write.csv(Step1_data_interest,file = file.path(ProcessedData.directory,sub_folder,paste('PM25_Step1_part_',processed_data_version,'_example_duplicates.csv',sep = "")),row.names = FALSE) # Write csv file

