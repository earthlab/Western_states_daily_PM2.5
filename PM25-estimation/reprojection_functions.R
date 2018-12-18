# Process_PM25_data_step3.R - reproject all dates and locations into same datum
# (this script is a modification of Reproject_monitors.R written by Ellen Considine)

#Reproject all monitor locations and output csv files
reproject_monitors.fn <- function(this_source_file_loc, this_source_file_loc_date, sub_folder) {

# define necessary libraries
library(dismo)
library(rgdal)
library(raster)

ProcessedData.directory <- define_file_paths.fn("ProcessedData.directory")
  
monitors<- read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file_loc), stringsAsFactors = FALSE) # load data
df84<- monitors[monitors$Datum == "WGS84",] # separate the locations that use WGS84
df27<- monitors[monitors$Datum == "NAD27",] # separate the locations that use NAD27
df83<- monitors[monitors$Datum == "NAD83",] # separate the locations that use NAD83

if (dim(df84)[1] + dim(df27)[1] + dim(df83)[1] != dim(monitors)[1]) { # check that all of the data is one of the 3 datums listed above
  stop("The number of rows is not adding up correctly, likely unknown or additional datums")
} # check that all of the data is one of the 3 datums listed above

# process WGS84 data
coordinates(df84) <- c("Longitude", "Latitude")
proj4string(df84) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
DF84 <- spTransform(df84, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
summary(DF84) # summarize data

# process NAD27 data
coordinates(df27) <- c("Longitude", "Latitude")
proj4string(df27) <- CRS("+proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs")
DF27 <- spTransform(df27, CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
summary(DF27) # summarize data

# NAD83 is the datum we're converting to, so not much processing needed for this
DF83<- df83[,1:2]
summary(DF83) # summarize data

col_names <- c("old_lon", "old_lat", "old_Datum", "Lon", "Lat","Datum") # define header for output data
nad83_vec <- data.frame(matrix("NAD83",nrow=dim(df84)[1],ncol=1)) # create vector indicating NAD83 (new datum for all)
table_84<- cbind(coordinates(df84), df84$Datum, coordinates(DF84), nad83_vec) # create table of data for this datum
rm(nad83_vec) # clear variable
colnames(table_84) <- col_names # assign header to table_84 #c("old_lon", "old_lat", "old_Datum", "Lon", "Lat","Datum")

nad83_vec <- data.frame(matrix("NAD83",nrow=dim(df83)[1],ncol=1)) # create vector indicating NAD83 (new datum for all)
table_83<- cbind(df83,DF83, nad83_vec) # bind old and new location info for this datum
table_83<- table_83[,c(2, 1, 3, 5, 4, 6)] # re-order columns
colnames(table_83) <- col_names # assign header to table #c("old_lon", "old_lat", "Datum", "Lon", "Lat")
rm(nad83_vec) # clear variable

nad83_vec <- data.frame(matrix("NAD83",nrow=dim(df27)[1],ncol=1)) # create vector indicating NAD83 (new datum for all)
table_27<- cbind(coordinates(df27), df27$Datum, coordinates(DF27), nad83_vec)
colnames(table_27)<- col_names # assign header to table #c("old_lon", "old_lat", "Datum", "Lon", "Lat")
rm(nad83_vec) # clear variable

All<- rbind(table_84, table_83, table_27, stringsAsFactors = FALSE) #1 = WGS84, 2 = NAD83, 3 = NAD27
for(i in c(1,2,4,5)){ # recognize numerical columns as numerical
  All[,i]<- as.numeric(All[,i]) # convert this column to numerical class
} # for(i in c(1,2,4,5)){ # recognize numerical columns as numerical
all_nad83<- All[,c("Lon", "Lat")]
coordinates(all_nad83)<- c("Lon", "Lat")
proj4string(all_nad83)<- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
albers<- spTransform(all_nad83, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
Albers<- coordinates(albers)
colnames(Albers)<- c("Easting", "Northing")
Final<- cbind(All, Albers)
row.names(Final)<- c()

#write.csv(Final, "C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\test_data\\monitors\\Projected_locations_part_a.csv")
#write.csv(Final, file = file.path(ProcessedData.directory,paste(substr(this_source_file_loc, 1, (nchar(this_source_file_loc)-4)),'_Projected','.csv',sep = "")),row.names = FALSE)
new_file_name <- update_file_name.fn(file_name_in = this_source_file_loc)
write.csv(Final, file = file.path(ProcessedData.directory,sub_folder,paste(new_file_name,'_Projected','.csv',sep = "")),row.names = FALSE)

#################################################
#Update all location/date pairs

loc_date <- read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file_loc_date), stringsAsFactors = FALSE) # load data

# create df for output file
col_names <- c("old_lon", "old_lat", "old_Datum", "Lon", "Lat","Datum","Easting","Northing","Date") # define header for output data
Final_Date <- data.frame(matrix(NA,nrow=dim(loc_date)[1],ncol=length(col_names))) # create vector indicating NAD83 (new datum for all)
names(Final_Date) <- col_names

# fill in Final_Date
# "Datum"   
Final_Date$old_lon <- loc_date$Longitude # "Longitude"
Final_Date$old_lat <- loc_date$Latitude # "Latitude"
Final_Date$Date <- loc_date$Date # "Longitude"
Final_Date$old_Datum <- loc_date$Datum # Datum
Final_Date$Datum <- "NAD83"

Final_Date$Lat<- Final[match(Final_Date$old_lat, Final$old_lat), 'Lat']
Final_Date$Lon<- Final[match(Final_Date$old_lon, Final$old_lon), 'Lon']
Final_Date$Northing<- Final[match(Final_Date$old_lat, Final$old_lat), 'Northing']
Final_Date$Easting<- Final[match(Final_Date$old_lon, Final$old_lon), 'Easting']
#loc_date <- read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file_loc_date), stringsAsFactors = FALSE) # load data
#loc_date$Lat<- Final[match(loc_date$Latitude, Final$old_lat), 'Lat']
#loc_date$Lon<- Final[match(loc_date$Longitude, Final$old_lon), 'Lon']
#loc_date$Northing<- Final[match(loc_date$Latitude, Final$old_lat), 'Northing']
#loc_date$Easting<- Final[match(loc_date$Longitude, Final$old_lon), 'Easting']


#write.csv(loc_date, "C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\test_data\\monitors\\Projected_locations_with_dates_part_a.csv")
#write.csv(loc_date, file = file.path(ProcessedData.directory,paste(substr(this_source_file_loc_date, 1, (nchar(this_source_file_loc_date)-4)),'_Projected','.csv',sep = "")),row.names = FALSE)
new_file_name <- update_file_name.fn(file_name_in = this_source_file_loc_date)
#write.csv(loc_date, file = file.path(ProcessedData.directory,sub_folder,paste(new_file_name,'_Projected','.csv',sep = "")),row.names = FALSE)
write.csv(Final_Date, file = file.path(ProcessedData.directory,sub_folder,paste(new_file_name,'_Projected','.csv',sep = "")),row.names = FALSE)

} # end of reproject_monitors.fn function

# update file name from Step2 to Step3
update_file_name.fn <- function(file_name_in) {
  # file_name_in <- this_source_file_loc
  file_name_in <- as.character(file_name_in) # grab first file name in list
  find_chars <- gregexpr(pattern = "Step2", text = file_name_in) # find where "Step2" text is located within file name
  file_name_start <- substr(file_name_in,1,find_chars[[1]][1]-1) # first part of file name
  file_name_mid <- "Step3" # mid part of file name
  file_name_end <- substr(file_name_in,(find_chars[[1]][1]+5),nchar(file_name_in)-4) # last part of file name
  file_name_out <- paste(file_name_start,file_name_mid,file_name_end,sep = "") # paste the 3 parts of the file name together
  return(file_name_out) # function output
} # end of update_file_name.fn function

# append projection info onto input_mat1
reprojected_into_input_mat1.fn <- function(ProcessedData.directory, sub_folder, this_source_file, this_source_file_loc) {
  #need? this_source_file_loc_date
  
  # load the file with the full PM2.5 data (with unprojected locations)
  input_mat1 <- read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file),header=TRUE) # load data file
  
  # load the reprojected location info
  new_file_name <- update_file_name.fn(file_name_in = this_source_file_loc)
  reproj_loc <- read.csv(file.path(ProcessedData.directory,sub_folder,paste(new_file_name,'_Projected','.csv',sep = "")),header = TRUE)
  
  # create df for output file
  #col_names <- c(colnames(reproj_loc),"NewDatum",colnames(input_mat1)) # define header for output data
  col_names <- c("Lat", "Lon", "Easting", "Northing","NewDatum",colnames(input_mat1)) # define header for output data
  input_mat2 <- data.frame(matrix(NA,nrow=dim(input_mat1)[1],ncol=length(col_names))) # create vector indicating NAD83 (new datum for all)
  names(input_mat2) <- col_names
  
  # fill in input_mat2
  input_mat2[ , (dim(input_mat2)[2]-length(colnames(input_mat1))+1):(dim(input_mat2)[2])] <- input_mat1
  
  #input_mat2$old_lon <- loc_date$Longitude # "Longitude"
  #input_mat2$old_lat <- loc_date$Latitude # "Latitude"
  #input_mat2$Date <- input_mat2$Date_Local#loc_date$Date # "Longitude"
  #input_mat2$old_Datum <- loc_date$Datum # Datum
  input_mat2$NewDatum <- "NAD83"
  input_mat2$Lat<- reproj_loc[match(input_mat2$PM2.5_Lat, reproj_loc$old_lat), 'Lat']
  input_mat2$Lon<- reproj_loc[match(input_mat2$PM2.5_Lon, reproj_loc$old_lon), 'Lon']
  input_mat2$Northing<- reproj_loc[match(input_mat2$PM2.5_Lat, reproj_loc$old_lat), 'Northing']
  input_mat2$Easting<- reproj_loc[match(input_mat2$PM2.5_Lon, reproj_loc$old_lon), 'Easting']
  
  new_file_name <- update_file_name.fn(file_name_in = this_source_file)
  write.csv(input_mat2, file = file.path(ProcessedData.directory,sub_folder,paste(new_file_name,'_Projected','.csv',sep = "")),row.names = FALSE)
  
} # end of reprojected_into_input_mat1.fn function
