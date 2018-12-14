# CountyCentroid_CreateLatLonDateFiles.R - create data frame of the dates/locations of county centroids (places we want to predict PM2.5)

print("Run Define_directories.R before running this file.")

#### Call Packages (Library) ####
library(rgdal)
library(geosphere)

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"State_Abbrev_Definitions_function.R"))
source(file.path(ML_Code.directory,"Plotting_and_LaTex_functions.R"))
source(file.path(ML_Code.directory,"ML_processing_functions.R"))
source(file.path(LocationsInterest_Code.directory,"Spatial_functions.R"))

#### define constants and variables needed for all R workers ####
#processed_data_version <- "c" # locations for predictions
processed_data_version <- "CountyCentroid"
study_states_abbrev <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
this_datum <- "NAD83"
#sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")
sub_folder <- processed_data_version

# load map shape file
WestCountymapGeom <- load_County_Boundaries.fn(USMaps.directory, study_states_abbrev)

# County Centroids file
#county_centroids_mat <- centroid(WestCountymapGeom) # https://www.rdocumentation.org/packages/geosphere/versions/1.5-5/topics/centroid
#this_header <- c("Lon","Lat","Datum")
#county_centroids_step <- data.frame(matrix(NA, nrow = dim(county_centroids_mat)[1], ncol = length(this_header))) # create data frame for input_mat1
#names(county_centroids_step) <- this_header # assign the header to data frame
#county_centroids_step[,1:2] <- county_centroids_mat # fill in centroid info
#county_centroids_step$Datum <- this_datum
#county_centroids_for_coord <- county_centroids_step
county_centroids_for_coord <- create_centroid_df.fn(WestCountymapGeom)

# fill in Easting/Northing
#coordinates(county_centroids_for_coord)<- c("Lon", "Lat")
#proj4string(county_centroids_for_coord)<- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
#albers<- spTransform(county_centroids_for_coord, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
#Albers<- coordinates(albers)
#colnames(Albers)<- c("Easting", "Northing")
##Final<- cbind(All, Albers)
#county_centroids <- cbind(county_centroids_step, Albers)
#row.names(county_centroids)<- c()
county_centroids <- fill_in_EastingNorthing_from_LatLon.fn(county_centroids_for_coord) 


#print(county_centroids)

# write centroids to file
#write.csv(county_centroids,file = file.path(ProcessedData.directory,sub_folder,paste(sub_folder,'_Locations_part_',processed_data_version,'.csv',sep = "")),row.names = FALSE)
write.csv(county_centroids,file = file.path(ProcessedData.directory,sub_folder,paste(sub_folder,'_Locations','.csv',sep = "")),row.names = FALSE)

# create a vector of dates
start_date <- "2008-01-01"
end_date <- "2018-12-31"
date_vec <- seq(as.Date(start_date), as.Date(end_date), by="days")

# create data frame for all locations/dates
date_place <- expand_date_location.fn(locations_of_interest = county_centroids, date_vec = date_vec, this_datum = this_datum)
#write.csv(date_place,file = file.path(ProcessedData.directory,sub_folder,paste(file_sub_label,'_Locations_Dates_part_',processed_data_version,"_",start_date,"to",end_date,'.csv',sep = "")),row.names = FALSE)
write.csv(date_place,file = file.path(ProcessedData.directory,sub_folder,paste(sub_folder,'_Locations_Dates',"_",start_date,"to",end_date,'.csv',sep = "")),row.names = FALSE)
