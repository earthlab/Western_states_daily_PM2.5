# ML_PM25_estimation_step2.R - create data frame of the dates/locations for which we want to predict PM2.5

#### Call Packages (Library) ####
library(rgdal)
library(geosphere)
#library(ggplot2)
#library(ggmap)
#library(rgeos)
#library(maptools)
#library(dplyr)
#library(tidyr)
#library(maps)

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"State_Abbrev_Definitions_function.R"))
source(file.path(ML_Code.directory,"Plotting_and_LaTex_functions.R"))
source(file.path(ML_Code.directory,"ML_processing_functions.R"))

#### define constants and variables needed for all R workers ####
processed_data_version <- "c" # locations for predictions
study_states_abbrev <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
this_datum <- "NAD83"

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

# County Centroids
county_centroids_mat <- centroid(WestCountymapGeom) # https://www.rdocumentation.org/packages/geosphere/versions/1.5-5/topics/centroid
#this_header <- c("Longitude","Latitude","Datum")
this_header <- c("Lon","Lat","Datum")#,"Easting","Northing")
county_centroids_step <- data.frame(matrix(NA, nrow = dim(county_centroids_mat)[1], ncol = length(this_header))) # create data frame for input_mat1
names(county_centroids_step) <- this_header # assign the header to data frame
county_centroids_step[,1:2] <- county_centroids_mat # fill in centroid info
county_centroids_step$Datum <- this_datum
county_centroids_for_coord <- county_centroids_step

# fill in Easting/Northing
#All<- rbind(table_84, table_83, table_27, stringsAsFactors = FALSE) #1 = WGS84, 2 = NAD83, 3 = NAD27
#for(i in c(1,2,4,5)){ # recognize numerical columns as numerical
#  All[,i]<- as.numeric(All[,i]) # convert this column to numerical class
#} # for(i in c(1,2,4,5)){ # recognize numerical columns as numerical
#all_nad83<- All[,c("Lon", "Lat")]
coordinates(county_centroids_for_coord)<- c("Lon", "Lat")
proj4string(county_centroids_for_coord)<- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
albers<- spTransform(county_centroids_for_coord, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
Albers<- coordinates(albers)
colnames(Albers)<- c("Easting", "Northing")
#Final<- cbind(All, Albers)
county_centroids <- cbind(county_centroids_step, Albers)
row.names(Final)<- c()


#print(county_centroids)

# plot centroids on map
#points(county_centroids$Longitude,county_centroids$Latitude,col="blue") # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
points(county_centroids$Lon,county_centroids$Lat,col="blue") # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
Plot_to_ImageFile_BottomOnly.fn(FigFileName = FigFileName, title_string = title_string) # finish image file

LaTex_code_4_figure.fn(LatexFileName = LatexFileName, title_string = title_string, file_sub_label = file_sub_label, plot_name_extension = plot_name_extension, output.directory.short = output.directory.short)

# write centroids to file
write.csv(county_centroids,file = file.path(ProcessedData.directory,paste(file_sub_label,'_Locations_part_',processed_data_version,'.csv',sep = "")),row.names = FALSE)

# create a vector of dates
start_date <- "2008-01-01"
end_date <- "2008-12-31"
date_vec <- seq(as.Date(start_date), as.Date(end_date), by="days")

# create data frame for all locations/dates
date_place <- expand_date_location.fn(locations_of_interest = county_centroids, date_vec = date_vec, this_datum = this_datum)
write.csv(date_place,file = file.path(ProcessedData.directory,paste(file_sub_label,'_Locations_Dates_part_',processed_data_version,"_",start_date,"to",end_date,'.csv',sep = "")),row.names = FALSE)
