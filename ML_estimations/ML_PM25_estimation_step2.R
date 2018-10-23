# ML_PM25_estimation_step2.R - create data frame of the dates/locations for which we want to predict PM2.5

#### Call Packages (Library) ####
library(ggplot2)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)
library(maps)
library(geosphere)

#### Call Load Functions that I created ####
#source(file.path(writingcode.directory,"State_Abbrev_Definitions_function.R"))
source(file.path(ML_Code.directory,"Plotting_and_LaTex_functions.R"))
Plotting_and_LaTex_fn_list <- c("Plot_to_ImageFile.fn", "Plot_and_latex.fn", "LaTex_code_4_figure.fn", "LaTex_code_start_subsection.fn")

#### define constants and variables needed for all R workers ####
study_states_abbrev <- c("AZ","CA","CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")


# County Centroids
county_centroids <- map_county_base_layer.fn(CountyMaps.directory, study_states_abbrev)

#### Create a data frame with just lat, and lon ####
#### Create data frame  ####
this_header <- c("Latitude","Longitude","Datum")
lat_lon_interest <- data.frame(matrix(NA,nrow=dim(county_centroids)[1],ncol=length(this_header))) # create data frame for input_mat1
names(lat_lon_interest) <- this_header # assign the header to input_mat1
#input_mat1 <- input_mat_change_data_classes.fn(input_mat1)

lat_lon_interest$Latitude <- county_centroids[ ,1]
pick up code here

three_cols_w_duplicates <- input_mat2[,c("PM2.5_Lat","PM2.5_Lon","Datum")]
three_cols_data <- three_cols_w_duplicates[!duplicated(three_cols_w_duplicates),]
names(three_cols_data) <- c("Latitude","Longitude","Datum")
#write.csv(three_cols_data,file = file.path(ProcessedData.directory,paste('Locations_PM25_Obs_from_clean_script_',Sys.Date(),'_part_',processed_data_version,'.csv',sep = "")),row.names = FALSE)
write.csv(three_cols_data,file = file.path(ProcessedData.directory,paste(file_sub_label,'_Locations','.csv',sep = "")),row.names = FALSE)

#### Create a data frame with just lat, lon, and date ####
four_cols_w_duplicates <- input_mat2[,c("PM2.5_Lat","PM2.5_Lon","Datum","Date_Local")]
four_cols_data <- four_cols_w_duplicates[!duplicated(four_cols_w_duplicates),]
names(four_cols_data) <- c("Latitude","Longitude","Datum","Date")
#write.csv(four_cols_data,file = file.path(ProcessedData.directory,paste('Locations_Dates_of_PM25_Obs_from_clean_script_',Sys.Date(),'_part',processed_data_version,'.csv',sep = "")),row.names = FALSE)
write.csv(four_cols_data,file = file.path(ProcessedData.directory,paste(file_sub_label,'_Locations_Dates','.csv',sep = "")),row.names = FALSE)

rm(four_cols_data,four_cols_w_duplicates)




