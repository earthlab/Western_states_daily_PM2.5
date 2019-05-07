ML_input_repeats <- ML_input[duplicated(ML_input),] # de-duplicate rows of data

ML_input_repeats <- ML_input2[duplicated(ML_input2),] # de-duplicate rows of data


ML_input_repeats <- ML_input_out[duplicated(ML_input_out),] # de-duplicate rows of data


Loc_data <- ML_input_out[ ,c("Latitude","Longitude")]
Loc_data_repeats <- Loc_data[duplicated(Loc_data), ]

which_repeated <- which(ML_input_out$Latitude == Loc_data_repeats$Latitude & ML_input_out$Longitude == Loc_data_repeats$Longitude)
print(which_repeated)

Loc_data <- ML_input2[ ,c("Latitude","Longitude")]
Loc_data_repeats <- Loc_data[duplicated(Loc_data), ]
which_repeated <- which(ML_input2$Latitude == Loc_data_repeats$Latitude & ML_input2$Longitude == Loc_data_repeats$Longitude)
print(which_repeated)
ML_input_repeats <- ML_input2[which_repeated, ]


# # remove below after trouble shooting
#Loc_data <- NED_data_step[ ,c("Latitude","Longitude")]
#Loc_data_repeats <- Loc_data[duplicated(Loc_data), ]
#which_repeated <- which(round(NED_data_step$Latitude,5) %in% round(Loc_data_repeats$Latitude,5) & round(NED_data_step$Longitude,5) %in% round(Loc_data_repeats$Longitude,5))
#print(which_repeated)
#NED_data_repeats <- NED_data_step[which_repeated, ]
# Lat_interest <- Loc_data_repeats$Latitude # 36.78133
# Lon_interest <- Loc_data_repeats$Longitude # -119.7732
# which_Loc_interest <- which(round(Fire_MODIS_data_date$Latitude,5) == round(Lat_interest,5) & round(Fire_MODIS_data_date$Longitude,5) == round(Lon_interest,5))
# #if (length(which_Loc_interest)>0) {
#   print(paste("The location of interest,",Lat_interest,Lon_interest,"has",length(which_Loc_interest),"data points in",Fire_MODIS_file_name[file_i]))
# #}
# # remove above after trouble shooting

MAIAC_data_repeats <- MAIAC_data[!duplicated(MAIAC_data), c("Latitude")]

which_MAIAC_not_PM25 <- which(round(MAIAC_data$Latitude,5) %!in% round(ML_input_in$Latitude,5))

# NAM dates Loc

NAM_dates_loc_w_repeats <- NAM_data_step[ ,c("Latitude","Longitude","Date")]
NAM_data_loc_no_repeats <- NAM_dates_loc_w_repeats[!duplicated(NAM_dates_loc_w_repeats), ]

NAM_data_no_repeats <- NAM_data_step[!duplicated(NAM_data_step), ]


this_lat <- 46.87484

rounded_Fire <- Fire_MODIS_data_date
rounded_Fire$Latitude <- round(Fire_MODIS_data_date$Latitude,5)
this_row <- which(rounded_Fire$Latitude == this_lat)
rows_interest <- rounded_Fire[this_row, ]

# find rows in orig file
which_rows <- which(round(Fire_MODIS_data_step$Latitude,5) == this_lat & Fire_MODIS_data_step$Date == this_Date)


# find out how many unique locations are in Fire modis data (500 km buffer)
locations_only_w_dups <- Fire_MODIS_data[ , c("Latitude","Longitude")] 
unique_locations <- locations_only_w_dups[!duplicated(locations_only_w_dups), ]
# round locations
unique_locations_rounded <- round(unique_locations,4)
unique_loc_rounded_no_dups <- unique_locations_rounded[!duplicated(unique_locations_rounded ), ]

# map data - trouble shoot Fire_MODIS_data
plot.new()
map_base_layer.fn(USMaps.directory, study_states_abbrev)
which_zeros <- which(Fire_MODIS_data$`500km_fire_count` == 0)
points(Fire_MODIS_data[which_zeros,"Longitude"],Fire_MODIS_data[which_zeros,"Latitude"],pch = 19,col="black")
which_ones <- which(Fire_MODIS_data$`500km_fire_count` == 1)
points(Fire_MODIS_data[which_ones,"Longitude"],Fire_MODIS_data[which_ones,"Latitude"],pch = 13,col="blue")
which_twos <- which(Fire_MODIS_data$`500km_fire_count` == 2)
points(Fire_MODIS_data[which_twos,"Longitude"],Fire_MODIS_data[which_twos,"Latitude"],pch = 0,col="red")
points(ML_input$Longitude,ML_input$Latitude,pch = 5, col = "green")
points(predictor_row_step1$Longitude, predictor_row_step1$Latitude, pch = 11, col = "orange")

## map data - trouble shoot MAIAC data having fewer locations than PM25 data
plot.new()
map_base_layer.fn(USMaps.directory, study_states_abbrev)
points(MAIAC_data$Longitude, MAIAC_data$Latitude, pch = 19, col = "black")
points(ML_input$Longitude,ML_input$Latitude,pch = 5, col = "green")
# load part e all locations
part_e_all_locations <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),"PM25_Locations_Dates",paste("PM25_Step3_part_",define_study_constants.fn("processed_data_version"),"_Locations_Dates_NAD83.csv", sep = "")))
part_e_all_locations$Date <- as.Date(part_e_all_locations$Date,"%Y-%m-%d")
which_this_date <- which(part_e_all_locations$Date == this_Date)
part_e_all_locations_this_date <- part_e_all_locations[which_this_date, ]
rm(which_this_date)
# load part e minus b
part_emb_locations <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),"PM25_Locations_Dates",paste("Part_e_not_in_b_Locations_Dates.csv", sep = "")))
part_emb_locations$Date <- as.Date(part_emb_locations$Date,"%Y-%m-%d")
which_this_date <- which(part_emb_locations$Date == this_Date)
part_emb_locations_this_date <- part_emb_locations[which_this_date, ]
