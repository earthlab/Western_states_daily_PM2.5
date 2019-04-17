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
