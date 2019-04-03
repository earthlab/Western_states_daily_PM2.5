ML_input_repeats <- ML_input[duplicated(ML_input),] # de-duplicate rows of data
ML_input_repeats <- ML_input_out[duplicated(ML_input_out),] # de-duplicate rows of data


Loc_data <- ML_input_out[ ,c("Latitude","Longitude")]
Loc_data_repeats <- Loc_data[duplicated(Loc_data), ]

which_repeated <- which(ML_input_out$Latitude == Loc_data_repeats$Latitude & ML_input_out$Longitude == Loc_data_repeats$Longitude)
print(which_repeated)

MAIAC_data_repeats <- MAIAC_data[!duplicated(MAIAC_data), c("Latitude")]

which_MAIAC_not_PM25 <- which(round(MAIAC_data$Latitude,5) %!in% round(ML_input_in$Latitude,5))

# NAM dates Loc

NAM_dates_loc_w_repeats <- NAM_data_step[ ,c("Latitude","Longitude","Date")]
NAM_data_loc_no_repeats <- NAM_dates_loc_w_repeats[!duplicated(NAM_dates_loc_w_repeats), ]

NAM_data_no_repeats <- NAM_data_step[!duplicated(NAM_data_step), ]
