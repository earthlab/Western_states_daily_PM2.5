ML_input_repeats <- ML_input[duplicated(ML_input),] # de-duplicate rows of data
ML_input_repeats <- ML_input_out[duplicated(ML_input_out),] # de-duplicate rows of data


MAIAC_data_repeats <- MAIAC_data[!duplicated(MAIAC_data), c("Latitude")]

which_MAIAC_not_PM25 <- which(round(MAIAC_data$Latitude,5) %!in% round(ML_input_in$Latitude,5))

