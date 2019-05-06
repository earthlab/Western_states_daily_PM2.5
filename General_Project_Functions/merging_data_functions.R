# Functions related to merging data
# this_Date <- Date_list[1]
# merge predictor variables together
merge_predictors.fn <- function(X) { #(predictand_data,predictand_col,latitude_col_t,longitude_col_t,datum_col_t, Easting_col_t, Northing_col_t,Dates_col_t, output_file_name, output_sub_folder, study_start_date, study_stop_date) {
  this_Date <- as.Date(Date_list[X]) # identify the date for this iteration
  print(this_Date)
  which_this_date <- which(Source_Data[ ,Dates_col_t] == this_Date) # identify all data for this date
  #length(which_this_date)  
  if (predictand_col %in% colnames(Source_Data)) { # this data includes PM2.5 data
   vars_to_include <- c(predictand_col,latitude_col_t,longitude_col_t,datum_col_t,Dates_col_t,"Year","Month","Day")
  } else { # if (predictand_col %in% colnames(Source_Data)) { # this data includes PM2.5 data
    Source_Data$Year <- input_mat_extract_year_from_date.fn(Source_Data[ ,c(Dates_col_t)])
    Source_Data$Month <- input_mat_extract_month_from_date.fn(Source_Data[ ,c(Dates_col_t)]) 
    Source_Data$Day <- input_mat_extract_day_from_date.fn(Source_Data[ ,c(Dates_col_t)])
    vars_to_include <- c(latitude_col_t,longitude_col_t,datum_col_t,Dates_col_t,"Year","Month","Day")
  } # if (predictand_col %in% colnames(Source_Data)) { # this data includes PM2.5 data
  ML_input_step <- Source_Data[which_this_date, vars_to_include] # start ML_input data frame
  ML_input <- ML_input_step[!duplicated(ML_input_step), ] # de-duplicate rows of data
  rm(ML_input_step)
  n_rows <- dim(ML_input)[1]
  n_rows_orig <- n_rows
  n_cols_orig <- dim(ML_input)[2]
  ML_input <- replace_column_names.fn(df_in = ML_input, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
  ML_input <- replace_column_names.fn(df_in = ML_input, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
  ML_input <- replace_column_names.fn(df_in = ML_input, old_col_name = "Date_Local", new_col_name = "Date") # replace "Lat" with "Latitude"
  rm(which_this_date)
  if (n_rows != dim(ML_input)[1]) {stop("Number of rows in ML_input is changing, line 22")}
  added_cols <- 0 # start counter
  
  # Load and merge Fire MODIS 25 km Data
  ML_input <- merge_Fire_MODIS_data.fn(Buffer_radius_km = 25, ML_input = ML_input, Fire_MODIS_file_name = fire_MODIS_25km_file_name,
                                       ProcessedData.directory = ProcessedData.directory, predictor_sub_folder = predictor_sub_folder,
                                       this_Date = this_Date)
  if (n_rows != dim(ML_input)[1]) {print("***Number of rows in ML_input is changing with after merging 25 km Fire MODIS data***")}
  additional_cols <- 1
  added_cols <- added_cols+additional_cols
  if (dim(ML_input)[2] != (n_cols_orig+added_cols)) {stop("Check number of columns after merging 25 km Fire MODIS data")}
  #n_rows <- dim(ML_input)[1] # REMOVE

  # Load and merge Fire MODIS 50 km Data
  ML_input <- merge_Fire_MODIS_data.fn(Buffer_radius_km = 50, ML_input = ML_input, Fire_MODIS_file_name = fire_MODIS_50km_file_name,ProcessedData.directory,predictor_sub_folder,this_Date)
  if (n_rows != dim(ML_input)[1]) {stop("Number of rows in ML_input is changing after merging 50 km Fire MODIS data")}
  #if (dim(ML_input)[2] != (n_cols_orig+2)) {stop("Check number of columns after merging 50 km Fire MODIS data")}
  additional_cols <- 1
  added_cols <- added_cols+additional_cols
  if (dim(ML_input)[2] != (n_cols_orig+added_cols)) {stop("Check number of columns after merging 50 km Fire MODIS data")}

  # Load and merge Fire MODIS 100 km Data
  ML_input <- merge_Fire_MODIS_data.fn(Buffer_radius_km = 100, ML_input = ML_input, Fire_MODIS_file_name = fire_MODIS_100km_file_name,ProcessedData.directory,predictor_sub_folder,this_Date)
  if (n_rows != dim(ML_input)[1]) {stop("Number of rows in ML_input is changing after merging 100 km Fire MODIS data")}
  #if (dim(ML_input)[2] != (n_cols_orig+3)) {stop("Check number of columns after merging 100 km Fire MODIS data")}
  additional_cols <- 1
  added_cols <- added_cols+additional_cols
  if (dim(ML_input)[2] != (n_cols_orig+added_cols)) {stop("Check number of columns after merging 100 km Fire MODIS data")}

  # Load and merge Fire MODIS 500 km Data
  ML_input <- merge_Fire_MODIS_data.fn(Buffer_radius_km = 500, ML_input = ML_input, Fire_MODIS_file_name = fire_MODIS_500km_file_name,ProcessedData.directory,predictor_sub_folder,this_Date)
  if (n_rows != dim(ML_input)[1]) {stop("Number of rows in ML_input is changing after merging 500 km Fire MODIS")}
  #if (dim(ML_input)[2] != (n_cols_orig+4)) {stop("Check number of columns after merging 500 km Fire MODIS data")}
  additional_cols <- 1
  added_cols <- added_cols+additional_cols
  if (dim(ML_input)[2] != (n_cols_orig+added_cols)) {stop("Check number of columns after merging 500 km Fire MODIS data")}
  
  # Load and merge GASP Data
  print("start merging GASP data")
  ML_input <- merge_GASP_data.fn(ML_input = ML_input, GASP_file_name = GASP_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder, this_Date = this_Date)#, study_start_date = study_start_date, study_stop_date = study_stop_date)
  #if (dim(ML_input)[2] != (n_cols_orig+5)) {stop("Check number of columns after merging GASP data")}
  if (n_rows != dim(ML_input)[1]) {stop(paste("Number of rows in ML_input is changing after merging GASP data. X =",X,"Date = ",this_Date))}
  additional_cols <- 1
  added_cols <- added_cols+additional_cols
  if (dim(ML_input)[2] != (n_cols_orig+added_cols)) {stop(paste("Number of rows in ML_input is changing after merging GASP data. X =",X,"Date = ",this_Date))}

  # Load and merge Highways Data
  print("start merging Highways data")
  ML_input <- merge_Highways_data.fn(ML_input = ML_input, Highways_file_name = Highways_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"),predictor_sub_folder = predictor_sub_folder)#, this_Date = this_Date) #, study_start_date = study_start_date, study_stop_date = study_stop_date)
  #if (dim(ML_input)[2] != (n_cols_orig+17)) {stop("Check number of columns after merging Highway data")}
  if (n_rows != dim(ML_input)[1]) {stop(paste("Number of rows in ML_input is changing after merging Highway data. X =",X,"Date = ",this_Date))}
  additional_cols <- 12
  added_cols <- added_cols+additional_cols
  if (dim(ML_input)[2] != (n_cols_orig+added_cols)) {stop(paste("Number of rows in ML_input is changing after merging Highway data. X =",X,"Date = ",this_Date))}

  # Load and merge MAIAC Data
  print("start merging MAIAC data")
  ML_input <- merge_MAIAC_data.fn(ML_input = ML_input, MAIAC_file_name = MAIAC_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder, this_Date = this_Date)# , study_start_date = study_start_date, study_stop_date = study_stop_date)
  #if (dim(ML_input)[2] != (n_cols_orig+18)) {stop("Check number of columns after merging MAIAC data")}
  if (n_rows != dim(ML_input)[1]) {stop(paste("Number of rows in ML_input is changing after merging MAIAC data. X =",X,"Date = ",this_Date))}
  additional_cols <- 1
  added_cols <- added_cols+additional_cols
  if (dim(ML_input)[2] != (n_cols_orig+added_cols)) {stop(paste("Number of rows in ML_input is changing after merging MAIAC data. X =",X,"Date = ",this_Date))}

  # Load and merge NED Data
  print("start merging NED data")
  ML_input <- merge_NED_data.fn(ML_input = ML_input, NED_file_name = NED_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder)
  #if (dim(ML_input)[2] != (n_cols_orig+19)) {stop("Check number of columns after merging NED data")}
  if (n_rows != dim(ML_input)[1]) {stop(paste("Number of rows in ML_input is changing after merging NED data. X =",X,"Date = ",this_Date))}
  additional_cols <- 1
  added_cols <- added_cols+additional_cols
  if (dim(ML_input)[2] != (n_cols_orig+added_cols)) {stop(paste("Number of rows in ML_input is changing after merging NED data. X =",X,"Date = ",this_Date))}

  # Load and merge NAM Data 
  print("start merging NAM data")
  ML_input <- merge_NAM_data.fn(ML_input = ML_input, NAM_file_name = NAM_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder, this_Date = this_Date)
  #if (dim(ML_input)[2] != (n_cols_orig+32)) {stop("Check number of columns after merging NAM data")}
  if (n_rows != dim(ML_input)[1]) {stop(paste("***Number of rows in ML_input is changing after merging NAM data*** X =",X,"Date = ",this_Date))}
  additional_cols <- 14
  added_cols <- added_cols+additional_cols
  if (dim(ML_input)[2] != (n_cols_orig+added_cols)) {stop(paste("Check number of columns after merging NAM data X =",X,"Date = ",this_Date))}

  # Load and merge 1 km NLCD Data
  print("start merging 1 km NLCD data")
  ML_input <- merge_NLCD_data.fn(buffer_radius = "1km", ML_input = ML_input, NLCD_file_name = NLCD_1km_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder)
  #if (dim(ML_input)[2] != (n_cols_orig+33)) {stop("Check number of columns after merging 1 km NLCD data")}
  if (n_rows != dim(ML_input)[1]) {stop(paste("Number of rows in ML_input is changing after merging NLCD 1 km data. X =",X,"Date = ",this_Date))}
  additional_cols <- 1
  added_cols <- added_cols+additional_cols
  if (dim(ML_input)[2] != (n_cols_orig+added_cols)) {stop(paste("Number of rows in ML_input is changing after merging 1 km NLCD data. X =",X,"Date = ",this_Date))}

  # Load and merge 5 km NLCD Data
  print("start merging 5 km NLCD data")
  ML_input <- merge_NLCD_data.fn(buffer_radius = "5km", ML_input = ML_input, NLCD_file_name = NLCD_5km_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder)
  #if (dim(ML_input)[2] != (n_cols_orig+34)) {stop("Check number of columns after merging NED data")}
  if (n_rows != dim(ML_input)[1]) {stop(paste("Number of rows in ML_input is changing after merging NLCD 5 km data. X =",X,"Date = ",this_Date))}
  additional_cols <- 1
  added_cols <- added_cols+additional_cols
  if (dim(ML_input)[2] != (n_cols_orig+added_cols)) {stop(paste("Number of rows in ML_input is changing after merging 5 km NLCD data. X =",X,"Date = ",this_Date))}

  # Load and merge 10 km NLCD Data
  print("start merging 10 km NLCD data")
  ML_input <- merge_NLCD_data.fn(buffer_radius = "10km", ML_input = ML_input, NLCD_file_name = NLCD_10km_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder)
  if (n_rows != dim(ML_input)[1]) {stop(paste("Number of rows in ML_input is changing after merging NLCD 10 km data. X =",X,"Date = ",this_Date))}
  #if (dim(ML_input)[2] != (n_cols_orig+35)) {stop("Check number of columns after merging NLCD 10 km data")}
  additional_cols <- 1
  added_cols <- added_cols+additional_cols
  if (dim(ML_input)[2] != (n_cols_orig+added_cols)) {stop(paste("Number of rows in ML_input is changing after merging 5 km NLCD data. X =",X,"Date = ",this_Date))}

  print("bottom of merge_predictors.fn")
  return(ML_input) # output from function
} # end of merge_predictors.fn function

# remove data points outside specified range of values
remove_data_outside_range.fn <- function(df_in, column_of_interest, upper_limit = NA, lower_limit = NA, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE, reason_removed = " ") {
  # df_in = input_mat1
  # column_of_interest = "PM2.5_Obs"
  # upper_limit = NA
  # lower_limit = 0
  # include_upper_limit = TRUE
  # include_lower_limit = TRUE
  # remove_NAs = TRUE
  # verbose = TRUE
  # reason_removed = "Remove negative and NA PM2.5"
  
  # remove NA values (step1)
  if (remove_NAs == TRUE) { # NA values should be removed
    which_not_NA <- which(is.na(df_in[ , column_of_interest]) == FALSE) # find the not-NAs
    df_step1 <- df_in[which_not_NA, ] # keep all data that doesn't have NA in the column of interest
    print(paste((dim(df_in)[1] - dim(df_step1)[1])," data points were removed due to having ",column_of_interest," as NA ",sep = ""))
    # track which data points have been removed and why
    which_remove <- which(is.na(df_in[ , column_of_interest]) == TRUE) # find the NAs
    remove_df_NA <- df_in[which_remove, ]
    if (dim(remove_df_NA)[1]>1) { # input reason for data removal into data frame
    remove_df_NA$Reason <- reason_removed
    } # if (dim(remove_df_NA)[1]>1) { # input reason for data removal
    rm(which_not_NA,which_remove)
  } else { # NA values should not be removed
    df_step1 <- df_in
    remove_df_NA <- df_in[0,]
  }
  
  # remove data above the upper limit (step2)
  if (is.na(upper_limit) == FALSE) { # an upper limit has been set
    if (include_lower_limit == TRUE) {# data of exactly the upper_limit will be kept
      which_in_range <- which(df_step1[ , column_of_interest] <= upper_limit) # find the data at or below the upper limit, to be kept
      # track which data points have been removed and why
      which_remove <- which(df_step1[ , column_of_interest] > upper_limit)
   } else { # if (include_lower_limit == TRUE) # data of exactly the upper_limit will be kept
      which_in_range <- which(df_step1[ , column_of_interest] < upper_limit) # find the data below the upper_limit, to be kept
      # track which data points have been removed and why
      which_remove <- which(df_step1[ , column_of_interest] >= upper_limit)
    } # if (include_lower_limit == TRUE) # data of exactly the upper_limit will be kept
    df_step2 <- df_step1[which_in_range, ] # keep only data within the specified range
    print(paste((dim(df_step1)[1] - dim(df_step2)[1])," data points were removed due to having ",column_of_interest," above ",upper_limit,sep = ""))
    remove_df_UL <- df_step1[which_remove, ]
    if (dim(remove_df_UL)[1]>1) {
    remove_df_UL$Reason <- reason_removed
    }
    rm(which_in_range,which_remove)
  } else { # if (is.na(upper_limit) == FALSE) { # an upper limit has been set
    df_step2 <- df_step1 # don't change the data
    remove_df_UL <- df_step1[0, ]
  } # if (is.na(upper_limit) == FALSE) { # an upper limit has been set
  
  # remove data below the lower limit
  if (is.na(lower_limit) == FALSE) { # an lower limit has been set
    if (include_lower_limit == TRUE) {# data of exactly the lower_limit will be kept
      which_in_range <- which(df_step2[ , column_of_interest] >= lower_limit) # find the data at or above the lower limit, to be kept
      which_remove <- which(df_step2[ , column_of_interest] < lower_limit) # track which data points have been removed and why
   } else { # if (is.na(include_lower_limit) == TRUE) # data of exactly the lower_limit will be kept
      which_in_range <- which(df_step2[ , column_of_interest] > lower_limit) # find the data above the lower_limit, to be kept
      which_remove <- which(df_step2[ , column_of_interest] <= lower_limit) # track which data points have been removed and why
    } # if (is.na(include_lower_limit) == TRUE) # data of exactly the lower_limit will be kept
    df_keep <- df_step2[which_in_range, ] # keep only data within the specified range
    print(paste((dim(df_step2)[1] - dim(df_keep)[1])," data points were removed due to having ",column_of_interest," below ",lower_limit,sep = ""))
    remove_df_LL <- df_step2[which_remove, ]
    if (dim(remove_df_LL)[1]>1) {
      remove_df_LL$Reason <- reason_removed
    }
    rm(which_in_range,which_remove)
  } else { # if (is.na(lower_limit) == FALSE) { # an lower limit has been set
    df_keep <- df_step2 # don't change the data
    remove_df_LL <- df_step2[0, ]
  } # if (is.na(lower_limit) == FALSE) { # an lower limit has been set
  
  df_remove <- rbind(remove_df_NA, remove_df_UL, remove_df_LL)
  df_out <- list(df_keep,df_remove)
  if (dim(df_in)[1] != dim(df_keep)[1]+dim(df_remove)[1]) {
    stop("number of rows not adding up correctly")
  }
  return(df_out)
} # end of remove_data_outside_range.fn function

# remove data points outside specified range of values
remove_data_2_criteria.fn <- function(df_in, column_of_interest1, upper_limit1 = NA, remove_NAs1 = TRUE, column_of_interest2, upper_limit2 = NA, lower_limit2 = NA, remove_NAs2 = TRUE, reason_removed = " ") {
    
  # remove NA values (step1)
  if (remove_NAs1 == TRUE & remove_NAs2 == TRUE) { # NA values should be removed
    which_not_NA <- which(is.na(df_in[ , column_of_interest1]) == FALSE & is.na(df_in[ , column_of_interest2]) == FALSE) # find the not-NAs
    df_step1 <- df_in[which_not_NA, ] # keep all data that doesn't have NA in the column of interest
    #print(paste((dim(df_in)[1] - dim(df_step1)[1])," data points were removed due to having ",column_of_interest," as NA ",sep = ""))
    print(paste((dim(df_in)[1] - dim(df_step1)[1])," data points were removed due to having ",column_of_interest1," or ",column_of_interest2," as NA ",sep = ""))
    
    # track which data points have been removed and why
    which_remove <- which(is.na(df_in[ , column_of_interest2]) == TRUE | is.na(df_in[ , column_of_interest2]) == TRUE) # find the NAs
    remove_df_NA <- df_in[which_remove, ]
    if (dim(remove_df_NA)[1]>1) { # input reason for data removal into data frame
      remove_df_NA$Reason <- reason_removed
    } # if (dim(remove_df_NA)[1]>1) { # input reason for data removal
    rm(which_not_NA,which_remove)
    } else if (remove_NAs1 == TRUE & remove_NAs2 == FALSE) {
      stop("Finish writing code in remove_data_2_criteria.fn")
    } else if (remove_NAs1 == FALSE & remove_NAs2 == TRUE) {
      stop("Finish writing code in remove_data_2_criteria.fn")
    } else if (remove_NAs1 == FALSE & remove_NAs2 == FALSE) { # NA values should not be removed
      df_step1 <- df_in
      remove_df_NA <- df_in[0,]
  }
  
  # remove data above fitting the criteria (step2)
  if (class(upper_limit1) == "character" & class(upper_limit2) == "Date") {
      which_remove <- which(df_step1[ , column_of_interest1] == upper_limit1 & df_step1[ , column_of_interest2] >= lower_limit2 & df_step1[ , column_of_interest2] <= upper_limit2)
      print(paste(length(which_remove)," data points were removed due to having ",column_of_interest1," of ",upper_limit1," and ",column_of_interest2," in the range ",lower_limit2,"-",upper_limit2,sep = ""))
      remove_df_criteria <- df_step1[which_remove, ]
      which_in_range <- which_rows_remain.fn(df_start = df_step1, which_rows_subset1 = which_remove) # figure out which data points remain
      #dummy_vec <- c(1:dim(df_step1)[1])
      #which_in_range <- which(dummy_vec %!in% which_remove)
      #rm(dummy_vec)
      #checksum.fn(N_original = dim(df_step1)[1],part_A = length(which_remove),part_B = length(which_in_range))
      df_keep <- df_step1[which_in_range, ] # keep only data within the specified range
    if (dim(remove_df_criteria)[1]>0) {
      remove_df_criteria$Reason <- reason_removed
    }
    rm(which_in_range,which_remove)
} else {
  stop("finish writing code for remove_data_2_criteria.fn")
}
  df_remove <- rbind(remove_df_NA, remove_df_criteria)
  df_out <- list(df_keep,df_remove)
  checksum.fn(N_original = dim(df_in)[1], part_A = dim(df_keep)[1], part_B = dim(df_remove)[1])
  #if (dim(df_in)[1] != dim(df_keep)[1]+dim(df_remove)[1]) {
  #  stop("number of rows not adding up correctly")
  #}
  return(df_out)
} # end of remove_data_outside_range.fn function

which_rows_remain.fn <- function(df_start,which_rows_subset1) {
  dummy_vec <- c(1:dim(df_start)[1])
  which_rows_subset2 <- which(dummy_vec %!in% which_rows_subset1)
  rm(dummy_vec)
  checksum.fn(N_original = dim(df_start)[1],part_A = length(which_rows_subset1),part_B = length(which_rows_subset2))
  return(which_rows_subset2)
} # end of which_rows_remain.fn function

# merge time-varying datasets
merge_time_varying_data.fn <- function(ML_input_in,predictor_data,latitude_col_s,longitude_col_s,datum_col_s,Dates_col_s) {
  # round lat/lon and recognize dates as dates for the two data frames to be joined
  ML_input_in$Latitude <- round(ML_input_in$Latitude, 5)
  ML_input_in$Longitude <- round(ML_input_in$Longitude, 5)
  ML_input_in$Date <- as.Date(ML_input_in$Date,"%Y-%m-%d") # recognize dates as dates
  ML_input_in_no_repeats <- ML_input_in[!duplicated(ML_input_in), ] # remove any repeated rows
  predictor_data[ , latitude_col_s] <- round(predictor_data[ , latitude_col_s], 5)
  predictor_data[ , longitude_col_s] <- round(predictor_data[ , longitude_col_s], 5)
  predictor_data[ , Dates_col_s] <- as.Date(predictor_data[ , Dates_col_s],"%Y-%m-%d") # recognize dates as dates
  predictor_data_no_repeats <- predictor_data[!duplicated(predictor_data), ] # remove any repeated rows
  
  # join data sets using the join function:
  #ML_input_out <- join(x = ML_input_in_no_repeats, y = predictor_data_no_repeats, by = c( "Latitude" = latitude_col_s, "Longitude" = longitude_col_s, "Date" = Dates_col_s), type = "left") 
  
  # join data sets using custom function:
  ML_input_out_list <- lapply(X = 1:dim(ML_input_in)[1],FUN = function(x){
    #print(names(predictor_data_no_repeats))
    row_number <- x
    print(row_number)
    ML_input_row <- ML_input_in[row_number, ]
    this_lat <- ML_input_row$Latitude
    N_dec_lat <- decimalplaces(this_lat) # how many decimal places are in the latitude variable?
    this_lon <- ML_input_row$Longitude
    N_dec_lon <- decimalplaces(this_lon) # how many decimal places are in the longitude variable?
    this_Date <- ML_input_row$Date
  
    # make sure we're matching on date
    which_match_date <- which(predictor_data_no_repeats$Date == this_Date)
    length(which_match_date)
    predictor_data_date <- predictor_data_no_repeats[which_match_date, ]
    
    # match on latitude & longitude
    which_match <- which(round(predictor_data_date[ , c("Latitude")],N_dec_lat) == round(ML_input_row$Latitude,N_dec_lat) & 
                           round(predictor_data_date[ , c("Longitude")],N_dec_lon) == round(ML_input_row$Longitude,N_dec_lon))
    if (length(which_match)>0) { # is there a match?
    predictor_row_step1 <- predictor_data_date[which_match, ] # isolate matching data
    predictor_row_step1$Longitude <- round(predictor_row_step1$Longitude,N_dec_lon) # round longitudes
    predictor_row_step1$Latitude <- round(predictor_row_step1$Latitude,N_dec_lon) # round latitudes
    predictor_row_all_col <- predictor_row_step1[!duplicated(predictor_row_step1), ] # de-duplicate rows of data
    match_found <- 1
      if (dim(predictor_row_all_col)[1]>1) { # multiple rows of data. Investigate and write more code
        stop(paste("multiple rows of data. Investigate and write more code. x = ",x))
      } # multiple rows of data. Investigate and write more code
    } else { # if (length(which_match)>0) { # is there a match?
      stop(paste("no match found."))
    } # if (length(which_match)>0) { # is there a match?
    
   # which_match_lat5 <- which(predictor_data_date$Latitude == this_lat) 
  #  if (length(which_match_lat5) == 1) { # is there exactly 1 match?
  #     if (round(predictor_data_date[which_match_lat5, c("Longitude")],4) == round(ML_input_row$Longitude,4)) { # does the longitude match?
  #      print("match found")
  #      match_found <- 1   
      # predictor_row_all_col <- predictor_data_date[which_match_lat5, ]
      # # remove extraneous columns
      # drop_cols <- c("Latitude","Longitude","Date") # define unnecessary columns
      # keep_cols <- which(names(predictor_row_all_col) %!in% drop_cols)
      # keep_names <- names(predictor_row_all_col[keep_cols])
      # predictor_row <- data.frame(matrix(NA,nrow = 1,ncol = length(keep_names))) # create data frame
      # names(predictor_row) <- keep_names
      # predictor_row[1 , ] <- predictor_row_all_col[1, keep_cols]
       #} # if (round(predictor_data_date[which_match_lat5, c("Longitude")],4) == round(ML_input_row$Longitude,4)) { # does the longitude match?
    #} else if (length(which_match_lat5) > 1) {
    #  which_match_lat4 <- which(round(predictor_data_date[ , c("Latitude")],5) == round(ML_input_row$Latitude,5) & 
    #                              round(predictor_data_date[ , c("Longitude")],4) == round(ML_input_row$Longitude,4))
    #} else { # if (length(which_match_lat5) == 1) { # is there exactly 1 match?
    #  match_found <- 0
    #  stop(paste("match not found. row",x))
    #} # if (length(which_match_lat5) == 1) { # is there exactly 1 match?
    
    if (match_found == 1) { # match was found
      #predictor_row_all_col <- predictor_data_date[which_match_lat5, ]
      # remove extraneous columns
      drop_cols <- c("Latitude","Longitude","Date") # define unnecessary columns
      keep_cols <- which(names(predictor_row_all_col) %!in% drop_cols)
      keep_names <- names(predictor_row_all_col[keep_cols])
      predictor_row <- data.frame(matrix(NA,nrow = 1,ncol = length(keep_names))) # create data frame
      names(predictor_row) <- keep_names
      predictor_row[1 , ] <- predictor_row_all_col[1, keep_cols]
    } else { # if (match_found == 1) { # match was found
      stop("write more code to accomodate no match being found in merge_time_varying_data.fn function")
    } # if (match_found == 1) { # match was found
    
    ML_input_out_row <- cbind(ML_input_row,predictor_row)
    return(ML_input_out_row)
  }) # end of ML_input_out_list lapply
  ML_input_out <- do.call("rbind",ML_input_out_list)
  return(ML_input_out)
} # end of merge_time_varying_data.fn function

# merge temporally static datasets
merge_time_static_data.fn <- function(ML_input_in,predictor_data,latitude_col_s,longitude_col_s) {
  # ML_input_in <- ML_input
  # predictor_data 
  # ML_input_in <- ML_input
  # predictor_data <- Highways_data
  #ML_input_in <- ML_input_step2
  # predictor_data <- GASP_data
  #ML_input_in <- ML_input_step4
  # predictor_data <- NAM_data
  
  # round lat/lon and recognize dates as dates for the two data frames to be joined
  ML_input_in$Latitude <- round(ML_input_in$Latitude, 5)
  ML_input_in$Longitude <- round(ML_input_in$Longitude, 5)
  predictor_data[ , latitude_col_s] <- round(predictor_data[ , latitude_col_s], 5)
  predictor_data[ , longitude_col_s] <- round(predictor_data[ , longitude_col_s], 5)
  
  ML_input_out <- join(x = ML_input_in, y = predictor_data, by = c( "Latitude" = latitude_col_s, "Longitude" = longitude_col_s)) # join data sets
  
  return(ML_input_out)
} # end of merge_time_static_data.fn function

# replace column names for data frame
replace_column_names.fn <- function(df_in,old_col_name,new_col_name) {
  # df_in <- Highways_data
  # old_col_name <- "Datum" #"Lat"
  # new_col_name <- "NewName" #"Latitude"
  
  if (old_col_name %in% colnames(df_in)) { # check if the old_col_name is a column name in df_in
    #Highways_data <- Highways_data[ , !(names(Highways_data) %in% )]
    which_col <- which(colnames(df_in) == old_col_name)
    colnames(df_in)[which_col] <- new_col_name
    } # if (old_col_name %in% colnames(df_in)) { # check if the old_col_name is a column name in df_in

  return(df_in) # output from function
} # end of replace_column_names.fn function

# Load and Merge Fire Modis Data
merge_Fire_MODIS_data.fn <- function(Buffer_radius_km, ML_input, Fire_MODIS_file_name,ProcessedData.directory,predictor_sub_folder,this_Date) {#, study_start_date, study_stop_date) {
  Fire_MODIS_data_list <- list() # create list for merging all data sets
  latitude_col_s <- "Latitude" # define latitude column
  longitude_col_s <- "Longitude" # define longitude column
  Dates_col_s <- "Date" # define date column
  for (file_i in 1:length(Fire_MODIS_file_name)) { # Load and merge all Fire_MODIS Data files
    print(paste("Processing Fire_MODIS file ",Fire_MODIS_file_name[file_i],sep = ""))
    Fire_MODIS_data_step <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, Fire_MODIS_file_name[file_i]),header=TRUE) # load data file
    Fire_MODIS_data_step<- as.data.frame(Fire_MODIS_data_step) # define data as data frame
    Fire_MODIS_data_step[ , c(Dates_col_s)] <- as.Date(Fire_MODIS_data_step[ , c(Dates_col_s)],"%m/%d/%Y") # recognize dates as dates
    
    # change column names
    Fire_MODIS_data_step <- replace_column_names.fn(df_in = Fire_MODIS_data_step, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
    Fire_MODIS_data_step <- replace_column_names.fn(df_in = Fire_MODIS_data_step, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
    #Fire_MODIS_data_step <- replace_column_names.fn(df_in = Fire_MODIS_data_step, old_col_name = "fire_count", new_col_name = paste(Buffer_radius_km,"km","_fire_count",sep = "")) # replace "Lat" with "Latitude"
    new_col_name <- paste(Buffer_radius_km,"km","_fire_count",sep = "")
    Fire_MODIS_data_step <- replace_column_names.fn(df_in = Fire_MODIS_data_step, old_col_name = "Fire_Count", new_col_name = new_col_name) # replace "Lat" with "Latitude"
    
    Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Latitude","Longitude","Date"), input_data = Fire_MODIS_data_step)
    if (length(Check_data)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}
    rm(Check_data)
    
    # remove extraneous columns
    drop_cols <- c("Datum","Easting","Northing","old_lon","old_lat","old_Datum") # define unnecessary columns
    Fire_MODIS_data_step <- Fire_MODIS_data_step[ , !(names(Fire_MODIS_data_step) %in% drop_cols)] # drop unnecessary columns
    
    # isolate data for this date
    which_this_date <- which(Fire_MODIS_data_step[ , c(Dates_col_s)] == this_Date) # which rows in the Fire_MODIS data are for this date?
    if (length(which_this_date) > 0) { # is there data for this date in this file?
      print(paste("There is Fire_MODIS data for ",this_Date," in ",Fire_MODIS_file_name[file_i]))
      Fire_MODIS_data_date_step <- Fire_MODIS_data_step[which_this_date, ] # isolate data for this date
      Fire_MODIS_data_date <- Fire_MODIS_data_date_step[!duplicated(Fire_MODIS_data_date_step), ] # de-duplicate rows of data
      rm(Fire_MODIS_data_date_step)
      Fire_MODIS_data_list[[Fire_MODIS_file_name[file_i]]] <- Fire_MODIS_data_date # input data into list
      
      # # remove below after trouble shooting
      # Lat_interest <- Loc_data_repeats$Latitude # 36.78133
      # Lon_interest <- Loc_data_repeats$Longitude # -119.7732
      # which_Loc_interest <- which(round(Fire_MODIS_data_date$Latitude,5) == round(Lat_interest,5) & round(Fire_MODIS_data_date$Longitude,5) == round(Lon_interest,5))
      # #if (length(which_Loc_interest)>0) {
      #   print(paste("The location of interest,",Lat_interest,Lon_interest,"has",length(which_Loc_interest),"data points in",Fire_MODIS_file_name[file_i]))
      # #}
      # # remove above after trouble shooting
      
      rm(Fire_MODIS_data_date)
    } else { # if (length(which_this_date) > 0) { # is there data for this date in this file? - No
      print(paste("No Fire_MODIS data for",this_Date,"in",Fire_MODIS_file_name[file_i]))
      #Fire_MODIS_data_date <- Fire_MODIS_data_step[1, ] # just grab first row as a place holder since nothing matches
    } # if (length(which_this_date) > 0) { # is there data for this date in this file?
    rm(Fire_MODIS_data_step) # clear variable
  } # for (file_i in 1:length(Highways_file_name)) { # Load and merge all Fire_MODIS Data files
  Fire_MODIS_data_w_dups <- do.call("rbind", Fire_MODIS_data_list) # unlist data from various files
  Fire_MODIS_data <- Fire_MODIS_data_w_dups[!duplicated(Fire_MODIS_data_w_dups),] # de-duplicate rows of data
  rm(Fire_MODIS_data_w_dups,Fire_MODIS_data_list)
  if (!is.null(Fire_MODIS_data)) { # merge No Fire_MODIS data if there is any for this date
  ML_input <- merge_time_varying_data.fn(ML_input_in = ML_input, predictor_data = Fire_MODIS_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s, datum_col_s = datum_col_s,Dates_col_s = Dates_col_s) # join wrapper function
  } # if (!is.null(Fire_MODIS_data)) { # merge No Fire_MODIS data if there is any for this date
  rm(Fire_MODIS_data) # clear variables
  # add column as space holder if there was no data
  if (new_col_name %!in% colnames(ML_input)) { # add column as space holder if there was no data
    ML_input[, new_col_name] <- NA # add column as space holder if there was no data
  } # add column as space holder if there was no data
  return(ML_input) # output from function
} # end of merge_Fire_MODIS_data.fn function

# Load and merge GASP Data
merge_GASP_data.fn <- function(ML_input, GASP_file_name,ProcessedData.directory,predictor_sub_folder, this_Date) { # study_start_date, study_stop_date) {
  GASP_data_list <- list() # create list for merging all data sets
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  Dates_col_s <- "Date"
  for (file_i in 1:length(GASP_file_name)) { # Load and merge all GASP Data files
    print(paste("Processing GASP file",GASP_file_name[file_i]))
    GASP_data_step <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, GASP_file_name[file_i]),header=TRUE) # load the AQS file
    GASP_data_step<- as.data.frame(GASP_data_step)
    GASP_data_step[ , c(Dates_col_s)] <- as.Date(GASP_data_step[ , c(Dates_col_s)],"%Y-%m-%d") # recognize dates as dates
    
    # change column names
    GASP_data_step <- replace_column_names.fn(df_in = GASP_data_step, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
    GASP_data_step <- replace_column_names.fn(df_in = GASP_data_step, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
    GASP_data_step <- replace_column_names.fn(df_in = GASP_data_step, old_col_name = "AOD", new_col_name = "GASP_AOD") # replace "Lat" with "Latitude"
    
    # remove extraneous columns
    drop_cols <- c("Datum","Easting","Northing","old_lon","old_lat","old_Datum","X","old_datum") # define unnecessary columns
    GASP_data_step <- GASP_data_step[ , !(names(GASP_data_step) %in% drop_cols)] # drop unnecessary columns
    # remove extraneous columns and dates that are not this_Date
    #GASP_data <- GASP_data_step[which_this_date,c("Latitude","Longitude","Date","GASP_AOD")]
    
    Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Latitude","Longitude","Date"), input_data = GASP_data_step)
    if (length(Check_data)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}
    rm(Check_data)
    
    # isolate data for this date
    which_this_date <- which(GASP_data_step[ , c(Dates_col_s)] == this_Date)
    if (length(which_this_date) > 0) { # is there data for this date?
      print(paste("There is GASP data for ",this_Date," in ",GASP_file_name[file_i]))
      GASP_data_date <- GASP_data_step[which_this_date, ] # isolate data for this date
      GASP_data_list[[GASP_file_name[file_i]]] <- GASP_data_date # input data into list
      rm(GASP_data_date)
    } else { # if (length(which_this_date) > 0) { # is there data for this date? - No
      print(paste("No GASP data for ",this_Date," in ",GASP_file_name[file_i]))
    } # if (length(which_this_date) > 0) { # is there data for this date?
    rm(GASP_data_step) # clear variable
    } # for (file_i in 1:length(GASP_file_name)) { # Load and merge all GASP Data files
    GASP_data_w_dups <- do.call("rbind", GASP_data_list) # unlist data from various files
    GASP_data <- GASP_data_w_dups[!duplicated(GASP_data_w_dups),] # de-duplicate rows of data
    rm(GASP_data_w_dups,GASP_data_list)
    if (!is.null(GASP_data)) { # merge GASP data if there is any for this date
    ML_input <- merge_time_varying_data.fn(ML_input_in = ML_input, predictor_data = GASP_data, latitude_col_s = latitude_col_s, longitude_col_s = longitude_col_s, datum_col_s = datum_col_s,Dates_col_s = Dates_col_s) # join wrapper function
    } # if (!is.null(GASP_data)) { # merge GASP data if there is any for this date
    rm(GASP_data)
  # add column as space holder if there was no data
  if ("GASP_AOD" %!in% colnames(ML_input)) { # add column as space holder if there was no data
    ML_input$GASP_AOD <- NA # add column as space holder if there was no data
  } # add column as space holder if there was no data
  return(ML_input)
} # end of merge_GASP_data.fn function

# Load and merge Highways Data
merge_Highways_data.fn <- function(ML_input, Highways_file_name, ProcessedData.directory,predictor_sub_folder) { # study_start_date, study_stop_date) {  # Load and merge Highways Data
  Highways_data_list <- list()
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  #datum_col_s <- "Datum"
  for (file_i in 1:length(Highways_file_name)) { # Load and merge all Highways Data files
    print(paste("Processing ",Highways_file_name[file_i]))
    Highways_data_step <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, Highways_file_name[file_i]),header=TRUE) # load the file
    Highways_data_step<- as.data.frame(Highways_data_step)

    # change column names
    Highways_data_step <- replace_column_names.fn(df_in = Highways_data_step, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
    Highways_data_step <- replace_column_names.fn(df_in = Highways_data_step, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"

    # remove extraneous columns
    drop_cols <- c("X","Datum","old_lon","old_lat","old_Datum","Easting","Northing")
    Highways_data_step <- Highways_data_step[ , !(names(Highways_data_step) %in% drop_cols)]

    Highways_data_list[[Highways_file_name[file_i]]] <- Highways_data_step
    rm(Highways_data_step)
  } # for (file_i in 1:length(Highways_file_name)) { # Load and merge all Highways Data files
  Highways_data_w_dups <- do.call("rbind", Highways_data_list) # unlist data from various files
  Highways_data <- Highways_data_w_dups[!duplicated(Highways_data_w_dups),] # de-duplicate rows of data
  
  Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Latitude","Longitude"), input_data = Highways_data)
  if (length(Check_data)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}
  rm(Check_data)
  
  # join wrapper function
  ML_input <- merge_time_static_data.fn(ML_input_in = ML_input, predictor_data = Highways_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s) 
  rm(Highways_data, Highways_data_list,Highways_data_w_dups) # clear variable
  return(ML_input)
} # end of merge_Highways_data.fn function

# Load and merge MAIAC Data
merge_MAIAC_data.fn <- function(ML_input,MAIAC_file_name,ProcessedData.directory,predictor_sub_folder, this_Date) { #study_start_date, study_stop_date) {
  MAIAC_data_list <- list() # create list for merging all data sets
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  Dates_col_s <- "Date"
  for (file_i in 1:length(MAIAC_file_name)) { # Load and merge all MAIAC Data files
    print(paste("Processing",MAIAC_file_name[file_i]))
    MAIAC_data_step <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, MAIAC_file_name[file_i]),header=TRUE, stringsAsFactors=FALSE) # load the AQS file
    MAIAC_data_step <- as.data.frame(MAIAC_data_step)
    date_format <- determine_date_format.fn(check_date = MAIAC_data_step[10, c(Dates_col_s)]) # determine format used for this MAIAC file; pick the 10th row as representative of the file (sometimes the first few rows are repeated headers)
    print(date_format) # REMOVE
    #MAIAC_data_step[ , c(Dates_col_s)] <- as.Date(MAIAC_data_step[ , c(Dates_col_s)],"%Y-%m-%d") # recognize dates as dates
    MAIAC_data_step[ , c(Dates_col_s)] <- as.Date(MAIAC_data_step[ , c(Dates_col_s)],date_format) # recognize dates as dates
    options(warn  =  1) # dont' throw an error when there's a warning and stop the code from running further
    MAIAC_data_step$MAIAC_AOD <- as.numeric(MAIAC_data_step$MAIAC_AOD)
    options(warn  =  2) # throw an error when there's a warning and stop the code from running further
    print(paste("Earliest date in file:",min(MAIAC_data_step[ , c(Dates_col_s)],na.rm = TRUE)))
    #plot.new()
    # #points(MAIAC_data_step$Date, MAIAC_data_step$MAIAC_AOD)
    #plot(MAIAC_data_step$Date, MAIAC_data_step$MAIAC_AOD)
    # change column names
    MAIAC_data_step <- replace_column_names.fn(df_in = MAIAC_data_step, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
    MAIAC_data_step <- replace_column_names.fn(df_in = MAIAC_data_step, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
    
    # remove extraneous columns
    drop_cols <- c("Datum","Easting","Northing","old_lon","old_lat","old_Datum","X") # define unnecessary columns
    MAIAC_data_step <- MAIAC_data_step[ , !(names(MAIAC_data_step) %in% drop_cols)] # drop unnecessary columns
    #MAIAC_data <- MAIAC_data_step[which_this_date ,c("Latitude","Longitude","Date","MAIAC_AOD")]
    #MAIAC_data$Latitude <- as.numeric(as.character(MAIAC_data$Latitude))
    #MAIAC_data$Longitude <- as.numeric(as.character(MAIAC_data$Longitude))
    #summary(MAIAC_data)
    
    # isolate data for this date
    which_this_date <- which(MAIAC_data_step[ , c(Dates_col_s)] == this_Date)
    if (length(which_this_date) > 0) { # is there any data for this date?
    print(paste("There is MAIAC data for ",this_Date," in ",MAIAC_file_name[file_i],sep = ""))
    MAIAC_data_date <- MAIAC_data_step[which_this_date, ] # isolate data for this date
    MAIAC_data_date[ , c(latitude_col_s)] <- as.numeric(as.character(MAIAC_data_date[ , c(latitude_col_s)])) # recognize latitude as numerical
    MAIAC_data_date[ , c(longitude_col_s)] <- as.numeric(as.character(MAIAC_data_date[ , c(longitude_col_s)])) # recognize longitude as numerical
    Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Latitude","Longitude","Date"), input_data = MAIAC_data_date)
    if (length(Check_data)>0) {print("***Check_4_NAs.fn found questionable data. Investigate.***")}
    rm(Check_data)
    #which_not_na_date <- which(!is.na(MAIAC_data_date$Date) & !is.na(MAIAC_data_date$Latitude))
    #MAIAC_data_date <- MAIAC_data_date[which_not_na_date, ]
    Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Latitude","Longitude","Date"), input_data = MAIAC_data_date)
    if (length(Check_data)>0) {print("***Check_4_NAs.fn found questionable data. Investigate.***")}
    rm(Check_data)
    MAIAC_data_date_no_repeats <- MAIAC_data_date[!duplicated(MAIAC_data_date), ]
    MAIAC_data_list[[MAIAC_file_name[file_i]]] <- MAIAC_data_date_no_repeats # input data into list
    rm(MAIAC_data_date,MAIAC_data_date_no_repeats)
    } else { # if (length(which_this_date) > 0) { # is there any data for this date? - No
      print(paste("No MAIAC data for ",this_Date," in ",MAIAC_file_name[file_i]))
    #  MAIAC_data_date <- MAIAC_data_step[20, ] # just grab 20th row as a place holder since nothing matches (preserve number of columns)
    } # if (length(which_this_date) > 0) { # is there any data for this date?  
    rm(MAIAC_data_step) # clear variable
  } # for (file_i in 1:length(MAIAC_file_name)) { # Load and merge all MAIAC Data files
  MAIAC_data_w_dups <- do.call("rbind", MAIAC_data_list) # unlist data from various files
  MAIAC_data <- MAIAC_data_w_dups[!duplicated(MAIAC_data_w_dups),] # de-duplicate rows of data

  if (!is.null(MAIAC_data)) { # merge MAIAC data if there is any for this date
    Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Latitude","Longitude","Date"), input_data = MAIAC_data)
    if (length(Check_data)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}
    rm(Check_data)
    if (class(MAIAC_data$Date) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
    if (class(MAIAC_data$Latitude) != "numeric") {stop("***class of Date_Local is not 'Date'. Investigate***")}
    rm(MAIAC_data_w_dups)
    ML_input <- merge_time_varying_data.fn(ML_input_in = ML_input, predictor_data = MAIAC_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s, datum_col_s = datum_col_s,Dates_col_s = Dates_col_s) # join wrapper function
  } # if (!is.null(MAIAC_data)) { # merge GASP data if there is any for this date
  rm(MAIAC_data)

  # add column as space holder if there was no data
  if ("MAIAC_AOD" %!in% colnames(ML_input)) { # add column as space holder if there was no data
    ML_input$MAIAC_AOD <- NA # add column as space holder if there was no data
  } # add column as space holder if there was no data
  return(ML_input)
} # end of merge_MAIAC_data.fn function

# Load and merge NAM Data
merge_NAM_data.fn <- function(ML_input, NAM_file_name,task_counter,ProcessedData.directory,predictor_sub_folder,this_Date) {#, study_start_date, study_stop_date) {
  NAM_data_list <- list() # create list for merging all data sets
  latitude_col_s <- "Latitude" # define latitude column
  longitude_col_s <- "Longitude" # define longitude column
  Dates_col_s <- "Date" # define date column
  for (file_i in 1:length(NAM_file_name)) { # Load and merge all NAM Data files
    print(paste("Processing NAM file ",NAM_file_name[file_i],sep = ""))
    #NAM_data_step <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, NAM_file_name[file_i]),header=TRUE) # load data file
    NAM_data_step <- read.csv(file.path(ProcessedData.directory,NAM_folder,NAM_sub_folder, NAM_file_name[file_i]),header=TRUE) # load data file
    NAM_data_step<- as.data.frame(NAM_data_step) # define data as data frame
    
    # change column names
    NAM_data_step <- replace_column_names.fn(df_in = NAM_data_step, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
    NAM_data_step <- replace_column_names.fn(df_in = NAM_data_step, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
    NAM_data_step <- replace_column_names.fn(df_in = NAM_data_step, old_col_name = "Local.Date", new_col_name = "Date") # replace "Lat" with "Latitude"
    
    NAM_data_step[ , c(Dates_col_s)] <- as.Date(NAM_data_step[ , c(Dates_col_s)],"%Y-%m-%d") # recognize dates as dates
    
    # remove extraneous columns
    drop_cols <- c("Datum","Easting","Northing","old_lon","old_lat","old_Datum") # define unnecessary columns
    NAM_data_step <- NAM_data_step[ , !(names(NAM_data_step) %in% drop_cols)] # drop unnecessary columns
    
    # track column names
    NAM_colnames_step <- colnames(NAM_data_step)
    NAM_colnames <- NAM_colnames_step[NAM_colnames_step %!in% c("Latitude","Longitude","Date")]
    rm(NAM_colnames_step)
    
    # # REMOVE after taking daily averages of NAM data - take first entry for NAM data for each date/location#
    # print("*** REMOVE lines of code in merge_NAM_data.fn that de-duplicate NAM data")
    # NAM_dates_loc_w_repeats <- NAM_data_step[ ,c("Latitude","Longitude","Date")] # REMOVE
    # NAM_data_loc_no_repeats <- NAM_dates_loc_w_repeats[!duplicated(NAM_dates_loc_w_repeats), ] # REMOVE
    # NAM_data_step2 <- join(x = NAM_data_loc_no_repeats, y = NAM_data_step, # REMOVE
    #                      by = c( "Latitude" = latitude_col_s, "Longitude" = longitude_col_s, "Date" = Dates_col_s), # REMOVE
    #                      type = "left", match = "first") # REMOVE
    # rm(NAM_data_step) # REMOVE
    # NAM_data_step <- NAM_data_step2 # REMOVE
    # rm(NAM_data_step2) # REMOVE
    
    Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Latitude","Longitude","Date"), input_data = NAM_data_step)
    if (length(Check_data)>0) {stop(paste("***Check_4_NAs.fn found questionable NAM data. Investigate.***X =",X,"Date = ",this_Date))}
    rm(Check_data)
    
    # isolate data for this date
    which_this_date <- which(NAM_data_step[ , c(Dates_col_s)] == this_Date) # which rows in the NAM data are for this date?
    if (length(which_this_date) > 0) { # is there data for this date in this file?
      print(paste("There is NAM data for ",this_Date," in ",NAM_file_name[file_i]))
      NAM_data_date <- NAM_data_step[which_this_date, ] # isolate data for this date
      NAM_data_list[[NAM_file_name[file_i]]] <- NAM_data_date # input data into list
      rm(NAM_data_date)
    } else { # if (length(which_this_date) > 0) { # is there data for this date in this file? - No
      print(paste("No NAM data for",this_Date,"in",NAM_file_name[file_i]))
      #NAM_data_date <- NAM_data_step[1, ] # just grab first row as a place holder since nothing matches
    } # if (length(which_this_date) > 0) { # is there data for this date in this file?
    #NAM_data_list[[NAM_file_name[file_i]]] <- NAM_data_date # input data into list
    rm(NAM_data_step) # clear variable
  } # for (file_i in 1:length(Highways_file_name)) { # Load and merge all NAM Data files
  NAM_data_w_dups <- do.call("rbind", NAM_data_list) # unlist data from various files
  NAM_data <- NAM_data_w_dups[!duplicated(NAM_data_w_dups), ] # de-duplicate rows of data
  rm(NAM_data_list,NAM_data_w_dups) # clear variables
  if (!is.null(NAM_data)) { # merge NAM data if there is any for this date
  ML_input <- merge_time_varying_data.fn(ML_input_in = ML_input, predictor_data = NAM_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s, datum_col_s = datum_col_s,Dates_col_s = Dates_col_s) # join wrapper function
  } # if (!is.null(NAM_data)) { # merge NAM data if there is any for this date
  rm(NAM_data)
  # add column as space holder if there was no data
  if (max(NAM_colnames %!in% colnames(ML_input))==1) { # add column as space holder if there was no data
    #ML_input$GASP_AOD <- NA # add column as space holder if there was no data
    ML_input[ ,NAM_colnames] <- NA
  } # add column as space holder if there was no data
  return(ML_input) # output from function
} # end of merge_NAM_data.fn function

# Load and merge NED Data
merge_NED_data.fn <- function(ML_input, NED_file_name, ProcessedData.directory,predictor_sub_folder) {
  NED_data_list <- list()
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  for (file_i in 1:length(NED_file_name)) { # Load and merge all NED Data files
  print(paste("Processing",NED_file_name[file_i]))
  NED_data_step <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, NED_file_name[file_i]),header=TRUE) # load the file
  NED_data_step<- as.data.frame(NED_data_step)
  
  # change column names
  NED_data_step <- replace_column_names.fn(df_in = NED_data_step, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
  NED_data_step <- replace_column_names.fn(df_in = NED_data_step, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
  
  # remove extraneous columns
  drop_cols <- c("Datum","Easting","Northing","old_lon","old_lat","old_Datum")
  NED_data_step <- NED_data_step[ , !(names(NED_data_step) %in% drop_cols)]
  
  NED_data_list[[NED_file_name[file_i]]] <- NED_data_step
  rm(NED_data_step)
  } # for (file_i in 1:length(NED_file_name)) { # Load and merge all NED Data files
  NED_data_w_dups <- do.call("rbind", NED_data_list)
  #NED_data_w_dups$elevation <- round(NED_data_w_dups$elevation,5)
  NED_data_w_dups$elevation <- round(NED_data_w_dups$elevation)
  NED_data_w_dups$Latitude <- round(NED_data_w_dups$Latitude,5)
  NED_data_w_dups$Longitude <- round(NED_data_w_dups$Longitude,5)
  NED_data_step2 <- NED_data_w_dups[!duplicated(NED_data_w_dups),]
  

  
  # round data to same value
  Loc_data <- NED_data_step2[ ,c("Latitude","Longitude")]
  Loc_data_repeats <- Loc_data[duplicated(Loc_data), ]
  for (repeat_i in 1:dim(Loc_data_repeats)[1]) { # cycle through locations that are repeated
    this_lat <- Loc_data_repeats[repeat_i,"Latitude"] # what is the latitude?
    this_lon <- Loc_data_repeats[repeat_i,"Longitude"] # what is the longitude?
    which_rows <- which(NED_data_step2$Latitude == this_lat & NED_data_step2$Longitude == this_lon) # find the rows with the repeated locations
    these_elevs <- NED_data_step2[which_rows,"elevation"]
    print(these_elevs)
    if (max(these_elevs)-min(these_elevs) > 10) {stop(paste("different estimates of elevation for a location vary by more than 10 meters for date ",unique(ML_input$Date)))}
    NED_data_step2[which_rows,"elevation"] <- mean(these_elevs)
  }
  NED_data <- NED_data_step2[!duplicated(NED_data_step2),]
  
  rm(NED_data_w_dups,NED_data_list,NED_data_step2)
  # join wrapper function
  ML_input <- merge_time_static_data.fn(ML_input_in = ML_input, predictor_data = NED_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s) 
  ML_input <- ML_input[!duplicated(ML_input), ] # get rid of repeated rows (not sure why they appear)
  #Loc_data <- ML_input_out[ ,c("Latitude","Longitude")]
  #Loc_data_repeats <- Loc_data[duplicated(Loc_data), ]
  #which_repeated <- which(ML_input_out$Latitude %in% Loc_data_repeats$Latitude & ML_input_out$Longitude %in% Loc_data_repeats$Longitude)
  #print(which_repeated)
  #ML_input_repeats <- ML_input_out[which_repeated, ]
  #ML_input_repeats2 <- ML_input_repeats[!duplicated(ML_input_repeats), ]
  
  if ("elevation" %!in% colnames(ML_input)) { # add column as space holder if there was no data
    ML_input$elevation <- NA # add column as space holder if there was no data
  } # add column as space holder if there was no data
  return(ML_input)
} # end of merge_NED_data.fn function

# Load and merge NLCD Data
merge_NLCD_data.fn <- function(buffer_radius, ML_input, NLCD_file_name,task_counter,ProcessedData.directory,predictor_sub_folder) {
  NLCD_data_list <- list()
  for (file_i in 1:length(NLCD_file_name)) { # Load and merge all NED Data files
  print(paste("Processing NLCD file ",NLCD_file_name[file_i],sep = ""))
  NLCD_data_step <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, NLCD_file_name[file_i]),header=TRUE) # load the file
  NLCD_data_step <- as.data.frame(NLCD_data_step)
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  
  # change column names
  NLCD_data_step <- replace_column_names.fn(df_in = NLCD_data_step, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
  NLCD_data_step <- replace_column_names.fn(df_in = NLCD_data_step, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
  var_col_name <- paste("NLCD_",buffer_radius,"_percent_urban_buffer",sep = "")
  #NLCD_data_step <- replace_column_names.fn(df_in = NLCD_data_step, old_col_name = "percent_urban_buffer", new_col_name = paste("NLCD_",buffer_radius,"_percent_urban_buffer",sep = "")) # input buffer radius into variable name
  NLCD_data_step <- replace_column_names.fn(df_in = NLCD_data_step, old_col_name = "percent_urban_buffer", new_col_name = var_col_name) # input buffer radius into variable name
  
  # remove extraneous columns
  drop_cols <- c("Datum","Easting","Northing","old_lon","old_lat","old_Datum")
  NLCD_data_step <- NLCD_data_step[ , !(names(NLCD_data_step) %in% drop_cols)]
  
  NLCD_data_list[[NLCD_file_name[file_i]]] <- NLCD_data_step
  rm(NLCD_data_step)
  } # for (file_i in 1:length(NLCD_file_name)) { # Load and merge all NED Data files
  NLCD_data_w_dups <- do.call("rbind", NLCD_data_list) # un-list NLCD data
  NLCD_data <- average_slight_LatLon_variations.fn(Data_w_dups = NLCD_data_w_dups,var_col_name = var_col_name)

  # join wrapper function
  ML_input <- merge_time_static_data.fn(ML_input_in = ML_input, predictor_data = NLCD_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s) 
  ML_input <- ML_input[!duplicated(ML_input), ] # get rid of repeated rows (not sure why they appear)
  return(ML_input)
} # end of merge_NLCD_data.fn function

# average location-rounded values
average_slight_LatLon_variations.fn <- function(Data_w_dups,var_col_name) {
  #Data_w_dups <- NLCD_data_w_dups
  Data_w_dups$Latitude <- round(Data_w_dups$Latitude,5) # round location info
  Data_w_dups$Longitude <- round(Data_w_dups$Longitude,5) # round location info
  Data_step1 <- Data_w_dups[!duplicated(Data_w_dups), ] # remove any rows that are complete repeats
  rm(Data_w_dups) # clear variables
  Loc_data <- Data_step1[ ,c("Latitude","Longitude")]
  Data_output <- Loc_data[!duplicated(Loc_data), ]
  Data_output[ ,c(var_col_name)] <- NA
  for (loc_i in 1:dim(Data_output)[1]) {
    this_lat <- Data_output[loc_i, c("Latitude")]
    this_lon <- Data_output[loc_i, c("Longitude")]
    which_this_loc <- which(Data_step1$Latitude == this_lat & Data_step1$Longitude == this_lon)
    Data_output[loc_i,var_col_name] <- mean(Data_step1[which_this_loc,var_col_name])
    rm(this_lat,this_lon,which_this_loc)
  }
  rm(loc_i)
  return(Data_output)
}

# determine format of date
determine_date_format.fn <- function(check_date) {
  check_date_char <- as.character(check_date)
  if (substr(check_date_char,(nchar(check_date_char)-4),(nchar(check_date_char)-4)) == "/") {
    date_format <- "%m/%d/%Y"
  } else if (substr(check_date_char,5,5) == "-") {
    date_format <- "%Y-%m-%d"
  } else {
    stop("unknown date format - expand code in determine_date_format.fn in merging_data_functions.R")
  }
  return(date_format)  
} # end of determine_date_format.fn function

'%!in%' <- function(x,y)!('%in%'(x,y)) # directly from https://stackoverflow.com/questions/5831794/opposite-of-in
