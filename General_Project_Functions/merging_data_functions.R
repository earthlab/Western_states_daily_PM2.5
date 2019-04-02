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
   vars_to_include <- c(latitude_col_t,longitude_col_t,datum_col_t,Dates_col_t,"Year","Month","Day")
  } # if (predictand_col %in% colnames(Source_Data)) { # this data includes PM2.5 data
  ML_input <- Source_Data[which_this_date, vars_to_include] # start ML_input data frame
  ML_input <- replace_column_names.fn(df_in = ML_input, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
  ML_input <- replace_column_names.fn(df_in = ML_input, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
  ML_input <- replace_column_names.fn(df_in = ML_input, old_col_name = "Date_Local", new_col_name = "Date") # replace "Lat" with "Latitude"
  rm(which_this_date)

  # Load and merge GASP Data
  print("start merging GASP data")
  ML_input <- merge_GASP_data.fn(ML_input = ML_input, GASP_file_name = GASP_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder, this_Date = this_Date)#, study_start_date = study_start_date, study_stop_date = study_stop_date)

  # Load and merge MAIAC Data
  print("start merging MAIAC data")
  ML_input <- merge_MAIAC_data.fn(ML_input = ML_input, MAIAC_file_name = MAIAC_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder, this_Date = this_Date)# , study_start_date = study_start_date, study_stop_date = study_stop_date)

  # Load and merge NED Data
  print("start merging NED data")
  ML_input <- merge_NED_data.fn(ML_input = ML_input, NED_file_name = NED_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder)

  # Load and merge Highways Data  
  print("start merging Highways data")
  ML_input <- merge_Highways_data.fn(ML_input = ML_input, Highways_file_name = Highways_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"),predictor_sub_folder = predictor_sub_folder)#, this_Date = this_Date) #, study_start_date = study_start_date, study_stop_date = study_stop_date)

  # Load and merge 1 km NLCD Data
  print("start merging 1 km NLCD data")
  ML_input <- merge_NLCD_data.fn(buffer_radius = "1km", ML_input = ML_input, NLCD_file_name = NLCD_1km_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder)
  
  # Load and merge 5 km NLCD Data
  print("start merging 5 km NLCD data")
  ML_input <- merge_NLCD_data.fn(buffer_radius = "5km", ML_input = ML_input, NLCD_file_name = NLCD_5km_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder)

  # Load and merge 10 km NLCD Data
  print("start merging 5 km NLCD data")
  ML_input <- merge_NLCD_data.fn(buffer_radius = "10km", ML_input = ML_input, NLCD_file_name = NLCD_10km_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder)

  # Load and merge NAM Data 
  print("start merging NAM data")
  ML_input <- merge_NAM_data.fn(ML_input = ML_input, NAM_file_name = NAM_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder, this_Date = this_Date)
  
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
#remove_data_2_criteria.fn <- function(df_in, column_of_interest1, upper_limit1 = NA, lower_limit1 = NA, include_upper_limit1 = TRUE, include_lower_limit1 = TRUE, remove_NAs1 = TRUE, column_of_interest2, upper_limit2 = NA, lower_limit2 = NA, include_upper_limit2 = TRUE, include_lower_limit2 = TRUE, remove_NAs2 = TRUE, verbose = TRUE, reason_removed = " ") {
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
  # ML_input_in <- ML_input
  # predictor_data <- Highways_data
  #ML_input_in <- ML_input_step2
  # predictor_data <- GASP_data
  #ML_input_in <- ML_input_step4
  # predictor_data <- NAM_data
  
  # round lat/lon and recognize dates as dates for the two data frames to be joined
  ML_input_in$Latitude <- round(ML_input_in$Latitude, 5)
  ML_input_in$Longitude <- round(ML_input_in$Longitude, 5)
  ML_input_in$Date <- as.Date(ML_input_in$Date,"%Y-%m-%d") # recognize dates as dates
  predictor_data[ , latitude_col_s] <- round(predictor_data[ , latitude_col_s], 5)
  predictor_data[ , longitude_col_s] <- round(predictor_data[ , longitude_col_s], 5)
  predictor_data[ , Dates_col_s] <- as.Date(predictor_data[ , Dates_col_s],"%Y-%m-%d") # recognize dates as dates
  
  # join data sets
  ML_input_out <- join(x = ML_input_in, y = predictor_data, by = c( "Latitude" = latitude_col_s, "Longitude" = longitude_col_s, "Date" = Dates_col_s)) # , "Date" = Dates_col_s
  
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

# Load and merge Highways Data
merge_Highways_data.fn <- function(ML_input, Highways_file_name, ProcessedData.directory,predictor_sub_folder) { # study_start_date, study_stop_date) {  # Load and merge Highways Data
  Highways_data_list <- list()
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  datum_col_s <- "Datum"
  for (file_i in 1:length(Highways_file_name)) { # Load and merge all Highways Data files
    print(file_i)
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

# Load and merge GASP Data
merge_GASP_data.fn <- function(ML_input, GASP_file_name,ProcessedData.directory,predictor_sub_folder, this_Date) { # study_start_date, study_stop_date) {
  for (file_i in 1:length(GASP_file_name)) { # Load and merge all GASP Data files
  
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  Dates_col_s <- "Date"
  
  GASP_data_step <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, GASP_file_name[file_i]),header=TRUE) # load the AQS file
  GASP_data_step<- as.data.frame(GASP_data_step)
  GASP_data_step[ , c(Dates_col_s)] <- as.Date(GASP_data_step[ , c(Dates_col_s)],"%Y-%m-%d") # recognize dates as dates
  # isolate data for this date
  which_this_date <- which(GASP_data_step[ , c(Dates_col_s)] == this_Date)
  if (length(which_this_date) > 0) {
    print(paste("There is GASP data for ",this_Date," in ",GASP_file_name[file_i]))
  # change column names
  GASP_data_step <- replace_column_names.fn(df_in = GASP_data_step, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
  GASP_data_step <- replace_column_names.fn(df_in = GASP_data_step, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
  GASP_data_step <- replace_column_names.fn(df_in = GASP_data_step, old_col_name = "AOD", new_col_name = "GASP_AOD") # replace "Lat" with "Latitude"

  # remove extraneous columns and dates that are not this_Date
  GASP_data <- GASP_data_step[which_this_date,c("Latitude","Longitude","Date","GASP_AOD")]
  summary(GASP_data)
  # join wrapper function
  ML_input <- merge_time_varying_data.fn(ML_input_in = ML_input, predictor_data = GASP_data, latitude_col_s = latitude_col_s, longitude_col_s = longitude_col_s, datum_col_s = datum_col_s,Dates_col_s = Dates_col_s)
  summary(ML_input)
  rm(GASP_data)
  } else {
    print(paste("No GASP data for ",this_Date," in ",GASP_file_name[file_i]))
  }
  } # for (file_i in 1:length(GASP_file_name)) { # Load and merge all GASP Data files
  return(ML_input)
} # end of merge_GASP_data.fn function

# Load and merge MAIAC Data
merge_MAIAC_data.fn <- function(ML_input,MAIAC_file_name,ProcessedData.directory,predictor_sub_folder, this_Date) { #study_start_date, study_stop_date) {
  for (file_i in 1:length(MAIAC_file_name)) { # Load and merge all MAIAC Data files
    
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  Dates_col_s <- "Date"
  
  MAIAC_data_step <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, MAIAC_file_name[file_i]),header=TRUE) # load the AQS file
  MAIAC_data_step <- as.data.frame(MAIAC_data_step)
  #MAIAC_data_step[ , c(Dates_col_s)] <- as.Date(MAIAC_data_step[ , c(Dates_col_s)],"%m/%d/%Y") # recognize dates as dates
  MAIAC_data_step[ , c(Dates_col_s)] <- as.Date(MAIAC_data_step[ , c(Dates_col_s)],"%Y-%m-%d") # recognize dates as dates
  
  # isolate data for this date
  which_this_date <- which(MAIAC_data_step[ , c(Dates_col_s)] == this_Date)
  if (length(which_this_date) > 0) { # is there any data for this date?
    print(paste("There is MAIAC data for ",this_Date," in ",MAIAC_file_name[file_i]))
  # change column names and get rid of repeated header
  print("look at merge_MAIAC_data.fn again when processing part b data")
  #MAIAC_data <- remove_data_outside_range.fn(df_in = MAIAC_data, column_of_interest = Dates_col_s, upper_limit = study_stop_date, lower_limit = study_start_date, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE) 
    
  # change column names
  MAIAC_data_step <- replace_column_names.fn(df_in = MAIAC_data_step, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
  MAIAC_data_step <- replace_column_names.fn(df_in = MAIAC_data_step, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
  
  # remove extraneous columns
  MAIAC_data <- MAIAC_data_step[which_this_date ,c("Latitude","Longitude","Date","MAIAC_AOD")]
  MAIAC_data$Latitude <- as.numeric(as.character(MAIAC_data$Latitude))
  MAIAC_data$Longitude <- as.numeric(as.character(MAIAC_data$Longitude))
  summary(MAIAC_data)
  
  Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Latitude","Longitude","Date"), input_data = MAIAC_data)
  if (length(Check_data)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}
  rm(Check_data)
  if (class(MAIAC_data$Date) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
  
  # join wrapper function
  ML_input <- merge_time_varying_data.fn(ML_input_in = ML_input, predictor_data = MAIAC_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s, datum_col_s = datum_col_s,Dates_col_s = Dates_col_s)
  rm(MAIAC_data)
  } else { # if (length(which_this_date) > 0) { # is there any data for this date?
    print(paste("No MAIAC data for ",this_Date," in ",MAIAC_file_name[file_i]))
  } # if (length(which_this_date) > 0) { # is there any data for this date?
  } # for (file_i in 1:length(MAIAC_file_name)) { # Load and merge all MAIAC Data files
  return(ML_input)
} # end of merge_MAIAC_data.fn function

# Load and merge NED Data
merge_NED_data.fn <- function(ML_input, NED_file_name, ProcessedData.directory,predictor_sub_folder) {
  NED_data_list <- list()
  #NED_data_list <- list('a','b','c')
  for (file_i in 1:length(NED_file_name)) { # Load and merge all NED Data files
 print(file_i)
   NED_data_step <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, NED_file_name[file_i]),header=TRUE) # load the file
  NED_data_step<- as.data.frame(NED_data_step)
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"

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
  NED_data <- NED_data_w_dups[!duplicated(NED_data_w_dups),]
  # join wrapper function
  ML_input <- merge_time_static_data.fn(ML_input_in = ML_input, predictor_data = NED_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s) 
  return(ML_input)
} # end of merge_NED_data.fn function

# Load and merge NLCD Data
merge_NLCD_data.fn <- function(buffer_radius, ML_input, NLCD_file_name,task_counter,ProcessedData.directory,predictor_sub_folder) {
  NLCD_data_list <- list()
  for (file_i in 1:length(NLCD_file_name)) { # Load and merge all NED Data files
  #print(file_i)
  print(paste("Processing NLCD file ",NLCD_file_name[file_i],sep = ""))
  NLCD_data_step <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, NLCD_file_name[file_i]),header=TRUE) # load the file
  NLCD_data_step <- as.data.frame(NLCD_data_step)
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  
  # change column names
  NLCD_data_step <- replace_column_names.fn(df_in = NLCD_data_step, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
  NLCD_data_step <- replace_column_names.fn(df_in = NLCD_data_step, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
  NLCD_data_step <- replace_column_names.fn(df_in = NLCD_data_step, old_col_name = "percent_urban_buffer", new_col_name = paste("NLCD_",buffer_radius,"_percent_urban_buffer",sep = "")) # input buffer radius into variable name

  # remove extraneous columns
  drop_cols <- c("Datum","Easting","Northing","old_lon","old_lat","old_Datum")
  NLCD_data_step <- NLCD_data_step[ , !(names(NLCD_data_step) %in% drop_cols)]
  
  NLCD_data_list[[NLCD_file_name[file_i]]] <- NLCD_data_step
  rm(NLCD_data_step)
  } # for (file_i in 1:length(NLCD_file_name)) { # Load and merge all NED Data files
  NLCD_data_w_dups <- do.call("rbind", NLCD_data_list) # un-list NLCD data
  NLCD_data <- NLCD_data_w_dups[!duplicated(NLCD_data_w_dups),] # de-duplicate NLCD data
  # join wrapper function
  ML_input <- merge_time_static_data.fn(ML_input_in = ML_input, predictor_data = NLCD_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s) 
  return(ML_input)
} # end of merge_NLCD_data.fn function

# Load and merge NAM Data
merge_NAM_data.fn <- function(ML_input, NAM_file_name,task_counter,ProcessedData.directory,predictor_sub_folder,this_Date) {#, study_start_date, study_stop_date) {
  NAM_data_list <- list()
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  Dates_col_s <- "Date"
  for (file_i in 1:length(NAM_file_name)) { # Load and merge all NAM Data files
    print(paste("Processing NAM file ",NAM_file_name[file_i],sep = ""))
    NAM_data_step <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, NAM_file_name[file_i]),header=TRUE) # load data file
    NAM_data_step<- as.data.frame(NAM_data_step)
    NAM_data_step[ , c(Dates_col_s)] <- as.Date(NAM_data_step[ , c(Dates_col_s)],"%Y-%m-%d") # recognize dates as dates
    
    # change column names
    NAM_data_step <- replace_column_names.fn(df_in = NAM_data_step, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
    NAM_data_step <- replace_column_names.fn(df_in = NAM_data_step, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
    
    # remove extraneous columns
    drop_cols <- c("Datum","Easting","Northing","old_lon","old_lat","old_Datum")
    NAM_data_step <- NAM_data_step[ , !(names(NAM_data_step) %in% drop_cols)]
    
    # isolate data for this date
    which_this_date <- which(NAM_data_step[ , c(Dates_col_s)] == this_Date)
    if (length(which_this_date) > 0) { # is there data for this date in this file?
      #stop("finish merge_NAM_data.fn in merging_data_functions.R")
      print(paste("There is NAM data for ",this_Date," in ",NAM_file_name[file_i]))
      NAM_data_date <- NAM_data_step[which_this_date, ]
   
  
    
    } else {
      print(paste("No NAM data for",this_Date,"in",NAM_file_name[file_i]))
      #NAM_data_step <- NAM_data_step[1, ] # just grab first row as a place holder since nothing matches
      NAM_data_date <- NAM_data_step[1, ] # just grab first row as a place holder since nothing matches
    } # if (length(which_this_date) > 0) { # is there data for this date in this file?
    
    ## remove extraneous columns
    #drop_cols <- c("Datum", "Easting", "Northing")
    #NAM_data_step <- NAM_data_step[ , !(names(NAM_data_step) %in% drop_cols)]
    
    #NAM_data_list[[NAM_file_name[file_i]]] <- NAM_data_step # input data into list
    NAM_data_list[[NAM_file_name[file_i]]] <- NAM_data_date # input data into list
    rm(NAM_data_step)
  } # for (file_i in 1:length(Highways_file_name)) { # Load and merge all NAM Data files
    NAM_data_w_dups <- do.call("rbind", NAM_data_list) # unlist data from various files
    NAM_data <- NAM_data_w_dups[!duplicated(NAM_data_w_dups),] # de-duplicate rows of data
    
    #if (length(NAM_data)>0) { # is there is data to match?
    # join wrapper function
    ML_input <- merge_time_varying_data.fn(ML_input_in = ML_input, predictor_data = NAM_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s, datum_col_s = datum_col_s,Dates_col_s = Dates_col_s)
    #} else {#if (length(NAM_data)>0) { # is there is data to match?
    #stop("add in code to put in space-holder columns")
    #} #if (length(NAM_data)>0) { # is there is data to match?
    rm(NAM_data,NAM_data_list,NAM_data_w_dups)
  
  return(ML_input)
} # end of merge_NAM_data.fn function

'%!in%' <- function(x,y)!('%in%'(x,y)) # directly from https://stackoverflow.com/questions/5831794/opposite-of-in
