# Functions related to merging data

# merge predictor variables together
merge_predictors.fn <- function(predictand_data,predictand_col,latitude_col_t,longitude_col_t,datum_col_t, Easting_col_t, Northing_col_t,Dates_col_t, output_file_name, output_sub_folder, study_start_date, study_stop_date) {
  print("top of merge_predictors.fn")
  # break file down by dates
  #study_start_date = study_stop_date, study_stop_date = study_stop_date
  # #### Remove data outside the study period (2008-2014) ####
  predictand_data[ ,Dates_col_t] <- as.Date(predictand_data[ , Dates_col_t],"%Y-%m-%d") # recognize dates as dates
  #predictand_data_full[ ,Dates_col_t] <- as.Date(predictand_data_full[ , Dates_col_t],"%Y-%m-%d") # recognize dates as dates
  #predictand_data <- remove_data_outside_range.fn(df_in = predictand_data_full, column_of_interest = Dates_col_t, upper_limit = study_stop_date, lower_limit = study_start_date, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE) 
  
  # Create data frame
  if (is.na(predictand_col)) { # is this data set is for predicting pm2.5 or training? 
    new_header <- c("Date","Latitude","Longitude","Datum","Easting","Northing")  
  } else {
    new_header <- c(predictand_col,"Date","Latitude","Longitude","Datum","Easting","Northing")
  } # if (is.na(predictant_col)) { # is this data set is for predicting pm2.5 or training? 
  ML_input <- data.frame(matrix(NA,nrow=dim(predictand_data)[1],ncol=length(new_header))) # create data frame for input_mat1
  names(ML_input) <- new_header # assign the header
  
  # fill in the predictand data
  if (is.na(predictand_col) == FALSE) { # is this data set is for predicting pm2.5 or training? 
    ML_input$PM2.5_Obs <- predictand_data[ , predictand_col] # fill in PM2.5 observations
  } # if (is.na(predictant_col)) { # is this data set is for predicting pm2.5 or training?
  ML_input[ , c("Date","Latitude","Longitude","Datum","Easting","Northing")] <- predictand_data[ , c(Dates_col_t,latitude_col_t,longitude_col_t,datum_col_t,Easting_col_t,Northing_col_t)] # fill in dates/locations
  ML_input<- as.data.frame(ML_input) # recognize data frame as a data frame
  ML_input$Date <- as.Date(ML_input$Date,"%Y-%m-%d") # recognize dates as dates: 'Date_Local' 
  
  study_start_date <- min(ML_input$Date)
  study_stop_date <- max(ML_input$Date)
  
  # Load and merge GASP Data
  print("start merging GASP data")
  ML_input <- merge_GASP_data.fn(ML_input = ML_input, GASP_file_name = GASP_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder, study_start_date = study_start_date, study_stop_date = study_stop_date)
  summary(ML_input)
  # Load and merge MAIAC Data
  print("start merging MAIAC data")
  ML_input <- merge_MAIAC_data.fn(ML_input = ML_input, MAIAC_file_name = MAIAC_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder, study_start_date = study_start_date, study_stop_date = study_stop_date)
  summary(ML_input)
  # Load and merge NED Data
  print("start merging NED data")
  ML_input <- merge_NED_data.fn(ML_input = ML_input, NED_file_name = NED_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder)
  
  # Load and merge NLCD Data
  print("start merging NLCD data")
  ML_input <- merge_NLCD_data.fn(ML_input = ML_input, NLCD_file_name = NLCD_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder)

  # Load and merge NAM Data 
  print("start merging NAM data")
  ML_input <- merge_NAM_data.fn(ML_input = ML_input, NAM_file_name = NAM_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), predictor_sub_folder = predictor_sub_folder, study_start_date = study_start_date, study_stop_date = study_stop_date)
 
  # Load and merge Highways Data  
  print("start merging Highways data")
  ML_input <- merge_Highways_data.fn(ML_input = ML_input, Highways_file_name = Highways_file_name, ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"),predictor_sub_folder = predictor_sub_folder, study_start_date = study_start_date, study_stop_date = study_stop_date)
  
  # write data to file
  print("start writing merged data frame to file")
  write.csv(ML_input,file = file.path(ProcessedData.directory = define_file_paths.fn("ProcessedData.directory"), output_sub_folder = output_sub_folder, paste(output_file_name,".csv",sep = "")), row.names = FALSE)
  
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
merge_Highways_data.fn <- function(ML_input, Highways_file_name, ProcessedData.directory,predictor_sub_folder, study_start_date, study_stop_date) {  # Load and merge Highways Data
  for (file_i in 1:length(Highways_file_name)) { # Load and merge all Highways Data files
    this_Highways_file <- Highways_file_name[file_i] # get the file name for this Highways file
    print(this_Highways_file)
    
    #Highway_cols <- c("A_100","C_100","Both_100","A_250","C_250","Both_250","A_500","C_500","Both_500","A_1000","C_1000","Both_1000")
    latitude_col_s <- "Latitude"
    longitude_col_s <- "Longitude"
    datum_col_s <- "Datum"
    Dates_col_s <- "Date"
    
    Highways_data <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, this_Highways_file),header=TRUE) # load the file
    Highways_data<- as.data.frame(Highways_data)
    #Highways_data[ , c(Dates_col_s)] <- as.Date(Highways_data[ , c(Dates_col_s)],"%Y-%m-%d") # recognize dates as dates
    Highways_data[ , c(Dates_col_s)] <- as.Date(Highways_data[ , c(Dates_col_s)],"%m/%d/%Y") # recognize dates as dates
    
    Highways_data <- remove_data_outside_range.fn(df_in = Highways_data, column_of_interest = Dates_col_s, 
                                                  upper_limit = study_stop_date, lower_limit = study_start_date, 
                                                  include_upper_limit = TRUE, include_lower_limit = TRUE, 
                                                  remove_NAs = TRUE, verbose = TRUE) 
    
    # change column names
    Highways_data <- replace_column_names.fn(df_in = Highways_data, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
    Highways_data <- replace_column_names.fn(df_in = Highways_data, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
    #if (this_Highways_file == "Highways_part_b.csv") {
    #  colnames(Highways_data)[6] <- "Latitude"
    #  colnames(Highways_data)[5] <- "Longitude"
    #}
    
    #print("Should remove extraneous columns")
    # remove extraneous columns
    drop_cols <- c("X","Datum")
    #if ("X" %in% colnames(Highways_data)) {Highways_data <- Highways_data[ , !(names(Highways_data) %in% )]}
    Highways_data <- Highways_data[ , !(names(Highways_data) %in% drop_cols)]
    
    # join wrapper function
    ML_input <- merge_time_varying_data.fn(ML_input_in = ML_input, predictor_data = Highways_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s, datum_col_s = datum_col_s,Dates_col_s = Dates_col_s)
    rm(Highways_data) # clear variable
  } # for (file_i in 1:length(Highways_file_name)) { # Load and merge all Highways Data files
  return(ML_input)
} # end of merge_Highways_data.fn function

# Load and merge GASP Data
merge_GASP_data.fn <- function(ML_input, GASP_file_name,ProcessedData.directory,predictor_sub_folder, study_start_date, study_stop_date) {
  for (file_i in 1:length(GASP_file_name)) { # Load and merge all GASP Data files
  
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  Dates_col_s <- "Date"
  
  GASP_data <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, GASP_file_name[file_i]),header=TRUE) # load the AQS file
  GASP_data<- as.data.frame(GASP_data)
  GASP_data[ , c(Dates_col_s)] <- as.Date(GASP_data[ , c(Dates_col_s)],"%Y-%m-%d") # recognize dates as dates

  GASP_data <- remove_data_outside_range.fn(df_in = GASP_data, column_of_interest = Dates_col_s, upper_limit = study_stop_date, lower_limit = study_start_date, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE) 

  # change column names
  GASP_data <- replace_column_names.fn(df_in = GASP_data, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
  GASP_data <- replace_column_names.fn(df_in = GASP_data, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
  GASP_data <- replace_column_names.fn(df_in = GASP_data, old_col_name = "AOD", new_col_name = "GASP_AOD") # replace "Lat" with "Latitude"

  # remove extraneous columns
  drop_cols <- c("X","Datum")
  GASP_data <- GASP_data[ , !(names(GASP_data) %in% drop_cols)]
  summary(GASP_data)
  # join wrapper function
  ML_input <- merge_time_varying_data.fn(ML_input_in = ML_input, predictor_data = GASP_data, latitude_col_s = latitude_col_s, longitude_col_s = longitude_col_s, datum_col_s = datum_col_s,Dates_col_s = Dates_col_s)
  #ML_input <- merge_time_varying_data.fn(ML_input_in = ML_input, predictor_data = GASP_data, latitude_col_s = latitude_col_s, longitude_col_s = longitude_col_s, datum_col_s = datum_col_s,Dates_col_s = Dates_col_s)
  summary(ML_input)
  rm(GASP_data)
  } # for (file_i in 1:length(GASP_file_name)) { # Load and merge all GASP Data files
  return(ML_input)
} # end of merge_GASP_data.fn function

# Load and merge MAIAC Data
merge_MAIAC_data.fn <- function(ML_input,MAIAC_file_name,ProcessedData.directory,predictor_sub_folder, study_start_date, study_stop_date) {
  for (file_i in 1:length(MAIAC_file_name)) { # Load and merge all MAIAC Data files
    
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  Dates_col_s <- "Date"
  
  MAIAC_data <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, MAIAC_file_name[file_i]),header=TRUE) # load the AQS file
  MAIAC_data <- as.data.frame(MAIAC_data)
  MAIAC_data[ , c(Dates_col_s)] <- as.Date(MAIAC_data[ , c(Dates_col_s)],"%m/%d/%Y") # recognize dates as dates
  
  # change column names and get rid of repeated header
  print("look at merge_MAIAC_data.fn again when processing part b data")
  #if (MAIAC_file_name[task_counter] == "MAIAC_extracted_part_b.csv") {
  #  colnames(MAIAC_data)[1] <- "Latitude"
  #  colnames(MAIAC_data)[2] <- "Longitude"
  #  MAIAC_data <- MAIAC_data[4:dim(MAIAC_data)[1], ]
  #  MAIAC_data<- as.data.frame(MAIAC_data)
  #  MAIAC_data[ , c(Dates_col_s)] <- as.Date(MAIAC_data[ , c(Dates_col_s)],"%Y-%m-%d") # recognize dates as dates
  #} else { # if (GASP_file_name[task_counter] == "GASP_extracted_part_b.csv") {
  #  MAIAC_data<- as.data.frame(MAIAC_data)
  #  MAIAC_data[ , c(Dates_col_s)] <- as.Date(MAIAC_data[ , c(Dates_col_s)],"%m/%d/%Y") # recognize dates as dates
  #} # if (MAIAC_file_name[task_counter] == "MAIAC_extracted_part_b.csv") {
  #MAIAC_data[ ,latitude_col_s] <- as.numeric(as.character(MAIAC_data[ ,latitude_col_s]))
  #MAIAC_data[ ,longitude_col_s] <- as.numeric(as.character(MAIAC_data[ ,longitude_col_s]))
  
  MAIAC_data <- remove_data_outside_range.fn(df_in = MAIAC_data, column_of_interest = Dates_col_s, upper_limit = study_stop_date, lower_limit = study_start_date, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE) 
    
  # change column names
  MAIAC_data <- replace_column_names.fn(df_in = MAIAC_data, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
  MAIAC_data <- replace_column_names.fn(df_in = MAIAC_data, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"

  # join wrapper function
  ML_input <- merge_time_varying_data.fn(ML_input_in = ML_input, predictor_data = MAIAC_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s, datum_col_s = datum_col_s,Dates_col_s = Dates_col_s)
  rm(MAIAC_data)
  } # for (file_i in 1:length(MAIAC_file_name)) { # Load and merge all MAIAC Data files
  return(ML_input)
} # end of merge_MAIAC_data.fn function

# Load and merge NED Data
merge_NED_data.fn <- function(ML_input, NED_file_name, ProcessedData.directory,predictor_sub_folder) {
  for (file_i in 1:length(NED_file_name)) { # Load and merge all NED Data files
  NED_data <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, NED_file_name[file_i]),header=TRUE) # load the file
  NED_data<- as.data.frame(NED_data)
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  #Dates_col_s <- "Date"
  #colnames(NED_data)[1] <- "Latitude"
  #colnames(NED_data)[2] <- "Longitude"

  # change column names
  NED_data <- replace_column_names.fn(df_in = NED_data, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
  NED_data <- replace_column_names.fn(df_in = NED_data, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
  
  # remove extraneous columns
  drop_cols <- c("Datum","Easting","Northing")
  NED_data <- NED_data[ , !(names(NED_data) %in% drop_cols)]
  
  # join wrapper function
  ML_input <- merge_time_static_data.fn(ML_input_in = ML_input, predictor_data = NED_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s) 
  rm(NED_data)
  } # for (file_i in 1:length(NED_file_name)) { # Load and merge all NED Data files
  return(ML_input)
} # end of merge_NED_data.fn function

# Load and merge NLCD Data
merge_NLCD_data.fn <- function(ML_input, NLCD_file_name,task_counter,ProcessedData.directory,predictor_sub_folder) {
  for (file_i in 1:length(NLCD_file_name)) { # Load and merge all NED Data files
  NLCD_data <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, NLCD_file_name[file_i]),header=TRUE) # load the file
  
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  #Dates_col_s <- "Date"
  NLCD_data <- as.data.frame(NLCD_data)
  
  # change column names
  NLCD_data <- replace_column_names.fn(df_in = NLCD_data, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
  NLCD_data <- replace_column_names.fn(df_in = NLCD_data, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
  
  # the variable name is the same for all of the different buffers, so a new variable name needs to be createde
  new_var_name_step <- gsub(pattern = "_extract.csv", replacement = "", x = NLCD_file_name[file_i], ignore.case = FALSE, perl = FALSE,
       fixed = FALSE, useBytes = FALSE)
  new_var_name_step2 <- gsub(pattern = "nlcd", replacement = "NLCD", x = new_var_name_step)
  new_var_name_step3 <- paste(new_var_name_step2,"_Fraction_Urban", sep = "")
  NLCD_data <- replace_column_names.fn(df_in = NLCD_data, old_col_name = "percent_urban_buffer", new_col_name = new_var_name_step3) # replace variable name
  rm(new_var_name_step, new_var_name_step2, new_var_name_step3)
  
  # remove extraneous columns
  drop_cols <- c("Datum","Easting","Northing")
  NLCD_data <- NLCD_data[ , !(names(NLCD_data) %in% drop_cols)]
  
  # join wrapper function
  ML_input <- merge_time_static_data.fn(ML_input_in = ML_input, predictor_data = NLCD_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s) 
  rm(NLCD_data)
  } # for (file_i in 1:length(NLCD_file_name)) { # Load and merge all NED Data files
  return(ML_input)
} # end of merge_NLCD_data.fn function

# Load and merge NAM Data
merge_NAM_data.fn <- function(ML_input, NAM_file_name,task_counter,ProcessedData.directory,predictor_sub_folder, study_start_date, study_stop_date) {
  for (file_i in 1:length(NAM_file_name)) { # Load and merge all Highways Data files
  NAM_data <- read.csv(file.path(ProcessedData.directory,predictor_sub_folder, NAM_file_name[file_i]),header=TRUE) # load the AQS file
  
  latitude_col_s <- "Latitude"
  longitude_col_s <- "Longitude"
  Dates_col_s <- "Date"
  NAM_data<- as.data.frame(NAM_data)
  NAM_data[ , c(Dates_col_s)] <- as.Date(NAM_data[ , c(Dates_col_s)],"%Y-%m-%d") # recognize dates as dates
  
  NAM_data <- remove_data_outside_range.fn(df_in = NAM_data, column_of_interest = Dates_col_s, upper_limit = study_stop_date, lower_limit = study_start_date, include_upper_limit = TRUE, include_lower_limit = TRUE, remove_NAs = TRUE, verbose = TRUE) 
  
  # change column names
  NAM_data <- replace_column_names.fn(df_in = NAM_data, old_col_name = "Lat", new_col_name = "Latitude") # replace "Lat" with "Latitude"
  NAM_data <- replace_column_names.fn(df_in = NAM_data, old_col_name = "Lon", new_col_name = "Longitude") # replace "Lat" with "Latitude"
  
  # remove extraneous columns
  drop_cols <- c("Datum", "Easting", "Northing")
  NAM_data <- NAM_data[ , !(names(NAM_data) %in% drop_cols)]
  
  # join wrapper function
  ML_input <- merge_time_varying_data.fn(ML_input_in = ML_input, predictor_data = NAM_data,latitude_col_s = latitude_col_s,longitude_col_s = longitude_col_s, datum_col_s = datum_col_s,Dates_col_s = Dates_col_s)
  rm(NAM_data)
  } # for (file_i in 1:length(Highways_file_name)) { # Load and merge all Highways Data files
  return(ML_input)
} # end of merge_NAM_data.fn function
