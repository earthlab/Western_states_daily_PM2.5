ML_merge_predictors_parallal_wrapper.fn <- function(task_counter){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  
  if (task_counter == 1) {
    # Load PM2.5 data (part d)
    
    predictand_col <- "PM2.5_Obs"
    latitude_col_t <- "Lat"
    longitude_col_t <- "Lon"
    datum_col_t <- "NewDatum"
    Easting_col_t <- "Easting"
    Northing_col_t <- "Northing"
    Dates_col_t <- "Date_Local"

    # Load Dates/Locations for predictions (part c)
    
    this_source_file <- files_to_merge_to[task_counter] 
    print(this_source_file)
    this_source_path <- file.path(ProcessedData.directory,file_paths_to_merge_to[task_counter])
    predictand_data_full <- read.csv(file.path(this_source_path,paste(this_source_file,".csv",sep = "")),header=TRUE) # load the AQS file
    
    ML_input_file_name_output <- paste("ML_input_",this_source_file,sep = "")
    #output_file_name <- ML_input_file_name_output
    output_sub_folder <- "ML_input_files"
    
    study_start_date <- as.Date("20080101",format="%Y%m%d") # first date in study period
    study_stop_date  <- as.Date("20141231",format="%Y%m%d") # last date in study period
    
    Merged_input_file <- merge_predictors.fn(predictand_data_full = predictand_data_full, predictand_col = predictand_col, 
                                             latitude_col_t = latitude_col_t, longitude_col_t = longitude_col_t, 
                                             datum_col_t = datum_col_t, Easting_col_t = Easting_col_t, Northing_col_t = Northing_col_t, 
                                             Dates_col_t = Dates_col_t, output_file_name = ML_input_file_name_output, output_sub_folder = output_sub_folder, 
                                             task_counter = task_counter, study_start_date = study_start_date, study_stop_date = study_stop_date)
    
    return(Merged_input_file) # output from function
    
  } else if (task_counter == 2) {
    # Load Dates/Locations for predictions (part c)
     
    this_source_file <- files_to_merge_to[task_counter] #"CountyGeometricCentroids_Locations_Dates_part_c_2008-01-01to2008-12-31" 
    this_source_path <- file.path(ProcessedData.directory,file_paths_to_merge_to[task_counter])
    predictand_data_full <- read.csv(file.path(this_source_path,paste(this_source_file,".csv",sep = "")),header=TRUE) # load the AQS file
    
    predictand_col <- NA
    latitude_col_t <- "Lat"
    longitude_col_t <- "Lon"
    datum_col_t <- "Datum"
    Dates_col_t <- "Date"
    Easting_col_t <- "Easting"
    Northing_col_t <- "Northing"
    
    ML_input_file_name_output <- paste("ML_input_",this_source_file,sep = "")
    #output_file_name <- ML_input_file_name_output
    output_sub_folder <- "ML_input_files"
    
    Merged_input_file <- merge_predictors.fn(predictand_data = predictand_data, predictand_col = predictand_col, latitude_col_t = latitude_col_t, longitude_col_t = longitude_col_t, datum_col_t = datum_col_t, Easting_col_t = Easting_col_t, Northing_col_t = Northing_col_t, Dates_col_t = Dates_col_t, output_file_name = ML_input_file_name_output, output_sub_folder = output_sub_folder, task_counter = task_counter)

    return(Merged_input_file) # output from function
    
  } # if (task_counter == 1)
} # end of ML_merge_predictors_parallel_wrapper.fn function
