ML_merge_predictors_parallal_wrapper.fn <- function(task_counter){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  
  if (task_counter == 1) {
    print("starting task 1")
    # Load Dates/Locations for predictions (part c) - County Centroids
    this_source_file <- files_to_merge_to[task_counter] #"CountyGeometricCentroids_Locations_Dates_part_c_2008-01-01to2008-12-31" 
    this_source_path <- file.path(define_file_paths.fn("ProcessedData.directory"),file_paths_to_merge_to[task_counter])
    #predictand_data_full <- read.csv(file.path(this_source_path,paste(this_source_file,".csv",sep = "")),header=TRUE) # load the AQS file
    predictand_data <- read.csv(file.path(this_source_path,paste(this_source_file,".csv",sep = "")),header=TRUE) # load the file
    print("dim(predictand_data)")
    print(dim(predictand_data))
    predictand_col <- NA
    latitude_col_t <- "Lat"
    longitude_col_t <- "Lon"
    datum_col_t <- "Datum"
    Dates_col_t <- "Date"
    Easting_col_t <- "Easting"
    Northing_col_t <- "Northing"
    
    ML_input_file_name_output <- paste("ML_predictors_",this_source_file,sep = "")
    #output_file_name <- ML_input_file_name_output
    #output_sub_folder <- "ML_input_files"
    output_sub_folder <- "ML_prediction_inputs"
    
    print("starting merge_predictors.fn")
    print("dim(predictand_data)")
    print(dim(predictand_data))
    #Merged_input_file <- merge_predictors.fn(predictand_data = predictand_data, predictand_col = predictand_col, latitude_col_t = latitude_col_t, longitude_col_t = longitude_col_t, datum_col_t = datum_col_t, Easting_col_t = Easting_col_t, Northing_col_t = Northing_col_t, Dates_col_t = Dates_col_t, output_file_name = ML_input_file_name_output, output_sub_folder = output_sub_folder, task_counter = task_counter)
    Merged_predictor_file <- merge_predictors.fn(predictand_data = predictand_data, predictand_col = predictand_col, latitude_col_t = latitude_col_t, longitude_col_t = longitude_col_t, datum_col_t = datum_col_t, Easting_col_t = Easting_col_t, Northing_col_t = Northing_col_t, Dates_col_t = Dates_col_t, output_file_name = ML_input_file_name_output, output_sub_folder = output_sub_folder)
    return(Merged_predictor_file) # output from function
    
  } # if (task_counter == 1)
} # end of ML_merge_predictors_parallel_wrapper.fn function
