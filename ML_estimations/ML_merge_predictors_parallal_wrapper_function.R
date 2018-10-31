ML_merge_predictors_parallal_wrapper.fn <- function(task_counter){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  
  if (task_counter == 1) {
    # Load PM2.5 data (part b)
    
    this_source_file <- "PM25_Step3_part_b_Projected"
    print("Still need to replace data file name with de-duplicated file")
    this_source_path <- file.path(ProcessedData.directory,"PM25_data_part_b")
    predictand_data <- read.csv(file.path(this_source_path,paste(this_source_file,".csv",sep = "")),header=TRUE) # load the AQS file
    
    predictand_col <- "PM2.5_Obs"
    latitude_col_t <- "Lat"
    longitude_col_t <- "Lon"
    datum_col_t <- "NewDatum"
    Easting_col_t <- "Easting"
    Northing_col_t <- "Northing"
    Dates_col_t <- "Date_Local"
    
    
    
  } else if (task_counter == 2) {
    # Load Dates/Locations for predictions (part c)
      
  } # if (task_counter == 1)
} # end of ML_merge_predictors_parallel_wrapper.fn function
