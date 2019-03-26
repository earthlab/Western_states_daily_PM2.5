ML_merge_predictors_parallal_wrapper.fn <- function(data_set_counter){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  # data_set_counter <- 1
  
  # Load file to be merged to
  this_source_file <- files_to_merge_to[data_set_counter] 
  this_source_path <- file.path(ProcessedData.directory,file_paths_to_merge_to[data_set_counter])
  Source_Data <- read.csv(file.path(this_source_path,paste(this_source_file,".csv",sep = "")),header=TRUE) # load the AQS file
  Source_Data <- input_mat_change_data_classes.fn(Source_Data)
  rm(this_source_file,this_source_path)
  
  # define column names
  predictand_col <- "PM2.5_Obs"
  latitude_col_t <- "Lat"
  longitude_col_t <- "Lon"
  datum_col_t <- "NewDatum"
  Dates_col_t <- "Date_Local"
  
    
    # define study period
    #study_start_date <- as.Date(define_study_constants.fn("start_date")) #as.Date("20080101",format="%Y%m%d") # first date in study period
    #study_stop_date  <- as.Date(define_study_constants.fn("end_date")) #as.Date("20141231",format="%Y%m%d") # last date in study period
    #Date_list <- c(study_start_date:study_stop_date)
    Date_list <- sort(unique(Source_Data$Date_Local))  
  
    this_task_vars <- c("Source_Data", "predictand_col", "latitude_col_t","longitude_col_t","datum_col_t","Dates_col_t")#,
                       # "study_start_date","study_stop_date")
    
    n_cores <- detectCores() - 1 # Calculate the number of cores
    print(paste(n_cores,"cores available for parallel processing",sep = " "))
    this_cluster <- makeCluster(n_cores) # Initiate cluster
    # export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
    clusterExport(cl = this_cluster, varlist = c("processed_data_version",directories_vector,
                                                 Plotting_and_LaTex_fn_list, ML_processing_fn_list, input_mat_functions,
                                                 all_files_list,this_task_vars), envir = .GlobalEnv)
    
    # send necessary libraries to each parallel worker
    clusterEvalQ(cl = this_cluster, library(plyr)) # copy this line and call function again if another library is needed
    
    #Merged_input_file <- merge_predictors.fn(predictand_data_full = Source_Data, predictand_col = predictand_col, 
    ##                                         latitude_col_t = latitude_col_t, longitude_col_t = longitude_col_t, 
    #                                         datum_col_t = datum_col_t,  
    #                                         Dates_col_t = Dates_col_t, output_file_name = ML_input_file_name_output, output_sub_folder = output_sub_folder, 
    #                                         task_counter = task_counter, study_start_date = study_start_date, study_stop_date = study_stop_date)
    
    par_output <- parLapply(this_cluster, X = Date_list[1:15], fun = merge_predictors.fn)#,
    
    #input_mat <- par_output[[1]]
    Merged_input_file <- do.call("rbind", par_output) #concatinate the output from each iteration
    
    # define path and file name for output
    ML_input_file_name_output <- paste("ML_input_",this_source_file,sep = "")
    output_sub_folder <- "ML_input_files"
    write.csv(Merged_input_file,file = file.path(ProcessedData.directory,output_sub_folder,paste(ML_input_file_name_output,'.csv',sep = "")),row.names = FALSE) # Write csv file

    # End use of parallel computing #
    stopCluster(this_cluster)
    rm(this_cluster, n_cores)
    
    return(Merged_input_file) # output from function
} # end of ML_merge_predictors_parallel_wrapper.fn function
