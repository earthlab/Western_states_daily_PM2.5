ML_merge_predictors_parallal_wrapper.fn <- function(data_set_counter,Merging_fn_list,input_mat_functions){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  # data_set_counter <- 1
  
  # Load file to be merged to
  this_source_file <- files_to_merge_to[data_set_counter] 
  this_source_path <- file.path(ProcessedData.directory,file_paths_to_merge_to[data_set_counter])
  Source_Data <- read.csv(file.path(this_source_path,paste(this_source_file,".csv",sep = "")),header=TRUE) # load the AQS file
  Source_Data <- input_mat_change_data_classes.fn(Source_Data)
  rm(this_source_file,this_source_path)
  print("(line 10) Dim Source_Data:")
  print(dim(Source_Data))
  
  # define column names
  predictand_col <- "PM2.5_Obs"
  latitude_col_t <- "Lat"
  longitude_col_t <- "Lon"
  datum_col_t <- "NewDatum"
  Dates_col_t <- "Date_Local"
  
    # define study period
    Date_list <- sort(unique(Source_Data$Date_Local))  
    n_dates <- length(Date_list)
  print(paste("Date_list has ",n_dates," days in it.",sep = ""))
    
    #this_task_vars <- c("Date_list")#c("Source_Data", "predictand_col", "latitude_col_t","longitude_col_t","datum_col_t","Dates_col_t","Date_list")#,
                       # "study_start_date","study_stop_date")
    
    n_cores <- detectCores() - 1 # Calculate the number of cores
    print(paste(n_cores,"cores available for parallel processing",sep = " "))
    this_cluster <- makeCluster(n_cores) # Initiate cluster
    # export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
    #clusterExport(cl = this_cluster, varlist = c("processed_data_version",directories_vector, Merging_fn_list,
    #                                            input_mat_functions,
    #                                             all_files_list,this_task_vars), envir = .GlobalEnv) # Plotting_and_LaTex_fn_list,  ML_processing_fn_list, 
    #clusterExport(cl = this_cluster, varlist = c("processed_data_version","Date_list",directories_vector, Merging_fn_list,
    #                                             input_mat_functions,
    #                                             all_files_list), envir = .GlobalEnv) # Plotting_and_LaTex_fn_list,  ML_processing_fn_list, 
    #clusterExport(cl = this_cluster, varlist = c("Date_list"), envir = .GlobalEnv) # Plotting_and_LaTex_fn_list,  ML_processing_fn_list, 
    #clusterExport(cl = this_cluster, varlist = c(Date_list), envir = .GlobalEnv) # Plotting_and_LaTex_fn_list,  ML_processing_fn_list, 
    print("start running clusterExport command")
    #clusterExport(cl = this_cluster, varlist = c(Date_list), envir = .GlobalEnv) # export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
   # print(clusterExport(cl = this_cluster, varlist = c("Date_list"), envir = .GlobalEnv))
    clusterExport(cl = this_cluster, varlist = c("Date_list"), envir = environment())
    print("finished running clusterExport command")
    
    # # put back into clusterExport: ,this_task_vars
    # 
    # # send necessary libraries to each parallel worker
    # clusterEvalQ(cl = this_cluster, library(plyr)) # copy this line and call function again if another library is needed
    # 
    # n_dates <- 30 # just for testing
    # par_output <- parLapply(this_cluster, X = 1:n_dates, fun = merge_predictors.fn)#,
    # 
    # #input_mat <- par_output[[1]]
    # Merged_input_file <- do.call("rbind", par_output) #concatinate the output from each iteration
    # 
    # # define path and file name for output
    # ML_input_file_name_output <- paste("ML_input_",this_source_file,sep = "")
    # output_sub_folder <- "ML_input_files"
    # write.csv(Merged_input_file,file = file.path(ProcessedData.directory,output_sub_folder,paste(ML_input_file_name_output,'.csv',sep = "")),row.names = FALSE) # Write csv file
    # 
    # # End use of parallel computing #
    stopCluster(this_cluster)
    rm(this_cluster, n_cores)
    # 
    # return(Merged_input_file) # output from function
    return(Date_list)
} # end of ML_merge_predictors_parallel_wrapper.fn function

## serial version of code
#par_output <- list()
#n_dates <- 3 # just for testing
#for (X in 1:n_dates) {
#  this_Date <- as.Date(Date_list[X])
#  print(this_Date)
#  par_output[[X]] <-  merge_predictors.fn(this_Date)
#}
#Merged_input_file <- do.call("rbind", par_output) #concatinate the output from each iteration
