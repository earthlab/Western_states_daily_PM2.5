ML_merge_predictors_parallal_wrapper.fn <- function(data_set_counter,General_fn_list,Merging_fn_list,directories_vector,input_mat_functions){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  # data_set_counter <- 1
  
  # Load file to be merged to
  this_source_file <- files_to_merge_to[data_set_counter] 
  print(this_source_file)
  this_source_path <- file.path(ProcessedData.directory,file_paths_to_merge_to[data_set_counter])
  #Source_Data_step <- read.csv(file.path(this_source_path,paste(this_source_file,".csv",sep = "")),header=TRUE) # load the AQS file
  #Source_Data_step <- input_mat_change_data_classes.fn(Source_Data_step)
  #Source_Data <- Source_Data_step[!duplicated(Source_Data_step), ]
  Source_Data <- read.csv(file.path(this_source_path,paste(this_source_file,".csv",sep = "")),header=TRUE) # load the AQS file
  
  predictand_col <- "PM2.5_Obs"
  if (predictand_col %in% colnames(Source_Data)) {
    Source_Data <- input_mat_change_data_classes.fn(Source_Data)
    Dates_col_t <- "Date_Local"
    datum_col_t <- "NewDatum"
  } else {
    Source_Data$Date <- as.Date(Source_Data$Date,"%Y-%m-%d") # recognize dates as dates
    Dates_col_t <- "Date"
    datum_col_t <- "Datum"
  }
  
  rm(this_source_path)
  #print("(line 10) Dim Source_Data:")
  #print(dim(Source_Data))
  
  # define column names
  
  latitude_col_t <- "Lat"
  longitude_col_t <- "Lon"
  
  # define study period
  #Date_list <- sort(unique(Source_Data$Date_Local))  
  Date_list <- sort(unique(Source_Data[ ,c(Dates_col_t)]))  
  n_dates <- length(Date_list)
  print(paste("Date_list has ",n_dates," days in it.",sep = ""))
    
  this_task_vars <- c("Source_Data", "predictand_col", "latitude_col_t","longitude_col_t","datum_col_t","Dates_col_t","Date_list")
    
  # prep for running in parallel
    n_cores <- detectCores() - 1 # Calculate the number of cores
    print(paste(n_cores,"cores available for parallel processing",sep = " "))
    this_cluster <- makeCluster(n_cores) # Initiate cluster
    print("start running clusterExport command")
    clusterExport(cl = this_cluster, varlist = c(this_task_vars,General_fn_list,Merging_fn_list,all_files_list,directories_vector,input_mat_functions), envir = environment()) # export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
    # "processed_data_version",directories_vector, Merging_fn_list,input_mat_functions,all_files_list,this_task_vars,Plotting_and_LaTex_fn_list,  ML_processing_fn_list, 
    print("finished running clusterExport command")
    # send necessary libraries to each parallel worker
    clusterEvalQ(cl = this_cluster, library(plyr)) # copy this line and call function again if another library is needed
    
  # Run parallel command and then process output
    #n_dates <- 1000 # just for testing # REMOVE
    #n_dates <- 731#366 # just for testing # REMOVE
    print("start running parLapply")
    #par_output <- parLapply(this_cluster, X = 1:n_dates, fun = merge_predictors.fn)
    #X = 1:n_dates
    test_start <- (365*6-5)
    test_stop <- (365*6-5)+20 #365*7
  #  X = test_start:test_stop
    par_output <- parLapply(this_cluster, X = test_start:test_stop, fun = merge_predictors.fn)
    
    print("finished running parLapply and starting to do.call('rbind', par_output)")
    Merged_input_file <- do.call("rbind", par_output) #concatinate the output from each iteration
    
  # add variables that are derived from other columns
    #stop('Make sure the day of week and decimal date columns are working')
    Merged_input_file$DayOfWeek <- wday(Merged_input_file$Date) # add day of week as predictor column
    #wday(x, week_start = getOption("lubridate.week.start", 7)) <- value
    Merged_input_file$DecimalDatewYear <- decimal_date(Merged_input_file$Date) # add date as a decimal of it's year
    #Merged_input_file$DecimalDate <- Merged_input_file$Year - Merged_input_file$DecimalDatewYear
    Merged_input_file$DecimalDate <- Merged_input_file$DecimalDatewYear - Merged_input_file$Year
    
  # define path and file name for output
    if (substr(this_source_file,(nchar(this_source_file)-8),nchar(this_source_file)) == "_ML_input") {
      ML_input_file_name_output_step <- substr(this_source_file,1,(nchar(this_source_file)-9))
    } else {
      ML_input_file_name_output_step <- this_source_file
    }
      #ML_input_file_name_output <- paste("ML_input_",this_source_file,sep = "")
      ML_input_file_name_output <- paste("ML_input_",ML_input_file_name_output_step,sep = "")
    output_sub_folder <- "ML_input_files"
    print("start writing data to file")
    write.csv(Merged_input_file,file = file.path(ProcessedData.directory,output_sub_folder,paste(ML_input_file_name_output,'.csv',sep = "")),row.names = FALSE) # Write csv file
   
  # End use of parallel computing #
    stopCluster(this_cluster)
    rm(this_cluster, n_cores)
    
  return(Merged_input_file) # output from function
} # end of ML_merge_predictors_parallel_wrapper.fn function

# ## serial version of code
# par_output <- list()
# # n_dates <- 15 # just for testing
#  for (X in 16:30) {
#   this_Date <- as.Date(Date_list[X])
#   print(this_Date)
#   print(X)
#   par_output[[X]] <-  merge_predictors.fn(X)
#   dim(par_output[[X]])[2]
#   if (dim(par_output[[X]])[2] != 43) {
#     stop("check number of columns")
#   }
# }
# Merged_input_file <- do.call("rbind", par_output) #concatinate the output from each iteration
