ML_merge_predictors_parallal_wrapper.fn <- function(data_set_counter,General_fn_list,Merging_fn_list,directories_vector,input_mat_functions,processed_data_version,output_sub_sub_folders){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  
  #### Load file to be merged to ####
    this_source_file <- files_to_merge_to[data_set_counter] # get name of file to be merged to
    print(this_source_file)
    this_source_path <- file.path(ProcessedData.directory,file_paths_to_merge_to[data_set_counter]) # get the file path of the file to be merged to
    Source_Data <- read.csv(file.path(this_source_path,paste(this_source_file,".csv",sep = "")),header=TRUE) # load the file
    rm(this_source_path) # clear variable
  #### Define columns and change names of columns as needed ####
    predictand_col <- "PM2.5_Obs"
    if (predictand_col %in% colnames(Source_Data)) { # does the data set include the variable "PM2.5_Obs"?
      Source_Data <- input_mat_change_data_classes.fn(Source_Data)
      Dates_col_t <- "Date_Local"
    } else { # if (predictand_col %in% colnames(Source_Data)) { # does the data set include the variable "PM2.5_Obs"? # No  
      Source_Data$Date <- as.Date(Source_Data$Date,"%Y-%m-%d") # recognize dates as dates
      Dates_col_t <- "Date"
    } # if (predictand_col %in% colnames(Source_Data)) { # does the data set include the variable "PM2.5_Obs"?
    # rename latitude and longitude columns 
      Source_Data <- replace_column_names.fn(df_in = Source_Data,old_col_name = "Latitude",new_col_name = "Lat")
      Source_Data <- replace_column_names.fn(df_in = Source_Data,old_col_name = "Longitude",new_col_name = "Lon")
    # define column names
      latitude_col_t <- "Lat"
      longitude_col_t <- "Lon"
  #### Define dates and file paths ####
    # define study period
      Date_list <- sort(unique(Source_Data[ ,c(Dates_col_t)]))  
      n_dates <- length(Date_list)
      print(paste("Date_list has ",n_dates," days in it.",sep = ""))
    # define path and file name for output
      if (substr(this_source_file,(nchar(this_source_file)-8),nchar(this_source_file)) == "_ML_input") {
        ML_input_file_name_output_step <- substr(this_source_file,1,(nchar(this_source_file)-9))
      } else {
        ML_input_file_name_output_step <- this_source_file
      }
      ML_input_file_name_output <- paste("ML_input_",ML_input_file_name_output_step,sep = "")
      output_sub_folder <- "ML_input_files"
      output_sub_sub_folder <- output_sub_sub_folders[data_set_counter] 
  #### prep for running in parallel ####
    if (predictand_col %in% colnames(Source_Data)) { # does the data set include the variable "PM2.5_Obs"?  
      n_cores <- detectCores() - 1 # Calculate the number of cores
    } else { # if (predictand_col %in% colnames(Source_Data)) { # does the data set include the variable "PM2.5_Obs"? # No 
      n_cores <- round(detectCores()/3)
    } #  } # if (predictand_col %in% colnames(Source_Data)) { # does the data set include the variable "PM2.5_Obs"?
      
    print(paste(n_cores,"cores available for parallel processing",sep = " "))
    this_cluster <- makeCluster(n_cores) # Initiate cluster
    #n_cores <- 1 # running out of memory if I try to do 15
    #this_cluster <- makeCluster(n_cores)
    #print(paste(n_cores,"cores available for parallel processing",sep = " "))
    print("start running clusterExport command")
    this_task_vars <- c("Source_Data", "predictand_col", "latitude_col_t","longitude_col_t","Dates_col_t","Date_list","ML_input_file_name_output","output_sub_folder","output_sub_sub_folder") # vector of names of variables to be exported to cluster
    clusterExport(cl = this_cluster, varlist = c(this_task_vars,General_fn_list,Merging_fn_list,all_files_list,directories_vector,input_mat_functions), envir = environment()) # export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
    print("finished running clusterExport command")
    # send necessary libraries to each parallel worker
    clusterEvalQ(cl = this_cluster, library(plyr)) # copy this line and call function again if another library is needed
    clusterEvalQ(cl = this_cluster, library(lubridate)) # copy this line and call function again if another library is needed
    clusterEvalQ(cl = this_cluster, library(stringr)) # copy this line and call function again if another library is needed
    clusterEvalQ(cl = this_cluster, library(sp)) # copy this line and call function again if another library is needed
    clusterEvalQ(cl = this_cluster, library(maps)) # copy this line and call function again if another library is needed
    clusterEvalQ(cl = this_cluster, library(maptools)) # copy this line and call function again if another library is needed

  #### Run parallel command and then process output ####
    print("start running parLapply")
    # X = 1:n_dates
  #  X <- 188
    print("**** Make sure all dates (X = 1:n_dates) is included in the parLapply Command ****")
    #par_output <- 
    #parLapply(this_cluster, X = 1:n_dates, fun = merge_predictors.fn)
    parLapply(this_cluster, X = 1:1000, fun = merge_predictors.fn)
    print("finished running parLapply for merge_predictors.fn")
    #all_dates <- seq(as.Date(define_study_constants.fn("start_date")), as.Date(define_study_constants.fn("end_date")), by="days")#unique(Step4_NAM_data$Local.Date)
    stopCluster(this_cluster) # End use of parallel computing 
    rm(this_cluster, n_cores)
    return(NA) # output from function
} # end of ML_merge_predictors_parallel_wrapper.fn function

#### serial version of code - keep, but commented ####
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
