ML_merge_predictors_parallal_wrapper.fn <- function(data_set_counter,General_fn_list,Merging_fn_list,directories_vector,input_mat_functions,processed_data_version,output_sub_sub_folders){ #, input_header, ProcessedData.directory, AQSData.directory, FireCache.directory, UintahData.directory) {
  
  #### Load file to be merged to ####
    this_source_file <- files_to_merge_to[data_set_counter] 
    print(this_source_file)
    this_source_path <- file.path(ProcessedData.directory,file_paths_to_merge_to[data_set_counter])
    Source_Data <- read.csv(file.path(this_source_path,paste(this_source_file,".csv",sep = "")),header=TRUE) # load the AQS file
  #### Define columns and change names of columns as needed ####
    predictand_col <- "PM2.5_Obs"
    if (predictand_col %in% colnames(Source_Data)) { # does the data set include the variable "PM2.5_Obs"?
      Source_Data <- input_mat_change_data_classes.fn(Source_Data)
      Dates_col_t <- "Date_Local"
      #datum_col_t <- "NewDatum"
    } else { # if (predictand_col %in% colnames(Source_Data)) { # does the data set include the variable "PM2.5_Obs"? # No  
      Source_Data$Date <- as.Date(Source_Data$Date,"%Y-%m-%d") # recognize dates as dates
      Dates_col_t <- "Date"
      datum_col_t <- NA
    } # if (predictand_col %in% colnames(Source_Data)) { # does the data set include the variable "PM2.5_Obs"?
    rm(this_source_path)
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
    n_cores <- detectCores() - 1 # Calculate the number of cores
    print(paste(n_cores,"cores available for parallel processing",sep = " "))
    this_cluster <- makeCluster(n_cores) # Initiate cluster
    print("start running clusterExport command")
    this_task_vars <- c("Source_Data", "predictand_col", "latitude_col_t","longitude_col_t","Dates_col_t","Date_list","ML_input_file_name_output","output_sub_folder","output_sub_sub_folder") # vector of names of variables to be exported to cluster
    clusterExport(cl = this_cluster, varlist = c(this_task_vars,General_fn_list,Merging_fn_list,all_files_list,directories_vector,input_mat_functions), envir = environment()) # export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
    #datum_col_t,
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
    print("**** Make sure all dates (X = 1:n_dates) is included in the parLapply Command ****")
    par_output <- parLapply(this_cluster, X = 1:15, fun = merge_predictors.fn)
    print("finished running parLapply for merge_predictors.fn")
    
    all_dates <- seq(as.Date(define_study_constants.fn("start_date")), as.Date(define_study_constants.fn("end_date")), by="days")#unique(Step4_NAM_data$Local.Date)
    
    
    # # Merge all of the files that could have data for this date into one data frame
    # NAM_data_date_step <- lapply(1:length(files_to_process), function(z){ # start of lapply to open each file
    #   this_file_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,input_sub_sub_folder,files_to_process[z])) # open file
    # }) # end of lapply - NAM_data_date_step <- lapply(1:length(files_to_process), function(z){ 
    # NAM_data_date_step <- do.call("rbind",NAM_data_date_step) # merge files into one data frame
    # 
    # print("finished running parLapply and starting to do.call('rbind', par_output)")
    # Merged_input_file <- do.call("rbind", par_output) #concatinate the output from each iteration
    # print("start writing data to file")
    # write.csv(Merged_input_file,file = file.path(ProcessedData.directory,output_sub_folder,paste(ML_input_file_name_output,'.csv',sep = "")),row.names = FALSE) # Write csv file
    # stopCluster(this_cluster) # End use of parallel computing 
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
