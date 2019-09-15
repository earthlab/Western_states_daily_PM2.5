# Process_NAM_data_step5.R - take 24-hr summaries of data

#### Clear variables and sinks; define working directory ####
rm(list  =  ls())
options(warn  =  2) # throw an error when there's a warning and stop the code from running further
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which  =  dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
working.directory  <-  "/home/rstudio"
setwd(working.directory) # set working directory

#### Call Packages (Library) ####
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
library(lubridate) # https://cran.r-project.org/web/packages/lubridate/lubridate.pdf

#### Source functions I've written ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))

functions_list <-c("replace_character_in_string.fn")

#### Define Constants ####
NAM_folder <- "NAM_data" # define folder for NAM data
input_sub_folder <- "NAM_Step4" # define location of input files
input_sub_sub_folder <- "NAM_Step4_Intermediary_Files"
output_sub_folder <- "NAM_Step5" # define location for output files
output_sub_sub_folder <- "NAM_Step5_Intermediary_Files"
output_file_name <- paste("NAM_Step5_processed_",Sys.Date(),sep = "") # define name of output file
this_batch_date <- define_study_constants.fn("NAM_batch_date")

#### Load and Process Data ####
# # determine which file from step 4 is most recent
# file_name_pattern <- "\\.csv$" # only looking for .csv files (don't want to pick up the sub-folder)
# this_file_list <- list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,"."), pattern = file_name_pattern, all.files = FALSE,
#                              full.names = FALSE, recursive = FALSE,
#                              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE) # get list of all .csv file in this folder
# print(paste("There are ",length(this_file_list),"files for NAM Step 4 data")) # optional output statement
# date_list <- unlist(lapply(this_file_list, function(x){ # start lapply and start defining function used in lapply
#   processed_date <- substr(x,nchar(x)-13,nchar(x)-4) # identify the time stamp for the file in this iteration
#   return(processed_date) # return the new file name so a new list of files can be created
#   }))

# Step 4 intermediary files
file_name_pattern <- "\\.csv$" # only looking for .csv files (don't want to pick up the sub-folder)
step4_file_list <- list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,input_sub_sub_folder,"."), pattern = file_name_pattern, all.files = FALSE,
                             full.names = FALSE, recursive = FALSE,
                             ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE) # get list of all .csv file in this folder
print(paste("There are ",length(step4_file_list),"files for NAM Step 4 data (Intermediary files)")) # optional output statement
date_list <- unlist(lapply(step4_file_list, function(x){ # start lapply and start defining function used in lapply
  data_date <- substr(x,nchar(x)-35,nchar(x)-39) # identify the time stamp for the file in this iteration
  return(data_date) # return the new file name so a new list of files can be created
}))
print(paste("there are",length(step4_file_list),"NAM Step4 files to be processed"))

#recent_processed_date <- max(as.Date(date_list)) # which date is the most recent file
#which_recent_file <- which(date_list == recent_processed_date) # locate the file name for the most recent file
#recent_file_name <- this_file_list[which_recent_file] # most recent file name
#print(paste(recent_file_name,"is the most recent file and will be used"))
# load the data created in step 3, which has all of the observations for the 4 timesteps per day in one data frame
#Step4_NAM_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,recent_file_name)) # open data file
#rm(file_name_pattern,this_file_list,date_list,recent_processed_date,which_recent_file,recent_file_name) # clear variables

# load information about meteo variables
this_source_file <- paste("MeteoVariablesNAM.csv")
MeteoVarsMultiType <- read.csv(file.path(define_file_paths.fn("NAM_Code.directory"),this_source_file))
# grab the list of relevant meteo variables for this file type from MeteoVars
which_meteo <- which(MeteoVarsMultiType$file_type == "grib2") # get grib2 files because grib1 files will be converted to grib2
MeteoVars <- MeteoVarsMultiType[which_meteo,] # matrix with just the relevant rows

#All_date_loc <- unique(Step4_NAM_data[ ,c("Local.Date","Latitude","Longitude")]) # get a list of dates/locations
all_dates <- seq(as.Date(define_study_constants.fn("start_date")), as.Date(define_study_constants.fn("end_date")), by="days")#unique(Step4_NAM_data$Local.Date)

#### Set up for parallel processing ####
n_cores <- detectCores() - 1 # Calculate the number of cores
print(paste(n_cores,"cores available for parallel processing",sep = " "))
this_cluster <- makeCluster(n_cores) # # Initiate cluster
#clusterExport(cl = this_cluster, varlist = c("All_date_loc","Step4_NAM_data","MeteoVars",functions_list), envir = .GlobalEnv) # export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
clusterExport(cl = this_cluster, varlist = c("step4_file_list","NAM_folder","input_sub_folder","input_sub_sub_folder","output_sub_folder","output_sub_sub_folder","step4_file_list","MeteoVars",functions_list), envir = .GlobalEnv) # export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
# "Step4_NAM_data", "all_dates",
#### call parallel function ####
#1:dim(All_date_loc)[1]
#NAM_data_list <- parLapply(this_cluster,X = 1:1000, fun = function(x){ # call parallel function
#X = 1:dim(All_date_loc)[1]
#X = 1:length(all_dates)
print("start parLapply function")
#X = 1:length(all_dates)
# X <- 178
par_output <- parLapply(this_cluster,X = 1:length(all_dates), fun = function(x){ # call parallel function
#par_output <- parLapply(this_cluster,X = 1:length(step4_file_list), fun = function(x){ # call parallel function  
  #this_file <- step4_file_list[x]
  #this_date <- as.Date(substr(this_file,17,26))
  #print(this_file)
  this_date <- all_dates[x]
  this_next_day <- this_date+1
  print(paste("Processing NAM data for",this_date))
  files_to_check <- c(paste("Step4_NAM_Step2_",this_date,"_00UTC_batch",this_batch_date,"_time.csv",sep = ""),
                      paste("Step4_NAM_Step2_",this_date,"_06UTC_batch",this_batch_date,"_time.csv",sep = ""),
                     paste("Step4_NAM_Step2_",this_date,"_12UTC_batch",this_batch_date,"_time.csv",sep = ""),
                     paste("Step4_NAM_Step2_",this_date,"_18UTC_batch",this_batch_date,"_time.csv",sep = ""),
                     paste("Step4_NAM_Step2_",this_next_day,"_00UTC_batch",this_batch_date,"_time.csv",sep = ""),
                     paste("Step4_NAM_Step2_",this_next_day,"_06UTC_batch",this_batch_date,"_time.csv",sep = ""))
  
  which_files_present <- which(files_to_check %in% step4_file_list)
  if (length(which_files_present) > 0) { # only try to process data if there is data to process
  files_to_process <- files_to_check[which_files_present]
  
  # Merge all of the files that could have data for this date into one data frame
  NAM_data_date_step <- lapply(1:length(files_to_process), function(z){ # start of lapply to open each file
    this_file_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,input_sub_sub_folder,files_to_process[z])) # open file
  }) # end of lapply - NAM_data_date_step <- lapply(1:length(files_to_process), function(z){ 
  NAM_data_date_step <- do.call("rbind",NAM_data_date_step) # merge files into one data frame
  
  NAM_data_date_step$Latitude <- round(NAM_data_date_step$Latitude,5) # round latitude to 5 digits
  NAM_data_date_step$Longitude <- round(NAM_data_date_step$Longitude,5) # round longitude to 5 digits
  NAM_data_date_step$Local.Date <- as.Date(NAM_data_date_step$Local.Date) # recognize dates as dates
  NAM_data_date_step$Local.Date.Time <- as_datetime(NAM_data_date_step$Local.Date.Time) # recognize datetime as such
  NAM_data_date_step$TimeZone <- as.character(NAM_data_date_step$TimeZone) # recognize times zones as characters
  
  print(paste("x = ",x,"date = ",this_date))
  #print(paste("x = ",x,"date = ",all_dates[x]))
  # isolate all data for this date
  which_this_date <- which(NAM_data_date_step$Local.Date == this_date)
  NAM_data_date <- NAM_data_date_step[which_this_date, ]
  rm(NAM_data_date_step)
  #which_this_date <- which(Step4_NAM_data$Local.Date == all_dates[x])
  #NAM_data_date <- Step4_NAM_data[which_this_date, ]
  
  # cycle through all locations on this date
  All_date_loc <- unique(NAM_data_date[ ,c("Latitude","Longitude")]) # get a list of dates/locations
  
  Step5_NAM_date_list <- lapply(X = 1:dim(All_date_loc)[1], FUN = function(y){ # start lapply and start defining function used in lapply
    
    print(y)
    # find all data points with this date/loc
    which_this_date_loc <- which(NAM_data_date$Latitude == All_date_loc[y, c("Latitude")] & NAM_data_date$Longitude == All_date_loc[y, c("Longitude")])
    this_date_loc_step <- NAM_data_date[which_this_date_loc, ]
    rm(which_this_date_loc)
    this_date_loc <- this_date_loc_step[!duplicated(this_date_loc_step), ]
    rm(this_date_loc_step)
    #if (length(which_this_date_loc)>5) {stop(paste("Check code and data - should not have more than 5 NAM data points for given day/location. date = ",all_dates[x]," x=",x," y=",y))}
    
    # can have 5 on the daylight savings switchover, but there should never be more than 5 rows
    if (dim(this_date_loc)[1]>5) {stop(paste("Check code and data - should not have more than 5 NAM data points for given day/location. date = ",all_dates[x]," x=",x," y=",y))}
    
    Step5_NAM_row <- data.frame(matrix(NA,nrow=1,ncol=length(colnames(NAM_data_date)))) # create data frame for input_mat1
    names(Step5_NAM_row) <- colnames(NAM_data_date) # assign the header to input_mat1
    # drop extraneous columns that don't apply to 24-hr data
    drop_cols <- c("Time.UTC","Date","Local.Date.Time","UTC.Date.Time") # define unnecessary columns
    Step5_NAM_row <- Step5_NAM_row[ , !(names(Step5_NAM_row) %in% drop_cols)] # drop unnecessary columns
    Step5_NAM_row[1, c("Latitude","Longitude",  "TimeZone")] <- unique(this_date_loc[ , c("Latitude","Longitude",  "TimeZone")]) # input meta data into step 5
    Step5_NAM_row$Local.Date <- unique(this_date_loc$Local.Date) # input dates
  
  for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
    #print(meteo_var_counter)
    thisMeteo_var_Name <- MeteoVars[meteo_var_counter,c("VariableName")] # get variable full name
    thisMeteo_variable <- MeteoVars[meteo_var_counter,c("VariableCode")] # get variable coded name
    thisMeteo_level <- MeteoVars[meteo_var_counter,c("AtmosLevelCode")] # get variable level name
    thisMeteo_units <- MeteoVars[meteo_var_counter,c("Units")] # get variable units
    thisMeteo_24_summary <- MeteoVars[meteo_var_counter,c("X24.hr.summary")]
    this_col_name_step <- as.character(paste(thisMeteo_variable,".",thisMeteo_level,sep = ""))
    this_col_name <- replace_character_in_string.fn(input_char = this_col_name_step,char2replace = " ",replacement_char = ".") 
    #print(this_col_name)
    if (thisMeteo_24_summary == "max") {
      this_meteo_value <- max(this_date_loc[ , this_col_name]) # what is the value for this variable at this level?
    } else if (thisMeteo_24_summary == "mean") {
      this_meteo_value <- mean(this_date_loc[ , this_col_name]) # what is the value for this variable at this level?
    } else if (thisMeteo_24_summary == "sum") {
      this_meteo_value <- sum(this_date_loc[ , this_col_name]) # what is the value for this variable at this level?
    }
    Step5_NAM_row[1, this_col_name] <- this_meteo_value
  } # for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
  return(Step5_NAM_row)
  }) # end of lapply function
  # re-combine data
  Step5_NAM_date <- do.call("rbind", Step5_NAM_date_list)
#} # remove - end of for loop
  } # if (length(which_files_present) > 0) { # only try to process data if there is data to process
  return(Step5_NAM_date)
} ) # call parallel function

#### Combine output from parLapply/lapply ####
print("combine output from parLapply")
NAM_data <- do.call("rbind", par_output) #concatinate the output from each iteration

# write step 5 data to csv file
print("Write Step 5 data to file")
write.csv(NAM_data,file = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,paste(output_file_name,".csv",sep = "")),row.names = FALSE) # write data to file

#### End use of parallel computing #####
stopCluster(this_cluster) # stop the cluster
rm(this_cluster,par_output)

# clear variables
rm(NAM_data,NAM_folder,input_sub_folder,output_sub_folder,output_file_name,working.directory)
rm(MeteoVars,MeteoVarsMultiType,Step4_NAM_data)
print(paste("Process_NAM_data_step5.R completed at",Sys.time(),sep = " ")) # print time of completion to sink file

#### lapply version of code ####
# NAM_data_list <- lapply(1:dim(All_date_loc)[1], function(x){ # x <- 1
# #NAM_data_list <- lapply(1:10, function(x){ # x <- 1
#   # find all data points with this date/loc
#   which_this_date_loc <- which(Step4_NAM_data$Local.Date == All_date_loc[x, c("Local.Date")] & Step4_NAM_data$Latitude == All_date_loc[x, c("Latitude")] & Step4_NAM_data$Longitude == All_date_loc[x, c("Longitude")])
#   this_date_loc <- Step4_NAM_data[which_this_date_loc, ]
#   if (length(which_this_date_loc)>4) {stop("Check code and data - should not have more than 4 NAM data points for given day/location")}
#   Step5_NAM_data <- data.frame(matrix(NA,nrow=1,ncol=length(colnames(Step4_NAM_data)))) # create data frame for input_mat1
#   names(Step5_NAM_data) <- colnames(Step4_NAM_data) # assign the header to input_mat1
#   # drop extraneous columns that don't apply to 24-hr data
#   drop_cols <- c("Time.UTC","Date","Local.Date.Time","UTC.Date.Time") # define unnecessary columns
#   Step5_NAM_data <- Step5_NAM_data[ , !(names(Step5_NAM_data) %in% drop_cols)] # drop unnecessary columns
#   Step5_NAM_data[1, c("Latitude","Longitude",  "TimeZone")] <- unique(this_date_loc[ , c("Latitude","Longitude",  "TimeZone")]) # input meta data into step 5
#   Step5_NAM_data$Local.Date <- unique(this_date_loc$Local.Date) # input dates
#   
#   for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
#     #print(meteo_var_counter)
#     thisMeteo_var_Name <- MeteoVars[meteo_var_counter,c("VariableName")] # get variable full name
#     thisMeteo_variable <- MeteoVars[meteo_var_counter,c("VariableCode")] # get variable coded name
#     thisMeteo_level <- MeteoVars[meteo_var_counter,c("AtmosLevelCode")] # get variable level name
#     thisMeteo_units <- MeteoVars[meteo_var_counter,c("Units")] # get variable units
#     thisMeteo_24_summary <- MeteoVars[meteo_var_counter,c("X24.hr.summary")]
#     this_col_name_step <- as.character(paste(thisMeteo_variable,".",thisMeteo_level,sep = ""))
#     this_col_name <- replace_character_in_string.fn(input_char = this_col_name_step,char2replace = " ",replacement_char = ".") 
#     #print(this_col_name)
#     if (thisMeteo_24_summary == "max") {
#     this_meteo_value <- max(this_date_loc[ , this_col_name]) # what is the value for this variable at this level?
#     } else if (thisMeteo_24_summary == "mean") {
#     this_meteo_value <- mean(this_date_loc[ , this_col_name]) # what is the value for this variable at this level?
#     } else if (thisMeteo_24_summary == "sum") {
#     this_meteo_value <- sum(this_date_loc[ , this_col_name]) # what is the value for this variable at this level?
#     }
#     Step5_NAM_data[1, this_col_name] <- this_meteo_value
#   } # for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
#   return(Step5_NAM_data)
#   }) # end lapply command

