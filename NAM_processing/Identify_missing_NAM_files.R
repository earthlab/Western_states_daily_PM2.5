# Identify_missing_NAM_files.R - identify missing NAM files

#### Clear variables and sinks; define working directory ####
rm(list  =  ls()) # clear all variables
options(warn  =  2) # throw an error when there's a warning and stop the code from running further
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which  =  dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
working.directory  <-  "/home/rstudio" # define working directory
setwd(working.directory) # set working directory

#### Source functions I've written ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R")) # used for defining file paths
source(file.path("estimate-pm25","General_Project_Functions","merging_data_functions.R")) # used for defining file paths

#### Define constants/folders ####
NAM_folder <- "NAM_data" # define folder for NAM data
input_sub_folder <- "NAM_Step2" # define location of input files
output_sub_folder <- "NAM_Step3" # define location for output files
intermediary_sub_folder <- "Identify_missing_files"#"NAM_Step3_Intermediary_Files" # define location for intermediary files (within NAM_Step3)
output_file_name <- paste("NAM_Step3_processed_",Sys.Date(),sep = "") # define name of output file

batch_date <- as.Date("2019-04-05") # used to know whether data has already been ran. This
print(paste("*** Make sure the batch_date matches the batch date in Step 2:",batch_date,"***"))
#### Load and process data ####
# get list of all NAM Step 2 file names:
this_file_list_step <- list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,"."), pattern = NULL, all.files = FALSE,
                                  full.names = FALSE, recursive = FALSE,
                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
n_files_exist <- length(this_file_list_step)
print(paste("There are ",length(this_file_list_step),"files for NAM (extracted to points)")) # optional output statement

start_study <- as.Date(define_study_constants.fn("start_date"),"%Y-%m-%d")
end_study <- as.Date(define_study_constants.fn("end_date"),"%Y-%m-%d")
n_days <- as.numeric(as.character(end_study-start_study+1))
files_per_day <- 4 # there should be 4 files per day
goal_n_files <- n_days*files_per_day
print(paste("There should be",goal_n_files,"files."))
n_missing_files <- goal_n_files - n_files_exist
print(paste(n_missing_files,"files are missing."))

Date_list <- seq(start_study,end_study,by = "days")#as.Date(c(start_study:end_study),"%Y-%m-%d")

model_hours <- c("00","06","12","18")
all_goal_file_names_step <- unlist(lapply(1:n_days, function(date_i){
  this_date <- Date_list[date_i]
  day_file_names <- unlist(lapply(1:files_per_day,function(run_i){
    file_hour <- model_hours[run_i]
    this_file_name <- paste("NAM_Step2_",this_date,"_",file_hour,"UTC_batch",batch_date,".csv",sep = "")
    return(this_file_name)
  })) # end of day_file_names lapply
  return(day_file_names)
})) # end of missing_files lapply


#which_missing <- which(all_goal_file_names_step %!in% this_file_list_step)
all_goal_file_names <- data.frame(matrix(NA,nrow = goal_n_files,ncol = 3))
names(all_goal_file_names) <- c("Date_i","Run_i","FileName")
all_goal_file_names$FileName <- all_goal_file_names_step
#all_goal_file_names$Date_i <- c(1:goal_n_files)
which_missing <- which(all_goal_file_names$FileName %!in% this_file_list_step)
missing_files <- all_goal_file_names[which_missing, ]

# fill in date_i and run_i for missing files
missing_file_info_list <- lapply(1:dim(missing_files)[1],function(file_i){
  this_row <- missing_files[file_i, ]
  this_row$Date_i <- which(Date_list == substr(this_row$FileName,11,20))
  this_row$Run_i <- substr(this_row$FileName,22,23)
  return(this_row)
}) # end all_goal_files_names_list lapply
missing_file_info <- do.call("rbind",missing_file_info_list)
write.csv(missing_file_info,file = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,intermediary_sub_folder,paste("missing_NAM_files_batch",batch_date,".csv",sep = "")),row.names = FALSE)

# find out how many files only have 6 columns (didn't have weather data) by figuring out how many columns each file has
n_cols <- NA # re-allocate
n_cols <- unlist(lapply(1:length(this_file_list_step), function(file_i){ # start definition of anonymous function
  file_name <- this_file_list_step[file_i] # get file name
  this_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,file_name)) # load Step 2 file
  this_n_cols <- dim(this_data)[2] # determine the number of columns
  rm(this_data) # clear variable
  return(this_n_cols) # output from function
})) # n_cols <- unlist(lapply(1:length(this_file_list_step), function(file_i){

which_odd_files <- which(n_cols != median(n_cols)) # bad files have 6 columns (location & date information but no weather data)
print(paste("There are ",length(which_odd_files),"files that do not have have",median(n_cols),"columns and will be disregarded (no weather data got put in):")) # optional output statement
odd_file_names <- this_file_list_step[which_odd_files] # isolate names of bad files
print(odd_file_names) # output names of bad files to screen

# create a data frame describing the files that have issues
which_problems <- which(all_goal_file_names$FileName %in% odd_file_names)
problem_files <- all_goal_file_names[which_problems, ]
print(problem_files)

# fill in date_i and run_i for problem files
problem_file_info_list <- lapply(1:dim(problem_files)[1],function(file_i){
  this_row <- problem_files[file_i, ]
  this_row$Date_i <- which(Date_list == substr(this_row$FileName,11,20))
  this_row$Run_i <- substr(this_row$FileName,22,23)
  return(this_row)
}) # end all_goal_files_names_list lapply
problem_file_info <- do.call("rbind",problem_file_info_list)
write.csv(problem_file_info,file = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,intermediary_sub_folder,paste("problem_NAM_files_batch",batch_date,".csv",sep = "")),row.names = FALSE)


#for (file_i in 1:length(odd_file_names)) { # cycle through each bad file name
#  file_name <- odd_file_names[file_i] 
#  print(file_name)
#}

# # process the remaining good files
# which_good_files <- which(n_cols == median(n_cols)) # determine which files are likely to be good (they have enough columns)
# print(paste("There are ",length(which_good_files)," files that have 19 columns (the expected number) and these will be processed."))
# this_file_list <- this_file_list_step[which_good_files] # list of only good files
# 
# # the files have the time stamp just in the file name, so the time stamp needs to be added as a column in the data
# new_file_list <- unlist(lapply(this_file_list, function(x){ # start lapply and start defining function used in lapply
#   this_UTC_timestamp <- substr(x,nchar(x)-24,nchar(x)-23) # identify the time stamp for the file in this iteration
#   #print(this_UTC_timestamp)
#   this_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,x)) # open data file
#   time_vec <- data.frame(matrix(rep_len(this_UTC_timestamp,dim(this_data)[1]),nrow=dim(this_data)[1],ncol=1)) # create data frame with one column that is the UTC time stamp 
#   names(time_vec) <- "Time.UTC" # name the column with the time stamp 
#   this_data_time <- cbind(time_vec,this_data) # merge the time stamp column with the rest of the meteo data in this file
#   new_file_name <- paste(substr(x,1,nchar(x)-4),"_time.csv",sep = "") # define the new file name
#   write.csv(this_data_time,file = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,intermediary_sub_folder,new_file_name),row.names = FALSE) # write the new file that has the time stamp as a column
#   return(new_file_name) # return the new file name so a new list of files can be created
# }))#, ProcessedData.directory,sub_folder)
# 
# setwd(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,intermediary_sub_folder)) # change working directory so the next step will work
# Step3_NAM_data <- do.call(rbind,lapply(new_file_list, read.csv)) # open and bind all files in list together # https://stackoverflow.com/questions/23995384/read-and-rbind-multiple-csv-files  
# setwd(working.directory) # go back to original working directory
# write.csv(Step3_NAM_data,file = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,paste(output_file_name,".csv",sep = "")),row.names = FALSE) # write data to file
# 
