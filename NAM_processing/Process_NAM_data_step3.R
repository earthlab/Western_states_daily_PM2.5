# Process_NAM_data_step3.R - merge all of the extracted NAM data files into a single file

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

#### Source functions I've written ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))

#### Define constants/folders ####
#NAM_processed_data_version <- "bc" # data part
NAM_folder <- "NAM_data"
input_sub_folder <- "NAM_Step2"
output_sub_folder <- "NAM_Step3"
intermediary_sub_folder <- "NAM_Step3_Intermediary_Files"
output_file_name <- paste("NAM_Step3_processed_",Sys.Date(),sep = "")
#sub_folder <- paste("NAM_data_part_",NAM_processed_data_version,sep = "") # subfolder withing ProcessedData.directory
#output_file_name <- "NAM_Step3_part_bc"

#### Load and process data ####
#file_name_pattern <- "UTC\\.csv$"
#this_file_list_step <- list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder), pattern = file_name_pattern)
this_file_list_step <- list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,"."), pattern = NULL, all.files = FALSE,
                          full.names = FALSE, recursive = FALSE,
                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

print(paste("There are ",length(this_file_list_step),"files for NAM (extracted to points)"))    

# find out how many files only have 6 columns (didn't have weather data) by figuring out how many columns each file has
n_cols <- NA
n_cols <- unlist(lapply(1:length(this_file_list_step), function(file_i){
  file_name <- this_file_list_step[file_i]
  #this_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,file_name))
  this_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,file_name))
  this_n_cols <- dim(this_data)[2]
  rm(this_data)
  return(this_n_cols)
})) # n_cols <- unlist(lapply(1:length(this_file_list_step), function(file_i){

which_odd_files <- which(n_cols != median(n_cols)) # bad files have 6 columns (location & date information but no weather data)
print(paste("There are ",length(which_odd_files),"files that do not have have",median(n_cols),"columns and will be disregarded (no weather data got put in):"))
odd_file_names <- this_file_list_step[which_odd_files]
for (file_i in 1:length(odd_file_names)) {
  file_name <- odd_file_names[file_i]
  print(file_name)
  #this_data <- read.csv(file.path(ProcessedData.directory,sub_folder,file_name))
}

# process the remaining good files
which_good_files <- which(n_cols == median(n_cols))
print(paste("There are ",length(which_good_files)," files that have 19 columns (the expected number) and these will be processed."))
this_file_list <- this_file_list_step[which_good_files] # list of only good files

# the files have the time stamp just in the file name, so the time stamp needs to be added as a column in the data
new_file_list <- unlist(lapply(this_file_list[1:2], function(x){ # start lapply and start defining function used in lapply
  this_UTC_timestamp <- substr(x,nchar(x)-24,nchar(x)-23) # identify the time stamp for the file in this iteration
  #print(this_UTC_timestamp)
  this_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,x)) # open data file
  time_vec <- data.frame(matrix(rep_len(this_UTC_timestamp,dim(this_data)[1]),nrow=dim(this_data)[1],ncol=1)) # create data frame with one column that is the UTC time stamp 
  names(time_vec) <- "Time.UTC" # name the column with the time stamp 
  this_data_time <- cbind(time_vec,this_data) # merge the time stamp column with the rest of the meteo data in this file
  new_file_name <- paste(substr(x,1,nchar(x)-4),"_time.csv",sep = "") # define the new file name
  write.csv(this_data_time,file = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,intermediary_sub_folder,new_file_name),row.names = FALSE) # write the new file that has the time stamp as a column
  return(new_file_name) # return the new file name so a new list of files can be created
  }))#, ProcessedData.directory,sub_folder)
      
setwd(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,intermediary_sub_folder)) # change working directory so the next step will work
Step3_NAM_data <- do.call(rbind,lapply(new_file_list, read.csv)) # open and bind all files in list together # https://stackoverflow.com/questions/23995384/read-and-rbind-multiple-csv-files  
setwd(working.directory)
# write data to file
write.csv(Step3_NAM_data,file = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,paste(output_file_name,".csv",sep = "")),row.names = FALSE)


