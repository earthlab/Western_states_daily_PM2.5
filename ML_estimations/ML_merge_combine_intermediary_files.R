# ML_merge_combine_intermediary_files.R - merge all of the extracted NAM data files into a single file

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

#### Define constants/folders ####
processed_data_version <- define_study_constants.fn("processed_data_version")
ML_input_folder <- "ML_input_files" # define folder for NAM data
input_sub_folder <-paste("ML_input_part_",processed_data_version,"_Intermediary_Files",sep = "") # define location of input files
#output_sub_folder <- "NAM_Step3" # define location for output files
output_sub_folder <- ML_input_folder
#intermediary_sub_folder <- "NAM_Step3_Intermediary_Files" # define location for intermediary files (within NAM_Step3)
# define path and file name for output
file_paths_to_merge_to <- c(paste("PM25_data_part_",processed_data_version,sep = ""),paste("PM25_data_part_",processed_data_version,sep = ""),"CountyCentroid")
files_to_merge_to <- c(paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input",sep = ""),paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_prioritize_24hr_obs_ML_input",sep = ""), "CountyCentroid_Locations_Dates_2008-01-01to2018-12-31")

n_data_sets <- 1 # REMOVE
for (data_set_counter in 1:n_data_sets) { # cycle through files
this_source_file <- files_to_merge_to[data_set_counter] 

if (substr(this_source_file,(nchar(this_source_file)-8),nchar(this_source_file)) == "_ML_input") {
  ML_input_file_name_output_step <- substr(this_source_file,1,(nchar(this_source_file)-9))
} else {
  ML_input_file_name_output_step <- this_source_file
}
#ML_input_file_name_output <- paste("ML_input_",this_source_file,sep = "")
ML_input_file_name_output <- paste("ML_input_",ML_input_file_name_output_step,sep = "")
output_sub_folder <- "ML_input_files"
output_sub_sub_folder <- paste("ML_input_part_",processed_data_version,"_Intermediary_Files",sep = "")

output_file_name <- file.path(ProcessedData.directory,output_sub_folder,paste(ML_input_file_name_output,'.csv',sep = ""))#paste("NAM_Step3_processed_",Sys.Date(),sep = "") # define name of output file

#### Load and process data ####
# get list of all ML file names matching this_source_file:
this_file_list_step <- list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),ML_input_folder,input_sub_folder,"."), pattern = NULL, all.files = FALSE,
                          full.names = FALSE, recursive = FALSE,
                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
print(paste("There are ",length(this_file_list_step),"files for NAM (extracted to points)")) # optional output statement

# find out how many files only have 6 columns (didn't have weather data) by figuring out how many columns each file has
n_cols <- NA # re-allocate
n_cols <- unlist(lapply(1:length(this_file_list_step), function(file_i){ # start definition of anonymous function
  file_name <- this_file_list_step[file_i] # get file name
  this_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),ML_input_folder,input_sub_folder,file_name)) # load Step 2 file
  this_n_cols <- dim(this_data)[2] # determine the number of columns
  rm(this_data) # clear variable
  return(this_n_cols) # output from function
})) # n_cols <- unlist(lapply(1:length(this_file_list_step), function(file_i){

which_odd_files <- which(n_cols != median(n_cols)) # bad files have 6 columns (location & date information but no weather data)
print(paste("There are ",length(which_odd_files),"files that do not have have",median(n_cols),"columns and will be disregarded (no weather data got put in):")) # optional output statement
odd_file_names <- this_file_list_step[which_odd_files] # isolate names of bad files
print(odd_file_names) # output names of bad files to screen
#for (file_i in 1:length(odd_file_names)) { # cycle through each bad file name
#  file_name <- odd_file_names[file_i] 
#  print(file_name)
#}

# process the remaining good files
which_good_files <- which(n_cols == median(n_cols)) # determine which files are likely to be good (they have enough columns)
print(paste("There are ",length(which_good_files)," files that have 19 columns (the expected number) and these will be processed."))
this_file_list <- this_file_list_step[which_good_files] # list of only good files

# the files have the time stamp just in the file name, so the time stamp needs to be added as a column in the data
new_file_list <- unlist(lapply(this_file_list, function(x){ # start lapply and start defining function used in lapply
  this_UTC_timestamp <- substr(x,nchar(x)-24,nchar(x)-23) # identify the time stamp for the file in this iteration
  #print(this_UTC_timestamp)
  this_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),ML_input_folder,input_sub_folder,x)) # open data file
  time_vec <- data.frame(matrix(rep_len(this_UTC_timestamp,dim(this_data)[1]),nrow=dim(this_data)[1],ncol=1)) # create data frame with one column that is the UTC time stamp 
  names(time_vec) <- "Time.UTC" # name the column with the time stamp 
  this_data_time <- cbind(time_vec,this_data) # merge the time stamp column with the rest of the meteo data in this file
  new_file_name <- paste(substr(x,1,nchar(x)-4),"_time.csv",sep = "") # define the new file name
  write.csv(this_data_time,file = file.path(define_file_paths.fn("ProcessedData.directory"),ML_input_folder,output_sub_folder,intermediary_sub_folder,new_file_name),row.names = FALSE) # write the new file that has the time stamp as a column
  return(new_file_name) # return the new file name so a new list of files can be created
  }))#, ProcessedData.directory,sub_folder)
      
setwd(file.path(define_file_paths.fn("ProcessedData.directory"),ML_input_folder,output_sub_folder,intermediary_sub_folder)) # change working directory so the next step will work
Step3_NAM_data <- do.call(rbind,lapply(new_file_list, read.csv)) # open and bind all files in list together # https://stackoverflow.com/questions/23995384/read-and-rbind-multiple-csv-files  
setwd(working.directory) # go back to original working directory
write.csv(Step3_NAM_data,file = file.path(define_file_paths.fn("ProcessedData.directory"),ML_input_folder,output_sub_folder,paste(output_file_name,".csv",sep = "")),row.names = FALSE) # write data to file

} # for (data_set_counter in 1:n_data_sets) { # cycle through files
