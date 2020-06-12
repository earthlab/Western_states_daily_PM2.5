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
#input_sub_folder <-paste("ML_input_part_",processed_data_version,"_Intermediary_Files",sep = "") # define location of input files
input_sub_folders <- c(paste("ML_input_part_",processed_data_version,"_Intermediary_Files_aves",sep = ""), paste("ML_input_part_",processed_data_version,"_Intermediary_Files_prioritize_24hr_obs",sep = ""),"ML_input_files_CountyCentroid_Intermediary_Files" )

#output_sub_folder <- ML_input_folder # define location for output files
#intermediary_sub_folder <- "NAM_Step3_Intermediary_Files" # define location for intermediary files (within NAM_Step3)
# define path and file name for output
#file_paths_to_merge_to <- c(paste("PM25_data_part_",processed_data_version,sep = ""),paste("PM25_data_part_",processed_data_version,sep = ""),"CountyCentroid")
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
#output_sub_sub_folder <- #paste("ML_input_part_",processed_data_version,"_Intermediary_Files",sep = "")
input_sub_folder <- input_sub_folders[data_set_counter]

output_file_name <- file.path(define_file_paths.fn("ProcessedData.directory"),output_sub_folder,paste(ML_input_file_name_output,'_compiled_',Sys.Date(),'.csv',sep = ""))#paste("NAM_Step3_processed_",Sys.Date(),sep = "") # define name of output file

#### Load and process data ####
# get list of all ML file names matching this_source_file:
this_file_list <- list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),ML_input_folder,input_sub_folder,"."), pattern = NULL, all.files = FALSE,
                          full.names = FALSE, recursive = FALSE,
                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
print(paste("There are",length(this_file_list),"intermediary ML_input files. (One file represents one day.) ")) # optional output statement

setwd(file.path(define_file_paths.fn("ProcessedData.directory"),ML_input_folder,input_sub_folder)) # change working directory so the next step will work
ML_input_all_days <- do.call(rbind,lapply(this_file_list, read.csv)) # open and bind all files in list together # https://stackoverflow.com/questions/23995384/read-and-rbind-multiple-csv-files  
setwd(working.directory) # go back to original working directory
write.csv(ML_input_all_days,file = file.path(paste(output_file_name,sep = "")),row.names = FALSE) # write data to file

} # for (data_set_counter in 1:n_data_sets) { # cycle through files
