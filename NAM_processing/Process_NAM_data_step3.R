# Process_NAM_data_step3.R - merge all of the extracted NAM data files into a single file

# Define constants/folders
NAM_processed_data_version <- "bc" # data part
sub_folder <- paste("NAM_data_part_",NAM_processed_data_version,sep = "") # subfolder withing ProcessedData.directory
#this_location_date_file <- "NAM_Step1_part_bc_Locations_Dates_wNextDay" # first p
output_file_name <- "NAM_Step3_part_bc"
#time_step <- "18"

#setwd(file.path(ProcessedData.directory,sub_folder))

#file_name_pattern <- paste(this_location_date_file,"_*_",time_step,"UTC.csv",sep = "")
file_name_pattern <- "UTC\\.csv$"
this_file_list_step <- list.files(path = file.path(ProcessedData.directory,sub_folder), pattern = file_name_pattern)
#file = file.path(ProcessedData.directory,sub_folder,paste(this_location_date_file,"_",as.character(theDate),"_",this_model.run,"UTC.csv",sep = "")
print(paste("There are ",length(this_file_list_step),"files for NAM (extracted to points)"))    

# find out how many files only have 6 columns (didn't have weather data) by figuring out how many columns each file has
n_cols <- NA
for (file_i in 1:length(this_file_list_step)) {
  file_name <- this_file_list_step[file_i]
  #print(file_name)
  this_data <- read.csv(file.path(ProcessedData.directory,sub_folder,file_name))
  n_cols[file_i] <- dim(this_data)[2]
}

which_odd_files <- which(n_cols == 6) # bad files have 6 columns (location & date information but no weather data)
print(paste("There are ",length(which_odd_files),"files that only have 6 columns and will be disregarded (no weather data got put in):"))
odd_file_names <- this_file_list_step[which_odd_files]
for (file_i in 1:length(odd_file_names)) {
  file_name <- odd_file_names[file_i]
  print(file_name)
  #this_data <- read.csv(file.path(ProcessedData.directory,sub_folder,file_name))
}

# process the remaining good files
which_good_files <- which(n_cols == 19)
print(paste("There are ",length(which_good_files)," files that have 19 columns (the expected number) and these will be processed."))
this_file_list <- this_file_list_step[which_good_files] # list of only good files
       
setwd(file.path(ProcessedData.directory,sub_folder)) # change working directory so the next step will work
Step3_NAM_data <- do.call(rbind,lapply(this_file_list, read.csv)) # open and bind all files in list together # https://stackoverflow.com/questions/23995384/read-and-rbind-multiple-csv-files  
setwd(working.directory)
# write data to file
write.csv(Step3_NAM_data,file = file.path(ProcessedData.directory,sub_folder,paste(output_file_name,".csv",sep = "")),row.names = FALSE)


