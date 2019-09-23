# Break up Active fire points files
# break_up_MODIS_files.R

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

# start timer for code
start_code_timer <- proc.time()
print(paste("Start break_up_MODIS_files.R at",Sys.time(),sep = " "))

#### Load Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path("estimate-pm25","General_Project_Functions","merging_data_functions.R"))

predictor_sub_folder <- "PredictorVariablesExtractedToDatesLocations"

Fire_MODIS_file_name <- c("fire_modis_part_f_wLags_25km_extract_final.csv","fire_modis_part_g_25km_extract_final.csv",
  "fire_modis_part_f_wLags_50km_extract_final.csv"  ,"fire_modis_part_g_50km_extract_final.csv",
  "fire_modis_part_f_wLags_100km_extract_final.csv"  ,"fire_modis_part_g_100km_extract_final.csv",
  "fire_modis_part_f_wLags_500km_extract_final.csv"  ,"fire_modis_part_g_500km_extract_final.csv") 

for (file_i in 1:length(Fire_MODIS_file_name)) { # cycle through MODIS files
  print(Fire_MODIS_file_name[file_i])
  name_no_suffix <- substr(Fire_MODIS_file_name[file_i],1,nchar(Fire_MODIS_file_name[file_i])-4)
  print(name_no_suffix)
  # create output folder if it doesn't already exist
  if(dir.exists(file.path(define_file_paths.fn("ProcessedData.directory"),predictor_sub_folder,name_no_suffix)) == FALSE) { # create directory if it doesn't already exist
    dir.create(file.path(define_file_paths.fn("ProcessedData.directory"),predictor_sub_folder,name_no_suffix))
  } # if(exists(file.path(define_file_paths.fn("ProcessedData.directory"),"NAM_data","NAM_Step3")) == FALSE) { # create directory if it doesn't already exist
  
  # load data
  print("start reading file")
  Fire_MODIS_data_step <- read.csv(file.path( define_file_paths.fn("ProcessedData.directory"),predictor_sub_folder, Fire_MODIS_file_name[file_i]),header=TRUE) # load data file
  print("finished reading file")
  Fire_MODIS_data_step<- as.data.frame(Fire_MODIS_data_step) # define data as data frame
  this_date_format <- determine_date_format.fn(Fire_MODIS_data_step[1,"Date"]) # determine format for date information
  Fire_MODIS_data_step[ , c("Date")] <- as.Date(Fire_MODIS_data_step[ , c("Date")],this_date_format) # recognize dates as dates
  
  # what dates are in this file?
  all_dates = sort(unique(Fire_MODIS_data_step$Date))
  
    for (date_i in 1:length(all_dates)) { # cycle through all dates in file
      this_date <- all_dates[date_i]
      print(this_date)
      which_this_date <- which(Fire_MODIS_data_step$Date == this_date)
      this_date_Fire <- Fire_MODIS_data_step[which_this_date, ]
      write.csv(this_date_Fire,file = file.path(define_file_paths.fn("ProcessedData.directory"),predictor_sub_folder,name_no_suffix,paste(name_no_suffix,'_',this_date,'.csv',sep = "")),row.names = FALSE) # Write csv file
      print(paste("finished writing file for",this_date))
      rm(this_date,which_this_date,this_date_Fire)
    } # for (date_i in 1:length(all_dates)) { # cycle through all dates in file
  rm(all_dates,Fire_MODIS_data_step,this_date_format,name_no_suffix)
  } # for (file_i in 1:length(Fire_MODIS_file_name)) { # cycle through MODIS files
  rm(file_i,Fire_MODIS_file_name,predictor_sub_folder)
