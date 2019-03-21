# De-Duplicate PM2.5 Observations

#### Clear variables and sinks; define working directory ####
rm(list  =  ls()) # clear all variables
options(warn  =  2) # throw an error when there's a warning and stop the code from running further
if (max(dev.cur())>1) { # make sure it isn't currently outputting to any figure files
  dev.off(which  =  dev.cur()) # stop outputting to figure files
} # if (max(dev.cur())>1) { # make sure it isn't currently outputting to any figure files
while (sink.number()>0) { # make sure it isn't currently outputting to any text files
  sink() # stop outputting to text files
} # while (sink.number()>0) { # make sure it isn't currently outputting to any text files
working.directory  <-  "/home/rstudio" # define working directory
setwd(working.directory) # set working directory

#### Start Timer ####
start_code_timer <- proc.time() # start timer for code
print(paste("Start Process_PM25_data_step4_parallel.R at",Sys.time(),sep = " ")) # output start time

#### Call Packages (Library) ####
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

#### Source Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"Combine_true_replicates_R_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"fill_input_mat_aves_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"concatinate_within_column_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"PM25_station_deduplicate_aves_parallel_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"prioritize_daily_obs_over_hourly_function.R"))

functions_list <- c("input_mat_change_data_classes.fn","Combine_true_replicates_R.fn", "fill_input_mat_aves.fn",
                "concatinate_within_column.fn", "PM25_station_deduplicate_aves_parallel.fn","concatinate_vector_of_strings.fn") #,"separate_AQS_Site_ID_data.fn")

#### Define constants and file names ####
processed_data_version <- define_study_constants.fn("processed_data_version") # determine data version (batch)
this_source_file <- paste("PM25_Step3_part_",processed_data_version,"_NAD83.csv",sep = "") # define file name
Locations_file <- paste("PM25_Step3_part_",processed_data_version,"_Locations_NAD83.csv",sep = "") # define file name
print(this_source_file) # output file name
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "") # define sub-folder name
given_digits <- define_study_constants.fn("round_lat_lon_digits") # determine number of decimal places set for rounding lat/lon numbers
ProcessedData.directory <- define_file_paths.fn("ProcessedData.directory") # define directory path

# Create Sink output file #
file_sub_label <- paste("PM25_Step4_part_",processed_data_version,sep = "") # define part of file name
SinkFileName=file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,"_sink.txt",sep = "")) # define full file name
sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # start output to text file
cat("output for Process_PM25_data_step4_parallel.R \n \n") # text for file
cat("Source file:") # text for file
cat(this_source_file) # text for file

#### Load Data file ####
print(paste("loading input file: ",this_source_file,sep = "")) # text for file
input_mat3 <- read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file),header=TRUE, stringsAsFactors=FALSE) # read step 3 full data file
input_mat3 <- input_mat_change_data_classes.fn(input_mat3) # set variable classes
input_mat3$Lat <- round(input_mat3$Lat,digits = given_digits) # this was rounded in step 3, but rounding again since the file was read in again
input_mat3$Lon <- round(input_mat3$Lon,digits = given_digits) # this was rounded in step 3, but rounding again since the file was read in again

# load locations file #
Locations_input_mat3 <- read.csv(file.path(ProcessedData.directory,sub_folder,Locations_file),header=TRUE, stringsAsFactors=FALSE) # read step 3 locations file
n_locations <- dim(Locations_input_mat3)[1] # determine the number of locations

#### Set up for parallel processing ####
n_cores <- detectCores() - 1 # Calculate the number of cores
print(paste(n_cores,"cores available for parallel processing",sep = " "))

# Initiate cluster
this_cluster <- makeCluster(n_cores) # # Initiate cluster

# export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
#clusterExport(cl = this_cluster, varlist = c(functions_list,"ProcessedData.directory","sub_folder", 
#                                             "input_mat3","Locations_input_mat3","given_digits"), envir = .GlobalEnv)

# send necessary libraries to each parallel worker
#clusterEvalQ(cl = this_cluster, library(rNOMADS)) # copy this line and call function again if another library is needed

#### Take average concentration at locations ####
# run function PM25_station_deduplicate_aves_parallel.fn in parallel
de_duplication_method <- "averages"
clusterExport(cl = this_cluster, varlist = c(functions_list,"ProcessedData.directory","sub_folder", 
                                             "input_mat3","Locations_input_mat3","given_digits",
                                             "de_duplication_method"), envir = .GlobalEnv) # export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
#test_locations <- 6965:6972#6972#1500#1289 #1276:1291#801:900#701:800#701:800#543:572 #444:452#450#460#441:500 #REMOVE
#X = 1:n_locations
n_locations <- 100 #COMMENT
par_out_aves <- parLapply(this_cluster,X = 1:n_locations, fun = PM25_station_deduplicate_aves_parallel.fn ) # call parallel function

input_mat4_aves <- do.call("rbind", par_out_aves) #concatinate the output from each iteration
input_mat4_aves <- input_mat_change_data_classes.fn(input_mat4_aves) # reset variable classes
write.csv(input_mat4_aves,file = file.path(ProcessedData.directory,sub_folder,paste('PM25_Step4_part_',processed_data_version,'_de_duplicated_aves_ML_input.csv',sep = "")),row.names = FALSE) # Write csv file
# output summary of data:
print("summary of input_mat4_aves output by Process_PM25_data_step4_parallel.R:")
summary(input_mat4_aves) # give summary of current state of data
print("file names still included")
unique(input_mat4_aves$Source_File)
rm(par_out_aves,input_mat4_aves,de_duplication_method) # clear variables

#### Stop Cluster and restart
stopCluster(this_cluster) # stop the cluster
this_cluster <- makeCluster(n_cores) # # Initiate cluster

#### Take average concentration at locations - Prefer daily observations over hourly ####
# run function PM25_station_deduplicate_aves_parallel.fn in parallel
de_duplication_method <- "prioritize_24Hour_Obs"
clusterExport(cl = this_cluster, varlist = c(functions_list,"ProcessedData.directory","sub_folder", 
                                             "input_mat3","Locations_input_mat3","given_digits",
                                             "de_duplication_method","prioritize_daily_obs_over_hourly.fn"), envir = .GlobalEnv) # export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
par_out_aves <- parLapply(this_cluster,X = 1:n_locations, fun = PM25_station_deduplicate_aves_parallel.fn ) # call parallel function
input_mat4_aves <- do.call("rbind", par_out_aves) #concatinate the output from each iteration
input_mat4_aves <- input_mat_change_data_classes.fn(input_mat4_aves) # reset variable classes
write.csv(input_mat4_aves,file = file.path(ProcessedData.directory,sub_folder,paste('PM25_Step4_part_',processed_data_version,'_de_duplicated_aves_ML_input.csv',sep = "")),row.names = FALSE) # Write csv file
# output summary of data:
print("summary of input_mat4_aves output by Process_PM25_data_step4_parallel.R:")
summary(input_mat4_aves) # give summary of current state of data
print("file names still included")
unique(input_mat4_aves$Source_File)
rm(par_out_aves,input_mat4_aves) # clear variables

#### End use of parallel computing #####
stopCluster(this_cluster) # stop the cluster
print(paste("Process_PM25_data_step4_parallel.R completed at",Sys.time(),sep = " ")) # print time of completion to sink file
proc.time() - start_code_timer # stop the timer
rm(start_code_timer, this_cluster) # clear variables

## Kept for reference and for trouble-shooting code:
## serial version of code
#while (sink.number()>0) {
#  sink()
#} # while (sink.number()>0) {
#test_locations <- 1:20
#for (X in test_locations) {
#  print("X = ")
#  print(X)
#  this_output <- PM25_station_deduplicate_aves_parallel.fn(X) # PM25_station_deduplicate_aves_parallel.fn(X)
#  rm(this_output)
#  } # for
