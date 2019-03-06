# De-Duplicate PM2.5 Observations

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
print(paste("Start Process_PM25_data_step5_parallel.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

#### Source Functions that I created ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"input_mat_functions.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"Combine_true_replicates_R_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"fill_input_mat_aves_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"concatinate_within_column_function.R"))
source(file.path(define_file_paths.fn("writingcode.directory"),"PM25_station_deduplicate_aves_parallel_function.R"))

functions_list <- c("input_mat_change_data_classes.fn","Combine_true_replicates_R.fn", "fill_input_mat_aves.fn",
                "concatinate_within_column.fn", "PM25_station_deduplicate_aves_parallel.fn")

#### define constants and file names ####
processed_data_version <- define_study_constants.fn("processed_data_version")
# file names
#this_source_file <- paste("PM25_Step3_part_",processed_data_version,"_Projected.csv",sep = "") # define file name
this_source_file <- paste("PM25_Step3_part_",processed_data_version,"_NAD83.csv",sep = "") # define file name
#Locations_file <- paste("PM25_Step3_part_",processed_data_version,"_Locations_Projected.csv",sep = "") # define file name
Locations_file <- paste("PM25_Step3_part_",processed_data_version,"_Locations_NAD83.csv",sep = "") # define file name
print(this_source_file)
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")

# Create Sink output file #
file_sub_label <- paste("PM25_Step4_part_",processed_data_version,sep = "")
SinkFileName=file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,"_sink.txt",sep = ""))
sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE)
cat("output for Process_PM25_data_step4_parallel.R \n \n")

cat("Source file:")
cat(this_source_file)

#### Set Tolerances/constants ####
given_digits <- define_study_constants.fn("round_lat_lon_digits")#("round_LatLon_digits") #0.00001 #0.000001 # 0.00000001
ProcessedData.directory <- define_file_paths.fn("ProcessedData.directory")

#### Load Data file ####
print(paste("loading input file: ",this_source_file,sep = ""))
input_mat3 <- read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file),header=TRUE, stringsAsFactors=FALSE)
input_mat3 <- input_mat_change_data_classes.fn(input_mat3)
input_mat3$Lat <- round(input_mat3$Lat,digits = given_digits) # this was rounded in step 3, but rounding again since the file was read in again
input_mat3$Lon <- round(input_mat3$Lon,digits = given_digits) # this was rounded in step 3, but rounding again since the file was read in again

# load locations file #
Locations_input_mat3 <- read.csv(file.path(ProcessedData.directory,sub_folder,Locations_file),header=TRUE, stringsAsFactors=FALSE)

#### Run the parallel loop ####
# Calculate the number of cores
n_cores <- detectCores() - 1
print(paste(n_cores,"cores available for parallel processing",sep = " "))

# Initiate cluster
this_cluster <- makeCluster(n_cores)

# export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
clusterExport(cl = this_cluster, varlist = c(functions_list,"ProcessedData.directory","sub_folder", 
                                             "input_mat3","Locations_input_mat3"), envir = .GlobalEnv)


# send necessary libraries to each parallel worker
#clusterEvalQ(cl = this_cluster, library(rNOMADS)) # copy this line and call function again if another library is needed

# run function loop_NAM_run_times.parallel.fn in parallel
n_locations <- dim(Locations_input_mat3)[1]
#X = 1:n_locations
#par_out_aves <- parLapply(this_cluster,X = 1:n_locations, fun = PM25_station_deduplicate_aves_parallel.fn )#,

# serial version of code
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
#for (X in 33:n_locations) {
for (X in c(33,104,368,376)) {
  print("X = ")
  print(X)
  this_output <- PM25_station_deduplicate_aves_parallel.fn(X)
  rm(this_output)
} # for

# #### concatinate the output from each iteration ####
input_mat4_aves <- do.call("rbind", par_out_aves)
input_mat4_aves <- input_mat_change_data_classes.fn(input_mat4_aves)

#input_mat4_aves_full <- rbind(input_mat5_aves,unknown_EPA_Code_data) # Recombine with observations that have unknown EPA code
 
#### Write csv files ####
# aves file
print("summary of input_mat4_aves output by Process_PM25_data_step4_parallel.R:")
summary(input_mat4_aves) # give summary of current state of data
print("file names still included")
unique(input_mat4_aves$Source_File)
write.csv(input_mat4_aves,file = file.path(ProcessedData.directory,sub_folder,paste('PM25_Step4_part_',processed_data_version,'_de_duplicated_aves_ML_input.csv',sep = "")),row.names = FALSE)
# 
# # now do co-located version
# par_out_colocated <- parLapply(this_cluster,X = 1:5, fun = loop_PM25_station_deduplicate.parallel.fn)#,
# #                     input_header = input_header, unique_EPA_Codes = unique_EPA_Codes)
# 
# #### concatinate the output from each iteration ####
# input_mat5_colocated <- do.call("rbind", par_output_colocated)
# input_mat5_colocated_full <- rbind(input_mat5_colocated,unknown_EPA_Code_data) # Recombine with observations that have unknown EPA code
# 
# # write colocated file
# print("summary of input_mat4_colocated output by DeDuplicate_ML_Input_File.R:")
# summary(input_mat5_colocated_full) # give summary of current state of data
# print("file names still included")
# unique(input_mat5_colocated_full$Source_File)
# write.csv(input_mat5_colocated_full,file = file.path(ProcessedData.directory,sub_folder,paste('PM25_Step5_part_',processed_data_version,'de_duplicated_colocated_ML_input.csv',sep = "")),row.names = FALSE)

# End use of parallel computing #
stopCluster(this_cluster)
rm(this_cluster)
# stop the timer
proc.time() - start_code_timer
rm(start_code_timer)