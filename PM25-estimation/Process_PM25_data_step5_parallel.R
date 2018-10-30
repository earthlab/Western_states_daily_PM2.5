# De-Duplicate PM2.5 Observations

print("run Define_directories.R before this script") 

# start timer for code
start_code_timer <- proc.time()
print(paste("Start Process_PM25_data_step5_parallel.R at",Sys.time(),sep = " "))

#### Call Packages (Library) ####
library(parallel) # see http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"Combine_true_replicates_R_function.R"))
source(file.path(writingcode.directory,"fill_in_aves_coloc_unique_PC_POC_MN_function.R"))  #"Input_de-duplicates_into_input_mat_functions.R"))
source(file.path(writingcode.directory,"set_data_types_by_column_R_function.R"))
source(file.path(writingcode.directory,"concatinate_within_column_function.R"))
source(file.path(writingcode.directory,"loop_PM25_station_deduplicate.parallel_function.R"))

funcions_list <- c("Combine_true_replicates_R.fn", "fill_in_aves_coloc_unique_PC_POC_MN.fn", 
                   "define_data_types_input_mat.fn", "concatinate_within_column.fn", "loop_PM25_station_deduplicate.parallel.fn")

#### define constants and file names ####
# file names
this_source_file <- paste("PM25_Step3_part_",processed_data_version,"_Projected.csv",sep = "") # define file name
sub_folder <- paste("PM25_data_part_",processed_data_version,sep = "")

# Create Sink output file #
# sink command sends R output to a file. 
# Don't try to open file until R has closed it at end of script. 
# https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
#SinkFileName=file.path(ProcessedData.directory,"DeDuplicate_ML_Input_File_sink.txt")
#sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
#sink() # comment
#cat("output for DeDuplicate_ML_Input_File.R \n \n")
file_sub_label <- paste("PM25_Step5_part_",processed_data_version,sep = "")
#SinkFileName=file.path(ProcessedData.directory,sub_folder,paste(file_sub_label,"_sink.txt",sep = ""))
#sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE)
cat("output for Process_PM25_data_step5.R \n \n")
cat("Source file:")
cat(this_source_file)

#### Set Tolerances/constants ####
#given_digits <- 0.000001 # 0.00000001
#lat_tolerance_threshold <- given_digits #0#0.00005
#lon_tolerance_threshold <- given_digits #0#0.00005

#### Load Data file ####
#input_file <- file.path(ProcessedData.directory,'reprojected_ML_input.csv')
print(paste("loading input file: ",this_source_file,sep = ""))
input_mat3 <- read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file),header=TRUE, stringsAsFactors=FALSE)

# over-write unprojected lat/lon
input_mat3$PM2.5_Lat <- NA
input_mat3$PM2.5_Lon <- NA
input_mat3$Datum <- NA

#### Start multiple Input files for machine learning based on different ways of combining duplicate data ####
input_header <-  colnames(input_mat3)
N_columns <- length(input_header) # how many columns are in header?

# # data just taking average of multiple obs at a location
# # create data frame for input_mat_4_aves
# input_mat4_aves <- data.frame(matrix(NA, nrow = 0, ncol = N_columns)) 
# names(input_mat4_aves) <- input_header # assign the header to input_mat_4_aves
# rstart_aves <- 1 # start counter
# 
# # data that keeps data from co-located monitors separate and just combines data that are 
# input_mat4_colocated <- data.frame(matrix(NA, nrow = 0, ncol = N_columns)) # create data frame for input_mat_4_aves
# names(input_mat4_colocated) <- input_header # assign the header to input_mat_4_aves
# rstart_colocated <- 1 # start counter
# 
# rm(N_columns) # clear variable

#### Separate data with complete EPA codes, which likely have duplicates, from others,
# which will be more difficult to tell.
# identify rows with known state code, county code, and site num, which together comprise the EPA code
which_known_EPA_Code <- which(!is.na(input_mat3$State_Code) & !is.na(input_mat3$County_Code) & !is.na(input_mat3$Site_Num) & !is.na(input_mat3$Parameter_Code) & !is.na(input_mat3$POC))
print(paste(length(which_known_EPA_Code)/dim(input_mat3)[1]*100,"% of rows in input_mat3 have known EPA codes",sep = ""))
which_unknown_EPA_Code <- which(is.na(input_mat3$State_Code) | is.na(input_mat3$County_Code) | is.na(input_mat3$Site_Num) | is.na(input_mat3$Parameter_Code) | is.na(input_mat3$POC))
print(paste(length(which_unknown_EPA_Code)/dim(input_mat3)[1]*100,"% of rows in input_mat3 have unknown EPA codes",sep = ""))
if (length(which_known_EPA_Code) + length(which_unknown_EPA_Code) != dim(input_mat3)[1]) { # check that number of rows makes sense
  stop("Number of rows not adding up")
  } # if (length(which_known_EPA_Code) + length(which_unknown_EPA_Code) != dim(input_mat3)[1]) { # check that number of rows makes sense

# create new data frames separating known and unknown EPA codes
known_EPA_Code_data <- input_mat3[which_known_EPA_Code,] # data with known codes
unknown_EPA_Code_data <- input_mat3[which_unknown_EPA_Code,] # data without known codes
rm(input_mat3,which_known_EPA_Code,which_unknown_EPA_Code) # clear variables

# figure out how many unique EPA codes are in the data
# create data frame with only EPA codes
Codes_only_repeats <- data.frame(matrix(NA, nrow = dim(known_EPA_Code_data)[1], ncol = 3))
names(Codes_only_repeats) <- c("State_Code","County_Code","Site_Num") # create header
# get the columns for the EPA codes from the input_mat
Codes_only_repeats <- known_EPA_Code_data[,c("State_Code","County_Code","Site_Num")]
# get rid of duplicates
unique_EPA_Codes <- Codes_only_repeats[!duplicated(Codes_only_repeats[,1:3]),]
print(paste("There are ", dim(unique_EPA_Codes)[1]," unique EPA codes (i.e. stations) in the data. (This includes slightly into bordering states.)",sep = ""))
rm(Codes_only_repeats) # clear variables

#### Run the parallel loop ####
# Calculate the number of cores
n_cores <- detectCores() - 1
print(paste(n_cores,"available for parallel processing",sep = " "))

# Initiate cluster
this_cluster <- makeCluster(n_cores)

# export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
clusterExport(cl = this_cluster, varlist = c(funcions_list,"ProcessedData.directory","sub_folder","input_header", "unique_EPA_Codes",
                                             "known_EPA_Code_data","N_columns"), envir = .GlobalEnv)

# send necessary librarys to each parallel worker
#clusterEvalQ(cl = this_cluster, library(rNOMADS)) # copy this line and call function again if another library is needed
#clusterEvalQ(cl = this_cluster, library(audio)) # copy this line and call function again if another library is needed

# run function loop_NAM_run_times.parallel.fn in parallel
n_stations <- dim(unique_EPA_Codes)[1]
#X = 1:n_stations
par_out <- parLapply(this_cluster,X = 1:5, fun = loop_PM25_station_deduplicate.parallel.fn)#,
#                     input_header = input_header, unique_EPA_Codes = unique_EPA_Codes)

# End use of parallel computing #
stopCluster(this_cluster)
rm(this_cluster)


# #### Create a data frame with just lat, lon, and date ####
#four_cols_w_duplicates <- input_mat4_aves[,c("PM2.5_Lat","PM2.5_Lon","Datum","Date_Local")]
#four_cols_data <- four_cols_w_duplicates[!duplicated(four_cols_w_duplicates),]
#names(four_cols_data) <- c("Latitude","Longitude","Datum","Date")
#write.csv(four_cols_data,file = file.path(ProcessedData.directory,'Locations_Dates_of_PM25_Obs_DeDuplicate.csv'),row.names = FALSE)
#rm(four_cols_data,four_cols_w_duplicates)

# #### Create a data frame with just lat, lon, and date ####
#three_cols_w_duplicates <- input_mat4_aves[,c("PM2.5_Lat","PM2.5_Lon","Datum")]
#three_cols_data <- three_cols_w_duplicates[!duplicated(three_cols_w_duplicates),]
#names(three_cols_data) <- c("Latitude","Longitude","Datum")
#write.csv(three_cols_data,file = file.path(ProcessedData.directory,'Locations_PM25_Obs_from_deduplicate_script.csv'),row.names = FALSE)
#rm(three_cols_data,three_cols_w_duplicates)

#### Recombine with observations that have unknown EPA code ####
input_mat4_aves_full <- rbind(input_mat4_aves,unknown_EPA_Code_data)
input_mat4_colocated_full <- rbind(input_mat4_colocated,unknown_EPA_Code_data)
rm(input_mat4_aves,input_mat4_colocated,unknown_EPA_Code_data)

#### Write csv files ####
# aves file
print("summary of input_mat4_aves output by DeDuplicate_ML_Input_File.R:")
summary(input_mat4_aves_full) # give summary of current state of data
print("file names still included")
unique(input_mat4_aves_full$Source_File)
write.csv(input_mat4_aves_full,file = file.path(ProcessedData.directory,sub_folder,paste('PM25_Step5_part_',processed_data_version,'_de_duplicated_aves_ML_input.csv',sep = "")),row.names = FALSE)

# colocated file
print("summary of input_mat4_colocated output by DeDuplicate_ML_Input_File.R:")
summary(input_mat4_colocated_full) # give summary of current state of data
print("file names still included")
unique(input_mat4_colocated_full$Source_File)
write.csv(input_mat4_colocated_full,file = file.path(ProcessedData.directory,sub_folder,paste('PM25_Step5_part_',processed_data_version,'de_duplicated_colocated_ML_input.csv',sep = "")),row.names = FALSE)
