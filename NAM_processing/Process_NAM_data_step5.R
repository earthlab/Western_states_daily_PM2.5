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

#### Source functions I've written ####
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))

#### Define Constants ####
NAM_folder <- "NAM_data" # define folder for NAM data
input_sub_folder <- "NAM_Step4" # define location of input files
output_sub_folder <- "NAM_Step5" # define location for output files
output_file_name <- paste("NAM_Step5_processed_",Sys.Date(),sep = "") # define name of output file

#### Load and Process Data ####
# determine which file from step 4 is most recent
file_name_pattern <- "\\.csv$" # only looking for .csv files (don't want to pick up the sub-folder)
this_file_list <- list.files(path = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,"."), pattern = file_name_pattern, all.files = FALSE,
                             full.names = FALSE, recursive = FALSE,
                             ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE) # get list of all .csv file in this folder
print(paste("There are ",length(this_file_list),"files for NAM Step 3 data")) # optional output statement
date_list <- unlist(lapply(this_file_list, function(x){ # start lapply and start defining function used in lapply
  processed_date <- substr(x,nchar(x)-13,nchar(x)-4) # identify the time stamp for the file in this iteration
  return(processed_date) # return the new file name so a new list of files can be created
}))
recent_processed_date <- max(as.Date(date_list)) # which date is the most recent file
which_recent_file <- which(date_list == recent_processed_date) # locate the file name for the most recent file
recent_file_name <- this_file_list[which_recent_file] # most recent file name
print(paste(recent_file_name,"is the most recent file and will be used"))
# load the data created in step 3, which has all of the observations for the 4 timesteps per day in one data frame
Step4_NAM_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,input_sub_folder,recent_file_name)) # open data file
rm(file_name_pattern,this_file_list,date_list,recent_processed_date,which_recent_file,recent_file_name) # clear variables

Step4_NAM_data$Latitude <- round(Step4_NAM_data$Latitude,5)
Step4_NAM_data$Longitude <- round(Step4_NAM_data$Longitude,5)
Step4_NAM_data$Local.Date <- as.Date(Step4_NAM_data$Local.Date)
Step4_NAM_data$Local.Date.Time <- as_datetime(Step4_NAM_data$Local.Date.Time)
Step4_NAM_data$TimeZone <- as.character(Step4_NAM_data$TimeZone)

# # load the data created in step 4, which has all of the observations for the 4 timesteps per day in one data frame
# part_number <- "bc"
# Step3_NAM_data <- read.csv(file.path(ProcessedData.directory,sub_folder,paste("NAM_Step3_part_",part_number,".csv",sep = ""))) # open data file
# stop("read in step4 data instead of step3")
# Step4_NAM_data <- Step3_NAM_data
# stop("remove line 7")
# Step4_NAM_data$Local.Date.Time <- Step4_NAM_data$UTC.Date.Time
# stop("remove line 9")
# Step4_NAM_data$Local.Date <- date(Step4_NAM_data$Local.Date.Time)
# stop("remove line 11")

# load information about meteo variables
this_source_file <- paste("MeteoVariablesNAM.csv")
MeteoVarsMultiType <- read.csv(file.path(define_file_paths.fn("NAM_Code.directory"),this_source_file))
# grab the list of relevant meteo variables for this file type from MeteoVars
which_meteo <- which(MeteoVarsMultiType$file_type == "grib2") # get grib2 files because grib1 files will be converted to grib2
MeteoVars <- MeteoVarsMultiType[which_meteo,] # matrix with just the relevant rows

All_date_loc <- unique(Step4_NAM_data[ ,c("Local.Date","Latitude","Longitude")]) # get a list of dates
# # Create data frame for output data
#Step5_NAM_data <- data.frame(matrix(NA,nrow=dim(All_date_loc)[1],ncol=length(colnames(Step4_NAM_data)))) # create data frame for input_mat1
#names(Step5_NAM_data) <- colnames(Step4_NAM_data) # assign the header to input_mat1
# # drop extraneous columns that don't apply to 24-hr data
#drop_cols <- c("Time.UTC","Date","Local.Date.Time","old_lon","old_lat","old_Datum") # define unnecessary columns
# Step5_NAM_data <- Step5_NAM_data[ , !(names(Step5_NAM_data) %in% drop_cols)] # drop unnecessary columns
#Step5_NAM_data$Time.UTC <- NULL
#Step5_NAM_data$Date <- NULL
#Step5_NAM_data$Local.Date.Time <- NULL
#Step5_NAM_data$Local.Date <- as.Date(Step5_NAM_data$Local.Date)

NAM_data_list <- lapply(1:dim(All_date_loc)[1], function(x){ # x <- 1
#NAM_data_list <- lapply(1:10, function(x){ # x <- 1
  # find all data points with this date/loc
  which_this_date_loc <- which(Step4_NAM_data$Local.Date == All_date_loc[x, c("Local.Date")] & Step4_NAM_data$Latitude == All_date_loc[x, c("Latitude")] & Step4_NAM_data$Longitude == All_date_loc[x, c("Longitude")])
  #which_this_date <- which(Step4_NAM_data$Local.Date == All_da)
  this_date_loc <- Step4_NAM_data[which_this_date_loc, ]
  if (length(which_this_date_loc)>4) {stop("Check code and data - should not have more than 4 NAM data points for given day/location")}
  
  #Step5_NAM_data <- data.frame(matrix(NA,nrow=dim(All_date_loc)[1],ncol=length(colnames(Step4_NAM_data)))) # create data frame for input_mat1
  Step5_NAM_data <- data.frame(matrix(NA,nrow=1,ncol=length(colnames(Step4_NAM_data)))) # create data frame for input_mat1
  
  names(Step5_NAM_data) <- colnames(Step4_NAM_data) # assign the header to input_mat1
  # drop extraneous columns that don't apply to 24-hr data
  drop_cols <- c("Time.UTC","Date","Local.Date.Time","UTC.Date.Time") # define unnecessary columns
  Step5_NAM_data <- Step5_NAM_data[ , !(names(Step5_NAM_data) %in% drop_cols)] # drop unnecessary columns
  
  Step5_NAM_data[1, c("Latitude","Longitude",  "TimeZone")] <- unique(this_date_loc[ , c("Latitude","Longitude",  "TimeZone")])
  Step5_NAM_data$Local.Date <- unique(this_date_loc$Local.Date)
  
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
    Step5_NAM_data[1, this_col_name] <- this_meteo_value

  } # for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
  return(Step5_NAM_data)
  }) # end lapply command

NAM_data <- do.call("rbind", NAM_data_list) #concatinate the output from each iteration

# write step 4 data to csv file
write.csv(NAM_data,file = file.path(define_file_paths.fn("ProcessedData.directory"),NAM_folder,output_sub_folder,paste(output_file_name,".csv",sep = "")),row.names = FALSE) # write data to file

# clear variables
rm(NAM_data,NAM_folder,input_sub_folder,output_sub_folder,output_file_name,working.directory)
