# De-Duplicate PM2.5 Observations

##### Create Sink output file ####
# sink command sends R output to a file. 
# Don't try to open file until R has closed it at end of script. 
# https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
SinkFileName=file.path(ProcessedData.directory,"DeDuplicate_ML_Input_File_sink.txt")
sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
sink() # comment
cat("output for DeDuplicate_ML_Input_File.R \n \n")

#### Set Tolerances/constants ####
given_digits <- 0.000001 # 0.00000001
lat_tolerance_threshold <- given_digits #0#0.00005
lon_tolerance_threshold <- given_digits #0#0.00005

#### Load Data file ####
input_file <- file.path(ProcessedData.directory,'reprojected_ML_input.csv')
print(paste("loading input file: ",input_file,sep = ""))
input_mat3 <- read.csv(input_file,header=TRUE, stringsAsFactors=FALSE)

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"Combine_true_replicates_R_function.R"))
source(file.path(writingcode.directory,"fill_in_aves_coloc_unique_PC_POC_MN_function.R"))  #"Input_de-duplicates_into_input_mat_functions.R"))
source(file.path(writingcode.directory,"set_data_types_by_column_R_function.R"))
source(file.path(writingcode.directory,"concatinate_within_column_function.R"))
#### Start multiple Input files for machine learning based on different ways of combining duplicate data ####
input_header <-  colnames(input_mat3)
N_columns <- length(input_header) # how many columns are in header?

# data just taking average of multiple obs at a location
# create data frame for input_mat_4_aves
input_mat4_aves <- data.frame(matrix(NA, nrow = 10, ncol = N_columns)) 
names(input_mat4_aves) <- input_header # assign the header to input_mat_4_aves
rstart_aves <- 1 # start counter

# data that keeps data from co-located monitors separate and just combines data that are 
input_mat4_colocated <- data.frame(matrix(NA, nrow = 10, ncol = N_columns)) # create data frame for input_mat_4_aves
names(input_mat4_colocated) <- input_header # assign the header to input_mat_4_aves
rstart_colocated <- 1 # start counter

rm(N_columns) # clear variable

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

#### Loop through all stations with EPA codes ####
print("starting loop through all stations for which we have EPA codes")
#stop("start at this_station_i <- 15 ... crashes there")
#for (this_station_i in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)
for (this_station_i in 477:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)
  this_station <- unique_EPA_Codes[this_station_i,] # what is the code for this station?
  # find which rows in input_mat correspond to this station
  which_this_station <- which(known_EPA_Code_data$State_Code == this_station$State_Code & 
                                known_EPA_Code_data$County_Code == this_station$County_Code & 
                                known_EPA_Code_data$Site_Num == this_station$Site_Num)
  # create data frame with this station's data
  this_station_data <- known_EPA_Code_data[which_this_station,] 

  # how many unique days are in this data?
  unique_days <- unique(this_station_data$Date_Local)
  
  # describe this station and it's data
  print(paste("station_i ", this_station_i, ": Station ", this_station$State_Code,"-",
              this_station$County_Code,"-",this_station$Site_Num," has ",
              length(which_this_station)," rows of data among ",length(unique_days),
              " unique days.",sep = ""))
  
  rm(which_this_station)

  # determine whether there were multiple monitors ever operating at this site (or duplicate data)
  if (length(unique_days)==dim(this_station_data)[1] & 
      length(unique(this_station_data$Data_Source_Name_Short))==1) { # determine whether there were multiple monitors ever operating at this site (or duplicate data)
    print("Each day of data for this station has only one monitor operating and there is no duplicate data.") 
    
    # aves data
    rstop_aves <- rstart_aves + dim(this_station_data)[1]-1
    input_mat4_aves[rstart_aves:rstop_aves, ] <- this_station_data
    rstart_aves <- rstop_aves+1
    
    # colocated data
    rstop_colocated <- rstart_colocated + dim(this_station_data)[1]-1
    input_mat4_colocated[rstart_colocated:rstop_colocated, ] <- this_station_data
    rstart_colocated <- rstop_colocated+1
    
  } else { # if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) there is duplicate data
    print("there is duplicate data")
    
    # cycle through days
    for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
      # get data for this day
      this_day <- unique_days[this_day_i] # get the date
      # find the rows for this day
      which_this_day <- which(this_station_data$Date_Local == this_day) 
      this_day_all_data <- this_station_data[which_this_day, ]
      #print(paste("Station ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num," has ",
      #            length(which_this_day)," rows of data on ",this_day,".",sep = ""))
      
      # call function of repeat entries of the same observations (usually event type is different) 
      # function to combine rows that are from the same source and have the same concentration (usually event type is the only/main difference)
      #this_day_all_data_combine_true_duplicates  <- Combine_true_replicates_R.fn(this_day_all_data)
      this_day_all_combined_true_dup  <- Combine_true_replicates_R.fn(this_day_all_data)
      
      # call function to fill in PM2.5 data
      #output_list <- fill_in_aves_coloc_unique_PC_POC_MN.fn(this_day_all_data_combine_true_duplicates,input_mat4_aves,rstart_aves,input_mat4_colocated,rstart_colocated,lat_tolerance_threshold,lon_tolerance_threshold)
      output_list <- fill_in_aves_coloc_unique_PC_POC_MN.fn(this_day_all_combined_true_dup,
                     input_mat4_aves,rstart_aves,input_mat4_colocated,rstart_colocated,
                     lat_tolerance_threshold,lon_tolerance_threshold)
      
      # clear old versions of variables, which will be replaced with the output from the function
      rm(input_mat4_aves,rstart_aves,input_mat4_colocated,rstart_colocated)
      # get the variables out of the output_list from the function
      input_mat4_aves <- output_list[[1]]
      rstart_aves <- output_list[[2]]
      input_mat4_colocated <- output_list[[3]]
      rstart_colocated <- output_list[[4]]
  
      rm(this_day, which_this_day, this_day_all_data, this_day_all_combined_true_dup, output_list)
      
    } # for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
  } # } else { # if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) there is duplicate data
  #rm(this_station,this_station_data,unique_days) #UNCOMMENT
} # for (this_station_i in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)  

#### Create a data frame with just lat, lon, and date ####
four_cols_w_duplicates <- input_mat_4_aves[,c("PM2.5_Lat","PM2.5_Lon","Datum","Date_Local")]
four_cols_data <- four_cols_w_duplicates[!duplicated(four_cols_w_duplicates),]
names(four_cols_data) <- c("Latitude","Longitude","Datum","Date")
write.csv(four_cols_data,file = file.path(ProcessedData.directory,'Locations_Dates_of_PM25_Obs_DeDuplicate.csv'),row.names = FALSE)
rm(four_cols_data,four_cols_w_duplicates)

#### Create a data frame with just lat, lon, and date ####
three_cols_w_duplicates <- input_mat_4_aves[,c("PM2.5_Lat","PM2.5_Lon","Datum")]
three_cols_data <- three_cols_w_duplicates[!duplicated(three_cols_w_duplicates),]
names(three_cols_data) <- c("Latitude","Longitude","Datum","Date")
write.csv(three_cols_data,file = file.path(ProcessedData.directory,'Locations_PM25_Obs_from_deduplicate_script.csv'),row.names = FALSE)
rm(three_cols_data,three_cols_w_duplicates)

#### Write csv file ####
stop("output csv file for both versions of input_mat4")
#### Save cleaned file to .csv ####
print("summary of the data output by DeDuplicate_ML_Input_File.R:")
summary(input_mat4_aves) # give summary of current state of data
print("file names still included")
unique(input_mat4_aves$Source_File)
write.csv(input_mat3,file = file.path(ProcessedData.directory,'de_duplicated_aves_ML_input.csv'),row.names = FALSE)

#### Save cleaned file to .csv ####
print("summary of the data output by DeDuplicate_ML_Input_File.R:")
summary(input_mat4_aves) # give summary of current state of data
print("file names still included")
unique(input_mat4_aves$Source_File)
write.csv(input_mat3,file = file.path(ProcessedData.directory,'de_duplicated_aves_ML_input.csv'),row.names = FALSE)




#  ###############################################################################     
#       # is the data all from one source or multiple sources?
#       if (length(unique(this_day_all_data$Data_Source_Name_Short)) == 1) { # is the data all from one source or multiple sources?
#       #print("data from one source")
#       # since all of the data is from one source, it should have unique ParameterCode-POC-Method_Name combinations
#         unique_ParamCode_POC_method <- this_day_all_data[!duplicated(this_day_all_data[,c("Parameter_Code","POC","Method_Name")]),c("Parameter_Code","POC","Method_Name")] # figure out how many unique station-days are in the DEQ data
#         #print(unique_ParamCode_POC_method)
#         if (dim(unique_ParamCode_POC_method)[1]!=dim(this_day_all_data)[1]) { # make sure that Parameter_Code - POC combinations are unique
#           #stop("check data and code, was expecting unique Parameter_Code - POC - method combinations")
#         
#         # call function of repeat entries of the same observations (usually event type is different) 
#         this_day_all_data_temp  <- deduplicate.combine.eventtype.fn(this_day_all_data) # function to combine rows that are from the same source and have the same concentration (usually event type is the only/main difference)
#         
#         # check that data now has unique ParameterCode, POC, MethodName values  
#         unique_ParamCode_POC_method_try2 <- this_day_all_data_temp[!duplicated(this_day_all_data_temp[,c("Parameter_Code","POC","Method_Name")]),c("Parameter_Code","POC","Method_Name")] # figure out how many unique station-days are in the DEQ data
#         #print(unique_ParamCode_POC_method_try2)
#         if (dim(unique_ParamCode_POC_method_try2)[1]!=dim(this_day_all_data_temp)[1]) {stop("function did not yield unique ParameterCode/POC/MethodName combinations as expected. Check data and code.")}
#         rm(unique_ParamCode_POC_method_try2)
#         
#         # call function to fill in PM2.5 data
#         output_list <- fill_in_aves_coloc_unique_PC_POC_MN.fn(this_day_all_data_temp,input_mat4_aves,rstart_aves,input_mat4_colocated,rstart_colocated,lat_tolerance_threshold,lon_tolerance_threshold)
#         # clear old versions of variables, which will be replaced with the output from the function
#         rm(input_mat4_aves,rstart_aves,input_mat4_colocated,rstart_colocated,this_day_all_data_temp)
#         # get the variables out of the output_list from the function
#         input_mat4_aves <- output_list[[1]]
#         rstart_aves <- output_list[[2]]
#         input_mat4_colocated <- output_list[[3]]
#         rstart_colocated <- output_list[[4]]
#         
#           
#         } else { # if (dim(unique_ParamCode_POC_method)[1]!=dim(this_day_all_data)[1]) { # make sure that Parameter_Code - POC combinations are unique
#           #print("Parameter_Code-POC-method_Name combinations are unique and all of the data is from one source. Write code to integrate data.")
#           # call function to fill in PM2.5 data
#           output_list <- fill_in_aves_coloc_unique_PC_POC_MN.fn(this_day_all_data_in,input_mat4_aves,rstart_aves,input_mat4_colocated,rstart_colocated,lat_tolerance_threshold,lon_tolerance_threshold)
#           # clear old versions of variables, which will be replaced with the output from the function
#           rm(input_mat4_aves,rstart_aves,input_mat4_colocated,rstart_colocated)
#           # get the variables out of the output_list from the function
#           input_mat4_aves <- output_list[[1]]
#           rstart_aves <- output_list[[2]]
#           input_mat4_colocated <- output_list[[3]]
#           rstart_colocated <- output_list[[4]]
# 
#         } # if (dim(unique_ParamCode_POC_method)[1]!=dim(this_day_all_data)[1]) { # make sure that Parameter_Code - POC combinations are unique
#           
#          # } # if (dim(unique_ParamCode_POC_method)[1]!=dim(this_day_all_data)[1]) { # make sure that Parameter_Code - POC combinations are unique
#       } else if (length(unique(this_day_all_data$Data_Source_Name_Short)) > 1) { # if (length(unique(this_day_all_data$Data_Source_Name_Short)) == 1) { # is the data all from one source or multiple sources?
#         stop("data from multiple sources - write code")
#         
#         stop("pick up writing code here")
#         # call function of repeat entries of the same observations (usually event type is different) 
#         this_day_all_data_temp  <- deduplicate.combine.eventtype.fn(this_day_all_data) # function to combine rows that are from the same source and have the same concentration (usually event type is the only/main difference)
#         
#         # check that data now has unique ParameterCode, POC, MethodName values  
#         unique_ParamCode_POC_method_try2 <- this_day_all_data_temp[!duplicated(this_day_all_data_temp[,c("Parameter_Code","POC","Method_Name")]),c("Parameter_Code","POC","Method_Name")] # figure out how many unique station-days are in the DEQ data
#         print(unique_ParamCode_POC_method_try2)
#         if (dim(unique_ParamCode_POC_method_try2)[1]!=dim(this_day_all_data_temp)[1]) {stop("function did not yield unique ParameterCode/POC/MethodName combinations as expected. Check data and code.")}
#         rm(unique_ParamCode_POC_method_try2)
#         
#         # call function to fill in PM2.5 data
#         output_list <- fill_in_aves_coloc_unique_PC_POC_MN.fn(this_day_all_data_temp,input_mat4_aves,rstart_aves,input_mat4_colocated,rstart_colocated,lat_tolerance_threshold,lon_tolerance_threshold)
#         # clear old versions of variables, which will be replaced with the output from the function
#         rm(input_mat4_aves,rstart_aves,input_mat4_colocated,rstart_colocated,this_day_all_data_temp)
#         # get the variables out of the output_list from the function
#         input_mat4_aves <- output_list[[1]]
#         rstart_aves <- output_list[[2]]
#         input_mat4_colocated <- output_list[[3]]
#         rstart_colocated <- output_list[[4]]
#         
#         
#       } else { # if (length(unique(this_day_all_data$Data_Source_Name_Short)) == 1) { # is the data all from one source or multiple sources?
#         stop("unexpected result when checking if data is from one or multiple sources - check data and code")
#         } # if (length(unique(this_day_all_data$Data_Source_Name_Short)) == 1) { # is the data all from one source or multiple sources?
#     } # for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
#   } # if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) { # determine whether there were multiple monitors ever operating at this site (or duplicate data)
# } # for (this_station_i in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)
#   
#   stop("write code to incorporate data from unknown_EPA_Code_data")
# 
#   stop("output location/date list: ")