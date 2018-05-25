# De-Duplicate PLM2.5 Observations

##### Create Sink output file ####
# sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
SinkFileName=file.path(ProcessedData.directory,"DeDuplicate_ML_Input_File_sink.txt")
#sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
cat("output for DeDuplicate_ML_Input_File.R \n \n")

#### Set Tolerances/constants ####
lat_tolerance_threshold <- 0.00005
lon_tolerance_threshold <- 0.00005

#### Load Data file ####
input_file <- file.path(ProcessedData.directory,'cleaned_ML_input.csv')
print(paste("loading input file: ",input_file,sep = ""))
#input_mat2 <- read.csv(file.path(ProcessedData.directory,'cleaned_ML_input.csv'),header=TRUE)
input_mat2 <- read.csv(input_file,header=TRUE, stringsAsFactors=FALSE)

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"Combine_true_replicates_R_functions.R"))
source(file.path(writingcode.directory,"Input_de-duplicates_into_input_mat.R"))
#source(file.path(writingcode.directory,"Try_Writing_R_functions.R"))
#source(file.path(writingcode.directory,"Second_function_script.R"))
#### Start multiple Input files for machine learning based on different ways of combining duplicate data ####
input_header <-  colnames(input_mat2)
N_columns <- length(input_header) # how many columns are in header?

# data just taking average of multiple obs at a location
input_mat3_aves <- data.frame(matrix(NA,nrow=10,ncol=N_columns)) # create data frame for input_mat1
names(input_mat3_aves) <- input_header # assign the header to input_mat1
rstart_aves <- 1 # start counter

# data that keeps data from co-located monitors separate and just combines data that are 
input_mat3_colocated <- data.frame(matrix(NA,nrow=10,ncol=N_columns)) # create data frame for input_mat1
names(input_mat3_colocated) <- input_header # assign the header to input_mat1
rstart_colocated <- 1 # start counter

rm(N_columns)

#### Separate data that might have duplicate obs from others that might be harder to tell (or don't) ####
# identify rows with known state code, county code, and site num, which together comprise the EPA code
which_known_EPA_Code <- which(!is.na(input_mat2$State_Code) & !is.na(input_mat2$County_Code) & !is.na(input_mat2$Site_Num) & !is.na(input_mat2$Parameter_Code) & !is.na(input_mat2$POC))
print(paste(length(which_known_EPA_Code)/dim(input_mat2)[1]*100,"% of rows in input_mat2 have known EPA codes",sep = ""))
which_unknown_EPA_Code <- which(is.na(input_mat2$State_Code) | is.na(input_mat2$County_Code) | is.na(input_mat2$Site_Num) | is.na(input_mat2$Parameter_Code) | is.na(input_mat2$POC))
print(paste(length(which_unknown_EPA_Code)/dim(input_mat2)[1]*100,"% of rows in input_mat2 have unknown EPA codes",sep = ""))
if (length(which_known_EPA_Code)+length(which_unknown_EPA_Code) != dim(input_mat2)[1]) {stop("Number of rows not adding up")} # check that number of rows makes sense

# create new data frames separating known and unknown EPA codes
known_EPA_Code_data <- input_mat2[which_known_EPA_Code,]
unknown_EPA_Code_data <- input_mat2[which_unknown_EPA_Code,]
rm(input_mat2,which_known_EPA_Code,which_unknown_EPA_Code)

# figure out how many unique EPA codes are in the data
Codes_only_repeats <- data.frame(matrix(NA, nrow = dim(known_EPA_Code_data)[1], ncol = 3)) # create data frame with only EPA codes
names(Codes_only_repeats) <- c("State_Code","County_Code","Site_Num") # create header
Codes_only_repeats <- known_EPA_Code_data[,c("State_Code","County_Code","Site_Num")]
unique_EPA_Codes <- Codes_only_repeats[!duplicated(Codes_only_repeats[,1:3]),]
print(paste("There are ",dim(unique_EPA_Codes)[1]," unique EPA codes (i.e. stations) in the data. (This includes slightly into bordering states.)",sep = ""))
rm(Codes_only_repeats)

print("starting loop through all stations for which we have EPA codes")
for (this_station_i in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)
  this_station <- unique_EPA_Codes[this_station_i,]
  
  which_this_station <- which(known_EPA_Code_data$State_Code == this_station$State_Code & known_EPA_Code_data$County_Code == this_station$County_Code & known_EPA_Code_data$Site_Num == this_station$Site_Num)
  this_station_data <- known_EPA_Code_data[which_this_station,]

  # how many unique days are in this data?
  unique_days <- unique(this_station_data$Date_Local)
  
  print(paste(this_station_i," Station ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num," has ",
              length(which_this_station)," rows of data among ",length(unique_days)," unique days.",sep = ""))
  
  rm(which_this_station)
  
  if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) { # determine whether there were multiple monitors ever operating at this site (or duplicate data)
    print("Each day of data for this station has only one monitor operating and there is no duplicate data.") 
    
    # aves data
    rstop_aves <- rstart_aves + dim(this_station_data)[1]-1
    input_mat3_aves[rstart_aves:rstop_aves,] <- this_station_data
    rstart_aves <- rstop_aves+1
    
    # colocated data
    rstop_colocated <- rstart_colocated + dim(this_station_data)[1]-1
    input_mat3_colocated[rstart_colocated:rstop_colocated,] <- this_station_data
    rstart_colocated <- rstop_colocated+1
    
  } else { # if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) there is duplicate data
    print("there is duplicate data")
    
    # cycle through days
    for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
      # get data for this day
      this_day <- unique_days[this_day_i]
      which_this_day <- which(this_station_data$Date_Local == this_day)
      this_day_all_data <- this_station_data[which_this_day,]
      print(paste("Station ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num," has ",
                  length(which_this_day)," rows of data on ",this_day,".",sep = ""))
      
      # call function of repeat entries of the same observations (usually event type is different) 
      this_day_all_data_combine_true_duplicates  <- deduplicate.combine.eventtype.fn(this_day_all_data) # function to combine rows that are from the same source and have the same concentration (usually event type is the only/main difference)

      # call function to fill in PM2.5 data
      output_list <- fill_in_aves_coloc_unique_PC_POC_MN.fn(this_day_all_data_combine_true_duplicates,input_mat3_aves,rstart_aves,input_mat3_colocated,rstart_colocated,lat_tolerance_threshold,lon_tolerance_threshold)
      # clear old versions of variables, which will be replaced with the output from the function
      rm(input_mat3_aves,rstart_aves,input_mat3_colocated,rstart_colocated,this_day_all_data_combine_true_duplicates)
      # get the variables out of the output_list from the function
      input_mat3_aves <- output_list[[1]]
      rstart_aves <- output_list[[2]]
      input_mat3_colocated <- output_list[[3]]
      rstart_colocated <- output_list[[4]]

    } # for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
  } # } else { # if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) there is duplicate data
  #rm(this_station,this_station_data,unique_days) #UNCOMMENT
} # for (this_station_i in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)  
      
 ###############################################################################     
      # is the data all from one source or multiple sources?
      if (length(unique(this_day_all_data$Data_Source_Name_Short)) == 1) { # is the data all from one source or multiple sources?
      #print("data from one source")
      # since all of the data is from one source, it should have unique ParameterCode-POC-Method_Name combinations
        unique_ParamCode_POC_method <- this_day_all_data[!duplicated(this_day_all_data[,c("Parameter_Code","POC","Method_Name")]),c("Parameter_Code","POC","Method_Name")] # figure out how many unique station-days are in the DEQ data
        #print(unique_ParamCode_POC_method)
        if (dim(unique_ParamCode_POC_method)[1]!=dim(this_day_all_data)[1]) { # make sure that Parameter_Code - POC combinations are unique
          #stop("check data and code, was expecting unique Parameter_Code - POC - method combinations")
        
        # call function of repeat entries of the same observations (usually event type is different) 
        this_day_all_data_temp  <- deduplicate.combine.eventtype.fn(this_day_all_data) # function to combine rows that are from the same source and have the same concentration (usually event type is the only/main difference)
        
        # check that data now has unique ParameterCode, POC, MethodName values  
        unique_ParamCode_POC_method_try2 <- this_day_all_data_temp[!duplicated(this_day_all_data_temp[,c("Parameter_Code","POC","Method_Name")]),c("Parameter_Code","POC","Method_Name")] # figure out how many unique station-days are in the DEQ data
        #print(unique_ParamCode_POC_method_try2)
        if (dim(unique_ParamCode_POC_method_try2)[1]!=dim(this_day_all_data_temp)[1]) {stop("function did not yield unique ParameterCode/POC/MethodName combinations as expected. Check data and code.")}
        rm(unique_ParamCode_POC_method_try2)
        
        # call function to fill in PM2.5 data
        output_list <- fill_in_aves_coloc_unique_PC_POC_MN.fn(this_day_all_data_temp,input_mat3_aves,rstart_aves,input_mat3_colocated,rstart_colocated,lat_tolerance_threshold,lon_tolerance_threshold)
        # clear old versions of variables, which will be replaced with the output from the function
        rm(input_mat3_aves,rstart_aves,input_mat3_colocated,rstart_colocated,this_day_all_data_temp)
        # get the variables out of the output_list from the function
        input_mat3_aves <- output_list[[1]]
        rstart_aves <- output_list[[2]]
        input_mat3_colocated <- output_list[[3]]
        rstart_colocated <- output_list[[4]]
        
          
        } else { # if (dim(unique_ParamCode_POC_method)[1]!=dim(this_day_all_data)[1]) { # make sure that Parameter_Code - POC combinations are unique
          #print("Parameter_Code-POC-method_Name combinations are unique and all of the data is from one source. Write code to integrate data.")
          # call function to fill in PM2.5 data
          output_list <- fill_in_aves_coloc_unique_PC_POC_MN.fn(this_day_all_data_in,input_mat3_aves,rstart_aves,input_mat3_colocated,rstart_colocated,lat_tolerance_threshold,lon_tolerance_threshold)
          # clear old versions of variables, which will be replaced with the output from the function
          rm(input_mat3_aves,rstart_aves,input_mat3_colocated,rstart_colocated)
          # get the variables out of the output_list from the function
          input_mat3_aves <- output_list[[1]]
          rstart_aves <- output_list[[2]]
          input_mat3_colocated <- output_list[[3]]
          rstart_colocated <- output_list[[4]]

        } # if (dim(unique_ParamCode_POC_method)[1]!=dim(this_day_all_data)[1]) { # make sure that Parameter_Code - POC combinations are unique
          
         # } # if (dim(unique_ParamCode_POC_method)[1]!=dim(this_day_all_data)[1]) { # make sure that Parameter_Code - POC combinations are unique
      } else if (length(unique(this_day_all_data$Data_Source_Name_Short)) > 1) { # if (length(unique(this_day_all_data$Data_Source_Name_Short)) == 1) { # is the data all from one source or multiple sources?
        stop("data from multiple sources - write code")
        
        stop("pick up writing code here")
        # call function of repeat entries of the same observations (usually event type is different) 
        this_day_all_data_temp  <- deduplicate.combine.eventtype.fn(this_day_all_data) # function to combine rows that are from the same source and have the same concentration (usually event type is the only/main difference)
        
        # check that data now has unique ParameterCode, POC, MethodName values  
        unique_ParamCode_POC_method_try2 <- this_day_all_data_temp[!duplicated(this_day_all_data_temp[,c("Parameter_Code","POC","Method_Name")]),c("Parameter_Code","POC","Method_Name")] # figure out how many unique station-days are in the DEQ data
        print(unique_ParamCode_POC_method_try2)
        if (dim(unique_ParamCode_POC_method_try2)[1]!=dim(this_day_all_data_temp)[1]) {stop("function did not yield unique ParameterCode/POC/MethodName combinations as expected. Check data and code.")}
        rm(unique_ParamCode_POC_method_try2)
        
        # call function to fill in PM2.5 data
        output_list <- fill_in_aves_coloc_unique_PC_POC_MN.fn(this_day_all_data_temp,input_mat3_aves,rstart_aves,input_mat3_colocated,rstart_colocated,lat_tolerance_threshold,lon_tolerance_threshold)
        # clear old versions of variables, which will be replaced with the output from the function
        rm(input_mat3_aves,rstart_aves,input_mat3_colocated,rstart_colocated,this_day_all_data_temp)
        # get the variables out of the output_list from the function
        input_mat3_aves <- output_list[[1]]
        rstart_aves <- output_list[[2]]
        input_mat3_colocated <- output_list[[3]]
        rstart_colocated <- output_list[[4]]
        
        
      } else { # if (length(unique(this_day_all_data$Data_Source_Name_Short)) == 1) { # is the data all from one source or multiple sources?
        stop("unexpected result when checking if data is from one or multiple sources - check data and code")
        } # if (length(unique(this_day_all_data$Data_Source_Name_Short)) == 1) { # is the data all from one source or multiple sources?
    } # for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
  } # if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) { # determine whether there were multiple monitors ever operating at this site (or duplicate data)
} # for (this_station_i in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)
  
  stop("write code to incorporate data from unknown_EPA_Code_data")