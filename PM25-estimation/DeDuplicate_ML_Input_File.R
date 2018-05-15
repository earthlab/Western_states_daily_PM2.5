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
source(file.path(writingcode.directory,"Try_Writing_R_functions.R"))
source(file.path(writingcode.directory,"Second_function_script.R"))
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
  
  print(paste("Station ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num," has ",
              length(which_this_station)," rows of data among ",length(unique_days)," unique days.",sep = ""))
  
  rm(which_this_station)
  
  if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) { # determine whether there were multiple monitors ever operating at this site (or duplicate data)
    #print("Each day of data for this station has only one monitor operating and there is no duplicate data.") 
    
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
      #print(paste("Station ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num," has ",
      #            length(which_this_day)," rows of data on ",this_day,".",sep = ""))
      
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
  
  

# ############### Commented/Obsolete code ####
# #    # cycle through days
# #    for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
# #      # get data for this day
# #      this_day <- unique_days[this_day_i]
# #      which_this_day <- which(this_station_data$Date_Local == this_day)
# #      this_day_all_data <- this_station_data[which_this_day,]
# #      print(paste("Station ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num," has ",
# #                  length(which_this_day)," rows of data on ",this_day,".",sep = ""))
#     
# #      # is the data all from one source or multiple sources?
# #      if (length(unique(this_day_all_data$Data_Source_Name_Short)) == 1) { # is the data all from one source or multiple sources?
# #        # data for this day can all be from one source
# #        print("one source of data for this day")
#         # verify that there are no ParameterCode/POC repeats
# 
#         print(this_day_all_data[,c("Parameter_Code","POC")])
#         unique_ParamCode_POC <- this_day_all_data[!duplicated(this_day_all_data[,c("Parameter_Code","POC")]),c("Parameter_Code","POC")] # figure out how many unique station-days are in the DEQ data
#         print(unique_ParamCode_POC)
#         
#         if (dim(unique_ParamCode_POC)[1] != dim(this_day_all_data)[1]) { # is every ParameterCode/POC combination unique?
#           stop("Check Code and Data. Is a ParameterCode/POC combination repeated within the same data source or unknown POC/ParameterCodes?")
#           } else { # if (dim(unique_ParamCode_POC)[1] != dim(this_day_all_data)[1]) { # is every ParameterCode/POC combination unique?
#             set_plot_color <- "azure3"
#             #### for the aves data - 2 data points, one is 88101 and one is 88502  ####
#             rstop_aves <- rstart_aves
#             # PM2.5 Obs (concentration):take average
#             input_mat3_aves[rstart_aves:rstop_aves,c("PM2.5_Obs")] <- as.numeric(mean(this_day_all_data$PM2.5_Obs)) # input average 
#             # latitude: input unique value
#             if (length(unique(this_day_all_data$PM2.5_Lat))>1) {stop("latitudes don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("PM2.5_Lat")] <- as.numeric(mean(this_day_all_data$PM2.5_Lat)) # input average 
#             # longitude: input unique value
#             if (length(unique(this_day_all_data$PM2.5_Lon))>1) {stop("longitudes don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("PM2.5_Lon")] <- as.numeric(mean(this_day_all_data$PM2.5_Lon)) # input average 
#             # Datum: input unique value
#             if (length(unique(this_day_all_data$Datum))>1) {stop("Datums don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Datum")] <- as.character(unique(this_day_all_data$Datum)) # input unique value
#             # Date_Local: input unique date 
#             if (unique(this_day_all_data$Date_Local)!=this_day) {stop("Date_Local don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Date_Local")] <- as.character(unique(this_day_all_data$Date_Local)) # input unique value
#             # Year: input unique year
#             if (length(unique(this_day_all_data$Year))>1) {stop("Years don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Year")] <- as.numeric(mean(this_day_all_data$Year)) # input average 
#             # Month: input unique month
#             if (length(unique(this_day_all_data$Month))>1) {stop("Months don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Month")] <- as.numeric(mean(this_day_all_data$Month)) # input average 
#             # Day: input unique day
#             if (length(unique(this_day_all_data$Day))>1) {stop("Day doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Day")] <- as.numeric(mean(this_day_all_data$Day)) # input average 
#             # State_Code: input unique state code
#             if (length(unique(this_day_all_data$State_Code))>1) {stop("State_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("State_Code")] <- as.numeric(mean(this_day_all_data$State_Code)) # input average 
#             # County_Code: input unique county code
#             if (length(unique(this_day_all_data$County_Code))>1) {stop("County_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("County_Code")] <- as.numeric(mean(this_day_all_data$County_Code)) # input average 
#             # Site_Num: input unique site num
#             if (length(unique(this_day_all_data$Site_Num))>1) {stop("Site_Num doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Site_Num")] <- as.numeric(mean(this_day_all_data$Site_Num)) # input average 
#             # Parameter Code: input 101502 since this data is a combination of 88101 and 88502
#             input_mat3_aves[rstart_aves:rstop_aves,c("Parameter_Code")] <- 101502
#             # POC: no check statement for POC since we know they are different - take the average and multiply by 10 as new POC value
#             input_mat3_aves[rstart_aves:rstop_aves,c("POC")] <- "multiple" #as.numeric(mean(this_day_all_data$POC)) # input average 
#             # Parameter_Name: input composite Parameter Name
#             if (length(unique(this_day_all_data$Parameter_Name))>1) {
#               for (PN_i in 1:dim(this_day_all_data)[1]) {
#                 if (PN_i==1) {
#                   all_PNs <- this_day_all_data[PN_i,c("Parameter_Name")]
#                 } else {
#                   all_PNs <- paste(all_PNs,this_day_all_data[PN_i,c("Parameter_Name")],sep = ", ")
#                 } # if (PN_i==1) {
#               } # for (PN_i in 1:dim(this_day_all_data)[1]) {
#             } else {
#               all_PNs <- unique(this_day_all_data$Parameter_Name)
#             }
#             input_mat3_aves[rstart_aves:rstop_aves,c("Parameter_Name")] <- all_PNs # input composite of data
#             rm(all_PNs)
#             # Sample Duration: input unique Sample Duration
#             if (length(unique(this_day_all_data$Sample_Duration))>1) {stop("Sample_Duration doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Sample_Duration")] <- as.character(unique(this_day_all_data$Sample_Duration)) # input unique value
#             # Pollutant_Standard: input composite Pollutant Standard
#             if (length(unique(this_day_all_data$Pollutant_Standard))>1) {
#               for (PS_i in 1:dim(this_day_all_data)[1]) { # loop through all values and paste them together
#                 if (PS_i==1) {
#                   all_PSs <- this_day_all_data[PS_i,c("Pollutant_Standard")]
#                 } else {
#                   all_PSs <- paste(all_PSs,this_day_all_data[PS_i,c("Pollutant_Standard")],sep = ", ")
#                 } # if (PS_i==1) {
#               } # for (PS_i in 1:dim(this_day_all_data)[1]) {
#             } else {
#               all_PSs <- unique(this_day_all_data$Pollutant_Standard)
#             }
#             input_mat3_aves[rstart_aves:rstop_aves,c("Pollutant_Standard")] <- all_PSs # input composite of data
#             rm(all_PSs)
#             # Units_of_Measure: input unique Units of Measure
#             if (length(unique(this_day_all_data$Units_of_Measure))>1) {stop("Units_of_Measure doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Units_of_Measure")] <- as.character(unique(this_day_all_data$Units_of_Measure)) # input unique value
#             # Event_Type: input unique event type
#             if (length(unique(this_day_all_data$Event_Type))>1) {stop("Event_Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Event_Type")] <- as.character(unique(this_day_all_data$Event_Type)) # input unique value
#             # Observation Count: sum the observation counts
#             if (min(this_day_all_data$Observation_Count)<1) {stop("Observation_Count doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Observation_Count")] <- as.numeric(sum(this_day_all_data$Observation_Count)) # input average 
#             # Observation Percent: sum the percentages, unless another idea comes along
#             if (min(this_day_all_data$Observation_Percent)<75) {stop("Observation_Percent doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Observation_Percent")] <- as.numeric(sum(this_day_all_data$Observation_Percent)) # input average 
#             # X1st_Max_Value: input mean X1st_Max_Value
#             if (min(this_day_all_data$X1st_Max_Value)<0) {stop("negative concentration (X1st_Max_Value) Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("X1st_Max_Value")] <- as.numeric(mean(this_day_all_data$X1st_Max_Value)) # input average 
#             # X1st_Max_Hour: input NA for X1st_Max_Hour, taking an average of hours of the day seems meaningless
#             input_mat3_aves[rstart_aves:rstop_aves,c("X1st_Max_Hour")] <- NA 
#             # AQI: input mean AQI
#             if (min(this_day_all_data$AQI)<0) {stop("negative concentration (AQI) Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("AQI")] <- as.numeric(mean(this_day_all_data$AQI)) # input average 
#             # Method_Code: input unique event type
#             if (length(unique(this_day_all_data$Method_Code))>1) {
#               for (Var_i in 1:dim(this_day_all_data)[1]) { # loop through all values and paste them together
#                 if (Var_i==1) {
#                   all_Vars <- this_day_all_data[Var_i,c("Method_Code")]
#                 } else {
#                   all_Vars <- paste(all_Vars,this_day_all_data[Var_i,c("Method_Code")],sep = ", ")
#                 } # if (Var_i==1) {
#               } # for (Var_i in 1:dim(this_day_all_data)[1]) {
#             } else {
#               all_Vars <- unique(this_day_all_data$Method_Code)
#             }
#             input_mat3_aves[rstart_aves:rstop_aves,c("Method_Code")] <- all_Vars # input composite of data
#             rm(all_Vars)
#             # Method_Name: input unique Method_Name
#             if (length(unique(this_day_all_data$Method_Name))>1) {
#               for (Var_i in 1:dim(this_day_all_data)[1]) { # loop through all values and paste them together
#                 if (Var_i==1) {
#                   all_Vars <- this_day_all_data[Var_i,c("Method_Name")]
#                 } else {
#                   all_Vars <- paste(all_Vars,this_day_all_data[Var_i,c("Method_Name")],sep = ", ")
#                 } # if (Var_i==1) {
#               } # for (Var_i in 1:dim(this_day_all_data)[1]) {
#             } else {
#               all_Vars <- unique(this_day_all_data$Method_Name)
#             }
#             input_mat3_aves[rstart_aves:rstop_aves,c("Method_Name")] <- all_Vars # input composite of data
#             rm(all_Vars)
#             # PM25_Station_Name: input unique PM25_Station_Name
#             if (length(unique(this_day_all_data$PM25_Station_Name))>1) {stop("PM25_Station_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("PM25_Station_Name")] <- as.character(unique(this_day_all_data$PM25_Station_Name)) # input unique value
#             # Address: input unique Address
#             if (length(unique(this_day_all_data$Address))>1) {stop("Address doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Address")] <- as.character(unique(this_day_all_data$Address)) # input unique value
#             # State_Name: input unique State_Name
#             if (length(unique(this_day_all_data$State_Name))>1) {stop("State_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("State_Name")] <- as.character(unique(this_day_all_data$State_Name)) # input unique value
#             # County_Name: input unique County_Name
#             if (length(unique(this_day_all_data$County_Name))>1) {stop("County_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("County_Name")] <- as.character(unique(this_day_all_data$County_Name)) # input unique value
#             # City_Name: input unique City_Name
#             if (length(unique(this_day_all_data$City_Name))>1) {stop("City_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("City_Name")] <- as.character(unique(this_day_all_data$City_Name)) # input unique value
#             # CBSA_Name: input unique CBSA_Name
#             if (length(unique(this_day_all_data$CBSA_Name))>1) {stop("CBSA_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("CBSA_Name")] <- as.character(unique(this_day_all_data$CBSA_Name)) # input unique value
#             # Date_of_Last_Change: input unique Date_of_Last_Change
#             if (length(unique(this_day_all_data$Date_of_Last_Change))>1) {
#               for (Var_i in 1:dim(this_day_all_data)[1]) { # loop through all values and paste them together
#                 if (Var_i==1) {
#                   all_Vars <- this_day_all_data[Var_i,c("Date_of_Last_Change")]
#                 } else {
#                   all_Vars <- paste(all_Vars,this_day_all_data[Var_i,c("Date_of_Last_Change")],sep = ", ")
#                 } # if (Var_i==1) {
#               } # for (Var_i in 1:dim(this_day_all_data)[1]) {
#             } else {
#               all_Vars <- unique(this_day_all_data$Date_of_Last_Change)
#             }
#             input_mat3_aves[rstart_aves:rstop_aves,c("Date_of_Last_Change")] <- all_Vars # input composite of data
#             rm(all_Vars)
#             # State_Abbrev: input unique State_Abbrev
#             if (length(unique(this_day_all_data$State_Abbrev))>1) {stop("State_Abbrev doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("State_Abbrev")] <- as.character(unique(this_day_all_data$State_Abbrev)) # input unique value
#             # Winter: input unique Winter
#             if (length(unique(this_day_all_data$Winter))>1) {stop("Winter doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Winter")] <- as.numeric(mean(this_day_all_data$Winter)) # input average 
#             # Data_Source_Name_Display: input unique Data_Source_Name_Display
#             if (length(unique(this_day_all_data$Data_Source_Name_Display))>1) {stop("Data_Source_Name_Display doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Data_Source_Name_Display")] <- as.character(unique(this_day_all_data$Data_Source_Name_Display)) # input unique value
#             # Data_Source_Name_Short: input unique Data_Source_Name_Short
#             if (length(unique(this_day_all_data$Data_Source_Name_Short))>1) {stop("Data_Source_Name_Short doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Data_Source_Name_Short")] <- as.character(unique(this_day_all_data$Data_Source_Name_Short)) # input unique value
#             # Data_Source_Counter: input unique value
#             if (length(unique(this_day_all_data$Data_Source_Counter))>1) {stop("Data_Source_Counter don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Data_Source_Counter")] <- as.numeric(mean(this_day_all_data$Data_Source_Counter)) # input average 
#             # "Source_File": input unique "Source_File"
#             if (length(unique(this_day_all_data$Source_File))>1) {
#               for (Var_i in 1:dim(this_day_all_data)[1]) { # loop through all values and paste them together
#                 if (Var_i==1) {
#                   all_Vars <- this_day_all_data[Var_i,c("Source_File")]
#                 } else {
#                   all_Vars <- paste(all_Vars,this_day_all_data[Var_i,c("Source_File")],sep = ", ")
#                 } # if (Var_i==1) {
#               } # for (Var_i in 1:dim(this_day_all_data)[1]) {
#             } else {
#               all_Vars <- unique(this_day_all_data$Source_File)
#             }
#             input_mat3_aves[rstart_aves:rstop_aves,c("Source_File")] <- all_Vars # input composite of data
#             rm(all_Vars)
#             # Composite_of_N_rows: sum the Composite_of_N_rows
#             if (min(this_day_all_data$Composite_of_N_rows)<1) {stop("Composite_of_N_rows doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Composite_of_N_rows")] <- as.numeric(sum(this_day_all_data$Composite_of_N_rows)) # input average 
#             # N_Negative_Obs: sum the N_Negative_Obs
#             if (max(this_day_all_data$N_Negative_Obs)>0) {stop("N_Negative_Obs doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("N_Negative_Obs")] <- as.numeric(sum(this_day_all_data$N_Negative_Obs)) # input average 
#             # "flg.Lat": input unique "flg.Lat"
#             if (length(unique(this_day_all_data$flg.Lat))>1) {stop("flg.Lat doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg.Lat")] <- as.character(unique(this_day_all_data$flg.Lat)) # input unique value
#             # "flg.Lon: input unique flg.Lon
#             if (length(unique(this_day_all_data$flg.Lon))>1) {stop("flg.Lon doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg.Lon")] <- as.character(unique(this_day_all_data$flg.Lon)) # input unique value
#             # "Type: input unique Type
#             if (length(unique(this_day_all_data$Type))>1) {stop("Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Type")] <- as.character(unique(this_day_all_data$Type)) # input unique value
#             # "flg.Type: input unique flg.Type
#             if (length(unique(this_day_all_data$flg.Type))>1) {stop("flg.Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg.Type")] <- as.character(unique(this_day_all_data$flg.Type)) # input unique value
#             # flg.Site_Num: input unique flg.Site_Num
#             if (length(unique(this_day_all_data$flg.Site_Num))>1) {stop("flg.Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg.Site_Num")] <- as.character(unique(this_day_all_data$flg.Site_Num)) # input unique value
#             # flg.PM25_Obs: input unique flg.PM25_Obs
#             if (length(unique(this_day_all_data$flg.PM25_Obs))>1) {stop("flg.PM25_Obs doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg.PM25_Obs")] <- as.character(unique(this_day_all_data$flg.PM25_Obs)) # input unique value
#             # l.m.Ave..Air.Flw: input unique l.m.Ave..Air.Flw
#             if (length(unique(this_day_all_data$l.m.Ave..Air.Flw))>1) {stop("l.m.Ave..Air.Flw doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("l.m.Ave..Air.Flw")] <- as.numeric(mean(this_day_all_data$l.m.Ave..Air.Flw)) # input average 
#             # flg.AirFlw: input unique flg.AirFlw
#             if (length(unique(this_day_all_data$flg.AirFlw))>1) {stop("flg.AirFlw doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg.AirFlw")] <- as.character(unique(this_day_all_data$flg.AirFlw)) # input unique value
#             # Deg.C.Av.Air.Temp: input unique Deg.C.Av.Air.Temp
#             if (length(unique(this_day_all_data$Deg.C.Av.Air.Temp))>1) {stop("Deg.C.Av.Air.Temp doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Deg.C.Av.Air.Temp")] <- as.numeric(mean(this_day_all_data$Deg.C.Av.Air.Temp)) # input average 
#             # flg.AirTemp: input unique flg.AirTemp
#             if (length(unique(this_day_all_data$flg.AirTemp))>1) {stop("flg.AirTemp doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg.AirFlw")] <- as.character(unique(this_day_all_data$flg.AirFlw)) # input unique value
#             # X..Rel.Humidty: input unique X..Rel.Humidty
#             if (length(unique(this_day_all_data$X..Rel.Humidty))>1) {stop("X..Rel.Humidty doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("X..Rel.Humidty")] <- as.numeric(mean(this_day_all_data$X..Rel.Humidty)) # input average 
#             # flg.RelHumid: input unique flg.RelHumid
#             if (length(unique(this_day_all_data$flg.RelHumid))>1) {stop("flg.RelHumid doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg.RelHumid")] <- as.character(unique(this_day_all_data$flg.RelHumid)) # input unique value
#             # mbar.Barom.Press: input unique mbar.Barom.Press
#             if (length(unique(this_day_all_data$mbar.Barom.Press))>1) {stop("mbar.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("mbar.Barom.Press")] <- as.numeric(mean(this_day_all_data$mbar.Barom.Press)) # input average 
#             # flg.Barom.Press: input unique flg.Barom.Press
#             if (length(unique(this_day_all_data$flg.Barom.Press))>1) {stop("flg.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg.Barom.Press")] <- as.character(unique(this_day_all_data$flg.Barom.Press)) # input unique value
#             # deg.C.Sensor..Int.AT: input unique deg.C.Sensor..Int.AT
#             if (length(unique(this_day_all_data$deg.C.Sensor..Int.AT))>1) {stop("deg.C.Sensor..Int.AT doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("deg.C.Sensor..Int.AT")] <- as.numeric(mean(this_day_all_data$deg.C.Sensor..Int.AT)) # input average 
#             # flg.deg.C.Sensor.Int.AT: input unique flg.deg.C.Sensor.Int.AT
#             if (length(unique(this_day_all_data$flg.deg.C.Sensor.Int.AT))>1) {stop("flg.deg.C.Sensor.Int.AT doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg.deg.C.Sensor.Int.AT")] <- as.character(unique(this_day_all_data$flg.deg.C.Sensor.Int.AT)) # input unique value
#             # X..Sensor.Int.RH: input unique X..Sensor.Int.RH
#             if (length(unique(this_day_all_data$X..Sensor.Int.RH))>1) {stop("X..Sensor.Int.RH doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("X..Sensor.Int.RH")] <- as.numeric(mean(this_day_all_data$X..Sensor.Int.RH)) # input average 
#             # flg.deg.C.Sensor.Int.A: input unique flg.deg.C.Sensor.Int.A
#             if (length(unique(this_day_all_data$flg..SensorIntRH))>1) {stop("flg..SensorIntRH doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg..SensorIntRH")] <- as.character(unique(this_day_all_data$flg..SensorIntRH)) # input unique value
#             # Wind.Speed.m.s: input unique Wind.Speed.m.s
#             if (length(unique(this_day_all_data$Wind.Speed.m.s))>1) {stop("Wind.Speed.m.s doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Wind.Speed.m.s")] <- as.numeric(mean(this_day_all_data$Wind.Speed.m.s)) # input average 
#             # flg.WindSpeed: input unique flg.WindSpeed
#             if (length(unique(this_day_all_data$flg.WindSpeed))>1) {stop("flg.WindSpeed doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg.WindSpeed")] <- as.character(unique(this_day_all_data$flg.WindSpeed)) # input unique value
#             # Battery.Voltage.volts: input unique Battery.Voltage.volts
#             if (length(unique(this_day_all_data$Battery.Voltage.volts))>1) {stop("Battery.Voltage.volts doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Battery.Voltage.volts")] <- as.numeric(mean(this_day_all_data$Battery.Voltage.volts)) # input average 
#             # flg.BatteryVoltage: input unique flg.BatteryVoltage
#             if (length(unique(this_day_all_data$flg.BatteryVoltage))>1) {stop("flg.BatteryVoltage doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg.BatteryVoltage")] <- as.character(unique(this_day_all_data$flg.BatteryVoltage)) # input unique value
#             # Alarm: input unique Alarm
#             if (length(unique(this_day_all_data$Alarm))>1) {stop("Alarm doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("Alarm")] <- as.numeric(mean(this_day_all_data$Alarm)) # input average 
#             # flg.Alarm: input unique flg.Alarm
#             if (length(unique(this_day_all_data$flg.Alarm))>1) {stop("flg.Alarm doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("flg.Alarm")] <- as.character(unique(this_day_all_data$flg.Alarm)) # input unique value
#             # InDayLatDiff: input unique InDayLatDiff
#             if (length(unique(this_day_all_data$InDayLatDiff))>1) {stop("InDayLatDiff doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("InDayLatDiff")] <- as.numeric(mean(this_day_all_data$InDayLatDiff)) # input average 
#             # InDayLonDiff: input unique InDayLonDiff
#             if (length(unique(this_day_all_data$InDayLonDiff))>1) {stop("InDayLonDiff doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_aves[rstart_aves:rstop_aves,c("InDayLonDiff")] <- as.numeric(mean(this_day_all_data$InDayLonDiff)) # input average 
#             # PlottingColor: setting value specific to this if-statement
#             input_mat3_aves[rstart_aves:rstop_aves,c("PlottingColor")] <- as.character(set_plot_color)
#             rstart_aves <- rstop_aves+1 # move counter up
#             
#             #rm(input_mat3_aves,rstart_aves,rstop_aves) # COMMENT
#             
#             #### for the colocated data 2 data points, one is 88101 and one is 88502 ####
#             rstop_colocated <- rstart_colocated+dim(this_day_all_data)[1]-1
#             #input_mat3_colocated[rstart_colocated:rstop_colocated,] <- this_day_all_data
#             #rstart_colocated <- rstop_colocated+1
#             
#             # PM2.5 Obs (concentration):take average
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("PM2.5_Obs")] <- as.numeric(this_day_all_data$PM2.5_Obs) # input values directly
#             # latitude: input unique value
#             if (length(unique(this_day_all_data$PM2.5_Lat))>1) {stop("latitudes don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("PM2.5_Lat")] <- as.numeric(mean(this_day_all_data$PM2.5_Lat)) # input average
#             # longitude: input unique value
#             if (length(unique(this_day_all_data$PM2.5_Lon))>1) {stop("longitudes don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("PM2.5_Lon")] <- as.numeric(mean(this_day_all_data$PM2.5_Lon)) # input average
#             # Datum: input unique value
#             if (length(unique(this_day_all_data$Datum))>1) {stop("Datums don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Datum")] <- as.character(unique(this_day_all_data$Datum)) # input unique value
#             # Date_Local: input unique date
#             if (unique(this_day_all_data$Date_Local)!=this_day) {stop("Date_Local don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Date_Local")] <- as.character(unique(this_day_all_data$Date_Local)) # input unique value
#             # Year: input unique year
#             if (length(unique(this_day_all_data$Year))>1) {stop("Years don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Year")] <- as.numeric(mean(this_day_all_data$Year)) # input average
#             # Month: input unique month
#             if (length(unique(this_day_all_data$Month))>1) {stop("Months don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Month")] <- as.numeric(mean(this_day_all_data$Month)) # input average
#             # Day: input unique day
#             if (length(unique(this_day_all_data$Day))>1) {stop("Day doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Day")] <- as.numeric(mean(this_day_all_data$Day)) # input average
#             # State_Code: input unique state code
#             if (length(unique(this_day_all_data$State_Code))>1) {stop("State_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("State_Code")] <- as.numeric(mean(this_day_all_data$State_Code)) # input average
#             # County_Code: input unique county code
#             if (length(unique(this_day_all_data$County_Code))>1) {stop("County_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("County_Code")] <- as.numeric(mean(this_day_all_data$County_Code)) # input average
#             # Site_Num: input unique site num
#             if (length(unique(this_day_all_data$Site_Num))>1) {stop("Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Site_Num")] <- as.numeric(mean(this_day_all_data$Site_Num)) # input average
#             # Parameter Code: input parameter codes (no check since we know they are different)
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Parameter_Code")] <- as.numeric(this_day_all_data$Parameter_Code) # input values directly
#             # POC: - input as they are
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("POC")] <- as.numeric(this_day_all_data$POC) # input average
#             # Parameter_Name: input Parameter Name - no check since they can be different
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Parameter_Name")] <- as.character(this_day_all_data$Parameter_Name) # input values directly
#             # Sample Duration: input unique Sample Duration
#             if (length(unique(this_day_all_data$Sample_Duration))>1) {stop("Sample_Duration doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Sample_Duration")] <- as.character(unique(this_day_all_data$Sample_Duration)) # input unique value
#             # Pollutant_Standard: input unique Pollutant Standard - no check since they can be different
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Pollutant_Standard")] <- as.character(this_day_all_data$Pollutant_Standard) # input unique value
#             # Units_of_Measure: input unique Units of Measure
#             if (length(unique(this_day_all_data$Units_of_Measure))>1) {stop("Units_of_Measure doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Units_of_Measure")] <- as.character(unique(this_day_all_data$Units_of_Measure)) # input unique value
#             # Event_Type: input unique event type
#             if (length(unique(this_day_all_data$Event_Type))>1) {stop("Event_Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Event_Type")] <- as.character(unique(this_day_all_data$Event_Type)) # input unique value
#             # Observation Count: input directly
#             if (min(this_day_all_data$Observation_Count)<1) {stop("Observation_Count doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Observation_Count")] <- as.numeric(this_day_all_data$Observation_Count) # input directly
#             # Observation Percent: input directly
#             if (min(this_day_all_data$Observation_Percent)<75) {stop("Observation_Percent doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Observation_Percent")] <- as.numeric(this_day_all_data$Observation_Percent) # input directly
#             # X1st_Max_Value: input directly
#             if (min(this_day_all_data$X1st_Max_Value)<0) {stop("negative concentration (X1st_Max_Value) Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("X1st_Max_Value")] <- as.numeric(this_day_all_data$X1st_Max_Value) # input average
#             # X1st_Max_Hour: input directly
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("X1st_Max_Hour")] <- as.character(this_day_all_data$X1st_Max_Hour)
#             # AQI: input mean AQI
#             if (min(this_day_all_data$AQI)<0) {stop("negative concentration (AQI) Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("AQI")] <- as.numeric(mean(this_day_all_data$AQI)) # input average
#             # Method_Code: input Method_Code - no check since they can be different
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Method_Code")] <- as.character(this_day_all_data$Method_Code) # input unique value
#             # Method_Name: input Method_Name - no check since they can be different
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Method_Name")] <- as.character(this_day_all_data$Method_Name) # input unique value
#             # PM25_Station_Name: input unique PM25_Station_Name
#             if (length(unique(this_day_all_data$PM25_Station_Name))>1) {stop("PM25_Station_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("PM25_Station_Name")] <- as.character(unique(this_day_all_data$PM25_Station_Name)) # input unique value
#             # Address: input unique Address
#             if (length(unique(this_day_all_data$Address))>1) {stop("Address doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Address")] <- as.character(unique(this_day_all_data$Address)) # input unique value
#             # State_Name: input unique State_Name
#             if (length(unique(this_day_all_data$State_Name))>1) {stop("State_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("State_Name")] <- as.character(unique(this_day_all_data$State_Name)) # input unique value
#             # County_Name: input unique County_Name
#             if (length(unique(this_day_all_data$County_Name))>1) {stop("County_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("County_Name")] <- as.character(unique(this_day_all_data$County_Name)) # input unique value
#             # City_Name: input unique City_Name
#             if (length(unique(this_day_all_data$City_Name))>1) {stop("City_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("City_Name")] <- as.character(unique(this_day_all_data$City_Name)) # input unique value
#             # CBSA_Name: input unique CBSA_Name
#             if (length(unique(this_day_all_data$CBSA_Name))>1) {stop("CBSA_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("CBSA_Name")] <- as.character(unique(this_day_all_data$CBSA_Name)) # input unique value
#             # Date_of_Last_Change: input Date_of_Last_Change - no check since they can be different
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Date_of_Last_Change")] <- as.character(this_day_all_data$Date_of_Last_Change) # input values directly
#             # State_Abbrev: input unique State_Abbrev
#             if (length(unique(this_day_all_data$State_Abbrev))>1) {stop("State_Abbrev doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("State_Abbrev")] <- as.character(unique(this_day_all_data$State_Abbrev)) # input unique value
#             # Winter: input unique Winter
#             if (length(unique(this_day_all_data$Winter))>1) {stop("Winter doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Winter")] <- as.numeric(mean(this_day_all_data$Winter)) # input average
#             # Data_Source_Name_Display: input unique Data_Source_Name_Display
#             if (length(unique(this_day_all_data$Data_Source_Name_Display))>1) {stop("Data_Source_Name_Display doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Data_Source_Name_Display")] <- as.character(unique(this_day_all_data$Data_Source_Name_Display)) # input unique value
#             # Data_Source_Name_Short: input unique Data_Source_Name_Short
#             if (length(unique(this_day_all_data$Data_Source_Name_Short))>1) {stop("Data_Source_Name_Short doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Data_Source_Name_Short")] <- as.character(unique(this_day_all_data$Data_Source_Name_Short)) # input unique value
#             # Data_Source_Counter: input unique value
#             if (length(unique(this_day_all_data$Data_Source_Counter))>1) {stop("Data_Source_Counter don't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Data_Source_Counter")] <- as.numeric(mean(this_day_all_data$Data_Source_Counter)) # input average
#             # "Source_File": input "Source_File" directly - no check since they can be different
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Source_File")] <- as.character(this_day_all_data$Source_File) # input unique value
#             # Composite_of_N_rows: input directly
#             if (min(this_day_all_data$Composite_of_N_rows)<1) {stop("Composite_of_N_rows doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Composite_of_N_rows")] <- as.numeric(this_day_all_data$Composite_of_N_rows) # input directly
#             # N_Negative_Obs: input_directly
#             if (max(this_day_all_data$N_Negative_Obs)>0) {stop("N_Negative_Obs doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("N_Negative_Obs")] <- as.numeric(this_day_all_data$N_Negative_Obs) # input average
#             # "flg.Lat": input unique "flg.Lat"
#             if (length(unique(this_day_all_data$flg.Lat))>1) {stop("flg.Lat doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Lat")] <- as.character(unique(this_day_all_data$flg.Lat)) # input unique value
#             # "flg.Lon: input unique flg.Lon
#             if (length(unique(this_day_all_data$flg.Lon))>1) {stop("flg.Lon doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Lon")] <- as.character(unique(this_day_all_data$flg.Lon)) # input unique value
#             # "Type: input unique Type
#             if (length(unique(this_day_all_data$Type))>1) {stop("Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Type")] <- as.character(unique(this_day_all_data$Type)) # input unique value
#             # "flg.Type: input unique flg.Type
#             if (length(unique(this_day_all_data$flg.Type))>1) {stop("flg.Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Type")] <- as.character(unique(this_day_all_data$flg.Type)) # input unique value
#             # flg.Site_Num: input unique flg.Site_Num
#             if (length(unique(this_day_all_data$flg.Site_Num))>1) {stop("flg.Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Site_Num")] <- as.character(unique(this_day_all_data$flg.Site_Num)) # input unique value
#             # flg.PM25_Obs: input unique flg.PM25_Obs
#             if (length(unique(this_day_all_data$flg.PM25_Obs))>1) {stop("flg.PM25_Obs doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.PM25_Obs")] <- as.character(unique(this_day_all_data$flg.PM25_Obs)) # input unique value
#             # l.m.Ave..Air.Flw: input unique l.m.Ave..Air.Flw
#             if (length(unique(this_day_all_data$l.m.Ave..Air.Flw))>1) {stop("l.m.Ave..Air.Flw doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("l.m.Ave..Air.Flw")] <- as.numeric(mean(this_day_all_data$l.m.Ave..Air.Flw)) # input average
#             # flg.AirFlw: input unique flg.AirFlw
#             if (length(unique(this_day_all_data$flg.AirFlw))>1) {stop("flg.AirFlw doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.AirFlw")] <- as.character(unique(this_day_all_data$flg.AirFlw)) # input unique value
#             # Deg.C.Av.Air.Temp: input unique Deg.C.Av.Air.Temp
#             if (length(unique(this_day_all_data$Deg.C.Av.Air.Temp))>1) {stop("Deg.C.Av.Air.Temp doesn't match. Look at data/code and write more code")} # check that latitudes match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Deg.C.Av.Air.Temp")] <- as.numeric(mean(this_day_all_data$Deg.C.Av.Air.Temp)) # input average
#             # flg.AirTemp: input unique flg.AirTemp
#             if (length(unique(this_day_all_data$flg.AirTemp))>1) {stop("flg.AirTemp doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.AirFlw")] <- as.character(unique(this_day_all_data$flg.AirFlw)) # input unique value
#             # X..Rel.Humidty: input unique X..Rel.Humidty
#             if (length(unique(this_day_all_data$X..Rel.Humidty))>1) {stop("X..Rel.Humidty doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("X..Rel.Humidty")] <- as.numeric(mean(this_day_all_data$X..Rel.Humidty)) # input average
#             # flg.RelHumid: input unique flg.RelHumid
#             if (length(unique(this_day_all_data$flg.RelHumid))>1) {stop("flg.RelHumid doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.RelHumid")] <- as.character(unique(this_day_all_data$flg.RelHumid)) # input unique value
#             # mbar.Barom.Press: input unique mbar.Barom.Press
#             if (length(unique(this_day_all_data$mbar.Barom.Press))>1) {stop("mbar.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("mbar.Barom.Press")] <- as.numeric(mean(this_day_all_data$mbar.Barom.Press)) # input average
#             # flg.Barom.Press: input unique flg.Barom.Press
#             if (length(unique(this_day_all_data$flg.Barom.Press))>1) {stop("flg.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Barom.Press")] <- as.character(unique(this_day_all_data$flg.Barom.Press)) # input unique value
#             # deg.C.Sensor..Int.AT: input unique deg.C.Sensor..Int.AT
#             if (length(unique(this_day_all_data$deg.C.Sensor..Int.AT))>1) {stop("deg.C.Sensor..Int.AT doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("deg.C.Sensor..Int.AT")] <- as.numeric(mean(this_day_all_data$deg.C.Sensor..Int.AT)) # input average
#             # flg.deg.C.Sensor.Int.AT: input unique flg.deg.C.Sensor.Int.AT
#             if (length(unique(this_day_all_data$flg.deg.C.Sensor.Int.AT))>1) {stop("flg.deg.C.Sensor.Int.AT doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.deg.C.Sensor.Int.AT")] <- as.character(unique(this_day_all_data$flg.deg.C.Sensor.Int.AT)) # input unique value
#             # X..Sensor.Int.RH: input unique X..Sensor.Int.RH
#             if (length(unique(this_day_all_data$X..Sensor.Int.RH))>1) {stop("X..Sensor.Int.RH doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("X..Sensor.Int.RH")] <- as.numeric(mean(this_day_all_data$X..Sensor.Int.RH)) # input average
#             # flg.deg.C.Sensor.Int.A: input unique flg.deg.C.Sensor.Int.A
#             if (length(unique(this_day_all_data$flg..SensorIntRH))>1) {stop("flg..SensorIntRH doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg..SensorIntRH")] <- as.character(unique(this_day_all_data$flg..SensorIntRH)) # input unique value
#             # Wind.Speed.m.s: input unique Wind.Speed.m.s
#             if (length(unique(this_day_all_data$Wind.Speed.m.s))>1) {stop("Wind.Speed.m.s doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Wind.Speed.m.s")] <- as.numeric(mean(this_day_all_data$Wind.Speed.m.s)) # input average
#             # flg.WindSpeed: input unique flg.WindSpeed
#             if (length(unique(this_day_all_data$flg.WindSpeed))>1) {stop("flg.WindSpeed doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.WindSpeed")] <- as.character(unique(this_day_all_data$flg.WindSpeed)) # input unique value
#             # Battery.Voltage.volts: input unique Battery.Voltage.volts
#             if (length(unique(this_day_all_data$Battery.Voltage.volts))>1) {stop("Battery.Voltage.volts doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Battery.Voltage.volts")] <- as.numeric(mean(this_day_all_data$Battery.Voltage.volts)) # input average
#             # flg.BatteryVoltage: input unique flg.BatteryVoltage
#             if (length(unique(this_day_all_data$flg.BatteryVoltage))>1) {stop("flg.BatteryVoltage doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.BatteryVoltage")] <- as.character(unique(this_day_all_data$flg.BatteryVoltage)) # input unique value
#             # Alarm: input unique Alarm
#             if (length(unique(this_day_all_data$Alarm))>1) {stop("Alarm doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("Alarm")] <- as.numeric(mean(this_day_all_data$Alarm)) # input average
#             # flg.Alarm: input unique flg.Alarm
#             if (length(unique(this_day_all_data$flg.Alarm))>1) {stop("flg.Alarm doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Alarm")] <- as.character(unique(this_day_all_data$flg.Alarm)) # input unique value
#             # InDayLatDiff: input unique InDayLatDiff
#             if (length(unique(this_day_all_data$InDayLatDiff))>1) {stop("InDayLatDiff doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("InDayLatDiff")] <- as.numeric(mean(this_day_all_data$InDayLatDiff)) # input average
#             # InDayLonDiff: input unique InDayLonDiff
#             if (length(unique(this_day_all_data$InDayLonDiff))>1) {stop("InDayLonDiff doesn't match. Look at data/code and write more code")} # check that values match
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("InDayLonDiff")] <- as.numeric(mean(this_day_all_data$InDayLonDiff)) # input average
#             # PlottingColor: setting value specific to this if-statement
#             input_mat3_colocated[rstart_colocated:rstop_colocated,c("PlottingColor")] <- as.character(set_plot_color)
#             
#             rstart_colocated <- rstop_colocated+1
#             
#             
#           } # if (dim(unique_ParamCode_POC)[1] != dim(this_day_all_data)[1]) { # is every ParameterCode/POC combination unique?
#         } # if (length(this_day_all_data$Data_Source_Name_Short) == 1) { # is the data all from one source or multiple sources?
#     } # for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
#       } else { # multiple sources of data
#         # data for this day can be from multiple sources
#         stop("multiple sources of data. Write code.")
#       } #if (length(this_day_all_data$Data_Source_Name_Short) == 1) { # is the data all from one source or multiple sources?
#     #### Clear variables before moving onto next day ####
#     rm(this_day_all_data)
#     
#   } # if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) { # determine whether there were multiple monitors ever operating at this site (or duplicate data)
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#  ##################################################### 
#   
#   else {
#     #print(paste("There are ",dim(this_station_data)[1]," rows of data for ",length(unique_days)," days of data at station ","EPA Code ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num,sep = ""))
#     print("There is duplicate data for this station. This could represent multiple monitors at a site, duplicated data from different sources, or both.")
#     # or loop cycling through days relevant for this station and combine data for this station-day in various ways
#     #print(paste("Starting loop through days for Station ",unique(this_station$State_Code),"-",this_station$County_Code,"-",this_station$Site_Num,sep = ""))
#     for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
#       this_day <- unique_days[this_day_i]
#       #print(this_day)
#       which_this_day <- which(this_station_data$Date_Local == this_day)
#       this_day_all_data <- this_station_data[which_this_day,]
#       rm(which_this_day)
#       print(paste("Station ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num," has ",
#                   length(which_this_day)," rows of data on ",this_day,".",sep = ""))
# 
#       # check the POC's
#       which_POC_NA <- which(is.na(this_day_all_data$POC))
#       which_Parameter_Code_NA <- which(is.na(this_day_all_data$Parameter_Code))
#       if (length(which_POC_NA)!=0 | length(which_Parameter_Code_NA)!=0) {stop("write code to handle unknown POCs and unknown Parameter Codes")
#         
#       } else if (length()) { # POC and Parameter Code values are known
#         #print("POC values are known")
#         # how many different POC's are there?  
#         unique_POCs <- unique(this_day_all_data$POC)
#         print(unique_POCs)
#           if (length(unique_POCs) != dim(this_day_all_data)[1]) { # are all the data points on this day at this station from the same POC?
#            print("all data points on this day have the same POC value.")
#             if (min(this_day_all_data$Parameter_Code)==88101 & max(this_day_all_data$Parameter_Code)==88502 & dim(this_day_all_data)[1]==2) { # 2 data points on this day, one is 88101 and the other is 88502
#               print("2 data points, one is 88101 and one is 88502")
#               set_plot_color <- "azure3"
#               #### for the aves data - 2 data points, one is 88101 and one is 88502  ####
#               rstop_aves <- rstart_aves
#               # PM2.5 Obs (concentration):take average
#               input_mat3_aves[rstart_aves:rstop_aves,c("PM2.5_Obs")] <- as.numeric(mean(this_day_all_data$PM2.5_Obs)) # input average 
#               # latitude: input unique value
#               if (length(unique(this_day_all_data$PM2.5_Lat))>1) {stop("latitudes don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("PM2.5_Lat")] <- as.numeric(mean(this_day_all_data$PM2.5_Lat)) # input average 
#               # longitude: input unique value
#               if (length(unique(this_day_all_data$PM2.5_Lon))>1) {stop("longitudes don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("PM2.5_Lon")] <- as.numeric(mean(this_day_all_data$PM2.5_Lon)) # input average 
#               # Datum: input unique value
#               if (length(unique(this_day_all_data$Datum))>1) {stop("Datums don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Datum")] <- as.character(unique(this_day_all_data$Datum)) # input unique value
#               # Date_Local: input unique date 
#               if (unique(this_day_all_data$Date_Local)!=this_day) {stop("Date_Local don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Date_Local")] <- as.character(unique(this_day_all_data$Date_Local)) # input unique value
#               # Year: input unique year
#               if (length(unique(this_day_all_data$Year))>1) {stop("Years don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Year")] <- as.numeric(mean(this_day_all_data$Year)) # input average 
#               # Month: input unique month
#               if (length(unique(this_day_all_data$Month))>1) {stop("Months don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Month")] <- as.numeric(mean(this_day_all_data$Month)) # input average 
#               # Day: input unique day
#               if (length(unique(this_day_all_data$Day))>1) {stop("Day doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Day")] <- as.numeric(mean(this_day_all_data$Day)) # input average 
#               # State_Code: input unique state code
#               if (length(unique(this_day_all_data$State_Code))>1) {stop("State_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("State_Code")] <- as.numeric(mean(this_day_all_data$State_Code)) # input average 
#               # County_Code: input unique county code
#               if (length(unique(this_day_all_data$County_Code))>1) {stop("County_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("County_Code")] <- as.numeric(mean(this_day_all_data$County_Code)) # input average 
#               # Site_Num: input unique site num
#               if (length(unique(this_day_all_data$Site_Num))>1) {stop("Site_Num doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Site_Num")] <- as.numeric(mean(this_day_all_data$Site_Num)) # input average 
#               # Parameter Code: input 101502 since this data is a combination of 88101 and 88502
#               input_mat3_aves[rstart_aves:rstop_aves,c("Parameter_Code")] <- 101502
#               # POC: no check statement for POC since we know they are different - take the average and multiply by 10 as new POC value
#               if (length(unique(this_day_all_data$POC))>1) {stop("POC doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("POC")] <- as.numeric(mean(this_day_all_data$POC)) # input average 
#               # Parameter_Name: input composite Parameter Name
#               if (length(unique(this_day_all_data$Parameter_Name))>1) {
#               for (PN_i in 1:dim(this_day_all_data)[1]) {
#                 if (PN_i==1) {
#                   all_PNs <- this_day_all_data[PN_i,c("Parameter_Name")]
#                 } else {
#                 all_PNs <- paste(all_PNs,this_day_all_data[PN_i,c("Parameter_Name")],sep = ", ")
#                 } # if (PN_i==1) {
#               } # for (PN_i in 1:dim(this_day_all_data)[1]) {
#               } else {
#                 all_PNs <- unique(this_day_all_data$Parameter_Name)
#               }
#               input_mat3_aves[rstart_aves:rstop_aves,c("Parameter_Name")] <- all_PNs # input composite of data
#               rm(all_PNs)
#               # Sample Duration: input unique Sample Duration
#               if (length(unique(this_day_all_data$Sample_Duration))>1) {stop("Sample_Duration doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Sample_Duration")] <- as.character(unique(this_day_all_data$Sample_Duration)) # input unique value
#               # Pollutant_Standard: input composite Pollutant Standard
#               if (length(unique(this_day_all_data$Pollutant_Standard))>1) {
#               for (PS_i in 1:dim(this_day_all_data)[1]) { # loop through all values and paste them together
#                 if (PS_i==1) {
#                   all_PSs <- this_day_all_data[PS_i,c("Pollutant_Standard")]
#                 } else {
#                   all_PSs <- paste(all_PSs,this_day_all_data[PS_i,c("Pollutant_Standard")],sep = ", ")
#                 } # if (PS_i==1) {
#               } # for (PS_i in 1:dim(this_day_all_data)[1]) {
#               } else {
#                 all_PSs <- unique(this_day_all_data$Pollutant_Standard)
#               }
#               input_mat3_aves[rstart_aves:rstop_aves,c("Pollutant_Standard")] <- all_PSs # input composite of data
#               rm(all_PSs)
#               # Units_of_Measure: input unique Units of Measure
#               if (length(unique(this_day_all_data$Units_of_Measure))>1) {stop("Units_of_Measure doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Units_of_Measure")] <- as.character(unique(this_day_all_data$Units_of_Measure)) # input unique value
#               # Event_Type: input unique event type
#               if (length(unique(this_day_all_data$Event_Type))>1) {stop("Event_Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Event_Type")] <- as.character(unique(this_day_all_data$Event_Type)) # input unique value
#               # Observation Count: sum the observation counts
#               if (min(this_day_all_data$Observation_Count)<1) {stop("Observation_Count doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Observation_Count")] <- as.numeric(sum(this_day_all_data$Observation_Count)) # input average 
#               # Observation Percent: sum the percentages, unless another idea comes along
#               if (min(this_day_all_data$Observation_Percent)<75) {stop("Observation_Percent doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Observation_Percent")] <- as.numeric(sum(this_day_all_data$Observation_Percent)) # input average 
#               # X1st_Max_Value: input mean X1st_Max_Value
#               if (min(this_day_all_data$X1st_Max_Value)<0) {stop("negative concentration (X1st_Max_Value) Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("X1st_Max_Value")] <- as.numeric(mean(this_day_all_data$X1st_Max_Value)) # input average 
#               # X1st_Max_Hour: input NA for X1st_Max_Hour, taking an average of hours of the day seems meaningless
#               input_mat3_aves[rstart_aves:rstop_aves,c("X1st_Max_Hour")] <- NA 
#               # AQI: input mean AQI
#               if (min(this_day_all_data$AQI)<0) {stop("negative concentration (AQI) Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("AQI")] <- as.numeric(mean(this_day_all_data$AQI)) # input average 
#               # Method_Code: input unique event type
#               if (length(unique(this_day_all_data$Method_Code))>1) {
#                 for (Var_i in 1:dim(this_day_all_data)[1]) { # loop through all values and paste them together
#                   if (Var_i==1) {
#                     all_Vars <- this_day_all_data[Var_i,c("Method_Code")]
#                   } else {
#                     all_Vars <- paste(all_Vars,this_day_all_data[Var_i,c("Method_Code")],sep = ", ")
#                   } # if (Var_i==1) {
#                 } # for (Var_i in 1:dim(this_day_all_data)[1]) {
#               } else {
#                 all_Vars <- unique(this_day_all_data$Method_Code)
#               }
#               input_mat3_aves[rstart_aves:rstop_aves,c("Method_Code")] <- all_Vars # input composite of data
#               rm(all_Vars)
#               # Method_Name: input unique Method_Name
#               if (length(unique(this_day_all_data$Method_Name))>1) {
#                 for (Var_i in 1:dim(this_day_all_data)[1]) { # loop through all values and paste them together
#                   if (Var_i==1) {
#                     all_Vars <- this_day_all_data[Var_i,c("Method_Name")]
#                   } else {
#                     all_Vars <- paste(all_Vars,this_day_all_data[Var_i,c("Method_Name")],sep = ", ")
#                   } # if (Var_i==1) {
#                 } # for (Var_i in 1:dim(this_day_all_data)[1]) {
#               } else {
#                 all_Vars <- unique(this_day_all_data$Method_Name)
#               }
#               input_mat3_aves[rstart_aves:rstop_aves,c("Method_Name")] <- all_Vars # input composite of data
#               rm(all_Vars)
#               # PM25_Station_Name: input unique PM25_Station_Name
#               if (length(unique(this_day_all_data$PM25_Station_Name))>1) {stop("PM25_Station_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("PM25_Station_Name")] <- as.character(unique(this_day_all_data$PM25_Station_Name)) # input unique value
#               # Address: input unique Address
#               if (length(unique(this_day_all_data$Address))>1) {stop("Address doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Address")] <- as.character(unique(this_day_all_data$Address)) # input unique value
#               # State_Name: input unique State_Name
#               if (length(unique(this_day_all_data$State_Name))>1) {stop("State_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("State_Name")] <- as.character(unique(this_day_all_data$State_Name)) # input unique value
#               # County_Name: input unique County_Name
#               if (length(unique(this_day_all_data$County_Name))>1) {stop("County_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("County_Name")] <- as.character(unique(this_day_all_data$County_Name)) # input unique value
#               # City_Name: input unique City_Name
#               if (length(unique(this_day_all_data$City_Name))>1) {stop("City_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("City_Name")] <- as.character(unique(this_day_all_data$City_Name)) # input unique value
#               # CBSA_Name: input unique CBSA_Name
#               if (length(unique(this_day_all_data$CBSA_Name))>1) {stop("CBSA_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("CBSA_Name")] <- as.character(unique(this_day_all_data$CBSA_Name)) # input unique value
#               # Date_of_Last_Change: input unique Date_of_Last_Change
#               if (length(unique(this_day_all_data$Date_of_Last_Change))>1) {
#                 for (Var_i in 1:dim(this_day_all_data)[1]) { # loop through all values and paste them together
#                   if (Var_i==1) {
#                     all_Vars <- this_day_all_data[Var_i,c("Date_of_Last_Change")]
#                   } else {
#                     all_Vars <- paste(all_Vars,this_day_all_data[Var_i,c("Date_of_Last_Change")],sep = ", ")
#                   } # if (Var_i==1) {
#                 } # for (Var_i in 1:dim(this_day_all_data)[1]) {
#               } else {
#                 all_Vars <- unique(this_day_all_data$Date_of_Last_Change)
#               }
#               input_mat3_aves[rstart_aves:rstop_aves,c("Date_of_Last_Change")] <- all_Vars # input composite of data
#               rm(all_Vars)
#               # State_Abbrev: input unique State_Abbrev
#               if (length(unique(this_day_all_data$State_Abbrev))>1) {stop("State_Abbrev doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("State_Abbrev")] <- as.character(unique(this_day_all_data$State_Abbrev)) # input unique value
#               # Winter: input unique Winter
#               if (length(unique(this_day_all_data$Winter))>1) {stop("Winter doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Winter")] <- as.numeric(mean(this_day_all_data$Winter)) # input average 
#               # Data_Source_Name_Display: input unique Data_Source_Name_Display
#               if (length(unique(this_day_all_data$Data_Source_Name_Display))>1) {stop("Data_Source_Name_Display doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Data_Source_Name_Display")] <- as.character(unique(this_day_all_data$Data_Source_Name_Display)) # input unique value
#               # Data_Source_Name_Short: input unique Data_Source_Name_Short
#               if (length(unique(this_day_all_data$Data_Source_Name_Short))>1) {stop("Data_Source_Name_Short doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Data_Source_Name_Short")] <- as.character(unique(this_day_all_data$Data_Source_Name_Short)) # input unique value
#               # Data_Source_Counter: input unique value
#               if (length(unique(this_day_all_data$Data_Source_Counter))>1) {stop("Data_Source_Counter don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Data_Source_Counter")] <- as.numeric(mean(this_day_all_data$Data_Source_Counter)) # input average 
#               # "Source_File": input unique "Source_File"
#               if (length(unique(this_day_all_data$Source_File))>1) {
#                 for (Var_i in 1:dim(this_day_all_data)[1]) { # loop through all values and paste them together
#                   if (Var_i==1) {
#                     all_Vars <- this_day_all_data[Var_i,c("Source_File")]
#                   } else {
#                     all_Vars <- paste(all_Vars,this_day_all_data[Var_i,c("Source_File")],sep = ", ")
#                   } # if (Var_i==1) {
#                 } # for (Var_i in 1:dim(this_day_all_data)[1]) {
#               } else {
#                 all_Vars <- unique(this_day_all_data$Source_File)
#               }
#               input_mat3_aves[rstart_aves:rstop_aves,c("Source_File")] <- all_Vars # input composite of data
#               rm(all_Vars)
#               # Composite_of_N_rows: sum the Composite_of_N_rows
#               if (min(this_day_all_data$Composite_of_N_rows)<1) {stop("Composite_of_N_rows doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Composite_of_N_rows")] <- as.numeric(sum(this_day_all_data$Composite_of_N_rows)) # input average 
#               # N_Negative_Obs: sum the N_Negative_Obs
#               if (max(this_day_all_data$N_Negative_Obs)>0) {stop("N_Negative_Obs doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("N_Negative_Obs")] <- as.numeric(sum(this_day_all_data$N_Negative_Obs)) # input average 
#               # "flg.Lat": input unique "flg.Lat"
#               if (length(unique(this_day_all_data$flg.Lat))>1) {stop("flg.Lat doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg.Lat")] <- as.character(unique(this_day_all_data$flg.Lat)) # input unique value
#               # "flg.Lon: input unique flg.Lon
#               if (length(unique(this_day_all_data$flg.Lon))>1) {stop("flg.Lon doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg.Lon")] <- as.character(unique(this_day_all_data$flg.Lon)) # input unique value
#               # "Type: input unique Type
#               if (length(unique(this_day_all_data$Type))>1) {stop("Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Type")] <- as.character(unique(this_day_all_data$Type)) # input unique value
#               # "flg.Type: input unique flg.Type
#               if (length(unique(this_day_all_data$flg.Type))>1) {stop("flg.Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg.Type")] <- as.character(unique(this_day_all_data$flg.Type)) # input unique value
#               # flg.Site_Num: input unique flg.Site_Num
#               if (length(unique(this_day_all_data$flg.Site_Num))>1) {stop("flg.Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg.Site_Num")] <- as.character(unique(this_day_all_data$flg.Site_Num)) # input unique value
#               # flg.PM25_Obs: input unique flg.PM25_Obs
#               if (length(unique(this_day_all_data$flg.PM25_Obs))>1) {stop("flg.PM25_Obs doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg.PM25_Obs")] <- as.character(unique(this_day_all_data$flg.PM25_Obs)) # input unique value
#               # l.m.Ave..Air.Flw: input unique l.m.Ave..Air.Flw
#               if (length(unique(this_day_all_data$l.m.Ave..Air.Flw))>1) {stop("l.m.Ave..Air.Flw doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("l.m.Ave..Air.Flw")] <- as.numeric(mean(this_day_all_data$l.m.Ave..Air.Flw)) # input average 
#               # flg.AirFlw: input unique flg.AirFlw
#               if (length(unique(this_day_all_data$flg.AirFlw))>1) {stop("flg.AirFlw doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg.AirFlw")] <- as.character(unique(this_day_all_data$flg.AirFlw)) # input unique value
#               # Deg.C.Av.Air.Temp: input unique Deg.C.Av.Air.Temp
#               if (length(unique(this_day_all_data$Deg.C.Av.Air.Temp))>1) {stop("Deg.C.Av.Air.Temp doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Deg.C.Av.Air.Temp")] <- as.numeric(mean(this_day_all_data$Deg.C.Av.Air.Temp)) # input average 
#               # flg.AirTemp: input unique flg.AirTemp
#               if (length(unique(this_day_all_data$flg.AirTemp))>1) {stop("flg.AirTemp doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg.AirFlw")] <- as.character(unique(this_day_all_data$flg.AirFlw)) # input unique value
#               # X..Rel.Humidty: input unique X..Rel.Humidty
#               if (length(unique(this_day_all_data$X..Rel.Humidty))>1) {stop("X..Rel.Humidty doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("X..Rel.Humidty")] <- as.numeric(mean(this_day_all_data$X..Rel.Humidty)) # input average 
#               # flg.RelHumid: input unique flg.RelHumid
#               if (length(unique(this_day_all_data$flg.RelHumid))>1) {stop("flg.RelHumid doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg.RelHumid")] <- as.character(unique(this_day_all_data$flg.RelHumid)) # input unique value
#               # mbar.Barom.Press: input unique mbar.Barom.Press
#               if (length(unique(this_day_all_data$mbar.Barom.Press))>1) {stop("mbar.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("mbar.Barom.Press")] <- as.numeric(mean(this_day_all_data$mbar.Barom.Press)) # input average 
#               # flg.Barom.Press: input unique flg.Barom.Press
#               if (length(unique(this_day_all_data$flg.Barom.Press))>1) {stop("flg.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg.Barom.Press")] <- as.character(unique(this_day_all_data$flg.Barom.Press)) # input unique value
#               # deg.C.Sensor..Int.AT: input unique deg.C.Sensor..Int.AT
#               if (length(unique(this_day_all_data$deg.C.Sensor..Int.AT))>1) {stop("deg.C.Sensor..Int.AT doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("deg.C.Sensor..Int.AT")] <- as.numeric(mean(this_day_all_data$deg.C.Sensor..Int.AT)) # input average 
#               # flg.deg.C.Sensor.Int.AT: input unique flg.deg.C.Sensor.Int.AT
#               if (length(unique(this_day_all_data$flg.deg.C.Sensor.Int.AT))>1) {stop("flg.deg.C.Sensor.Int.AT doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg.deg.C.Sensor.Int.AT")] <- as.character(unique(this_day_all_data$flg.deg.C.Sensor.Int.AT)) # input unique value
#               # X..Sensor.Int.RH: input unique X..Sensor.Int.RH
#               if (length(unique(this_day_all_data$X..Sensor.Int.RH))>1) {stop("X..Sensor.Int.RH doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("X..Sensor.Int.RH")] <- as.numeric(mean(this_day_all_data$X..Sensor.Int.RH)) # input average 
#               # flg.deg.C.Sensor.Int.A: input unique flg.deg.C.Sensor.Int.A
#               if (length(unique(this_day_all_data$flg..SensorIntRH))>1) {stop("flg..SensorIntRH doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg..SensorIntRH")] <- as.character(unique(this_day_all_data$flg..SensorIntRH)) # input unique value
#               # Wind.Speed.m.s: input unique Wind.Speed.m.s
#               if (length(unique(this_day_all_data$Wind.Speed.m.s))>1) {stop("Wind.Speed.m.s doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Wind.Speed.m.s")] <- as.numeric(mean(this_day_all_data$Wind.Speed.m.s)) # input average 
#               # flg.WindSpeed: input unique flg.WindSpeed
#               if (length(unique(this_day_all_data$flg.WindSpeed))>1) {stop("flg.WindSpeed doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg.WindSpeed")] <- as.character(unique(this_day_all_data$flg.WindSpeed)) # input unique value
#               # Battery.Voltage.volts: input unique Battery.Voltage.volts
#               if (length(unique(this_day_all_data$Battery.Voltage.volts))>1) {stop("Battery.Voltage.volts doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Battery.Voltage.volts")] <- as.numeric(mean(this_day_all_data$Battery.Voltage.volts)) # input average 
#               # flg.BatteryVoltage: input unique flg.BatteryVoltage
#               if (length(unique(this_day_all_data$flg.BatteryVoltage))>1) {stop("flg.BatteryVoltage doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg.BatteryVoltage")] <- as.character(unique(this_day_all_data$flg.BatteryVoltage)) # input unique value
#               # Alarm: input unique Alarm
#               if (length(unique(this_day_all_data$Alarm))>1) {stop("Alarm doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("Alarm")] <- as.numeric(mean(this_day_all_data$Alarm)) # input average 
#               # flg.Alarm: input unique flg.Alarm
#               if (length(unique(this_day_all_data$flg.Alarm))>1) {stop("flg.Alarm doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("flg.Alarm")] <- as.character(unique(this_day_all_data$flg.Alarm)) # input unique value
#               # InDayLatDiff: input unique InDayLatDiff
#               if (length(unique(this_day_all_data$InDayLatDiff))>1) {stop("InDayLatDiff doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("InDayLatDiff")] <- as.numeric(mean(this_day_all_data$InDayLatDiff)) # input average 
#               # InDayLonDiff: input unique InDayLonDiff
#               if (length(unique(this_day_all_data$InDayLonDiff))>1) {stop("InDayLonDiff doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_aves[rstart_aves:rstop_aves,c("InDayLonDiff")] <- as.numeric(mean(this_day_all_data$InDayLonDiff)) # input average 
#               # PlottingColor: setting value specific to this if-statement
#               input_mat3_aves[rstart_aves:rstop_aves,c("PlottingColor")] <- as.character(set_plot_color)
#               rstart_aves <- rstop_aves+1 # move counter up
#               
#               #rm(input_mat3_aves,rstart_aves,rstop_aves) # COMMENT
#               
#               #### for the colocated data 2 data points, one is 88101 and one is 88502 ####
#               rstop_colocated <- rstart_colocated+dim(this_day_all_data)[1]-1
#               #input_mat3_colocated[rstart_colocated:rstop_colocated,] <- this_day_all_data
#               #rstart_colocated <- rstop_colocated+1
#               
#               # PM2.5 Obs (concentration):take average
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("PM2.5_Obs")] <- as.numeric(this_day_all_data$PM2.5_Obs) # input values directly
#               # latitude: input unique value
#               if (length(unique(this_day_all_data$PM2.5_Lat))>1) {stop("latitudes don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("PM2.5_Lat")] <- as.numeric(mean(this_day_all_data$PM2.5_Lat)) # input average
#               # longitude: input unique value
#               if (length(unique(this_day_all_data$PM2.5_Lon))>1) {stop("longitudes don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("PM2.5_Lon")] <- as.numeric(mean(this_day_all_data$PM2.5_Lon)) # input average
#               # Datum: input unique value
#               if (length(unique(this_day_all_data$Datum))>1) {stop("Datums don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Datum")] <- as.character(unique(this_day_all_data$Datum)) # input unique value
#               # Date_Local: input unique date
#               if (unique(this_day_all_data$Date_Local)!=this_day) {stop("Date_Local don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Date_Local")] <- as.character(unique(this_day_all_data$Date_Local)) # input unique value
#               # Year: input unique year
#               if (length(unique(this_day_all_data$Year))>1) {stop("Years don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Year")] <- as.numeric(mean(this_day_all_data$Year)) # input average
#               # Month: input unique month
#               if (length(unique(this_day_all_data$Month))>1) {stop("Months don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Month")] <- as.numeric(mean(this_day_all_data$Month)) # input average
#               # Day: input unique day
#               if (length(unique(this_day_all_data$Day))>1) {stop("Day doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Day")] <- as.numeric(mean(this_day_all_data$Day)) # input average
#               # State_Code: input unique state code
#               if (length(unique(this_day_all_data$State_Code))>1) {stop("State_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("State_Code")] <- as.numeric(mean(this_day_all_data$State_Code)) # input average
#               # County_Code: input unique county code
#               if (length(unique(this_day_all_data$County_Code))>1) {stop("County_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("County_Code")] <- as.numeric(mean(this_day_all_data$County_Code)) # input average
#               # Site_Num: input unique site num
#               if (length(unique(this_day_all_data$Site_Num))>1) {stop("Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Site_Num")] <- as.numeric(mean(this_day_all_data$Site_Num)) # input average
#               # Parameter Code: input parameter codes (no check since we know they are different)
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Parameter_Code")] <- as.numeric(this_day_all_data$Parameter_Code) # input values directly
#               # POC: - input as they are
#               if (length(unique(this_day_all_data$POC))>1) {stop("POC doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("POC")] <- as.numeric(this_day_all_data$POC) # input average
#               # Parameter_Name: input Parameter Name - no check since they can be different
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Parameter_Name")] <- as.character(this_day_all_data$Parameter_Name) # input values directly
#               # Sample Duration: input unique Sample Duration
#               if (length(unique(this_day_all_data$Sample_Duration))>1) {stop("Sample_Duration doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Sample_Duration")] <- as.character(unique(this_day_all_data$Sample_Duration)) # input unique value
#               # Pollutant_Standard: input unique Pollutant Standard - no check since they can be different
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Pollutant_Standard")] <- as.character(this_day_all_data$Pollutant_Standard) # input unique value
#               # Units_of_Measure: input unique Units of Measure
#               if (length(unique(this_day_all_data$Units_of_Measure))>1) {stop("Units_of_Measure doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Units_of_Measure")] <- as.character(unique(this_day_all_data$Units_of_Measure)) # input unique value
#               # Event_Type: input unique event type
#               if (length(unique(this_day_all_data$Event_Type))>1) {stop("Event_Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Event_Type")] <- as.character(unique(this_day_all_data$Event_Type)) # input unique value
#               # Observation Count: input directly
#               if (min(this_day_all_data$Observation_Count)<1) {stop("Observation_Count doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Observation_Count")] <- as.numeric(this_day_all_data$Observation_Count) # input directly
#               # Observation Percent: input directly
#               if (min(this_day_all_data$Observation_Percent)<75) {stop("Observation_Percent doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Observation_Percent")] <- as.numeric(this_day_all_data$Observation_Percent) # input directly
#               # X1st_Max_Value: input directly
#               if (min(this_day_all_data$X1st_Max_Value)<0) {stop("negative concentration (X1st_Max_Value) Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("X1st_Max_Value")] <- as.numeric(this_day_all_data$X1st_Max_Value) # input average
#               # X1st_Max_Hour: input directly
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("X1st_Max_Hour")] <- as.character(this_day_all_data$X1st_Max_Hour)
#               # AQI: input mean AQI
#               if (min(this_day_all_data$AQI)<0) {stop("negative concentration (AQI) Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("AQI")] <- as.numeric(mean(this_day_all_data$AQI)) # input average
#               # Method_Code: input Method_Code - no check since they can be different
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Method_Code")] <- as.character(this_day_all_data$Method_Code) # input unique value
#               # Method_Name: input Method_Name - no check since they can be different
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Method_Name")] <- as.character(this_day_all_data$Method_Name) # input unique value
#               # PM25_Station_Name: input unique PM25_Station_Name
#               if (length(unique(this_day_all_data$PM25_Station_Name))>1) {stop("PM25_Station_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("PM25_Station_Name")] <- as.character(unique(this_day_all_data$PM25_Station_Name)) # input unique value
#               # Address: input unique Address
#               if (length(unique(this_day_all_data$Address))>1) {stop("Address doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Address")] <- as.character(unique(this_day_all_data$Address)) # input unique value
#               # State_Name: input unique State_Name
#               if (length(unique(this_day_all_data$State_Name))>1) {stop("State_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("State_Name")] <- as.character(unique(this_day_all_data$State_Name)) # input unique value
#               # County_Name: input unique County_Name
#               if (length(unique(this_day_all_data$County_Name))>1) {stop("County_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("County_Name")] <- as.character(unique(this_day_all_data$County_Name)) # input unique value
#               # City_Name: input unique City_Name
#               if (length(unique(this_day_all_data$City_Name))>1) {stop("City_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("City_Name")] <- as.character(unique(this_day_all_data$City_Name)) # input unique value
#               # CBSA_Name: input unique CBSA_Name
#               if (length(unique(this_day_all_data$CBSA_Name))>1) {stop("CBSA_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("CBSA_Name")] <- as.character(unique(this_day_all_data$CBSA_Name)) # input unique value
#               # Date_of_Last_Change: input Date_of_Last_Change - no check since they can be different
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Date_of_Last_Change")] <- as.character(this_day_all_data$Date_of_Last_Change) # input values directly
#               # State_Abbrev: input unique State_Abbrev
#               if (length(unique(this_day_all_data$State_Abbrev))>1) {stop("State_Abbrev doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("State_Abbrev")] <- as.character(unique(this_day_all_data$State_Abbrev)) # input unique value
#               # Winter: input unique Winter
#               if (length(unique(this_day_all_data$Winter))>1) {stop("Winter doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Winter")] <- as.numeric(mean(this_day_all_data$Winter)) # input average
#               # Data_Source_Name_Display: input unique Data_Source_Name_Display
#               if (length(unique(this_day_all_data$Data_Source_Name_Display))>1) {stop("Data_Source_Name_Display doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Data_Source_Name_Display")] <- as.character(unique(this_day_all_data$Data_Source_Name_Display)) # input unique value
#               # Data_Source_Name_Short: input unique Data_Source_Name_Short
#               if (length(unique(this_day_all_data$Data_Source_Name_Short))>1) {stop("Data_Source_Name_Short doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Data_Source_Name_Short")] <- as.character(unique(this_day_all_data$Data_Source_Name_Short)) # input unique value
#               # Data_Source_Counter: input unique value
#               if (length(unique(this_day_all_data$Data_Source_Counter))>1) {stop("Data_Source_Counter don't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Data_Source_Counter")] <- as.numeric(mean(this_day_all_data$Data_Source_Counter)) # input average
#               # "Source_File": input "Source_File" directly - no check since they can be different
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Source_File")] <- as.character(this_day_all_data$Source_File) # input unique value
#               # Composite_of_N_rows: input directly
#               if (min(this_day_all_data$Composite_of_N_rows)<1) {stop("Composite_of_N_rows doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Composite_of_N_rows")] <- as.numeric(this_day_all_data$Composite_of_N_rows) # input directly
#               # N_Negative_Obs: input_directly
#               if (max(this_day_all_data$N_Negative_Obs)>0) {stop("N_Negative_Obs doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("N_Negative_Obs")] <- as.numeric(this_day_all_data$N_Negative_Obs) # input average
#               # "flg.Lat": input unique "flg.Lat"
#               if (length(unique(this_day_all_data$flg.Lat))>1) {stop("flg.Lat doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Lat")] <- as.character(unique(this_day_all_data$flg.Lat)) # input unique value
#               # "flg.Lon: input unique flg.Lon
#               if (length(unique(this_day_all_data$flg.Lon))>1) {stop("flg.Lon doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Lon")] <- as.character(unique(this_day_all_data$flg.Lon)) # input unique value
#               # "Type: input unique Type
#               if (length(unique(this_day_all_data$Type))>1) {stop("Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Type")] <- as.character(unique(this_day_all_data$Type)) # input unique value
#               # "flg.Type: input unique flg.Type
#               if (length(unique(this_day_all_data$flg.Type))>1) {stop("flg.Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Type")] <- as.character(unique(this_day_all_data$flg.Type)) # input unique value
#               # flg.Site_Num: input unique flg.Site_Num
#               if (length(unique(this_day_all_data$flg.Site_Num))>1) {stop("flg.Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Site_Num")] <- as.character(unique(this_day_all_data$flg.Site_Num)) # input unique value
#               # flg.PM25_Obs: input unique flg.PM25_Obs
#               if (length(unique(this_day_all_data$flg.PM25_Obs))>1) {stop("flg.PM25_Obs doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.PM25_Obs")] <- as.character(unique(this_day_all_data$flg.PM25_Obs)) # input unique value
#               # l.m.Ave..Air.Flw: input unique l.m.Ave..Air.Flw
#               if (length(unique(this_day_all_data$l.m.Ave..Air.Flw))>1) {stop("l.m.Ave..Air.Flw doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("l.m.Ave..Air.Flw")] <- as.numeric(mean(this_day_all_data$l.m.Ave..Air.Flw)) # input average
#               # flg.AirFlw: input unique flg.AirFlw
#               if (length(unique(this_day_all_data$flg.AirFlw))>1) {stop("flg.AirFlw doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.AirFlw")] <- as.character(unique(this_day_all_data$flg.AirFlw)) # input unique value
#               # Deg.C.Av.Air.Temp: input unique Deg.C.Av.Air.Temp
#               if (length(unique(this_day_all_data$Deg.C.Av.Air.Temp))>1) {stop("Deg.C.Av.Air.Temp doesn't match. Look at data/code and write more code")} # check that latitudes match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Deg.C.Av.Air.Temp")] <- as.numeric(mean(this_day_all_data$Deg.C.Av.Air.Temp)) # input average
#               # flg.AirTemp: input unique flg.AirTemp
#               if (length(unique(this_day_all_data$flg.AirTemp))>1) {stop("flg.AirTemp doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.AirFlw")] <- as.character(unique(this_day_all_data$flg.AirFlw)) # input unique value
#               # X..Rel.Humidty: input unique X..Rel.Humidty
#               if (length(unique(this_day_all_data$X..Rel.Humidty))>1) {stop("X..Rel.Humidty doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("X..Rel.Humidty")] <- as.numeric(mean(this_day_all_data$X..Rel.Humidty)) # input average
#               # flg.RelHumid: input unique flg.RelHumid
#               if (length(unique(this_day_all_data$flg.RelHumid))>1) {stop("flg.RelHumid doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.RelHumid")] <- as.character(unique(this_day_all_data$flg.RelHumid)) # input unique value
#               # mbar.Barom.Press: input unique mbar.Barom.Press
#               if (length(unique(this_day_all_data$mbar.Barom.Press))>1) {stop("mbar.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("mbar.Barom.Press")] <- as.numeric(mean(this_day_all_data$mbar.Barom.Press)) # input average
#               # flg.Barom.Press: input unique flg.Barom.Press
#               if (length(unique(this_day_all_data$flg.Barom.Press))>1) {stop("flg.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Barom.Press")] <- as.character(unique(this_day_all_data$flg.Barom.Press)) # input unique value
#               # deg.C.Sensor..Int.AT: input unique deg.C.Sensor..Int.AT
#               if (length(unique(this_day_all_data$deg.C.Sensor..Int.AT))>1) {stop("deg.C.Sensor..Int.AT doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("deg.C.Sensor..Int.AT")] <- as.numeric(mean(this_day_all_data$deg.C.Sensor..Int.AT)) # input average
#               # flg.deg.C.Sensor.Int.AT: input unique flg.deg.C.Sensor.Int.AT
#               if (length(unique(this_day_all_data$flg.deg.C.Sensor.Int.AT))>1) {stop("flg.deg.C.Sensor.Int.AT doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.deg.C.Sensor.Int.AT")] <- as.character(unique(this_day_all_data$flg.deg.C.Sensor.Int.AT)) # input unique value
#               # X..Sensor.Int.RH: input unique X..Sensor.Int.RH
#               if (length(unique(this_day_all_data$X..Sensor.Int.RH))>1) {stop("X..Sensor.Int.RH doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("X..Sensor.Int.RH")] <- as.numeric(mean(this_day_all_data$X..Sensor.Int.RH)) # input average
#               # flg.deg.C.Sensor.Int.A: input unique flg.deg.C.Sensor.Int.A
#               if (length(unique(this_day_all_data$flg..SensorIntRH))>1) {stop("flg..SensorIntRH doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg..SensorIntRH")] <- as.character(unique(this_day_all_data$flg..SensorIntRH)) # input unique value
#               # Wind.Speed.m.s: input unique Wind.Speed.m.s
#               if (length(unique(this_day_all_data$Wind.Speed.m.s))>1) {stop("Wind.Speed.m.s doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Wind.Speed.m.s")] <- as.numeric(mean(this_day_all_data$Wind.Speed.m.s)) # input average
#               # flg.WindSpeed: input unique flg.WindSpeed
#               if (length(unique(this_day_all_data$flg.WindSpeed))>1) {stop("flg.WindSpeed doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.WindSpeed")] <- as.character(unique(this_day_all_data$flg.WindSpeed)) # input unique value
#               # Battery.Voltage.volts: input unique Battery.Voltage.volts
#               if (length(unique(this_day_all_data$Battery.Voltage.volts))>1) {stop("Battery.Voltage.volts doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Battery.Voltage.volts")] <- as.numeric(mean(this_day_all_data$Battery.Voltage.volts)) # input average
#               # flg.BatteryVoltage: input unique flg.BatteryVoltage
#               if (length(unique(this_day_all_data$flg.BatteryVoltage))>1) {stop("flg.BatteryVoltage doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.BatteryVoltage")] <- as.character(unique(this_day_all_data$flg.BatteryVoltage)) # input unique value
#               # Alarm: input unique Alarm
#               if (length(unique(this_day_all_data$Alarm))>1) {stop("Alarm doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("Alarm")] <- as.numeric(mean(this_day_all_data$Alarm)) # input average
#               # flg.Alarm: input unique flg.Alarm
#               if (length(unique(this_day_all_data$flg.Alarm))>1) {stop("flg.Alarm doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Alarm")] <- as.character(unique(this_day_all_data$flg.Alarm)) # input unique value
#               # InDayLatDiff: input unique InDayLatDiff
#               if (length(unique(this_day_all_data$InDayLatDiff))>1) {stop("InDayLatDiff doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("InDayLatDiff")] <- as.numeric(mean(this_day_all_data$InDayLatDiff)) # input average
#               # InDayLonDiff: input unique InDayLonDiff
#               if (length(unique(this_day_all_data$InDayLonDiff))>1) {stop("InDayLonDiff doesn't match. Look at data/code and write more code")} # check that values match
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("InDayLonDiff")] <- as.numeric(mean(this_day_all_data$InDayLonDiff)) # input average
#               # PlottingColor: setting value specific to this if-statement
#               input_mat3_colocated[rstart_colocated:rstop_colocated,c("PlottingColor")] <- as.character(set_plot_color)
#               
#               rstart_colocated <- rstop_colocated+1
#               
#               rm(this_day_all_data)
#             } # if (min(this_day_all_data$Parameter_Code)==88101 & max(this_day_all_data$Parameter_Code)==88502 & dim(this_day_all_data)[1]==2) { # 2 data points on this day, one is 88101 and the other is 88502
#             
#             
#             
#             
#           } else { # different values of POC
#             #print("POC values are not all the same")
#             
#               if (length(unique(this_day_all_data$Data_Source_Name_Short))==1) { # is all the data from one source?
#                 print(paste("data all from one source: ",unique(this_day_all_data$Data_Source_Name_Short),sep = ""))
#                 set_plot_color <- "aquamarine2"
#                 #### for the aves data - all data from one source, different POCs  ####
#                 rstop_aves <- rstart_aves
#                 # PM2.5 Obs (concentration):take average
#                 input_mat3_aves[rstart_aves:rstop_aves,c("PM2.5_Obs")] <- as.numeric(mean(this_day_all_data$PM2.5_Obs)) # input average 
#                 # latitude: input unique value
#                 if (length(unique(this_day_all_data$PM2.5_Lat))>1) {stop("latitudes don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("PM2.5_Lat")] <- as.numeric(mean(this_day_all_data$PM2.5_Lat)) # input average 
#                 # longitude: input unique value
#                 if (length(unique(this_day_all_data$PM2.5_Lon))>1) {stop("longitudes don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("PM2.5_Lon")] <- as.numeric(mean(this_day_all_data$PM2.5_Lon)) # input average 
#                 # Datum: input unique value
#                 if (length(unique(this_day_all_data$Datum))>1) {stop("Datums don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Datum")] <- as.character(unique(this_day_all_data$Datum)) # input unique value
#                 # Date_Local: input unique date 
#                 if (unique(this_day_all_data$Date_Local)!=this_day) {stop("Date_Local don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Date_Local")] <- as.character(unique(this_day_all_data$Date_Local)) # input unique value
#                 # Year: input unique year
#                 if (length(unique(this_day_all_data$Year))>1) {stop("Years don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Year")] <- as.numeric(mean(this_day_all_data$Year)) # input average 
#                 # Month: input unique month
#                 if (length(unique(this_day_all_data$Month))>1) {stop("Months don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Month")] <- as.numeric(mean(this_day_all_data$Month)) # input average 
#                 # Day: input unique day
#                 if (length(unique(this_day_all_data$Day))>1) {stop("Day doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Day")] <- as.numeric(mean(this_day_all_data$Day)) # input average 
#                 # State_Code: input unique state code
#                 if (length(unique(this_day_all_data$State_Code))>1) {stop("State_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("State_Code")] <- as.numeric(mean(this_day_all_data$State_Code)) # input average 
#                 # County_Code: input unique county code
#                 if (length(unique(this_day_all_data$County_Code))>1) {stop("County_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("County_Code")] <- as.numeric(mean(this_day_all_data$County_Code)) # input average 
#                 # Site_Num: input unique site num
#                 if (length(unique(this_day_all_data$Site_Num))>1) {stop("Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Site_Num")] <- as.numeric(mean(this_day_all_data$Site_Num)) # input average 
#                 # Parameter Code: input unique parameter code
#                 if (length(unique(this_day_all_data$Parameter_Code))>1) {stop("Parameter_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Parameter_Code")] <- as.numeric(mean(this_day_all_data$Parameter_Code)) # input average 
#                 # POC: no check statement for POC since we know they are different - take the average and multiply by 10 as new POC value
#                 input_mat3_aves[rstart_aves:rstop_aves,c("POC")] <- as.numeric(mean(this_day_all_data$POC)*10) # input average 
#                 # Parameter_Name: input unique Parameter Name
#                 if (length(unique(this_day_all_data$Parameter_Name))>1) {stop("Parameter_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Parameter_Name")] <- as.character(unique(this_day_all_data$Parameter_Name)) # input unique value
#                 # Sample Duration: input unique Sample Duration
#                 if (length(unique(this_day_all_data$Sample_Duration))>1) {stop("Sample_Duration doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Sample_Duration")] <- as.character(unique(this_day_all_data$Sample_Duration)) # input unique value
#                 # Pollutant_Standard: input unique Pollutant Standard
#                 if (length(unique(this_day_all_data$Pollutant_Standard))>1) {stop("Pollutant_Standard doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Pollutant_Standard")] <- as.character(unique(this_day_all_data$Pollutant_Standard)) # input unique value
#                 # Units_of_Measure: input unique Units of Measure
#                 if (length(unique(this_day_all_data$Units_of_Measure))>1) {stop("Units_of_Measure doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Units_of_Measure")] <- as.character(unique(this_day_all_data$Units_of_Measure)) # input unique value
#                 # Event_Type: input unique event type
#                 if (length(unique(this_day_all_data$Event_Type))>1) {stop("Event_Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Event_Type")] <- as.character(unique(this_day_all_data$Event_Type)) # input unique value
#                 # Observation Count: sum the observation counts
#                 if (min(this_day_all_data$Observation_Count)<1) {stop("Observation_Count doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Observation_Count")] <- as.numeric(sum(this_day_all_data$Observation_Count)) # input average 
#                 # Observation Percent: sum the percentages, unless another idea comes along
#                 if (min(this_day_all_data$Observation_Percent)<75) {stop("Observation_Percent doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Observation_Percent")] <- as.numeric(sum(this_day_all_data$Observation_Percent)) # input average 
#                 # X1st_Max_Value: input mean X1st_Max_Value
#                 if (min(this_day_all_data$X1st_Max_Value)<0) {stop("negative concentration (X1st_Max_Value) Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("X1st_Max_Value")] <- as.numeric(mean(this_day_all_data$X1st_Max_Value)) # input average 
#                 # X1st_Max_Hour: input NA for X1st_Max_Hour, taking an average of hours of the day seems meaningless
#                 input_mat3_aves[rstart_aves:rstop_aves,c("X1st_Max_Hour")] <- NA 
#                 # AQI: input mean AQI
#                 if (min(this_day_all_data$AQI)<0) {stop("negative concentration (AQI) Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("AQI")] <- as.numeric(mean(this_day_all_data$AQI)) # input average 
#                 # Method_Code: input unique event type
#                 if (length(unique(this_day_all_data$Method_Code))>1) {stop("Method_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Method_Code")] <- as.character(unique(this_day_all_data$Method_Code)) # input unique value
#                 # Method_Name: input unique Method_Name
#                 if (length(unique(this_day_all_data$Method_Name))>1) {stop("Method_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Method_Name")] <- as.character(unique(this_day_all_data$Method_Name)) # input unique value
#                 # PM25_Station_Name: input unique PM25_Station_Name
#                 if (length(unique(this_day_all_data$PM25_Station_Name))>1) {stop("PM25_Station_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("PM25_Station_Name")] <- as.character(unique(this_day_all_data$PM25_Station_Name)) # input unique value
#                 # Address: input unique Address
#                 if (length(unique(this_day_all_data$Address))>1) {stop("Address doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Address")] <- as.character(unique(this_day_all_data$Address)) # input unique value
#                 # State_Name: input unique State_Name
#                 if (length(unique(this_day_all_data$State_Name))>1) {stop("State_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("State_Name")] <- as.character(unique(this_day_all_data$State_Name)) # input unique value
#                 # County_Name: input unique County_Name
#                 if (length(unique(this_day_all_data$County_Name))>1) {stop("County_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("County_Name")] <- as.character(unique(this_day_all_data$County_Name)) # input unique value
#                 # City_Name: input unique City_Name
#                 if (length(unique(this_day_all_data$City_Name))>1) {stop("City_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("City_Name")] <- as.character(unique(this_day_all_data$City_Name)) # input unique value
#                 # CBSA_Name: input unique CBSA_Name
#                 if (length(unique(this_day_all_data$CBSA_Name))>1) {stop("CBSA_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("CBSA_Name")] <- as.character(unique(this_day_all_data$CBSA_Name)) # input unique value
#                 # Date_of_Last_Change: input unique Date_of_Last_Change
#                 if (length(unique(this_day_all_data$Date_of_Last_Change))>1) {stop("Date_of_Last_Change doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Date_of_Last_Change")] <- as.character(unique(this_day_all_data$Date_of_Last_Change)) # input unique value
#                 # State_Abbrev: input unique State_Abbrev
#                 if (length(unique(this_day_all_data$State_Abbrev))>1) {stop("State_Abbrev doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("State_Abbrev")] <- as.character(unique(this_day_all_data$State_Abbrev)) # input unique value
#                 # Winter: input unique Winter
#                 if (length(unique(this_day_all_data$Winter))>1) {stop("Winter doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Winter")] <- as.numeric(mean(this_day_all_data$Winter)) # input average 
#                 # Data_Source_Name_Display: input unique Data_Source_Name_Display
#                 if (length(unique(this_day_all_data$Data_Source_Name_Display))>1) {stop("Data_Source_Name_Display doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Data_Source_Name_Display")] <- as.character(unique(this_day_all_data$Data_Source_Name_Display)) # input unique value
#                 # Data_Source_Name_Short: input unique Data_Source_Name_Short
#                 if (length(unique(this_day_all_data$Data_Source_Name_Short))>1) {stop("Data_Source_Name_Short doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Data_Source_Name_Short")] <- as.character(unique(this_day_all_data$Data_Source_Name_Short)) # input unique value
#                 # Data_Source_Counter: input unique value
#                 if (length(unique(this_day_all_data$Data_Source_Counter))>1) {stop("Data_Source_Counter don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Data_Source_Counter")] <- as.numeric(mean(this_day_all_data$Data_Source_Counter)) # input average 
#                 # "Source_File": input unique "Source_File"
#                 if (length(unique(this_day_all_data$Source_File))>1) {stop("Source_File doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Source_File")] <- as.character(unique(this_day_all_data$Source_File)) # input unique value
#                 # Composite_of_N_rows: sum the Composite_of_N_rows
#                 if (min(this_day_all_data$Composite_of_N_rows)<1) {stop("Composite_of_N_rows doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Composite_of_N_rows")] <- as.numeric(sum(this_day_all_data$Composite_of_N_rows)) # input average 
#                 # N_Negative_Obs: sum the N_Negative_Obs
#                 if (max(this_day_all_data$N_Negative_Obs)>0) {stop("N_Negative_Obs doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("N_Negative_Obs")] <- as.numeric(sum(this_day_all_data$N_Negative_Obs)) # input average 
#                 # "flg.Lat": input unique "flg.Lat"
#                 if (length(unique(this_day_all_data$flg.Lat))>1) {stop("flg.Lat doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg.Lat")] <- as.character(unique(this_day_all_data$flg.Lat)) # input unique value
#                 # "flg.Lon: input unique flg.Lon
#                 if (length(unique(this_day_all_data$flg.Lon))>1) {stop("flg.Lon doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg.Lon")] <- as.character(unique(this_day_all_data$flg.Lon)) # input unique value
#                 # "Type: input unique Type
#                 if (length(unique(this_day_all_data$Type))>1) {stop("Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Type")] <- as.character(unique(this_day_all_data$Type)) # input unique value
#                 # "flg.Type: input unique flg.Type
#                 if (length(unique(this_day_all_data$flg.Type))>1) {stop("flg.Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg.Type")] <- as.character(unique(this_day_all_data$flg.Type)) # input unique value
#                 # flg.Site_Num: input unique flg.Site_Num
#                 if (length(unique(this_day_all_data$flg.Site_Num))>1) {stop("flg.Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg.Site_Num")] <- as.character(unique(this_day_all_data$flg.Site_Num)) # input unique value
#                 # flg.PM25_Obs: input unique flg.PM25_Obs
#                 if (length(unique(this_day_all_data$flg.PM25_Obs))>1) {stop("flg.PM25_Obs doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg.PM25_Obs")] <- as.character(unique(this_day_all_data$flg.PM25_Obs)) # input unique value
#                 # l.m.Ave..Air.Flw: input unique l.m.Ave..Air.Flw
#                 if (length(unique(this_day_all_data$l.m.Ave..Air.Flw))>1) {stop("l.m.Ave..Air.Flw doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("l.m.Ave..Air.Flw")] <- as.numeric(mean(this_day_all_data$l.m.Ave..Air.Flw)) # input average 
#                 # flg.AirFlw: input unique flg.AirFlw
#                 if (length(unique(this_day_all_data$flg.AirFlw))>1) {stop("flg.AirFlw doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg.AirFlw")] <- as.character(unique(this_day_all_data$flg.AirFlw)) # input unique value
#                 # Deg.C.Av.Air.Temp: input unique Deg.C.Av.Air.Temp
#                 if (length(unique(this_day_all_data$Deg.C.Av.Air.Temp))>1) {stop("Deg.C.Av.Air.Temp doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Deg.C.Av.Air.Temp")] <- as.numeric(mean(this_day_all_data$Deg.C.Av.Air.Temp)) # input average 
#                 # flg.AirTemp: input unique flg.AirTemp
#                 if (length(unique(this_day_all_data$flg.AirTemp))>1) {stop("flg.AirTemp doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg.AirFlw")] <- as.character(unique(this_day_all_data$flg.AirFlw)) # input unique value
#                 # X..Rel.Humidty: input unique X..Rel.Humidty
#                 if (length(unique(this_day_all_data$X..Rel.Humidty))>1) {stop("X..Rel.Humidty doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("X..Rel.Humidty")] <- as.numeric(mean(this_day_all_data$X..Rel.Humidty)) # input average 
#                 # flg.RelHumid: input unique flg.RelHumid
#                 if (length(unique(this_day_all_data$flg.RelHumid))>1) {stop("flg.RelHumid doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg.RelHumid")] <- as.character(unique(this_day_all_data$flg.RelHumid)) # input unique value
#                 # mbar.Barom.Press: input unique mbar.Barom.Press
#                 if (length(unique(this_day_all_data$mbar.Barom.Press))>1) {stop("mbar.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("mbar.Barom.Press")] <- as.numeric(mean(this_day_all_data$mbar.Barom.Press)) # input average 
#                 # flg.Barom.Press: input unique flg.Barom.Press
#                 if (length(unique(this_day_all_data$flg.Barom.Press))>1) {stop("flg.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg.Barom.Press")] <- as.character(unique(this_day_all_data$flg.Barom.Press)) # input unique value
#                 # deg.C.Sensor..Int.AT: input unique deg.C.Sensor..Int.AT
#                 if (length(unique(this_day_all_data$deg.C.Sensor..Int.AT))>1) {stop("deg.C.Sensor..Int.AT doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("deg.C.Sensor..Int.AT")] <- as.numeric(mean(this_day_all_data$deg.C.Sensor..Int.AT)) # input average 
#                 # flg.deg.C.Sensor.Int.AT: input unique flg.deg.C.Sensor.Int.AT
#                 if (length(unique(this_day_all_data$flg.deg.C.Sensor.Int.AT))>1) {stop("flg.deg.C.Sensor.Int.AT doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg.deg.C.Sensor.Int.AT")] <- as.character(unique(this_day_all_data$flg.deg.C.Sensor.Int.AT)) # input unique value
#                 # X..Sensor.Int.RH: input unique X..Sensor.Int.RH
#                 if (length(unique(this_day_all_data$X..Sensor.Int.RH))>1) {stop("X..Sensor.Int.RH doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("X..Sensor.Int.RH")] <- as.numeric(mean(this_day_all_data$X..Sensor.Int.RH)) # input average 
#                 # flg.deg.C.Sensor.Int.A: input unique flg.deg.C.Sensor.Int.A
#                 if (length(unique(this_day_all_data$flg..SensorIntRH))>1) {stop("flg..SensorIntRH doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg..SensorIntRH")] <- as.character(unique(this_day_all_data$flg..SensorIntRH)) # input unique value
#                 # Wind.Speed.m.s: input unique Wind.Speed.m.s
#                 if (length(unique(this_day_all_data$Wind.Speed.m.s))>1) {stop("Wind.Speed.m.s doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Wind.Speed.m.s")] <- as.numeric(mean(this_day_all_data$Wind.Speed.m.s)) # input average 
#                 # flg.WindSpeed: input unique flg.WindSpeed
#                 if (length(unique(this_day_all_data$flg.WindSpeed))>1) {stop("flg.WindSpeed doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg.WindSpeed")] <- as.character(unique(this_day_all_data$flg.WindSpeed)) # input unique value
#                 # Battery.Voltage.volts: input unique Battery.Voltage.volts
#                 if (length(unique(this_day_all_data$Battery.Voltage.volts))>1) {stop("Battery.Voltage.volts doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Battery.Voltage.volts")] <- as.numeric(mean(this_day_all_data$Battery.Voltage.volts)) # input average 
#                 # flg.BatteryVoltage: input unique flg.BatteryVoltage
#                 if (length(unique(this_day_all_data$flg.BatteryVoltage))>1) {stop("flg.BatteryVoltage doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg.BatteryVoltage")] <- as.character(unique(this_day_all_data$flg.BatteryVoltage)) # input unique value
#                 # Alarm: input unique Alarm
#                 if (length(unique(this_day_all_data$Alarm))>1) {stop("Alarm doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("Alarm")] <- as.numeric(mean(this_day_all_data$Alarm)) # input average 
#                 # flg.Alarm: input unique flg.Alarm
#                 if (length(unique(this_day_all_data$flg.Alarm))>1) {stop("flg.Alarm doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("flg.Alarm")] <- as.character(unique(this_day_all_data$flg.Alarm)) # input unique value
#                 # InDayLatDiff: input unique InDayLatDiff
#                 if (length(unique(this_day_all_data$InDayLatDiff))>1) {stop("InDayLatDiff doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("InDayLatDiff")] <- as.numeric(mean(this_day_all_data$InDayLatDiff)) # input average 
#                 # InDayLonDiff: input unique InDayLonDiff
#                 if (length(unique(this_day_all_data$InDayLonDiff))>1) {stop("InDayLonDiff doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_aves[rstart_aves:rstop_aves,c("InDayLonDiff")] <- as.numeric(mean(this_day_all_data$InDayLonDiff)) # input average 
#                 # PlottingColor: setting value specific to this if-statement
#                 input_mat3_aves[rstart_aves:rstop_aves,c("PlottingColor")] <- as.character(set_plot_color)
#                 rstart_aves <- rstop_aves+1 # move counter up
#                 
#                 #rm(input_mat3_aves,rstart_aves,rstop_aves) # COMMENT
#                 
#                 #### for the colocated data all data from one source, different POCs ####
#                 rstop_colocated <- rstart_colocated+dim(this_day_all_data)[1]-1
#                 #input_mat3_colocated[rstart_colocated:rstop_colocated,] <- this_day_all_data
#                 #rstart_colocated <- rstop_colocated+1
#                 
#                 # PM2.5 Obs (concentration):take average
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("PM2.5_Obs")] <- as.numeric(this_day_all_data$PM2.5_Obs) # input average
#                 # latitude: input unique value
#                 if (length(unique(this_day_all_data$PM2.5_Lat))>1) {stop("latitudes don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("PM2.5_Lat")] <- as.numeric(mean(this_day_all_data$PM2.5_Lat)) # input average
#                 # longitude: input unique value
#                 if (length(unique(this_day_all_data$PM2.5_Lon))>1) {stop("longitudes don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("PM2.5_Lon")] <- as.numeric(mean(this_day_all_data$PM2.5_Lon)) # input average
#                 # Datum: input unique value
#                 if (length(unique(this_day_all_data$Datum))>1) {stop("Datums don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Datum")] <- as.character(unique(this_day_all_data$Datum)) # input unique value
#                 # Date_Local: input unique date
#                 if (unique(this_day_all_data$Date_Local)!=this_day) {stop("Date_Local don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Date_Local")] <- as.character(unique(this_day_all_data$Date_Local)) # input unique value
#                 # Year: input unique year
#                 if (length(unique(this_day_all_data$Year))>1) {stop("Years don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Year")] <- as.numeric(mean(this_day_all_data$Year)) # input average
#                 # Month: input unique month
#                 if (length(unique(this_day_all_data$Month))>1) {stop("Months don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Month")] <- as.numeric(mean(this_day_all_data$Month)) # input average
#                 # Day: input unique day
#                 if (length(unique(this_day_all_data$Day))>1) {stop("Day doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Day")] <- as.numeric(mean(this_day_all_data$Day)) # input average
#                 # State_Code: input unique state code
#                 if (length(unique(this_day_all_data$State_Code))>1) {stop("State_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("State_Code")] <- as.numeric(mean(this_day_all_data$State_Code)) # input average
#                 # County_Code: input unique county code
#                 if (length(unique(this_day_all_data$County_Code))>1) {stop("County_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("County_Code")] <- as.numeric(mean(this_day_all_data$County_Code)) # input average
#                 # Site_Num: input unique site num
#                 if (length(unique(this_day_all_data$Site_Num))>1) {stop("Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Site_Num")] <- as.numeric(mean(this_day_all_data$Site_Num)) # input average
#                 # Parameter Code: input unique parameter code
#                 if (length(unique(this_day_all_data$Parameter_Code))>1) {stop("Parameter_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Parameter_Code")] <- as.numeric(mean(this_day_all_data$Parameter_Code)) # input average
#                 # POC: no check statement for POC since we know they are different - input as they are
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("POC")] <- as.numeric(this_day_all_data$POC) # input average
#                 # Parameter_Name: input unique Parameter Name
#                 if (length(unique(this_day_all_data$Parameter_Name))>1) {stop("Parameter_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Parameter_Name")] <- as.character(unique(this_day_all_data$Parameter_Name)) # input unique value
#                 # Sample Duration: input unique Sample Duration
#                 if (length(unique(this_day_all_data$Sample_Duration))>1) {stop("Sample_Duration doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Sample_Duration")] <- as.character(unique(this_day_all_data$Sample_Duration)) # input unique value
#                 # Pollutant_Standard: input unique Pollutant Standard
#                 if (length(unique(this_day_all_data$Pollutant_Standard))>1) {stop("Pollutant_Standard doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Pollutant_Standard")] <- as.character(unique(this_day_all_data$Pollutant_Standard)) # input unique value
#                 # Units_of_Measure: input unique Units of Measure
#                 if (length(unique(this_day_all_data$Units_of_Measure))>1) {stop("Units_of_Measure doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Units_of_Measure")] <- as.character(unique(this_day_all_data$Units_of_Measure)) # input unique value
#                 # Event_Type: input unique event type
#                 if (length(unique(this_day_all_data$Event_Type))>1) {stop("Event_Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Event_Type")] <- as.character(unique(this_day_all_data$Event_Type)) # input unique value
#                 # Observation Count: input directly
#                 if (min(this_day_all_data$Observation_Count)<1) {stop("Observation_Count doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Observation_Count")] <- as.numeric(this_day_all_data$Observation_Count) # input directly
#                 # Observation Percent: input directly
#                 if (min(this_day_all_data$Observation_Percent)<75) {stop("Observation_Percent doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Observation_Percent")] <- as.numeric(this_day_all_data$Observation_Percent) # input directly
#                 # X1st_Max_Value: input directly
#                 if (min(this_day_all_data$X1st_Max_Value)<0) {stop("negative concentration (X1st_Max_Value) Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("X1st_Max_Value")] <- as.numeric(this_day_all_data$X1st_Max_Value) # input average
#                 # X1st_Max_Hour: input directly
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("X1st_Max_Hour")] <- as.character(this_day_all_data$X1st_Max_Hour)
#                 # AQI: input mean AQI
#                 if (min(this_day_all_data$AQI)<0) {stop("negative concentration (AQI) Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("AQI")] <- as.numeric(mean(this_day_all_data$AQI)) # input average
#                 # Method_Code: input unique event type
#                 if (length(unique(this_day_all_data$Method_Code))>1) {stop("Method_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Method_Code")] <- as.character(unique(this_day_all_data$Method_Code)) # input unique value
#                 # Method_Name: input unique Method_Name
#                 if (length(unique(this_day_all_data$Method_Name))>1) {stop("Method_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Method_Name")] <- as.character(unique(this_day_all_data$Method_Name)) # input unique value
#                 # PM25_Station_Name: input unique PM25_Station_Name
#                 if (length(unique(this_day_all_data$PM25_Station_Name))>1) {stop("PM25_Station_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("PM25_Station_Name")] <- as.character(unique(this_day_all_data$PM25_Station_Name)) # input unique value
#                 # Address: input unique Address
#                 if (length(unique(this_day_all_data$Address))>1) {stop("Address doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Address")] <- as.character(unique(this_day_all_data$Address)) # input unique value
#                 # State_Name: input unique State_Name
#                 if (length(unique(this_day_all_data$State_Name))>1) {stop("State_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("State_Name")] <- as.character(unique(this_day_all_data$State_Name)) # input unique value
#                 # County_Name: input unique County_Name
#                 if (length(unique(this_day_all_data$County_Name))>1) {stop("County_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("County_Name")] <- as.character(unique(this_day_all_data$County_Name)) # input unique value
#                 # City_Name: input unique City_Name
#                 if (length(unique(this_day_all_data$City_Name))>1) {stop("City_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("City_Name")] <- as.character(unique(this_day_all_data$City_Name)) # input unique value
#                 # CBSA_Name: input unique CBSA_Name
#                 if (length(unique(this_day_all_data$CBSA_Name))>1) {stop("CBSA_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("CBSA_Name")] <- as.character(unique(this_day_all_data$CBSA_Name)) # input unique value
#                 # Date_of_Last_Change: input unique Date_of_Last_Change
#                 if (length(unique(this_day_all_data$Date_of_Last_Change))>1) {stop("Date_of_Last_Change doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Date_of_Last_Change")] <- as.character(unique(this_day_all_data$Date_of_Last_Change)) # input unique value
#                 # State_Abbrev: input unique State_Abbrev
#                 if (length(unique(this_day_all_data$State_Abbrev))>1) {stop("State_Abbrev doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("State_Abbrev")] <- as.character(unique(this_day_all_data$State_Abbrev)) # input unique value
#                 # Winter: input unique Winter
#                 if (length(unique(this_day_all_data$Winter))>1) {stop("Winter doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Winter")] <- as.numeric(mean(this_day_all_data$Winter)) # input average
#                 # Data_Source_Name_Display: input unique Data_Source_Name_Display
#                 if (length(unique(this_day_all_data$Data_Source_Name_Display))>1) {stop("Data_Source_Name_Display doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Data_Source_Name_Display")] <- as.character(unique(this_day_all_data$Data_Source_Name_Display)) # input unique value
#                 # Data_Source_Name_Short: input unique Data_Source_Name_Short
#                 if (length(unique(this_day_all_data$Data_Source_Name_Short))>1) {stop("Data_Source_Name_Short doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Data_Source_Name_Short")] <- as.character(unique(this_day_all_data$Data_Source_Name_Short)) # input unique value
#                 # Data_Source_Counter: input unique value
#                 if (length(unique(this_day_all_data$Data_Source_Counter))>1) {stop("Data_Source_Counter don't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Data_Source_Counter")] <- as.numeric(mean(this_day_all_data$Data_Source_Counter)) # input average
#                 # "Source_File": input unique "Source_File"
#                 if (length(unique(this_day_all_data$Source_File))>1) {stop("Source_File doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Source_File")] <- as.character(unique(this_day_all_data$Source_File)) # input unique value
#                 # Composite_of_N_rows: input directly
#                 if (min(this_day_all_data$Composite_of_N_rows)<1) {stop("Composite_of_N_rows doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Composite_of_N_rows")] <- as.numeric(this_day_all_data$Composite_of_N_rows) # input directly
#                 # N_Negative_Obs: input_directly
#                 if (max(this_day_all_data$N_Negative_Obs)>0) {stop("N_Negative_Obs doesn't make sense. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("N_Negative_Obs")] <- as.numeric(this_day_all_data$N_Negative_Obs) # input average
#                 # "flg.Lat": input unique "flg.Lat"
#                 if (length(unique(this_day_all_data$flg.Lat))>1) {stop("flg.Lat doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Lat")] <- as.character(unique(this_day_all_data$flg.Lat)) # input unique value
#                 # "flg.Lon: input unique flg.Lon
#                 if (length(unique(this_day_all_data$flg.Lon))>1) {stop("flg.Lon doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Lon")] <- as.character(unique(this_day_all_data$flg.Lon)) # input unique value
#                 # "Type: input unique Type
#                 if (length(unique(this_day_all_data$Type))>1) {stop("Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Type")] <- as.character(unique(this_day_all_data$Type)) # input unique value
#                 # "flg.Type: input unique flg.Type
#                 if (length(unique(this_day_all_data$flg.Type))>1) {stop("flg.Type doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Type")] <- as.character(unique(this_day_all_data$flg.Type)) # input unique value
#                 # flg.Site_Num: input unique flg.Site_Num
#                 if (length(unique(this_day_all_data$flg.Site_Num))>1) {stop("flg.Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Site_Num")] <- as.character(unique(this_day_all_data$flg.Site_Num)) # input unique value
#                 # flg.PM25_Obs: input unique flg.PM25_Obs
#                 if (length(unique(this_day_all_data$flg.PM25_Obs))>1) {stop("flg.PM25_Obs doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.PM25_Obs")] <- as.character(unique(this_day_all_data$flg.PM25_Obs)) # input unique value
#                 # l.m.Ave..Air.Flw: input unique l.m.Ave..Air.Flw
#                 if (length(unique(this_day_all_data$l.m.Ave..Air.Flw))>1) {stop("l.m.Ave..Air.Flw doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("l.m.Ave..Air.Flw")] <- as.numeric(mean(this_day_all_data$l.m.Ave..Air.Flw)) # input average
#                 # flg.AirFlw: input unique flg.AirFlw
#                 if (length(unique(this_day_all_data$flg.AirFlw))>1) {stop("flg.AirFlw doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.AirFlw")] <- as.character(unique(this_day_all_data$flg.AirFlw)) # input unique value
#                 # Deg.C.Av.Air.Temp: input unique Deg.C.Av.Air.Temp
#                 if (length(unique(this_day_all_data$Deg.C.Av.Air.Temp))>1) {stop("Deg.C.Av.Air.Temp doesn't match. Look at data/code and write more code")} # check that latitudes match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Deg.C.Av.Air.Temp")] <- as.numeric(mean(this_day_all_data$Deg.C.Av.Air.Temp)) # input average
#                 # flg.AirTemp: input unique flg.AirTemp
#                 if (length(unique(this_day_all_data$flg.AirTemp))>1) {stop("flg.AirTemp doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.AirFlw")] <- as.character(unique(this_day_all_data$flg.AirFlw)) # input unique value
#                 # X..Rel.Humidty: input unique X..Rel.Humidty
#                 if (length(unique(this_day_all_data$X..Rel.Humidty))>1) {stop("X..Rel.Humidty doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("X..Rel.Humidty")] <- as.numeric(mean(this_day_all_data$X..Rel.Humidty)) # input average
#                 # flg.RelHumid: input unique flg.RelHumid
#                 if (length(unique(this_day_all_data$flg.RelHumid))>1) {stop("flg.RelHumid doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.RelHumid")] <- as.character(unique(this_day_all_data$flg.RelHumid)) # input unique value
#                 # mbar.Barom.Press: input unique mbar.Barom.Press
#                 if (length(unique(this_day_all_data$mbar.Barom.Press))>1) {stop("mbar.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("mbar.Barom.Press")] <- as.numeric(mean(this_day_all_data$mbar.Barom.Press)) # input average
#                 # flg.Barom.Press: input unique flg.Barom.Press
#                 if (length(unique(this_day_all_data$flg.Barom.Press))>1) {stop("flg.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Barom.Press")] <- as.character(unique(this_day_all_data$flg.Barom.Press)) # input unique value
#                 # deg.C.Sensor..Int.AT: input unique deg.C.Sensor..Int.AT
#                 if (length(unique(this_day_all_data$deg.C.Sensor..Int.AT))>1) {stop("deg.C.Sensor..Int.AT doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("deg.C.Sensor..Int.AT")] <- as.numeric(mean(this_day_all_data$deg.C.Sensor..Int.AT)) # input average
#                 # flg.deg.C.Sensor.Int.AT: input unique flg.deg.C.Sensor.Int.AT
#                 if (length(unique(this_day_all_data$flg.deg.C.Sensor.Int.AT))>1) {stop("flg.deg.C.Sensor.Int.AT doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.deg.C.Sensor.Int.AT")] <- as.character(unique(this_day_all_data$flg.deg.C.Sensor.Int.AT)) # input unique value
#                 # X..Sensor.Int.RH: input unique X..Sensor.Int.RH
#                 if (length(unique(this_day_all_data$X..Sensor.Int.RH))>1) {stop("X..Sensor.Int.RH doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("X..Sensor.Int.RH")] <- as.numeric(mean(this_day_all_data$X..Sensor.Int.RH)) # input average
#                 # flg.deg.C.Sensor.Int.A: input unique flg.deg.C.Sensor.Int.A
#                 if (length(unique(this_day_all_data$flg..SensorIntRH))>1) {stop("flg..SensorIntRH doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg..SensorIntRH")] <- as.character(unique(this_day_all_data$flg..SensorIntRH)) # input unique value
#                 # Wind.Speed.m.s: input unique Wind.Speed.m.s
#                 if (length(unique(this_day_all_data$Wind.Speed.m.s))>1) {stop("Wind.Speed.m.s doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Wind.Speed.m.s")] <- as.numeric(mean(this_day_all_data$Wind.Speed.m.s)) # input average
#                 # flg.WindSpeed: input unique flg.WindSpeed
#                 if (length(unique(this_day_all_data$flg.WindSpeed))>1) {stop("flg.WindSpeed doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.WindSpeed")] <- as.character(unique(this_day_all_data$flg.WindSpeed)) # input unique value
#                 # Battery.Voltage.volts: input unique Battery.Voltage.volts
#                 if (length(unique(this_day_all_data$Battery.Voltage.volts))>1) {stop("Battery.Voltage.volts doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Battery.Voltage.volts")] <- as.numeric(mean(this_day_all_data$Battery.Voltage.volts)) # input average
#                 # flg.BatteryVoltage: input unique flg.BatteryVoltage
#                 if (length(unique(this_day_all_data$flg.BatteryVoltage))>1) {stop("flg.BatteryVoltage doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.BatteryVoltage")] <- as.character(unique(this_day_all_data$flg.BatteryVoltage)) # input unique value
#                 # Alarm: input unique Alarm
#                 if (length(unique(this_day_all_data$Alarm))>1) {stop("Alarm doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("Alarm")] <- as.numeric(mean(this_day_all_data$Alarm)) # input average
#                 # flg.Alarm: input unique flg.Alarm
#                 if (length(unique(this_day_all_data$flg.Alarm))>1) {stop("flg.Alarm doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("flg.Alarm")] <- as.character(unique(this_day_all_data$flg.Alarm)) # input unique value
#                 # InDayLatDiff: input unique InDayLatDiff
#                 if (length(unique(this_day_all_data$InDayLatDiff))>1) {stop("InDayLatDiff doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("InDayLatDiff")] <- as.numeric(mean(this_day_all_data$InDayLatDiff)) # input average
#                 # InDayLonDiff: input unique InDayLonDiff
#                 if (length(unique(this_day_all_data$InDayLonDiff))>1) {stop("InDayLonDiff doesn't match. Look at data/code and write more code")} # check that values match
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("InDayLonDiff")] <- as.numeric(mean(this_day_all_data$InDayLonDiff)) # input average
#                 # PlottingColor: setting value specific to this if-statement
#                 input_mat3_colocated[rstart_colocated:rstop_colocated,c("PlottingColor")] <- as.character(set_plot_color)
# 
#                 rstart_colocated <- rstop_colocated+1
#                 
#                 
#                 
#               } else {
#                 stop("write code to handle data from multiple sources")
#               } # if (length(unique(this_day_all_data$Data_Source_Name_Short))==1) { # is all the data from one source?
#             
#             
#           
#           
#           } # if (length(unique_POCs) == dim(this_day_all_data)[1]) { # are all the data points on this day at this station from the same POC?
# 
#         } # if (length(which_POC_NA)!=0) {stop("write code to handle unknown POCs")
# 
#     } # for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
#     
#     } # if (length(unique_days)==dim(this_station_data)[1]) { # determine whether there were multiple monitors ever operating at this site (or duplicate data)
#   rm(this_station_data)
# } # for (this_station_i in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)
# print("finished loop through all stations for which we have EPA Codes")
# # #### write code to check lat/lon for repeats in the unknown_EPA_Code_data and merge it into input_mat3* ####
# # stop('pick up writing code here')
# # 
# # input_header <-  c('PM2.5_Obs','PM2.5_Lat','PM2.5_Lon','Datum','Date_Local','Year','Month','Day','State_Code','County_Code','Site_Num','Parameter_Code','POC','Parameter_Name','Sample_Duration','Pollutant_Standard','Units_of_Measure','Event_Type','Observation_Count','Observation_Percent','1st_Max_Value','1st_Max_Hour','AQI','Method_Code','Method_Name','PM25_Station_Name','Address','State_Name','County_Name','City_Name','CBSA_Name','Date_of_Last_Change', # columns in AQS data
# #                    'State_Abbrev','Winter','Data_Source_Name_Display','Data_Source_Name_Short','Data_Source_Counter','Source_File','Composite_of_N_rows','N_Negative_Obs', # other columns to include
# #                    "flg.Lat","flg.Lon","Type","flg.Type","flg.Site_Num","flg.PM25_Obs","l/m Ave. Air Flw", # DRI variables
# #                    "flg.AirFlw","Deg C Av Air Temp","flg.AirTemp","% Rel Humidty","flg.RelHumid","mbar Barom Press","flg.Barom Press", # DRI variables
# #                    "deg C Sensor  Int AT","flg.deg C Sensor Int AT","% Sensor Int RH","flg.%SensorIntRH", # DRI variables
# #                    "Wind Speed m/s","flg.WindSpeed","Battery Voltage volts","flg.BatteryVoltage","Alarm","flg.Alarm", # DRI variables
# #                    "InDayLatDiff","InDayLonDiff","PlottingColor")
# # 
# # # create and fill in data frame for 24-hr data (originally hourly data)
# # #date_station <- data.frame(matrix(NA,nrow = dim(UTDEQ_data)[1], ncol = 2)) # create empty matrix
# # #all_date_times <- as.Date(UTDEQ_data$Date,"%m/%d/%Y") # get dates in UT DEQ data
# # #date_station[,1] <- all_date_times # fill in dates (with repeats) into date_station
# # #date_station[,2] <- UTDEQ_data$Station # fill in station names into date_station
# # #rm(all_date_times) # clear variables
# # 
# # unique_date_station <- date_station[!duplicated(date_station[,c(1,2)]),] # figure out how many unique station-days are in the DEQ data
# # rm(date_station)
# # 
# # UTDEQ_data$X <- as.Date(UTDEQ_data$Date,"%m/%d/%Y") # fill in dates (without times) into an empty column in UTDEQ_data
# # 
# # UTDEQ_24hr_ave <- data.frame(matrix(NA,nrow = dim(unique_date_station)[1],ncol = 20)) # create data frame
# # names(UTDEQ_24hr_ave) <- c("Date","Station","PM25Conc","EPACode","Latitude","Longitude","StateCode","CountyCode","SiteNum","N_Obs","PercentObs","N_neg","POC","County_Name","Parameter_Code","Parameter_Name","Sample_Duration","Address","City_Name","State_Abbrev") # assign the header            
# # row_stop <- row_start+dim(UTDEQ_24hr_ave)[1]-1 # what is the last row number in input_mat1 for inputing this block of data?
