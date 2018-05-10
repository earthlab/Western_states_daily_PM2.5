# De-Duplicate PLM2.5 Observations

##### Create Sink output file ####
# sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
SinkFileName=file.path(ProcessedData.directory,"DeDuplicate_ML_Input_File_sink.txt")
sink(file =SinkFileName, append = FALSE, type = c("output","message"), split = FALSE) # UNCOMMENT
cat("output for DeDuplicate_ML_Input_File.R \n \n")

#### Load Data file ####
input_file <- file.path(ProcessedData.directory,'cleaned_ML_input.csv')
print(paste("loading input file: ",input_file,sep = ""))
#input_mat2 <- read.csv(file.path(ProcessedData.directory,'cleaned_ML_input.csv'),header=TRUE)
input_mat2 <- read.csv(input_file,header=TRUE)

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
which_known_EPA_Code <- which(!is.na(input_mat2$State_Code) & !is.na(input_mat2$County_Code) & !is.na(input_mat2$Site_Num))
print(paste(length(which_known_EPA_Code)/dim(input_mat2)[1]*100,"% of rows in input_mat2 have known EPA codes",sep = ""))
which_unknown_EPA_Code <- which(is.na(input_mat2$State_Code) | is.na(input_mat2$County_Code) | is.na(input_mat2$Site_Num))
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

for (this_station_i in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)
  this_station <- unique_EPA_Codes[1,]
  
  which_this_station <- which(known_EPA_Code_data$State_Code == this_station$State_Code & known_EPA_Code_data$County_Code == this_station$County_Code & known_EPA_Code_data$Site_Num == this_station$Site_Num)
  this_station_data <- known_EPA_Code_data[which_this_station,]
  
  
  # how many unique days are in this data?
  unique_days <- unique(this_station_data$Date_Local)
  
  print(paste("Station ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num," has ",
              length(which_this_station)," rows of data among ",length(unique_days)," unique days.",sep = ""))
  
  rm(which_this_station)
  
  if (length(unique_days)==dim(this_station_data)[1]) { # determine whether there were multiple monitors ever operating at this site (or duplicate data)
    stop("Each day of data for this station has only one monitor operating and there is no duplicate data. Finish code to pull this data in.")
  } else {
    print(paste("There are ",dim(this_station_data)[1]," rows of data for ",length(unique_days)," days of data at station ","EPA Code ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num,sep = ""))
    print("This could represent multiple monitors at a site, duplicated data from different sources, or both.")
    
    for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
      this_day <- unique_days[this_day_i]
      print(this_day)
      which_this_day <- which(this_station_data$Date_Local == this_day)
      this_day_all_data <- this_station_data[which_this_day,]
      
      print(paste("Station ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num," has ",
                  length(which_this_day)," rows of data on ",this_day,".",sep = ""))
      
      
      # check the POC's
      which_POC_NA <- which(is.na(this_day_all_data$POC))
      if (length(which_POC_NA)!=0) {stop("write code to handle unknown POCs")
        
      } else { # POC values are known
        print("POC values are known")
        # how many different POC's are there?  
        unique_POCs <- unique(this_day_all_data$POC)
        print(unique_POCs)
          if (length(unique_POCs) == dim(this_day_all_data)[1]) { # are all the data points on this day at this station from the same POC?
           stop("all data points on this day have the same POC value. Finish code")
          } else { # different values of POC
            print("POC values are not all the same")
            
              if (length(unique(this_day_all_data$Data_Source_Name_Short))==1) { # is all the data from one source?
                print(paste("data all from one source: ",unique(this_day_all_data$Data_Source_Name_Short),sep = ""))
                
                ## for the aves data - 
                rstop_aves <- rstart_aves
                # PM2.5 Obs (concentration):take average
                input_mat3_aves[rstart_aves:rstop_aves,c("PM2.5_Obs")] <- as.numeric(mean(this_day_all_data$PM2.5_Obs)) # input average 
                # latitude: input unique value
                if (length(unique(this_day_all_data$PM2.5_Lat))>1) {stop("latitudes don't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("PM2.5_Lat")] <- as.numeric(mean(this_day_all_data$PM2.5_Lat)) # input average 
                # longitude: input unique value
                if (length(unique(this_day_all_data$PM2.5_Lon))>1) {stop("longitudes don't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("PM2.5_Lon")] <- as.numeric(mean(this_day_all_data$PM2.5_Lon)) # input average 
                # Datum: input unique value
                if (length(unique(this_day_all_data$Datum))>1) {stop("Datums don't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Datum")] <- as.character(unique(this_day_all_data$Datum)) # input unique value
                # Date_Local: input unique date 
                if (unique(this_day_all_data$Date_Local)!=this_day) {stop("Date_Local don't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Date_Local")] <- as.character(unique(this_day_all_data$Date_Local)) # input unique value
                # Year: input unique year
                if (length(unique(this_day_all_data$Year))>1) {stop("Years don't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Year")] <- as.numeric(mean(this_day_all_data$Year)) # input average 
                # Month: input unique month
                if (length(unique(this_day_all_data$Month))>1) {stop("Months don't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Month")] <- as.numeric(mean(this_day_all_data$Month)) # input average 
                # Day: input unique day
                if (length(unique(this_day_all_data$Day))>1) {stop("Day doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Day")] <- as.numeric(mean(this_day_all_data$Day)) # input average 
                # State_Code: input unique state code
                if (length(unique(this_day_all_data$State_Code))>1) {stop("State_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("State_Code")] <- as.numeric(mean(this_day_all_data$State_Code)) # input average 
                # County_Code: input unique county code
                if (length(unique(this_day_all_data$County_Code))>1) {stop("County_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("County_Code")] <- as.numeric(mean(this_day_all_data$County_Code)) # input average 
                # Site_Num: input unique site num
                if (length(unique(this_day_all_data$Site_Num))>1) {stop("Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Site_Num")] <- as.numeric(mean(this_day_all_data$Site_Num)) # input average 
                # Parameter Code: input unique parameter code
                if (length(unique(this_day_all_data$Parameter_Code))>1) {stop("Parameter_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Parameter_Code")] <- as.numeric(mean(this_day_all_data$Parameter_Code)) # input average 
                # POC: no check statement for POC since we know they are different - take the average and multiply by 10 as new POC value
                input_mat3_aves[rstart_aves:rstop_aves,c("POC")] <- as.numeric(mean(this_day_all_data$POC)*10) # input average 
                # Parameter_Name: input unique Parameter Name
                if (length(unique(this_day_all_data$Parameter_Name))>1) {stop("Parameter_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Parameter_Name")] <- as.character(unique(this_day_all_data$Parameter_Name)) # input unique value
                # Sample Duration: input unique Sample Duration
                if (length(unique(this_day_all_data$Sample_Duration))>1) {stop("Sample_Duration doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Sample_Duration")] <- as.character(unique(this_day_all_data$Sample_Duration)) # input unique value
                # Pollutant_Standard: input unique Pollutant Standard
                if (length(unique(this_day_all_data$Pollutant_Standard))>1) {stop("Pollutant_Standard doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Pollutant_Standard")] <- as.character(unique(this_day_all_data$Pollutant_Standard)) # input unique value
                # Units_of_Measure: input unique Units of Measure
                if (length(unique(this_day_all_data$Units_of_Measure))>1) {stop("Units_of_Measure doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Units_of_Measure")] <- as.character(unique(this_day_all_data$Units_of_Measure)) # input unique value
                # Event_Type: input unique event type
                if (length(unique(this_day_all_data$Event_Type))>1) {stop("Event_Type doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Event_Type")] <- as.character(unique(this_day_all_data$Event_Type)) # input unique value
                # Observation Count: sum the observation counts
                if (min(this_day_all_data$Observation_Count)<1) {stop("Observation_Count doesn't make sense. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Observation_Count")] <- as.numeric(sum(this_day_all_data$Observation_Count)) # input average 
                # Observation Percent: sum the percentages, unless another idea comes along
                if (min(this_day_all_data$Observation_Percent)<75) {stop("Observation_Percent doesn't make sense. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Observation_Percent")] <- as.numeric(sum(this_day_all_data$Observation_Percent)) # input average 
                # X1st_Max_Value: input mean X1st_Max_Value
                if (min(this_day_all_data$X1st_Max_Value)<0) {stop("negative concentration (X1st_Max_Value) Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("X1st_Max_Value")] <- as.numeric(mean(this_day_all_data$X1st_Max_Value)) # input average 
                # X1st_Max_Hour: input NA for X1st_Max_Hour, taking an average of hours of the day seems meaningless
                input_mat3_aves[rstart_aves:rstop_aves,c("X1st_Max_Hour")] <- NA 
                # AQI: input mean AQI
                if (min(this_day_all_data$AQI)<0) {stop("negative concentration (AQI) Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("AQI")] <- as.numeric(mean(this_day_all_data$AQI)) # input average 
                # Method_Code: input unique event type
                if (length(unique(this_day_all_data$Method_Code))>1) {stop("Method_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Method_Code")] <- as.character(unique(this_day_all_data$Method_Code)) # input unique value
                # Method_Name: input unique Method_Name
                if (length(unique(this_day_all_data$Method_Name))>1) {stop("Method_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Method_Name")] <- as.character(unique(this_day_all_data$Method_Name)) # input unique value
                # PM25_Station_Name: input unique PM25_Station_Name
                if (length(unique(this_day_all_data$PM25_Station_Name))>1) {stop("PM25_Station_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("PM25_Station_Name")] <- as.character(unique(this_day_all_data$PM25_Station_Name)) # input unique value
                # Address: input unique Address
                if (length(unique(this_day_all_data$Address))>1) {stop("Address doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Address")] <- as.character(unique(this_day_all_data$Address)) # input unique value
                # State_Name: input unique State_Name
                if (length(unique(this_day_all_data$State_Name))>1) {stop("State_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("State_Name")] <- as.character(unique(this_day_all_data$State_Name)) # input unique value
                # County_Name: input unique County_Name
                if (length(unique(this_day_all_data$County_Name))>1) {stop("County_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("County_Name")] <- as.character(unique(this_day_all_data$County_Name)) # input unique value
                # City_Name: input unique City_Name
                if (length(unique(this_day_all_data$City_Name))>1) {stop("City_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("City_Name")] <- as.character(unique(this_day_all_data$City_Name)) # input unique value
                # CBSA_Name: input unique CBSA_Name
                if (length(unique(this_day_all_data$CBSA_Name))>1) {stop("CBSA_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("CBSA_Name")] <- as.character(unique(this_day_all_data$CBSA_Name)) # input unique value
                # Date_of_Last_Change: input unique Date_of_Last_Change
                if (length(unique(this_day_all_data$Date_of_Last_Change))>1) {stop("Date_of_Last_Change doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Date_of_Last_Change")] <- as.character(unique(this_day_all_data$Date_of_Last_Change)) # input unique value
                # State_Abbrev: input unique State_Abbrev
                if (length(unique(this_day_all_data$State_Abbrev))>1) {stop("State_Abbrev doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("State_Abbrev")] <- as.character(unique(this_day_all_data$State_Abbrev)) # input unique value
                # Winter: input unique Winter
                if (length(unique(this_day_all_data$Winter))>1) {stop("Winter doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Winter")] <- as.numeric(mean(this_day_all_data$Winter)) # input average 
                # Data_Source_Name_Display: input unique Data_Source_Name_Display
                if (length(unique(this_day_all_data$Data_Source_Name_Display))>1) {stop("Data_Source_Name_Display doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Data_Source_Name_Display")] <- as.character(unique(this_day_all_data$Data_Source_Name_Display)) # input unique value
                # Data_Source_Name_Short: input unique Data_Source_Name_Short
                if (length(unique(this_day_all_data$Data_Source_Name_Short))>1) {stop("Data_Source_Name_Short doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Data_Source_Name_Short")] <- as.character(unique(this_day_all_data$Data_Source_Name_Short)) # input unique value
                # Data_Source_Counter: input unique value
                if (length(unique(this_day_all_data$Data_Source_Counter))>1) {stop("Data_Source_Counter don't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Data_Source_Counter")] <- as.numeric(mean(this_day_all_data$Data_Source_Counter)) # input average 
                # "Source_File": input unique "Source_File"
                if (length(unique(this_day_all_data$Source_File))>1) {stop("Source_File doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Source_File")] <- as.character(unique(this_day_all_data$Source_File)) # input unique value
                # Composite_of_N_rows: sum the Composite_of_N_rows
                if (min(this_day_all_data$Composite_of_N_rows)<1) {stop("Composite_of_N_rows doesn't make sense. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Composite_of_N_rows")] <- as.numeric(sum(this_day_all_data$Composite_of_N_rows)) # input average 
                # N_Negative_Obs: sum the N_Negative_Obs
                if (max(this_day_all_data$N_Negative_Obs)>1) {stop("N_Negative_Obs doesn't make sense. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("N_Negative_Obs")] <- as.numeric(sum(this_day_all_data$N_Negative_Obs)) # input average 
                # "flg.Lat": input unique "flg.Lat"
                if (length(unique(this_day_all_data$flg.Lat))>1) {stop("flg.Lat doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("flg.Lat")] <- as.character(unique(this_day_all_data$flg.Lat)) # input unique value
                # "flg.Lon: input unique flg.Lon
                if (length(unique(this_day_all_data$flg.Lon))>1) {stop("flg.Lon doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("flg.Lon")] <- as.character(unique(this_day_all_data$flg.Lon)) # input unique value
                # "Type: input unique Type
                if (length(unique(this_day_all_data$Type))>1) {stop("Type doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("Type")] <- as.character(unique(this_day_all_data$Type)) # input unique value
                # "flg.Type: input unique flg.Type
                if (length(unique(this_day_all_data$flg.Type))>1) {stop("flg.Type doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("flg.Type")] <- as.character(unique(this_day_all_data$flg.Type)) # input unique value
                # flg.Site_Num: input unique flg.Site_Num
                if (length(unique(this_day_all_data$flg.Site_Num))>1) {stop("flg.Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
                input_mat3_aves[rstart_aves:rstop_aves,c("flg.Site_Num")] <- as.character(unique(this_day_all_data$flg.Site_Num)) # input unique value
                
                
                ""            
                [46] "flg.PM25_Obs"             "l.m.Ave..Air.Flw"         "flg.AirFlw"               "Deg.C.Av.Air.Temp"        "flg.AirTemp"             
                [51] "X..Rel.Humidty"           "flg.RelHumid"             "mbar.Barom.Press"         "flg.Barom.Press"          "deg.C.Sensor..Int.AT"    
                [56] "flg.deg.C.Sensor.Int.AT"  "X..Sensor.Int.RH"         "flg..SensorIntRH"         "Wind.Speed.m.s"           "flg.WindSpeed"           
                [61] "Battery.Voltage.volts"    "flg.BatteryVoltage"       "Alarm"                    "flg.Alarm"                "InDayLatDiff"            
                [66] "InDayLonDiff"             "PlottingColor"           
                
                # for the colocated data
                rstop_colocated <- rstart_colocated+dim(this_day_all_data)[1]-1
                "PM2.5_Obs"                "PM2.5_Lat"                "PM2.5_Lon"                "Datum"                    "Date_Local"              
                [6] "Year"                     "Month"                    "Day"                      "State_Code"               "County_Code"             
                [11] "Site_Num"                 "Parameter_Code"           "POC"                      "Parameter_Name"           "Sample_Duration"         
                [16] "Pollutant_Standard"       "Units_of_Measure"         "Event_Type"               "Observation_Count"        "Observation_Percent"     
                [21] "X1st_Max_Value"           "X1st_Max_Hour"            "AQI"                      "Method_Code"              "Method_Name"             
                [26] "PM25_Station_Name"        "Address"                  "State_Name"               "County_Name"              "City_Name"               
                [31] "CBSA_Name"                "Date_of_Last_Change"      "State_Abbrev"             "Winter"                   "Data_Source_Name_Display"
                [36] "Data_Source_Name_Short"   "Data_Source_Counter"      "Source_File"              "Composite_of_N_rows"      "N_Negative_Obs"          
                [41] "flg.Lat"                  "flg.Lon"                  "Type"                     "flg.Type"                 "flg.Site_Num"            
                [46] "flg.PM25_Obs"             "l.m.Ave..Air.Flw"         "flg.AirFlw"               "Deg.C.Av.Air.Temp"        "flg.AirTemp"             
                [51] "X..Rel.Humidty"           "flg.RelHumid"             "mbar.Barom.Press"         "flg.Barom.Press"          "deg.C.Sensor..Int.AT"    
                [56] "flg.deg.C.Sensor.Int.AT"  "X..Sensor.Int.RH"         "flg..SensorIntRH"         "Wind.Speed.m.s"           "flg.WindSpeed"           
                [61] "Battery.Voltage.volts"    "flg.BatteryVoltage"       "Alarm"                    "flg.Alarm"                "InDayLatDiff"            
                [66] "InDayLonDiff"             "PlottingColor"           
                
              } else {
                stop("write code to handle data from multiple sources")
              } # if (length(unique(this_day_all_data$Data_Source_Name_Short))==1) { # is all the data from one source?
            
            
          
          
          } # if (length(unique_POCs) == dim(this_day_all_data)[1]) { # are all the data points on this day at this station from the same POC?

        } # if (length(which_POC_NA)!=0) {stop("write code to handle unknown POCs")

    } # for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
    
    } # if (length(unique_days)==dim(this_station_data)[1]) { # determine whether there were multiple monitors ever operating at this site (or duplicate data)

  # write for loop cycling through days relevant for this station
  
  # combine data for this station-day in various ways
  
}

# write code to check lat/lon for repeats in the unknown_EPA_Code_data and merge it into input_mat3*


input_header <-  c('PM2.5_Obs','PM2.5_Lat','PM2.5_Lon','Datum','Date_Local','Year','Month','Day','State_Code','County_Code','Site_Num','Parameter_Code','POC','Parameter_Name','Sample_Duration','Pollutant_Standard','Units_of_Measure','Event_Type','Observation_Count','Observation_Percent','1st_Max_Value','1st_Max_Hour','AQI','Method_Code','Method_Name','PM25_Station_Name','Address','State_Name','County_Name','City_Name','CBSA_Name','Date_of_Last_Change', # columns in AQS data
                   'State_Abbrev','Winter','Data_Source_Name_Display','Data_Source_Name_Short','Data_Source_Counter','Source_File','Composite_of_N_rows','N_Negative_Obs', # other columns to include
                   "flg.Lat","flg.Lon","Type","flg.Type","flg.Site_Num","flg.PM25_Obs","l/m Ave. Air Flw", # DRI variables
                   "flg.AirFlw","Deg C Av Air Temp","flg.AirTemp","% Rel Humidty","flg.RelHumid","mbar Barom Press","flg.Barom Press", # DRI variables
                   "deg C Sensor  Int AT","flg.deg C Sensor Int AT","% Sensor Int RH","flg.%SensorIntRH", # DRI variables
                   "Wind Speed m/s","flg.WindSpeed","Battery Voltage volts","flg.BatteryVoltage","Alarm","flg.Alarm", # DRI variables
                   "InDayLatDiff","InDayLonDiff","PlottingColor")

# create and fill in data frame for 24-hr data (originally hourly data)
#date_station <- data.frame(matrix(NA,nrow = dim(UTDEQ_data)[1], ncol = 2)) # create empty matrix
#all_date_times <- as.Date(UTDEQ_data$Date,"%m/%d/%Y") # get dates in UT DEQ data
#date_station[,1] <- all_date_times # fill in dates (with repeats) into date_station
#date_station[,2] <- UTDEQ_data$Station # fill in station names into date_station
#rm(all_date_times) # clear variables

unique_date_station <- date_station[!duplicated(date_station[,c(1,2)]),] # figure out how many unique station-days are in the DEQ data
rm(date_station)

UTDEQ_data$X <- as.Date(UTDEQ_data$Date,"%m/%d/%Y") # fill in dates (without times) into an empty column in UTDEQ_data

UTDEQ_24hr_ave <- data.frame(matrix(NA,nrow = dim(unique_date_station)[1],ncol = 20)) # create data frame
names(UTDEQ_24hr_ave) <- c("Date","Station","PM25Conc","EPACode","Latitude","Longitude","StateCode","CountyCode","SiteNum","N_Obs","PercentObs","N_neg","POC","County_Name","Parameter_Code","Parameter_Name","Sample_Duration","Address","City_Name","State_Abbrev") # assign the header            
row_stop <- row_start+dim(UTDEQ_24hr_ave)[1]-1 # what is the last row number in input_mat1 for inputing this block of data?
