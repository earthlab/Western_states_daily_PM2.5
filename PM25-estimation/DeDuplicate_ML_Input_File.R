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
rm(input_mat2)

# figure out how many unique EPA codes are in the data
Codes_only_repeats <- data.frame(matrix(NA, nrow = dim(known_EPA_Code_data)[1], ncol = 3)) # create data frame with only EPA codes
names(Codes_only_repeats) <- c("State_Code","County_Code","Site_Num") # create header
Codes_only_repeats <- known_EPA_Code_data[,c("State_Code","County_Code","Site_Num")]
unique_EPA_Codes <- Codes_only_repeats[!duplicated(Codes_only_repeats[,1:3]),]
print(paste("There are ",dim(unique_EPA_Codes)[1]," unique EPA codes (i.e. stations) in the data. (This includes the bordering states.)",sep = ""))

for (this_station in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)
  
  # write for loop cycling through days relevant for this station
  
  # combine data for this station-day in various ways
  
}


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
