# De-Duplicate PLM2.5 Observations

#### Read in Data file ####
input_mat2 <- read.csv(file.path(ProcessedData.directory,'cleaned_ML_input.csv'),header=TRUE)

#### Separate data that might have duplicate obs from others that might be harder to tell (or don't) ####

which_known_EPA_Code <- which(!is.na(input_mat2$State_Code))

input_header <-  c('PM2.5_Obs','PM2.5_Lat','PM2.5_Lon','Datum','Date_Local','Year','Month','Day','State_Code','County_Code','Site_Num','Parameter_Code','POC','Parameter_Name','Sample_Duration','Pollutant_Standard','Units_of_Measure','Event_Type','Observation_Count','Observation_Percent','1st_Max_Value','1st_Max_Hour','AQI','Method_Code','Method_Name','PM25_Station_Name','Address','State_Name','County_Name','City_Name','CBSA_Name','Date_of_Last_Change', # columns in AQS data
                   'State_Abbrev','Winter','Data_Source_Name_Display','Data_Source_Name_Short','Data_Source_Counter','Source_File','Composite_of_N_rows','N_Negative_Obs', # other columns to include
                   "flg.Lat","flg.Lon","Type","flg.Type","flg.Site_Num","flg.PM25_Obs","l/m Ave. Air Flw", # DRI variables
                   "flg.AirFlw","Deg C Av Air Temp","flg.AirTemp","% Rel Humidty","flg.RelHumid","mbar Barom Press","flg.Barom Press", # DRI variables
                   "deg C Sensor  Int AT","flg.deg C Sensor Int AT","% Sensor Int RH","flg.%SensorIntRH", # DRI variables
                   "Wind Speed m/s","flg.WindSpeed","Battery Voltage volts","flg.BatteryVoltage","Alarm","flg.Alarm", # DRI variables
                   "InDayLatDiff","InDayLonDiff","PlottingColor")

# create and fill in data frame for 24-hr data (originally hourly data)
date_station <- data.frame(matrix(NA,nrow = dim(UTDEQ_data)[1], ncol = 2)) # create empty matrix
all_date_times <- as.Date(UTDEQ_data$Date,"%m/%d/%Y") # get dates in UT DEQ data
date_station[,1] <- all_date_times # fill in dates (with repeats) into date_station
date_station[,2] <- UTDEQ_data$Station # fill in station names into date_station
rm(all_date_times) # clear variables

unique_date_station <- date_station[!duplicated(date_station[,c(1,2)]),] # figure out how many unique station-days are in the DEQ data
rm(date_station)

UTDEQ_data$X <- as.Date(UTDEQ_data$Date,"%m/%d/%Y") # fill in dates (without times) into an empty column in UTDEQ_data

UTDEQ_24hr_ave <- data.frame(matrix(NA,nrow = dim(unique_date_station)[1],ncol = 20)) # create data frame
names(UTDEQ_24hr_ave) <- c("Date","Station","PM25Conc","EPACode","Latitude","Longitude","StateCode","CountyCode","SiteNum","N_Obs","PercentObs","N_neg","POC","County_Name","Parameter_Code","Parameter_Name","Sample_Duration","Address","City_Name","State_Abbrev") # assign the header            
row_stop <- row_start+dim(UTDEQ_24hr_ave)[1]-1 # what is the last row number in input_mat1 for inputing this block of data?
