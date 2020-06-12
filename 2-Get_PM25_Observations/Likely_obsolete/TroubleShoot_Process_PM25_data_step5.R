# TroubleShoot_Process_PM25_data_step5.R
# orig
this_source_file <- paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input_orig.csv", sep = "") # PM25_Step4_part_e_de_duplicated_aves_ML_input.csv
PM25_data_orig <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,this_source_file),header=TRUE) # load the PM25 data file
PM25_data_orig <- input_mat_change_data_classes.fn(PM25_data_orig)
Check_data_step5_orig <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = PM25_data_orig)
if (length(Check_data_step5_orig)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}

# current
this_source_file <- paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input.csv", sep = "") # PM25_Step4_part_e_de_duplicated_aves_ML_input.csv
PM25_data <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,this_source_file),header=TRUE) # load the PM25 data file
PM25_data <- input_mat_change_data_classes.fn(PM25_data)
Check_data_step5 <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = PM25_data)
if (length(Check_data_step5)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}

write.csv(Check_data_step5,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste('PM25_Step4_part_',processed_data_version,'_de_duplicated_aves_Check_data',sep = "")),row.names = FALSE) # Write csv file

three_cols_w_duplicates <- Check_data_step5[,c("Lat","Lon","NewDatum")]
Locations_input_mat3 <- three_cols_w_duplicates[!duplicated(three_cols_w_duplicates),]
#names(three_cols_data) <- c("Latitude","Longitude","Datum")
#write.csv(three_cols_data,file = file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,paste(file_sub_label,'_Locations','.csv',sep = "")),row.names = FALSE)
#rm(three_cols_data,three_cols_w_duplicates)

this_source_file <- paste("TEST-PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input.csv", sep = "") # PM25_Step4_part_e_de_duplicated_aves_ML_input.csv


# these columns should not have any NA values:
no_NAs_allowed <- c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day")

#this_var <- no_NAs_allowed[1]
for (this_var in no_NAs_allowed) {
  print(this_var)
  which_na <- which(is.na(PM25_data[ , c(this_var)]))
  if (length(which_na)>0) {
    Check_data <- PM25_data[which_na, ]
    stop(paste("There should be no NA values for ",this_var,". There are ",length(which_na),"values."))
  }
  
}

#three_cols_w_duplicates <- Check_data[,c("Lat","Lon","NewDatum")]
#Locations_input_mat3 <- three_cols_w_duplicates[!duplicated(three_cols_w_duplicates),]
#names(Locations_input_mat3) <- c("Lat","Lon","Datum")

#input_mat3 <- Check_data
test_row <- 1
this_lat_test <- Check_data_step5_orig[test_row,c("Lat")]
this_lon_test <- Check_data_step5_orig[test_row,c("Lon")]
this_lat <- this_lat_test
this_lon <- this_lon_test
which_input_mat3 <- which(input_mat3$Lat == this_lat & input_mat3$Lon == this_lon) # find the rows of data with this location
row_interest <- input_mat3[which_input_mat3, ] 

which_PM25_orig <- which(PM25_data_orig$Lat == this_lat_test & PM25_data_orig$Lon == this_lon_test)
X <- which(Locations_input_mat3$Lat == this_lat_test & Locations_input_mat3$Lon == this_lon_test)

which_PM25_orig <- which(PM25_data_orig$PM25_Station_Name == " Smoke USFS R1-306 " & PM25_data_orig$Year == 2018 &
                         PM25_data_orig$Month == 8 & PM25_data_orig$Day == 28)


test_locations <- 1:20
for (X in test_locations) {
  print("X = ")
  print(X)
  this_output <- PM25_station_deduplicate_aves_parallel.fn(X) # PM25_station_deduplicate_aves_parallel.fn(X)
  rm(this_output)
} # for

[9] "State_Code"               "County_Code"              "Site_Num"                 "Parameter_Code"          
[13] "POC"                      "Parameter_Name"           "Sample_Duration"          "Pollutant_Standard"      
[17] "Units_of_Measure"         "Event_Type"               "Observation_Count"        "Observation_Percent"     
[21] "X1st_Max_Value"           "X1st_Max_Hour"            "AQI"                      "Method_Code"             
[25] "Method_Name"              "PM25_Station_Name"        "Address"                  "State_Name"              
[29] "County_Name"              "City_Name"                "CBSA_Name"                "Date_of_Last_Change"     
[33] "State_Abbrev"             "Winter"                   "Data_Source_Name_Display" "Data_Source_Name_Short"  
[37] "Data_Source_Counter"      "Source_File"              "Composite_of_N_rows"      "N_Negative_Obs"          
[41] "flg.Lat"                  "flg.Lon"                  "Type"                     "flg.Type"                
[45] "flg.Site_Num"             "flg.PM25_Obs"             "l.m.Ave..Air.Flw"         "flg.AirFlw"              
[49] "Deg.C.Av.Air.Temp"        "flg.AirTemp"              "X..Rel.Humidty"           "flg.RelHumid"            
[53] "mbar.Barom.Press"         "flg.Barom.Press"          "deg.C.Sensor..Int.AT"     "flg.deg.C.Sensor.Int.AT" 
[57] "X..Sensor.Int.RH"         "flg..SensorIntRH"         "Wind.Speed.m.s"           "flg.WindSpeed"           
[61] "Battery.Voltage.volts"    "flg.BatteryVoltage"       "Alarm"                    "flg.Alarm"               
[65] "InDayLatDiff"             "InDayLonDiff"             "PlottingColor"            "SerialNumber"        


# load data from a previous data set to see if those also have the missing values
#this_source_file <- paste("PM25_Step4_part_",processed_data_version,"_de_duplicated_aves_ML_input.csv", sep = "") # PM25_Step4_part_e_de_duplicated_aves_ML_input.csv
this_source_file <- "PM25_Step3_part_e_NAD83.csv"
Step3 <- read.csv(file.path(define_file_paths.fn("ProcessedData.directory"),sub_folder,this_source_file),header=TRUE) # load the PM25 data file
#this_var <- no_NAs_allowed[1]
for (this_var in no_NAs_allowed) {
  print(this_var)
  which_na_Step3 <- which(is.na(Step3[ , c(this_var)]))
  if (length(which_na_Step3)>0) {
    stop(paste("There should be no NA values for ",this_var,". There are ",length(which_na_Step3),"values."))
  }
}

