# TroubleShoot_Process_PM25_data_step5.R

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

