# https://nicercode.github.io/guides/functions/
deduplicate.combine.eventtype.fn <- function(this_day_all_data_in) {
  unique_conc_obs <- this_day_all_data_in[!duplicated(this_day_all_data_in[,c("PM2.5_Obs")]),c("PM2.5_Obs")] # figure out how many unique PM2.5 Conc Obs there are
  print(unique_conc_obs)
  if (length(unique_conc_obs)==dim(this_day_all_data_in)[1]) {stop("was expecting fewer unique concentrations than there are rows of data. Check data and code.")}
  # create small data frame for output from this function
  this_day_all_data_out <- data.frame(matrix(NA,nrow=length(unique_conc_obs),ncol=dim(this_day_all_data_in)[2])) # create data frame for input_mat1
  names(this_day_all_data_out) <- colnames(this_day_all_data_in) # assign the header to input_mat1
  
  for (this_conc_i in 1:length(unique_conc_obs)) {
    this_conc <- unique_conc_obs[this_conc_i]
    print(this_conc)
  
    rows_of_interest <- which(this_day_all_data_in$PM2.5_Obs==this_conc)
    print(rows_of_interest)  
    
    this_day_one_conc <- this_day_all_data_in[rows_of_interest,]
    
    # PM2.5 Obs (concentration): unique value
    if (length(unique(this_day_one_conc$PM2.5_Obs))>1) {stop("PM2.5 conc doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("PM2.5_Obs")] <- as.numeric(unique(this_day_one_conc$PM2.5_Obs)) # input average 
    # latitude: input unique value
    if (length(unique(this_day_one_conc$PM2.5_Lat))>1) {stop("latitudes don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("PM2.5_Lat")] <- as.numeric(mean(this_day_one_conc$PM2.5_Lat)) # input average 
    # longitude: input unique value
    if (length(unique(this_day_one_conc$PM2.5_Lon))>1) {stop("longitudes don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("PM2.5_Lon")] <- as.numeric(mean(this_day_one_conc$PM2.5_Lon)) # input average 
    # Datum: input unique value
    if (length(unique(this_day_one_conc$Datum))>1) {stop("Datums don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Datum")] <- as.character(unique(this_day_one_conc$Datum)) # input unique value
    # Date_Local: input unique date 
    if (unique(this_day_one_conc$Date_Local)!=this_day) {stop("Date_Local don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Date_Local")] <- as.character(unique(this_day_one_conc$Date_Local)) # input unique value
    # Year: input unique year
    if (length(unique(this_day_one_conc$Year))>1) {stop("Years don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Year")] <- as.numeric(mean(this_day_one_conc$Year)) # input average 
    # Month: input unique month
    if (length(unique(this_day_one_conc$Month))>1) {stop("Months don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Month")] <- as.numeric(mean(this_day_one_conc$Month)) # input average 
    # Day: input unique day
    if (length(unique(this_day_one_conc$Day))>1) {stop("Day doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Day")] <- as.numeric(mean(this_day_one_conc$Day)) # input average 
    # State_Code: input unique state code
    if (length(unique(this_day_one_conc$State_Code))>1) {stop("State_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("State_Code")] <- as.numeric(mean(this_day_one_conc$State_Code)) # input average 
    # County_Code: input unique county code
    if (length(unique(this_day_one_conc$County_Code))>1) {stop("County_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("County_Code")] <- as.numeric(mean(this_day_one_conc$County_Code)) # input average 
    # Site_Num: input unique site num
    if (length(unique(this_day_one_conc$Site_Num))>1) {stop("Site_Num doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("Site_Num")] <- as.numeric(mean(this_day_one_conc$Site_Num)) # input average 
    # Parameter_Code: input unique Parameter Code
    if (length(unique(this_day_one_conc$Parameter_Code))>1) {stop("Parameter_Code doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("Parameter_Code")] <- as.numeric(mean(this_day_one_conc$Parameter_Code)) # input average 
    # POC: input unique POC
    if (length(unique(this_day_one_conc$POC))>1) {stop("POC doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("POC")] <- as.numeric(mean(this_day_one_conc$POC)) # input average
    # Parameter_Name: input composite Parameter Name
    if (length(unique(this_day_one_conc$Parameter_Name))>1) {stop("Parameter_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Parameter_Name")] <- as.character(unique(this_day_one_conc$Parameter_Name)) # input unique value
    # Sample Duration: input unique Sample Duration
    if (length(unique(this_day_one_conc$Sample_Duration))>1) {stop("Sample_Duration doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Sample_Duration")] <- as.character(unique(this_day_one_conc$Sample_Duration)) # input unique value
    # Pollutant_Standard: input composite Pollutant Standard
    if (length(unique(this_day_one_conc$Pollutant_Standard))>1) {stop("Pollutant_Standard doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Pollutant_Standard")] <- as.character(unique(this_day_one_conc$Pollutant_Standard)) # input unique value
    # Units_of_Measure: input unique Units of Measure
    if (length(unique(this_day_one_conc$Units_of_Measure))>1) {stop("Units_of_Measure doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Units_of_Measure")] <- as.character(unique(this_day_one_conc$Units_of_Measure)) # input unique value
    # Event_Type: input composite event type
    var_interest <- "Event_Type"
    if (length(unique(this_day_one_conc[,var_interest]))>1) {
      for (Var_i in 1:dim(this_day_one_conc)[1]) { # loop through all values and paste them together
        if (Var_i==1) {
          all_Vars <- this_day_one_conc[Var_i,c(var_interest)]
        } else {
          all_Vars <- paste(all_Vars,this_day_one_conc[Var_i,c(var_interest)],sep = ", ")
        } # if (Var_i==1) {
      } # for (Var_i in 1:dim(this_day_one_conc)[1]) {
    } else {
      all_Vars <- unique(this_day_one_conc[,var_interest])
    }
    this_day_all_data_out[this_conc_i,c(var_interest)] <- all_Vars # input composite of data
    rm(all_Vars,var_interest)
    # Observation_Count: sum the observation counts
    if (length(unique(this_day_one_conc$Observation_Count))>1) {stop("Observation_Count doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("Observation_Count")] <- as.numeric(mean(this_day_one_conc$Observation_Count)) # input average
    # Observation_Percent: sum the percentages, unless another idea comes along
    if (length(unique(this_day_one_conc$Observation_Percent))>1) {stop("Observation_Percent doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("Observation_Percent")] <- as.numeric(mean(this_day_one_conc$Observation_Percent)) # input average
    # X1st_Max_Value: input mean X1st_Max_Value
    if (length(unique(this_day_one_conc$X1st_Max_Value))>1) {stop("X1st_Max_Value doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("X1st_Max_Value")] <- as.numeric(mean(this_day_one_conc$X1st_Max_Value)) # input average
    # X1st_Max_Hour: input NA for X1st_Max_Hour, taking an average of hours of the day seems meaningless
    if (length(unique(this_day_one_conc$X1st_Max_Hour))>1) {stop("X1st_Max_Hour doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("X1st_Max_Hour")] <- as.character(unique(this_day_one_conc$X1st_Max_Hour)) # input average
    # AQI: input mean AQI
    if (length(unique(this_day_one_conc$AQI))>1) {stop("AQI doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("AQI")] <- as.numeric(mean(this_day_one_conc$AQI)) # input average
    # Method_Code: input unique event type
    if (length(unique(this_day_one_conc$Method_Code))>1) {stop("Method_Code doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("Method_Code")] <- as.character(unique(this_day_one_conc$Method_Code)) # input average
    # Method_Name: input unique Method_Name
    if (length(unique(this_day_one_conc$Method_Name))>1) {stop("Method_Name doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("Method_Name")] <- as.character(unique(this_day_one_conc$Method_Name)) # input average
    # PM25_Station_Name: input unique PM25_Station_Name
    if (length(unique(this_day_one_conc$PM25_Station_Name))>1) {stop("PM25_Station_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("PM25_Station_Name")] <- as.character(unique(this_day_one_conc$PM25_Station_Name)) # input unique value
    # Address: input unique Address
    if (length(unique(this_day_one_conc$Address))>1) {stop("Address doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Address")] <- as.character(unique(this_day_one_conc$Address)) # input unique value
    # State_Name: input unique State_Name
    if (length(unique(this_day_one_conc$State_Name))>1) {stop("State_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("State_Name")] <- as.character(unique(this_day_one_conc$State_Name)) # input unique value
    # County_Name: input unique County_Name
    if (length(unique(this_day_one_conc$County_Name))>1) {stop("County_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("County_Name")] <- as.character(unique(this_day_one_conc$County_Name)) # input unique value
    # City_Name: input unique City_Name
    if (length(unique(this_day_one_conc$City_Name))>1) {stop("City_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("City_Name")] <- as.character(unique(this_day_one_conc$City_Name)) # input unique value
    # CBSA_Name: input unique CBSA_Name
    if (length(unique(this_day_one_conc$CBSA_Name))>1) {stop("CBSA_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("CBSA_Name")] <- as.character(unique(this_day_one_conc$CBSA_Name)) # input unique value
    # Date_of_Last_Change: input unique Date_of_Last_Change
    if (length(unique(this_day_one_conc$Date_of_Last_Change))>1) {stop("Date_of_Last_Change doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Date_of_Last_Change")] <- as.character(unique(this_day_one_conc$Date_of_Last_Change)) # input unique value
    # State_Abbrev: input unique State_Abbrev
    if (length(unique(this_day_one_conc$State_Abbrev))>1) {stop("State_Abbrev doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("State_Abbrev")] <- as.character(unique(this_day_one_conc$State_Abbrev)) # input unique value
    # Winter: input unique Winter
    if (length(unique(this_day_one_conc$Winter))>1) {stop("Winter doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Winter")] <- as.numeric(mean(this_day_one_conc$Winter)) # input average 
    # Data_Source_Name_Display: input unique Data_Source_Name_Display
    if (length(unique(this_day_one_conc$Data_Source_Name_Display))>1) {stop("Data_Source_Name_Display doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Data_Source_Name_Display")] <- as.character(unique(this_day_one_conc$Data_Source_Name_Display)) # input unique value
    # Data_Source_Name_Short: input unique Data_Source_Name_Short
    if (length(unique(this_day_one_conc$Data_Source_Name_Short))>1) {stop("Data_Source_Name_Short doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Data_Source_Name_Short")] <- as.character(unique(this_day_one_conc$Data_Source_Name_Short)) # input unique value
    # Data_Source_Counter: input unique value
    if (length(unique(this_day_one_conc$Data_Source_Counter))>1) {stop("Data_Source_Counter don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Data_Source_Counter")] <- as.numeric(mean(this_day_one_conc$Data_Source_Counter)) # input average 
    # "Source_File": input unique "Source_File"
    if (length(unique(this_day_one_conc$Source_File))>1) {stop("Source_File doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Source_File")] <- as.character(unique(this_day_one_conc$Source_File)) # input unique value
    # Composite_of_N_rows: sum the Composite_of_N_rows
    if (min(this_day_one_conc$Composite_of_N_rows)<1) {stop("Composite_of_N_rows doesn't make sense. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Composite_of_N_rows")] <- as.numeric(sum(this_day_one_conc$Composite_of_N_rows)) # input average 
    # N_Negative_Obs: sum the N_Negative_Obs
    if (max(this_day_one_conc$N_Negative_Obs)>0) {stop("N_Negative_Obs doesn't make sense. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("N_Negative_Obs")] <- as.numeric(sum(this_day_one_conc$N_Negative_Obs)) # input average 
    # "flg.Lat": input unique "flg.Lat"
    if (length(unique(this_day_one_conc$flg.Lat))>1) {stop("flg.Lat doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("flg.Lat")] <- as.character(unique(this_day_one_conc$flg.Lat)) # input unique value
    # "flg.Lon: input unique flg.Lon
    if (length(unique(this_day_one_conc$flg.Lon))>1) {stop("flg.Lon doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("flg.Lon")] <- as.character(unique(this_day_one_conc$flg.Lon)) # input unique value
    # "Type: input unique Type
    if (length(unique(this_day_one_conc$Type))>1) {stop("Type doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Type")] <- as.character(unique(this_day_one_conc$Type)) # input unique value
    # "flg.Type: input unique flg.Type
    if (length(unique(this_day_one_conc$flg.Type))>1) {stop("flg.Type doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("flg.Type")] <- as.character(unique(this_day_one_conc$flg.Type)) # input unique value
    # flg.Site_Num: input unique flg.Site_Num
    if (length(unique(this_day_one_conc$flg.Site_Num))>1) {stop("flg.Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("flg.Site_Num")] <- as.character(unique(this_day_one_conc$flg.Site_Num)) # input unique value
    # flg.PM25_Obs: input unique flg.PM25_Obs
    if (length(unique(this_day_one_conc$flg.PM25_Obs))>1) {stop("flg.PM25_Obs doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("flg.PM25_Obs")] <- as.character(unique(this_day_one_conc$flg.PM25_Obs)) # input unique value
    # l.m.Ave..Air.Flw: input unique l.m.Ave..Air.Flw
    if (length(unique(this_day_one_conc$l.m.Ave..Air.Flw))>1) {stop("l.m.Ave..Air.Flw doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("l.m.Ave..Air.Flw")] <- as.numeric(mean(this_day_one_conc$l.m.Ave..Air.Flw)) # input average 
    # flg.AirFlw: input unique flg.AirFlw
    if (length(unique(this_day_one_conc$flg.AirFlw))>1) {stop("flg.AirFlw doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("flg.AirFlw")] <- as.character(unique(this_day_one_conc$flg.AirFlw)) # input unique value
    # Deg.C.Av.Air.Temp: input unique Deg.C.Av.Air.Temp
    if (length(unique(this_day_one_conc$Deg.C.Av.Air.Temp))>1) {stop("Deg.C.Av.Air.Temp doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_conc_i,c("Deg.C.Av.Air.Temp")] <- as.numeric(mean(this_day_one_conc$Deg.C.Av.Air.Temp)) # input average 
    # flg.AirTemp: input unique flg.AirTemp
    if (length(unique(this_day_one_conc$flg.AirTemp))>1) {stop("flg.AirTemp doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("flg.AirFlw")] <- as.character(unique(this_day_one_conc$flg.AirFlw)) # input unique value
    # X..Rel.Humidty: input unique X..Rel.Humidty
    if (length(unique(this_day_one_conc$X..Rel.Humidty))>1) {stop("X..Rel.Humidty doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("X..Rel.Humidty")] <- as.numeric(mean(this_day_one_conc$X..Rel.Humidty)) # input average 
    # flg.RelHumid: input unique flg.RelHumid
    if (length(unique(this_day_one_conc$flg.RelHumid))>1) {stop("flg.RelHumid doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("flg.RelHumid")] <- as.character(unique(this_day_one_conc$flg.RelHumid)) # input unique value
    # mbar.Barom.Press: input unique mbar.Barom.Press
    if (length(unique(this_day_one_conc$mbar.Barom.Press))>1) {stop("mbar.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("mbar.Barom.Press")] <- as.numeric(mean(this_day_one_conc$mbar.Barom.Press)) # input average 
    # flg.Barom.Press: input unique flg.Barom.Press
    if (length(unique(this_day_one_conc$flg.Barom.Press))>1) {stop("flg.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("flg.Barom.Press")] <- as.character(unique(this_day_one_conc$flg.Barom.Press)) # input unique value
    # deg.C.Sensor..Int.AT: input unique deg.C.Sensor..Int.AT
    if (length(unique(this_day_one_conc$deg.C.Sensor..Int.AT))>1) {stop("deg.C.Sensor..Int.AT doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("deg.C.Sensor..Int.AT")] <- as.numeric(mean(this_day_one_conc$deg.C.Sensor..Int.AT)) # input average 
    # flg.deg.C.Sensor.Int.AT: input unique flg.deg.C.Sensor.Int.AT
    if (length(unique(this_day_one_conc$flg.deg.C.Sensor.Int.AT))>1) {stop("flg.deg.C.Sensor.Int.AT doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("flg.deg.C.Sensor.Int.AT")] <- as.character(unique(this_day_one_conc$flg.deg.C.Sensor.Int.AT)) # input unique value
    # X..Sensor.Int.RH: input unique X..Sensor.Int.RH
    if (length(unique(this_day_one_conc$X..Sensor.Int.RH))>1) {stop("X..Sensor.Int.RH doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("X..Sensor.Int.RH")] <- as.numeric(mean(this_day_one_conc$X..Sensor.Int.RH)) # input average 
    # flg.deg.C.Sensor.Int.A: input unique flg.deg.C.Sensor.Int.A
    if (length(unique(this_day_one_conc$flg..SensorIntRH))>1) {stop("flg..SensorIntRH doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("flg..SensorIntRH")] <- as.character(unique(this_day_one_conc$flg..SensorIntRH)) # input unique value
    # Wind.Speed.m.s: input unique Wind.Speed.m.s
    if (length(unique(this_day_one_conc$Wind.Speed.m.s))>1) {stop("Wind.Speed.m.s doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("Wind.Speed.m.s")] <- as.numeric(mean(this_day_one_conc$Wind.Speed.m.s)) # input average 
    # flg.WindSpeed: input unique flg.WindSpeed
    if (length(unique(this_day_one_conc$flg.WindSpeed))>1) {stop("flg.WindSpeed doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("flg.WindSpeed")] <- as.character(unique(this_day_one_conc$flg.WindSpeed)) # input unique value
    # Battery.Voltage.volts: input unique Battery.Voltage.volts
    if (length(unique(this_day_one_conc$Battery.Voltage.volts))>1) {stop("Battery.Voltage.volts doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("Battery.Voltage.volts")] <- as.numeric(mean(this_day_one_conc$Battery.Voltage.volts)) # input average 
    # flg.BatteryVoltage: input unique flg.BatteryVoltage
    if (length(unique(this_day_one_conc$flg.BatteryVoltage))>1) {stop("flg.BatteryVoltage doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("flg.BatteryVoltage")] <- as.character(unique(this_day_one_conc$flg.BatteryVoltage)) # input unique value
    # Alarm: input unique Alarm
    if (length(unique(this_day_one_conc$Alarm))>1) {stop("Alarm doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("Alarm")] <- as.numeric(mean(this_day_one_conc$Alarm)) # input average 
    # flg.Alarm: input unique flg.Alarm
    if (length(unique(this_day_one_conc$flg.Alarm))>1) {stop("flg.Alarm doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("flg.Alarm")] <- as.character(unique(this_day_one_conc$flg.Alarm)) # input unique value
    # InDayLatDiff: input unique InDayLatDiff
    if (length(unique(this_day_one_conc$InDayLatDiff))>1) {stop("InDayLatDiff doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("InDayLatDiff")] <- as.numeric(mean(this_day_one_conc$InDayLatDiff)) # input average 
    # InDayLonDiff: input unique InDayLonDiff
    if (length(unique(this_day_one_conc$InDayLonDiff))>1) {stop("InDayLonDiff doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("InDayLonDiff")] <- as.numeric(mean(this_day_one_conc$InDayLonDiff)) # input average 
    # PlottingColor: setting value specific to this if-statement
    #this_day_all_data_out[this_conc_i,c("PlottingColor")] <- as.character(set_plot_color)
    if (length(unique(this_day_one_conc$PlottingColor))>1) {stop("PlottingColor doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_conc_i,c("PlottingColor")] <- as.character(unique(this_day_one_conc$PlottingColor)) # input unique value
    
    } # for (this_conc_i in 1:length(unique_conc_obs)) {
  this_day_all_data_out   # return value 
} # deduplicate.combine.eventtype.fn <- function(this_day_all_data_in) {
