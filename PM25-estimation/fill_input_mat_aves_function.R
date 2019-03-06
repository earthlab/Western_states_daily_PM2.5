#fill_input_mat_aves.fn <- function(this_day_all_combined_true_dup,input_mat4_aves,rstart_aves, lat_tolerance_threshold,lon_tolerance_threshold, this_day) {
fill_input_mat_aves.fn <- function(this_day_all_combined_true_dup,input_mat4_aves,rstart_aves, this_day) { # lat_tolerance_threshold,lon_tolerance_threshold, this_day) {
    
  set_plot_color <- "azure3"
  #### for the aves data  ####
  rstop_aves <- rstart_aves
  # PM2.5 Obs (concentration):take average
  # input average 
  input_mat4_aves[rstart_aves:rstop_aves,c("PM2.5_Obs")] <- as.numeric(mean(this_day_all_combined_true_dup$PM2.5_Obs)) 
  # latitude: input unique value
  if (length(unique(this_day_all_combined_true_dup$Lat))>1) {# check that latitudes match
    stop("latitudes don't match exactly.")
    # is the latitude difference within tolerance?
    if (abs(max(this_day_all_combined_true_dup$Lat) - min(this_day_all_combined_true_dup$Lat)) < lat_tolerance_threshold) { # is the latitude difference within tolerance?
      #print("latitudes don't match, but are within tolerance")
    } else {stop(print(this_day_all_combined_true_dup[1,]))} # if (abs(max(this_day_all_combined_true_dup$Lat)-min(this_day_all_combined_true_dup$Lat))<lat_tolerance_threshold) { # is the latitude difference within tolerance?
    #} else {stop("latitude differences are not within tolerance. check data and code.")} # if (abs(max(this_day_all_combined_true_dup$Lat)-min(this_day_all_combined_true_dup$Lat))<lat_tolerance_threshold) { # is the latitude difference within tolerance?
  } # check that latitudes match
  
  input_mat4_aves[rstart_aves:rstop_aves,c("Lat")] <- as.numeric(mean(this_day_all_combined_true_dup$Lat)) # input average 
  # longitude: input unique value
  #if (length(unique(this_day_all_combined_true_dup$Lon))>1) {stop("longitudes don't match. Look at data/code and write more code")} # check that latitudes match
  if (length(unique(this_day_all_combined_true_dup$Lon))>1) {# check that longitudes match
    #print("longitudes don't match exactly.")
    if (abs(max(this_day_all_combined_true_dup$Lon)-min(this_day_all_combined_true_dup$Lon))<lon_tolerance_threshold) { # is the longitude difference within tolerance?
     # print("longitudes don't match, but are within tolerance")
    } else {stop("latitude differences are not within tolerance. check data and code.")} # if (abs(max(this_day_all_combined_true_dup$Lat)-min(this_day_all_combined_true_dup$Lat))<lat_tolerance_threshold) { # is the latitude difference within tolerance?
  } # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("Lon")] <- as.numeric(mean(this_day_all_combined_true_dup$Lon)) # input average 

  # Datum: input value
  input_mat4_aves[rstart_aves:rstop_aves,("NewDatum")] <- unique(this_day_all_combined_true_dup$NewDatum) # input Datum
 
  # Date_Local: input unique date 
  if (unique(this_day_all_combined_true_dup$Date_Local)!=this_day) {stop("Date_Local don't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("Date_Local")] <- as.character(unique(this_day_all_combined_true_dup$Date_Local)) # input unique value
  # Year: input unique year
  if (length(unique(this_day_all_combined_true_dup$Year))>1) {stop("Years don't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("Year")] <- as.numeric(mean(this_day_all_combined_true_dup$Year)) # input average 
  # Month: input unique month
  if (length(unique(this_day_all_combined_true_dup$Month))>1) {stop("Months don't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("Month")] <- as.numeric(mean(this_day_all_combined_true_dup$Month)) # input average 
  # Day: input unique day
  if (length(unique(this_day_all_combined_true_dup$Day))>1) {stop("Day doesn't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("Day")] <- as.numeric(mean(this_day_all_combined_true_dup$Day)) # input average 
  # State_Code: input unique state code
  State_Code_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$State_Code)), c("State_Code")])
  if (length(State_Code_real)>1) {stop("State_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
  if (length(State_Code_real)==0) {State_Code_real <- NA} # fill in NA if info is not known
  input_mat4_aves[rstart_aves:rstop_aves,c("State_Code")] <- unique(State_Code_real) #as.numeric(mean(this_day_all_combined_true_dup$State_Code)) # input average 
  # County_Code: input unique county code
  #if (length(unique(this_day_all_combined_true_dup$County_Code))>1) {stop("County_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
  County_Code_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$County_Code)), c("County_Code")])
  if (length(County_Code_real)>1) {stop("County_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
  if (length(County_Code_real)==0) {County_Code_real <- NA} # fill in NA if info is not known
  input_mat4_aves[rstart_aves:rstop_aves,c("County_Code")] <- unique(County_Code_real) # input value
  #input_mat4_aves[rstart_aves:rstop_aves,c("County_Code")] <- as.numeric(mean(this_day_all_combined_true_dup$County_Code)) # input average 
  # Site_Num: input unique site num
  #if (length(unique(this_day_all_combined_true_dup$Site_Num))>1) {stop("Site_Num doesn't match. Look at data/code and write more code")} # check that values match
  #input_mat4_aves[rstart_aves:rstop_aves,c("Site_Num")] <- as.numeric(mean(this_day_all_combined_true_dup$Site_Num)) # input average 
  Site_Num_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$Site_Num)), c("Site_Num")])
  if (length(Site_Num_real)>1) {stop("Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
  if (length(Site_Num_real)==0) {Site_Num_real <- NA} # fill in NA if info is not known
  input_mat4_aves[rstart_aves:rstop_aves,c("Site_Num")] <- unique(Site_Num_real) # input value
  rm(Site_Num_real)
  # Parameter Code: input mean value of Parameter Code
  #input_mat4_aves[rstart_aves:rstop_aves,c("Parameter_Code")] <- as.numeric(mean(this_day_all_combined_true_dup$Parameter_Code))#101502
  input_mat4_aves[rstart_aves:rstop_aves,c("Parameter_Code")] <-concatinate_vector_of_strings.fn(this_day_all_combined_true_dup$Parameter_Code)
  
  # POC: no check statement for POC since we know they are different - take the average and multiply by 10 as new POC value
  input_mat4_aves[rstart_aves:rstop_aves,c("POC")] <- "multiple" #as.numeric(mean(this_day_all_combined_true_dup$POC)) # input average 
  # Parameter_Name: input composite Parameter Name
  if (length(unique(this_day_all_combined_true_dup$Parameter_Name))>1) {
    for (PN_i in 1:dim(this_day_all_combined_true_dup)[1]) {
      if (PN_i==1) {
        all_PNs <- this_day_all_combined_true_dup[PN_i,c("Parameter_Name")]
      } else {
        all_PNs <- paste(all_PNs,this_day_all_combined_true_dup[PN_i,c("Parameter_Name")],sep = ", ")
      } # if (PN_i==1) {
    } # for (PN_i in 1:dim(this_day_all_combined_true_dup)[1]) {
  } else {
    all_PNs <- unique(this_day_all_combined_true_dup$Parameter_Name)
  }
  input_mat4_aves[rstart_aves:rstop_aves,c("Parameter_Name")] <- all_PNs # input composite of data
  rm(all_PNs)
  
  var_interest <- "Sample_Duration"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup) 
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  
  var_interest <- "Pollutant_Standard"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup) 
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  
  # Units_of_Measure: input unique Units of Measure
  #if (length(unique(this_day_all_combined_true_dup$Units_of_Measure))>1) {stop("Units_of_Measure doesn't match. Look at data/code and write more code")} # check that latitudes match
  var_interest <- "Units_of_Measure"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup) 
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)

  # Event_Type: input unique event type; no check since they don't have to match
  #if (length(unique(this_day_all_combined_true_dup$Event_Type))>1) {stop("Event_Type doesn't match. Look at data/code and write more code")} # check that latitudes match
  var_interest <- "Event_Type"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup)
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  # Observation Count: sum the observation counts
  if (min(this_day_all_combined_true_dup$Observation_Count)<1) {stop("Observation_Count doesn't make sense. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("Observation_Count")] <- as.numeric(sum(this_day_all_combined_true_dup$Observation_Count)) # input average 
  # Observation Percent: sum the percentages, unless another idea comes along
  if (min(this_day_all_combined_true_dup$Observation_Percent)<75) {stop("Observation_Percent doesn't make sense. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("Observation_Percent")] <- as.numeric(sum(this_day_all_combined_true_dup$Observation_Percent)) # input average 
  # X1st_Max_Value: input mean X1st_Max_Value
  
  if (length(unique(this_day_all_combined_true_dup$X1st_Max_Value))==1){
    if (max(is.na(this_day_all_combined_true_dup$X1st_Max_Value))==1) {
    #print("All X1st_Max_Value values for this station on this day are NA") 
    } else {
      if (min(this_day_all_combined_true_dup$X1st_Max_Value, na.rm = TRUE)<0) {stop("negative concentration (X1st_Max_Value) Look at data/code and write more code")} # check that latitudes match
    }
  } else {
  if (min(this_day_all_combined_true_dup$X1st_Max_Value, na.rm = TRUE)<0) {stop("negative concentration (X1st_Max_Value) Look at data/code and write more code")} # check that latitudes match
  }
  input_mat4_aves[rstart_aves:rstop_aves,c("X1st_Max_Value")] <- as.numeric(mean(this_day_all_combined_true_dup$X1st_Max_Value)) # input average 
  # X1st_Max_Hour: input NA for X1st_Max_Hour, taking an average of hours of the day seems meaningless
  input_mat4_aves[rstart_aves:rstop_aves,c("X1st_Max_Hour")] <- NA 
  # AQI: input mean AQI, not doing a check since they can be different, and can be NA
  #if (min(this_day_all_combined_true_dup$AQI)<0) {stop("negative concentration (AQI) Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("AQI")] <- as.numeric(mean(this_day_all_combined_true_dup$AQI)) # input average 
  # Method_Code: input unique event type
  var_interest <- "Method_Code"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup)
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  # Method_Name: input unique Method_Name
  var_interest <- "Method_Name"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup)
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  # PM25_Station_Name: input unique PM25_Station_Name
  var_interest <- "PM25_Station_Name"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup)
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  # Address: input unique Address
  var_interest <- "Address"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup)
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  # State_Name: input unique State_Name
  State_Name_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$State_Name)), c("State_Name")])
  if (length(State_Name_real)>1) {stop("State_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
  if (length(State_Name_real)==0) {State_Name_real <- NA} # fill in NA if info is not known
  input_mat4_aves[rstart_aves:rstop_aves,c("State_Name")] <- unique(State_Name_real) # input value
  rm(State_Name_real)
  #var_interest <- "State_Name"
  #all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup)
  #input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  #rm(all_Vars,var_interest)
  # County_Name: input unique County_Name
  var_interest <- "County_Name"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup)
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  # City_Name: input unique City_Name
  var_interest <- "City_Name"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup)
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  # CBSA_Name: input unique CBSA_Name
  var_interest <- "CBSA_Name"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup)
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  # Date_of_Last_Change: input unique Date_of_Last_Change
  var_interest <- "Date_of_Last_Change"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup)
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  # State_Abbrev: input unique State_Abbrev
  if (length(unique(this_day_all_combined_true_dup$State_Abbrev))>1) {stop("State_Abbrev doesn't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("State_Abbrev")] <- as.character(unique(this_day_all_combined_true_dup$State_Abbrev)) # input unique value
  # Winter: input unique Winter
  if (length(unique(this_day_all_combined_true_dup$Winter))>1) {stop("Winter doesn't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("Winter")] <- as.numeric(mean(this_day_all_combined_true_dup$Winter)) # input average 
  # Data_Source_Name_Display: input unique Data_Source_Name_Display
  var_interest <- "Data_Source_Name_Display"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup)
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  # Data_Source_Name_Short: input unique Data_Source_Name_Short
  var_interest <- "Data_Source_Name_Short"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup)
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  # Data_Source_Counter: input unique value
  input_mat4_aves[rstart_aves:rstop_aves,c("Data_Source_Counter")] <- as.numeric(mean(this_day_all_combined_true_dup$Data_Source_Counter)*-1) # input average times -1
  # "Source_File": input unique "Source_File"
  var_interest <- "Source_File"
  all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup)
  input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
  rm(all_Vars,var_interest)
  # Composite_of_N_rows: sum the Composite_of_N_rows
  if (min(this_day_all_combined_true_dup$Composite_of_N_rows)<1) {stop("Composite_of_N_rows doesn't make sense. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("Composite_of_N_rows")] <- as.numeric(sum(this_day_all_combined_true_dup$Composite_of_N_rows)) # input average 
  # N_Negative_Obs: sum the N_Negative_Obs
  if (max(this_day_all_combined_true_dup$N_Negative_Obs)>0) {stop("N_Negative_Obs doesn't make sense. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("N_Negative_Obs")] <- as.numeric(sum(this_day_all_combined_true_dup$N_Negative_Obs)) # input average 
  # "flg.Lat": input unique "flg.Lat"
  if (length(unique(this_day_all_combined_true_dup$flg.Lat))>1) {stop("flg.Lat doesn't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg.Lat")] <- as.character(unique(this_day_all_combined_true_dup$flg.Lat)) # input unique value
  # "flg.Lon: input unique flg.Lon
  if (length(unique(this_day_all_combined_true_dup$flg.Lon))>1) {stop("flg.Lon doesn't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg.Lon")] <- as.character(unique(this_day_all_combined_true_dup$flg.Lon)) # input unique value
  # "Type: input unique Type
  if (length(unique(this_day_all_combined_true_dup$Type))>1) {stop("Type doesn't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("Type")] <- as.character(unique(this_day_all_combined_true_dup$Type)) # input unique value
  # "flg.Type: input unique flg.Type
  if (length(unique(this_day_all_combined_true_dup$flg.Type))>1) {stop("flg.Type doesn't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg.Type")] <- as.character(unique(this_day_all_combined_true_dup$flg.Type)) # input unique value
  # flg.Site_Num: input unique flg.Site_Num
  if (length(unique(this_day_all_combined_true_dup$flg.Site_Num))>1) {stop("flg.Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg.Site_Num")] <- as.character(unique(this_day_all_combined_true_dup$flg.Site_Num)) # input unique value
  # flg.PM25_Obs: input unique flg.PM25_Obs
  #if (length(unique(this_day_all_combined_true_dup$flg.PM25_Obs))>1) {stop("flg.PM25_Obs doesn't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg.PM25_Obs")] <- concatinate_vector_of_strings.fn(unique(this_day_all_combined_true_dup$flg.PM25_Obs))#as.character(unique(this_day_all_combined_true_dup$flg.PM25_Obs)) # input unique value
  # l.m.Ave..Air.Flw: input unique l.m.Ave..Air.Flw
  if (length(unique(this_day_all_combined_true_dup$l.m.Ave..Air.Flw))>1) {stop("l.m.Ave..Air.Flw doesn't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("l.m.Ave..Air.Flw")] <- as.numeric(mean(this_day_all_combined_true_dup$l.m.Ave..Air.Flw)) # input average 
  # flg.AirFlw: input unique flg.AirFlw
  if (length(unique(this_day_all_combined_true_dup$flg.AirFlw))>1) {stop("flg.AirFlw doesn't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg.AirFlw")] <- as.character(unique(this_day_all_combined_true_dup$flg.AirFlw)) # input unique value
  # Deg.C.Av.Air.Temp: input unique Deg.C.Av.Air.Temp
  if (length(unique(this_day_all_combined_true_dup$Deg.C.Av.Air.Temp))>1) {stop("Deg.C.Av.Air.Temp doesn't match. Look at data/code and write more code")} # check that latitudes match
  input_mat4_aves[rstart_aves:rstop_aves,c("Deg.C.Av.Air.Temp")] <- as.numeric(mean(this_day_all_combined_true_dup$Deg.C.Av.Air.Temp)) # input average 
  # flg.AirTemp: input unique flg.AirTemp
  if (length(unique(this_day_all_combined_true_dup$flg.AirTemp))>1) {stop("flg.AirTemp doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg.AirFlw")] <- as.character(unique(this_day_all_combined_true_dup$flg.AirFlw)) # input unique value
  # X..Rel.Humidty: input unique X..Rel.Humidty
  if (length(unique(this_day_all_combined_true_dup$X..Rel.Humidty))>1) {stop("X..Rel.Humidty doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("X..Rel.Humidty")] <- as.numeric(mean(this_day_all_combined_true_dup$X..Rel.Humidty)) # input average 
  # flg.RelHumid: input unique flg.RelHumid
  if (length(unique(this_day_all_combined_true_dup$flg.RelHumid))>1) {stop("flg.RelHumid doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg.RelHumid")] <- as.character(unique(this_day_all_combined_true_dup$flg.RelHumid)) # input unique value
  # mbar.Barom.Press: input unique mbar.Barom.Press
  if (length(unique(this_day_all_combined_true_dup$mbar.Barom.Press))>1) {stop("mbar.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("mbar.Barom.Press")] <- as.numeric(mean(this_day_all_combined_true_dup$mbar.Barom.Press)) # input average 
  # flg.Barom.Press: input unique flg.Barom.Press
  if (length(unique(this_day_all_combined_true_dup$flg.Barom.Press))>1) {stop("flg.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg.Barom.Press")] <- as.character(unique(this_day_all_combined_true_dup$flg.Barom.Press)) # input unique value
  # deg.C.Sensor..Int.AT: input unique deg.C.Sensor..Int.AT
  if (length(unique(this_day_all_combined_true_dup$deg.C.Sensor..Int.AT))>1) {stop("deg.C.Sensor..Int.AT doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("deg.C.Sensor..Int.AT")] <- as.numeric(mean(this_day_all_combined_true_dup$deg.C.Sensor..Int.AT)) # input average 
  # flg.deg.C.Sensor.Int.AT: input unique flg.deg.C.Sensor.Int.AT
  if (length(unique(this_day_all_combined_true_dup$flg.deg.C.Sensor.Int.AT))>1) {stop("flg.deg.C.Sensor.Int.AT doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg.deg.C.Sensor.Int.AT")] <- as.character(unique(this_day_all_combined_true_dup$flg.deg.C.Sensor.Int.AT)) # input unique value
  # X..Sensor.Int.RH: input unique X..Sensor.Int.RH
  if (length(unique(this_day_all_combined_true_dup$X..Sensor.Int.RH))>1) {stop("X..Sensor.Int.RH doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("X..Sensor.Int.RH")] <- as.numeric(mean(this_day_all_combined_true_dup$X..Sensor.Int.RH)) # input average 
  # flg.deg.C.Sensor.Int.A: input unique flg.deg.C.Sensor.Int.A
  if (length(unique(this_day_all_combined_true_dup$flg..SensorIntRH))>1) {stop("flg..SensorIntRH doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg..SensorIntRH")] <- as.character(unique(this_day_all_combined_true_dup$flg..SensorIntRH)) # input unique value
  # Wind.Speed.m.s: input unique Wind.Speed.m.s
  if (length(unique(this_day_all_combined_true_dup$Wind.Speed.m.s))>1) {stop("Wind.Speed.m.s doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("Wind.Speed.m.s")] <- as.numeric(mean(this_day_all_combined_true_dup$Wind.Speed.m.s)) # input average 
  # flg.WindSpeed: input unique flg.WindSpeed
  if (length(unique(this_day_all_combined_true_dup$flg.WindSpeed))>1) {stop("flg.WindSpeed doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg.WindSpeed")] <- as.character(unique(this_day_all_combined_true_dup$flg.WindSpeed)) # input unique value
  # Battery.Voltage.volts: input unique Battery.Voltage.volts
  if (length(unique(this_day_all_combined_true_dup$Battery.Voltage.volts))>1) {stop("Battery.Voltage.volts doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("Battery.Voltage.volts")] <- as.numeric(mean(this_day_all_combined_true_dup$Battery.Voltage.volts)) # input average 
  # flg.BatteryVoltage: input unique flg.BatteryVoltage
  if (length(unique(this_day_all_combined_true_dup$flg.BatteryVoltage))>1) {stop("flg.BatteryVoltage doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg.BatteryVoltage")] <- as.character(unique(this_day_all_combined_true_dup$flg.BatteryVoltage)) # input unique value
  # Alarm: input unique Alarm
  if (length(unique(this_day_all_combined_true_dup$Alarm))>1) {stop("Alarm doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("Alarm")] <- as.numeric(mean(this_day_all_combined_true_dup$Alarm)) # input average 
  # flg.Alarm: input unique flg.Alarm
  if (length(unique(this_day_all_combined_true_dup$flg.Alarm))>1) {stop("flg.Alarm doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("flg.Alarm")] <- as.character(unique(this_day_all_combined_true_dup$flg.Alarm)) # input unique value
  # InDayLatDiff: input unique InDayLatDiff
  if (length(unique(this_day_all_combined_true_dup$InDayLatDiff))>1) {stop("InDayLatDiff doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("InDayLatDiff")] <- as.numeric(mean(this_day_all_combined_true_dup$InDayLatDiff)) # input average 
  # InDayLonDiff: input unique InDayLonDiff
  if (length(unique(this_day_all_combined_true_dup$InDayLonDiff))>1) {stop("InDayLonDiff doesn't match. Look at data/code and write more code")} # check that values match
  input_mat4_aves[rstart_aves:rstop_aves,c("InDayLonDiff")] <- as.numeric(mean(this_day_all_combined_true_dup$InDayLonDiff)) # input average 
  # PlottingColor: setting value specific to this if-statement
  input_mat4_aves[rstart_aves:rstop_aves,c("PlottingColor")] <- as.character(set_plot_color)
  rstart_aves <- rstop_aves+1 # move counter up
  
  #rm(input_mat4_aves,rstart_aves,rstop_aves) # COMMENT
  
  output_list <- list(input_mat4_aves,rstart_aves)   # return value 
  return(output_list)
}

