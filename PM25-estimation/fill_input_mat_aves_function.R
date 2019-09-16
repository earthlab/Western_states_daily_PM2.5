fill_input_mat_aves.fn <- function(this_day_all_combined_true_dup,input_mat4_aves,rstart_aves, this_day) { 
    
  Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = this_day_all_combined_true_dup)
  if (length(Check_data)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}
  rm(Check_data)
  if (class(this_day_all_combined_true_dup$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
  if (class(input_mat4_aves$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
  set_plot_color <- "azure3" # define plotting color to be output (arbitrary)
  #### for the aves data  ####
  rstop_aves <- rstart_aves # row counter
  # PM2.5 Obs (concentration):take average
    input_mat4_aves[rstart_aves:rstop_aves,c("PM2.5_Obs")] <- as.numeric(mean(this_day_all_combined_true_dup$PM2.5_Obs)) # input average concentration
  # latitude: input unique value
    if (length(unique(round(this_day_all_combined_true_dup$Lat,given_digits)))>1) {# check that latitudes match
        print("latitudes don't match exactly.")
      if (length(unique(round(this_day_all_combined_true_dup$Lat,(given_digits-1))))==1) {# check that latitudes match after rounding to given_digits-1
        print("latitudes match if rounded to given_digits-1")
        input_mat4_aves[rstart_aves:rstop_aves,c("Lat")] <- as.numeric(unique(round(this_day_all_combined_true_dup$Lat,(given_digits-1)))) # input average 
      } else { # if (length(unique(round(this_day_all_combined_true_dup$Lat,(given_digits-1))))==1) {# check that latitudes match after rounding to given_digits-1
        stop("latitudes still don't match after rounding to given_digits-1")
      }# if (length(unique(round(this_day_all_combined_true_dup$Lat,(given_digits-1))))==1) {# check that latitudes match after rounding to given_digits-1
    } else { # # if (length(unique(round(this_day_all_combined_true_dup$Lat,given_digits)))>1) {# check that latitudes match
      input_mat4_aves[rstart_aves:rstop_aves,c("Lat")] <- as.numeric(unique(this_day_all_combined_true_dup$Lat)) # input average 
    } # if (length(unique(round(this_day_all_combined_true_dup$Lat,given_digits)))>1) {# check that latitudes match
  # longitude: input unique value
    if (length(unique(round(this_day_all_combined_true_dup$Lon,given_digits)))>1) {# check that longitudes match
      print("longitudes don't match exactly.")
      if (length(unique(round(this_day_all_combined_true_dup$Lon,(given_digits-1))))==1) {# check that longitudes match after rounding to given_digits-1
        print("longitudes match if rounded to given_digits-1")
        input_mat4_aves[rstart_aves:rstop_aves,c("Lon")] <- as.numeric(unique(round(this_day_all_combined_true_dup$Lon,(given_digits-1)))) # input average 
      } else { # if (length(unique(round(this_day_all_combined_true_dup$Lon,(given_digits-1))))==1) {# check that longitudes match after rounding to given_digits-1
        if (length(unique(round(this_day_all_combined_true_dup$Lon,(given_digits-2))))==1) {# check that longitudes match after rounding to given_digits-2
          print("longitudes match if rounded to given_digits-2")
          input_mat4_aves[rstart_aves:rstop_aves,c("Lon")] <- as.numeric(unique(round(this_day_all_combined_true_dup$Lon,(given_digits-2)))) # input average 
        } else { # if (length(unique(round(this_day_all_combined_true_dup$Lon,(given_digits-2))))==1) {# check that longitudes match after rounding to given_digits-2
          stop("longitudes still don't match after rounding to given_digits-2")
        } # if (length(unique(round(this_day_all_combined_true_dup$Lon,(given_digits-2))))==1) {# check that longitudes match after rounding to given_digits-2
      }# if (length(unique(round(this_day_all_combined_true_dup$Lon,(given_digits-1))))==1) {# check that longitudes match after rounding to given_digits-1
    } else { # # if (length(unique(round(this_day_all_combined_true_dup$Lon,given_digits)))>1) {# check that longitudes match
      input_mat4_aves[rstart_aves:rstop_aves,c("Lon")] <- as.numeric(unique(this_day_all_combined_true_dup$Lon)) # input average 
    } # if (length(unique(round(this_day_all_combined_true_dup$Lon,given_digits)))>1) {# check that longitudes match
  # Datum: input value
    input_mat4_aves[rstart_aves:rstop_aves,("NewDatum")] <- unique(this_day_all_combined_true_dup$NewDatum) # input Datum
  # Date_Local: input unique date 
    if (unique(this_day_all_combined_true_dup$Date_Local)!=this_day) {stop("Date_Local don't match. Look at data/code and write more code")} # check that latitudes match
    input_mat4_aves[rstart_aves:rstop_aves,c("Date_Local")] <- as.Date(unique(this_day_all_combined_true_dup$Date_Local)) # input unique value
    Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local"), input_data = input_mat4_aves[rstart_aves:rstop_aves, ])
    if (class(input_mat4_aves$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
    if (length(Check_data)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}
    rm(Check_data)
    if (class(this_day_all_combined_true_dup$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
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
    County_Code_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$County_Code)), c("County_Code")])
    if (length(County_Code_real)>1) {stop("County_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
    if (length(County_Code_real)==0) {County_Code_real <- NA} # fill in NA if info is not known
    input_mat4_aves[rstart_aves:rstop_aves,c("County_Code")] <- unique(County_Code_real) # input value
  # Site_Num: input unique site num
    Site_Num_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$Site_Num)), c("Site_Num")])
    if (length(Site_Num_real)>1) {
      if (State_Code_real == 6  & County_Code_real == 41) {
        print("There is a note in the original CARB data indicating that site 06-041-002 and 06-041-003 were co-located,")
        print("so it is OK that the Site_Num values do not match.")
        Site_Num_real <- mean(Site_Num_real)
        print(paste("Inputing Site_Num as",Site_Num_real))
      } else { # if (State_Code_real == 6  & County_Code_real == 41) {
      stop("Site_Num doesn't match. Look at data/code and write more code")
      } # if (State_Code_real == 6  & County_Code_real == 41) {
      } # if (length(Site_Num_real)>1) { # check that latitudes match
    if (length(Site_Num_real)==0) {Site_Num_real <- NA} # fill in NA if info is not known
    input_mat4_aves[rstart_aves:rstop_aves,c("Site_Num")] <- unique(Site_Num_real) # input value
    rm(Site_Num_real,County_Code_real,State_Code_real)
  # Parameter Code: input mean value of Parameter Code
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
  # Sample_Duration
    var_interest <- "Sample_Duration"
    all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup) 
    input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
    rm(all_Vars,var_interest)
  # Pollutant_Standard
    var_interest <- "Pollutant_Standard"
    all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup) 
    input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
    rm(all_Vars,var_interest)
  # Units_of_Measure: input unique Units of Measure
    var_interest <- "Units_of_Measure"
    all_Vars <- concatinate_within_column.fn(var_interest, this_day_all_combined_true_dup) 
    input_mat4_aves[rstart_aves:rstop_aves,c(var_interest)] <- all_Vars # input composite of data
    rm(all_Vars,var_interest)
  # Event_Type: input unique event type; no check since they don't have to match
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
    which_non_NA <- which(!is.na(this_day_all_combined_true_dup$State_Abbrev))
    if (length(unique(this_day_all_combined_true_dup[which_non_NA, c('State_Abbrev')])) >1) {stop("State_Abbrev doesn't match. Look at data/code and write more code")} # check that latitudes match
    if (length(unique(this_day_all_combined_true_dup[which_non_NA, c('State_Abbrev')])) >0) {
    input_mat4_aves[rstart_aves:rstop_aves,c("State_Abbrev")] <- as.character(unique(this_day_all_combined_true_dup[which_non_NA,c('State_Abbrev')])) # input unique value
    }
    rm(which_non_NA)
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
    input_mat4_aves[rstart_aves:rstop_aves,c("flg.PM25_Obs")] <- concatinate_vector_of_strings.fn(unique(this_day_all_combined_true_dup$flg.PM25_Obs))#as.character(unique(this_day_all_combined_true_dup$flg.PM25_Obs)) # input unique value
  # l.m.Ave..Air.Flw: input unique l.m.Ave..Air.Flw
    l.m.Ave..Air.Flw_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$l.m.Ave..Air.Flw)), c("l.m.Ave..Air.Flw")])
    if (length(l.m.Ave..Air.Flw_real)==0) {l.m.Ave..Air.Flw_real <- NA} # fill in NA if info is not known
    input_mat4_aves[rstart_aves:rstop_aves,c("l.m.Ave..Air.Flw")] <- as.numeric(mean(this_day_all_combined_true_dup$l.m.Ave..Air.Flw)) # input average 
    rm(l.m.Ave..Air.Flw_real)
  # flg.AirFlw: input unique flg.AirFlw
    which_non_NA <- which(!is.na(this_day_all_combined_true_dup$flg.AirFlw))
    if (length(unique(this_day_all_combined_true_dup[which_non_NA, c('flg.AirFlw')]))>1) {print("flg.AirFlw doesn't match. Concatinating values.")} # check that latitudes match
    if (length(unique(this_day_all_combined_true_dup[which_non_NA, c('flg.AirFlw')]))>0) { # only try to input information if there is something other than NAs
    input_mat4_aves[rstart_aves:rstop_aves,c("flg.AirFlw")] <- concatinate_vector_of_strings.fn(as.character(unique(this_day_all_combined_true_dup[which_non_NA,c('flg.AirFlw')]))) # input concatinated value
    } # if (length(unique(this_day_all_combined_true_dup[which_non_NA, c('flg.AirFlw')]))>0) { # only try to input information if there is something other than NAs
    rm(which_non_NA)
  # Deg.C.Av.Air.Temp: input mean Deg.C.Av.Air.Temp
    Deg.C.Av.Air.Temp_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$Deg.C.Av.Air.Temp)), c("Deg.C.Av.Air.Temp")])
    if (length(Deg.C.Av.Air.Temp_real)==0) {Deg.C.Av.Air.Temp_real <- NA} # fill in NA if info is not known
    input_mat4_aves[rstart_aves:rstop_aves,c("Deg.C.Av.Air.Temp")] <- as.numeric(mean(this_day_all_combined_true_dup$Deg.C.Av.Air.Temp)) # input average 
  # flg.AirTemp: input unique flg.AirTemp
    which_non_NA <- which(!is.na(this_day_all_combined_true_dup$flg.AirTemp))
    if (length(unique(this_day_all_combined_true_dup[which_non_NA,c('flg.AirTemp')]))>1) {stop("flg.AirTemp doesn't match. Look at data/code and write more code")} # check that values match
    if (length(which_non_NA) > 0) {
    input_mat4_aves[rstart_aves:rstop_aves,c("flg.AirFlw")] <- as.character(unique(this_day_all_combined_true_dup[which_non_NA,c('flg.AirFlw')])) # input unique value
    }
    rm(which_non_NA)
  # X..Rel.Humidty: input unique X..Rel.Humidty
    X..Rel.Humidty_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$X..Rel.Humidty)), c("X..Rel.Humidty")])
    if (length(X..Rel.Humidty_real)==0) {X..Rel.Humidty_real <- NA} # fill in NA if info is not known
    input_mat4_aves[rstart_aves:rstop_aves,c("X..Rel.Humidty")] <- as.numeric(mean(this_day_all_combined_true_dup$X..Rel.Humidty)) # input average 
  # flg.RelHumid: input unique flg.RelHumid
    if (length(unique(this_day_all_combined_true_dup$flg.RelHumid))>1) {stop("flg.RelHumid doesn't match. Look at data/code and write more code")} # check that values match
    input_mat4_aves[rstart_aves:rstop_aves,c("flg.RelHumid")] <- as.character(unique(this_day_all_combined_true_dup$flg.RelHumid)) # input unique value
  # mbar.Barom.Press: input unique mbar.Barom.Press
    mbar.Barom.Press_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$mbar.Barom.Press)), c("mbar.Barom.Press")])
    if (length(mbar.Barom.Press_real)==0) {mbar.Barom.Press_real <- NA} # fill in NA if info is not known
    input_mat4_aves[rstart_aves:rstop_aves,c("mbar.Barom.Press")] <- as.numeric(mean(this_day_all_combined_true_dup$mbar.Barom.Press)) # input average 
  # flg.Barom.Press: input unique flg.Barom.Press
    flg.Barom.Press_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$flg.Barom.Press)), c("flg.Barom.Press")])
    input_mat4_aves[rstart_aves:rstop_aves,c("flg.Barom.Press")] <- concatinate_vector_of_strings.fn(flg.Barom.Press_real)
    rm(flg.Barom.Press_real)
  # deg.C.Sensor..Int.AT: input unique deg.C.Sensor..Int.AT
    if (length(unique(this_day_all_combined_true_dup$deg.C.Sensor..Int.AT))>1) {stop("deg.C.Sensor..Int.AT doesn't match. Look at data/code and write more code")} # check that values match
    input_mat4_aves[rstart_aves:rstop_aves,c("deg.C.Sensor..Int.AT")] <- as.numeric(mean(this_day_all_combined_true_dup$deg.C.Sensor..Int.AT)) # input average 
  # flg.deg.C.Sensor.Int.AT: input unique flg.deg.C.Sensor.Int.AT
    if (length(unique(this_day_all_combined_true_dup$flg.deg.C.Sensor.Int.AT))>1) {stop("flg.deg.C.Sensor.Int.AT doesn't match. Look at data/code and write more code")} # check that values match
    input_mat4_aves[rstart_aves:rstop_aves,c("flg.deg.C.Sensor.Int.AT")] <- as.character(unique(this_day_all_combined_true_dup$flg.deg.C.Sensor.Int.AT)) # input unique value
  # X..Sensor.Int.RH: input unique X..Sensor.Int.RH
    X..Sensor.Int.RH_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$X..Sensor.Int.RH)), c("X..Sensor.Int.RH")])
    if (length(X..Sensor.Int.RH_real)==0) {X..Sensor.Int.RH_real <- NA} # fill in NA if info is not known
    input_mat4_aves[rstart_aves:rstop_aves,c("X..Sensor.Int.RH")] <- as.numeric(mean(this_day_all_combined_true_dup$X..Sensor.Int.RH)) # input average 
    rm(X..Rel.Humidty_real)
  # flg.deg.C.Sensor.Int.A: input unique flg.deg.C.Sensor.Int.A
    which_non_NA <- which(!is.na(this_day_all_combined_true_dup$flg..SensorIntRH))
    if (length(unique(this_day_all_combined_true_dup[which_non_NA,c('flg..SensorIntRH')]))>1) {print("flg..SensorIntRH doesn't match. Concatinating data.")} # check that values match
    if (length(which_non_NA) > 0) { # only input data if there are values other than 'NA'
      input_mat4_aves[rstart_aves:rstop_aves,c("flg..SensorIntRH")] <- concatinate_vector_of_strings.fn(as.character(unique(this_day_all_combined_true_dup[which_non_NA,c('flg..SensorIntRH')])))
    } # if (length(which_non_NA) > 0) { # only input data if there are values other than 'NA'
    rm(which_non_NA)
  # Wind.Speed.m.s: input unique Wind.Speed.m.s
    Wind.Speed.m.s_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$Wind.Speed.m.s)), c("Wind.Speed.m.s")])
    if (length(Wind.Speed.m.s_real)==0) {Wind.Speed.m.s_real <- NA} # fill in NA if info is not known
    input_mat4_aves[rstart_aves:rstop_aves,c("Wind.Speed.m.s")] <- as.numeric(mean(this_day_all_combined_true_dup$Wind.Speed.m.s)) # input average 
    rm(Wind.Speed.m.s_real)
  # flg.WindSpeed: input unique flg.WindSpeed
    if (length(unique(this_day_all_combined_true_dup$flg.WindSpeed))>1) {stop("flg.WindSpeed doesn't match. Look at data/code and write more code")} # check that values match
    input_mat4_aves[rstart_aves:rstop_aves,c("flg.WindSpeed")] <- as.character(unique(this_day_all_combined_true_dup$flg.WindSpeed)) # input unique value
  # Battery.Voltage.volts: input unique Battery.Voltage.volts
    Battery.Voltage.volts_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$Battery.Voltage.volts)), c("Battery.Voltage.volts")])
    if (length(Battery.Voltage.volts_real)==0) {Battery.Voltage.volts_real <- NA} # fill in NA if info is not known
    input_mat4_aves[rstart_aves:rstop_aves,c("Battery.Voltage.volts")] <- as.numeric(mean(this_day_all_combined_true_dup$Battery.Voltage.volts)) # input average 
  # flg.BatteryVoltage: input unique flg.BatteryVoltage
    which_non_NA <- which(!is.na(this_day_all_combined_true_dup$flg.BatteryVoltage))
    if (length(unique(this_day_all_combined_true_dup[which_non_NA, c('flg.BatteryVoltage')]))>1) {stop("flg.BatteryVoltage doesn't match. Look at data/code and write more code")} # check that values match
    if (length(which_non_NA) > 0) {
    input_mat4_aves[rstart_aves:rstop_aves,c("flg.BatteryVoltage")] <- as.character(unique(this_day_all_combined_true_dup[which_non_NA,c('flg.BatteryVoltage')])) # input unique value
    }
    rm(which_non_NA)
  # Alarm: input unique Alarm
    Alarm_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$Alarm)), c("Alarm")])
    input_mat4_aves[rstart_aves:rstop_aves,c("Alarm")] <- concatinate_vector_of_strings.fn(Alarm_real)
  # flg.Alarm: input unique flg.Alarm
    flg.Alarm_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$flg.Alarm)), c("flg.Alarm")])
    input_mat4_aves[rstart_aves:rstop_aves,c("flg.Alarm")] <- concatinate_vector_of_strings.fn(flg.Alarm_real)
  # InDayLatDiff: input max InDayLatDiff
    InDayLatDiff_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$InDayLatDiff)), c("InDayLatDiff")])
    if (length(InDayLatDiff_real)==0) {InDayLatDiff_real <- NA} # fill in NA if info is not known
    input_mat4_aves[rstart_aves:rstop_aves,c("InDayLatDiff")] <- as.numeric(max(this_day_all_combined_true_dup$InDayLatDiff)) # input average 
  # InDayLonDiff: input unique InDayLonDiff
    InDayLonDiff_real <- unique(this_day_all_combined_true_dup[which(!is.na(this_day_all_combined_true_dup$InDayLonDiff)), c("InDayLonDiff")])
    if (length(InDayLonDiff_real)==0) {InDayLonDiff_real <- NA} # fill in NA if info is not known
    input_mat4_aves[rstart_aves:rstop_aves,c("InDayLonDiff")] <- as.numeric(max(this_day_all_combined_true_dup$InDayLonDiff)) # input average 
    rm(InDayLonDiff_real)
  # PlottingColor: setting value specific to this if-statement
    input_mat4_aves[rstart_aves:rstop_aves,c("PlottingColor")] <- as.character(set_plot_color)
    
  rstart_aves <- rstop_aves+1 # move counter up
  Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = input_mat4_aves[1:(rstart_aves-1), ])
  if (length(Check_data)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}
  rm(Check_data)
  if (class(input_mat4_aves$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
  output_list <- list(input_mat4_aves,rstart_aves)   # return value 
  return(output_list) # output from function
} # end of function # fill_input_mat_aves.fn <- function(this_day_all_combined_true_dup,input_mat4_aves,rstart_aves, this_day) { 
