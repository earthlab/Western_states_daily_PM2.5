# https://nicercode.github.io/guides/functions/
#this_day_all_data_in <- this_day_all_data
Combine_true_replicates_R.fn <- function(this_day_all_data_in, this_day) {  
  set_plot_color <- "burlywood4"
  
  unique_conc <- this_day_all_data_in[!duplicated(this_day_all_data_in[,c("PM2.5_Obs")]),c("PM2.5_Obs")] # figure out how many unique parameter code/POC/Method name/PM2.5 obs combinations there are
  # create small data frame for output from this function
  this_day_all_data_out <- data.frame(matrix(NA,nrow=length(unique_conc)[1],ncol=dim(this_day_all_data_in)[2])) # create data frame for input_mat1
  names(this_day_all_data_out) <- colnames(this_day_all_data_in) # assign the header to input_mat1
  this_day_all_data_out <- input_mat_change_data_classes.fn(this_day_all_data_out) # reset variable classes
  if (class(this_day_all_data_out$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
  
  for (this_out_row in 1:length(unique_conc)) {  
    this_conc <- unique_conc[this_out_row]
    rows_of_interest <- which(this_day_all_data_in$PM2.5_Obs==this_conc)
    this_unique_obs_in_day <- this_day_all_data_in[rows_of_interest,]
    Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = this_unique_obs_in_day)
    if (length(Check_data)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}
    rm(Check_data)
    if (class(this_unique_obs_in_day$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
    
    # PM2.5 Obs (concentration): unique value
    if (length(unique(this_unique_obs_in_day$PM2.5_Obs))>1) {stop("PM2.5 conc doesn't match. Look at data/code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("PM2.5_Obs")] <- as.numeric(unique(this_unique_obs_in_day$PM2.5_Obs)) # input average 
    
    # latitude: input unique value
    #if (length(unique(this_unique_obs_in_day$Lat))>1) {stop("latitudes don't match. Look at data/code and write more code")} # check that latitudes match
    #this_day_all_data_out[this_out_row,c("Lat")] <- as.numeric(mean(this_unique_obs_in_day$Lat)) # input average 
    if (length(unique(round(this_unique_obs_in_day$Lat,given_digits)))>1) {# check that latitudes match
      print("latitudes don't match exactly.")
      if (length(unique(round(this_unique_obs_in_day$Lat,(given_digits-1))))==1) {# check that latitudes match after rounding to given_digits-1
        print("latitudes match if rounded to given_digits-1")
        this_day_all_data_out[this_out_row,c("Lat")] <- as.numeric(unique(round(this_unique_obs_in_day$Lat,(given_digits-1)))) # input average 
      } else { # if (length(unique(round(this_unique_obs_in_day$Lat,(given_digits-1))))==1) {# check that latitudes match after rounding to given_digits-1
        if (length(unique(round(this_unique_obs_in_day$Lat,(given_digits-2))))==1) {# check that latitudes match after rounding to given_digits-2
          print("latitudes match if rounded to given_digits-2")
          this_day_all_data_out[this_out_row,c("Lat")] <- as.numeric(unique(round(this_unique_obs_in_day$Lat,(given_digits-2)))) # input average 
        } else { # if (length(unique(round(this_unique_obs_in_day$Lat,(given_digits-2))))==1) {# check that latitudes match after rounding to given_digits-2
          stop("latitudes still don't match after rounding to given_digits-2")
        } # if (length(unique(round(this_unique_obs_in_day$Lat,(given_digits-2))))==1) {# check that latitudes match after rounding to given_digits-2
      }# if (length(unique(round(this_unique_obs_in_day$Lat,(given_digits-1))))==1) {# check that latitudes match after rounding to given_digits-1
    } else { # # if (length(unique(round(this_unique_obs_in_day$Lat,given_digits)))>1) {# check that latitudes match
      this_day_all_data_out[this_out_row,c("Lat")] <- as.numeric(unique(this_unique_obs_in_day$Lat)) # input average 
    } # if (length(unique(round(this_unique_obs_in_day$Lat,given_digits)))>1) {# check that latitudes match
    
   
    
    ## longitude: input unique value
    #if (length(unique(this_unique_obs_in_day$Lon))>1) {stop("longitudes don't match. Look at data/code and write more code")} # check that latitudes match
    #this_day_all_data_out[this_out_row,c("Lon")] <- as.numeric(mean(this_unique_obs_in_day$Lon)) # input average 
    # longitude: input unique value
    if (length(unique(round(this_unique_obs_in_day$Lon,given_digits)))>1) {# check that longitudes match
      print("longitudes don't match exactly.")
      if (length(unique(round(this_unique_obs_in_day$Lon,(given_digits-1))))==1) {# check that longitudes match after rounding to given_digits-1
        print("longitudes match if rounded to given_digits-1")
        this_day_all_data_out[this_out_row,c("Lon")] <- as.numeric(unique(round(this_unique_obs_in_day$Lon,(given_digits-1)))) # input average 
      } else { # if (length(unique(round(this_unique_obs_in_day$Lon,(given_digits-1))))==1) {# check that longitudes match after rounding to given_digits-1
        if (length(unique(round(this_unique_obs_in_day$Lon,(given_digits-2))))==1) {# check that longitudes match after rounding to given_digits-2
          print("longitudes match if rounded to given_digits-2")
          this_day_all_data_out[this_out_row,c("Lon")] <- as.numeric(unique(round(this_unique_obs_in_day$Lon,(given_digits-2)))) # input average 
        } else { # if (length(unique(round(this_unique_obs_in_day$Lon,(given_digits-2))))==1) {# check that longitudes match after rounding to given_digits-2
          stop("longitudes still don't match after rounding to given_digits-2")
        } # if (length(unique(round(this_unique_obs_in_day$Lon,(given_digits-2))))==1) {# check that longitudes match after rounding to given_digits-2
      }# if (length(unique(round(this_unique_obs_in_day$Lon,(given_digits-1))))==1) {# check that longitudes match after rounding to given_digits-1
    } else { # # if (length(unique(round(this_unique_obs_in_day$Lon,given_digits)))>1) {# check that longitudes match
      this_day_all_data_out[this_out_row,c("Lon")] <- as.numeric(unique(this_unique_obs_in_day$Lon)) # input average 
    } # if (length(unique(round(this_unique_obs_in_day$Lon,given_digits)))>1) {# check that longitudes match
    
    
    # NewDatum: input unique value
    if (unique(this_unique_obs_in_day$NewDatum)!="NAD83") {stop("Datums don't match NAD83. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("NewDatum")] <- as.character(unique(this_unique_obs_in_day$NewDatum)) # input unique value
    # Date_Local: input unique date 
    if (unique(this_unique_obs_in_day$Date_Local)!=this_day) {stop("Date_Local don't match. Look at data/code and write more code")} # check that latitudes match
    #this_day_all_data_out[this_out_row,c("Date_Local")] <- as.character(unique(this_unique_obs_in_day$Date_Local)) # input unique value
    this_day_all_data_out[this_out_row,c("Date_Local")] <- as.Date(unique(this_unique_obs_in_day$Date_Local)) # input unique value
    if (class(this_day_all_data_out$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
    # Year: input unique year
    years_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Year)), c("Year")])
    if (length(years_real)>1) {stop("Years don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("Year")] <- as.numeric(mean(years_real)) # input average 
    rm(years_real)
    # Month: input unique month
    months_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Month)), c("Month")])
    if (length(months_real)>1) {stop("Months don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("Month")] <- as.numeric(mean(months_real)) # input average 
    rm(months_real)
    # Day: input unique day
    Day_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Day)), c("Day")])
    if (length(Day_real)>1) {stop("Day don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("Day")] <- as.numeric(mean(Day_real)) # input average 
    rm(Day_real)
    # State_Code: input unique state code
    State_Code_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$State_Code)), c("State_Code")])
    if (length(State_Code_real)>1) {stop("State_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
    if (length(State_Code_real)==0) {State_Code_real <- NA} 
    this_day_all_data_out[this_out_row,c("State_Code")] <- as.numeric(mean(this_unique_obs_in_day$State_Code)) # input average 
    # County_Code: input unique county code
    County_Code_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$County_Code)), c("County_Code")])
    if (length(County_Code_real)>1) {stop("County_Code doesn't match. Look at data/code and write more code")} # check that latitudes match
    if (length(County_Code_real)==0) {County_Code_real <- NA} 
    this_day_all_data_out[this_out_row,c("County_Code")] <- as.numeric(mean(this_unique_obs_in_day$County_Code)) # input average 
    # Site_Num: input unique site num
    Site_Num_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Site_Num)), c("Site_Num")])
    if (length(Site_Num_real)>1) {stop("Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
    if (length(Site_Num_real)==0) {Site_Num_real <- NA} 
    this_day_all_data_out[this_out_row,c("Site_Num")] <- as.numeric(mean(this_unique_obs_in_day$Site_Num)) # input average 
    # Parameter_Code: input unique Parameter Code
    Parameter_Code_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Parameter_Code)), c("Parameter_Code")])
    if (length(Parameter_Code_real)==0) {# handle all-null values
        Parameter_Code_real <- NA
      } # if (length(Parameter_Code_real)==0) # handle all-null values
    this_day_all_data_out[this_out_row,c("Parameter_Code")] <- concatinate_vector_of_strings.fn(Parameter_Code_real) 
    rm(Parameter_Code_real)
    # POC: input unique POC
    POC_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$POC)), c("POC")])
    #if (length(POC_real)>1) {stop("POC don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("POC")] <- concatinate_vector_of_strings.fn(POC_real) #as.numeric(mean(POC_real)) # input average 
    rm(POC_real)
    # Parameter_Name: input composite Parameter Name
    Parameter_Name_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Parameter_Name)), c("Parameter_Name")])
    this_day_all_data_out[this_out_row,c("Parameter_Name")] <- concatinate_vector_of_strings.fn(Parameter_Name_real) 
    rm(Parameter_Name_real)
    # Sample_Duration: input unique Sample Duration
    Sample_Duration_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Sample_Duration)), c("Sample_Duration")])
    this_day_all_data_out[this_out_row,c("Sample_Duration")] <- concatinate_vector_of_strings.fn(Sample_Duration_real) # input average 
    rm(Sample_Duration_real)
    
    # Pollutant_Standard: input composite Pollutant Standard
    #if (length(unique(this_unique_obs_in_day$Pollutant_Standard))>1) {stop("Pollutant_Standard doesn't match. Look at data/code and write more code")} # check that latitudes match
    #this_day_all_data_out[this_out_row,c("Pollutant_Standard")] <- as.character(unique(this_unique_obs_in_day$Pollutant_Standard)) # input unique value
    Pollutant_Standard_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Pollutant_Standard)), c("Pollutant_Standard")])
    #if (length(Pollutant_Standard_real)>1) {stop("Pollutant_Standard don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("Pollutant_Standard")] <- concatinate_vector_of_strings.fn(Pollutant_Standard_real) 
    rm(Pollutant_Standard_real)
    # Units_of_Measure: input unique Units of Measure
    #if (length(unique(this_unique_obs_in_day$Units_of_Measure))>1) {stop("Units_of_Measure doesn't match. Look at data/code and write more code")} # check that latitudes match
    #this_day_all_data_out[this_out_row,c("Units_of_Measure")] <- as.character(unique(this_unique_obs_in_day$Units_of_Measure)) # input unique value
    Units_of_Measure_real <- this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Units_of_Measure)), c("Units_of_Measure")]
    #if (length(Units_of_Measure_real)>1) {stop("Units_of_Measure don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("Units_of_Measure")] <- concatinate_vector_of_strings.fn(Units_of_Measure_real) 
    rm(Units_of_Measure_real)
    # Event_Type
    Event_Type_real <- this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Event_Type)), c("Event_Type")]
    this_day_all_data_out[this_out_row,c("Event_Type")] <- concatinate_vector_of_strings.fn(Event_Type_real) 
    rm(Event_Type_real)
    # Observation_Count: sum the observation counts
    Observation_Count_real <- this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Observation_Count)), c("Observation_Count")]
    this_day_all_data_out[this_out_row,c("Observation_Count")] <- as.numeric(mean(Observation_Count_real)) 
    rm(Observation_Count_real)
    # Observation_Percent: sum the percentages, unless another idea comes along
    Observation_Percent_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Observation_Percent)), c("Observation_Percent")])
    this_day_all_data_out[this_out_row,c("Observation_Percent")] <- as.numeric(min(this_unique_obs_in_day$Observation_Percent)) # input average
    
    # X1st_Max_Value: input mean X1st_Max_Value
    X1st_Max_Value_real <- this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$X1st_Max_Value)), c("X1st_Max_Value")]
    this_day_all_data_out[this_out_row,c("X1st_Max_Value")] <- as.numeric(mean(X1st_Max_Value_real))#concatinate_vector_of_strings.fn(X1st_Max_Value_real) 
    rm(X1st_Max_Value_real)
    # X1st_Max_Hour: input NA for X1st_Max_Hour, taking an average of hours of the day seems meaningless
    X1st_Max_Hour_real <- this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$X1st_Max_Hour)), c("X1st_Max_Hour")]
    this_day_all_data_out[this_out_row,c("X1st_Max_Hour")] <- concatinate_vector_of_strings.fn(X1st_Max_Hour_real) 
    rm(X1st_Max_Hour_real)
    # AQI: input mean AQI
    AQI_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$AQI)), c("AQI")])
    if (length(AQI_real)>1) {stop("AQI don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("AQI")] <- as.numeric(mean(AQI_real)) # input average 
    rm(AQI_real)
    # Method_Code: input unique event type
    Method_Code_real <- this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Method_Code)), c("Method_Code")]
    this_day_all_data_out[this_out_row,c("Method_Code")] <- concatinate_vector_of_strings.fn(Method_Code_real) 
    rm(Method_Code_real)
    # Method_Name: input unique Method_Name
    Method_Name_real <- this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Method_Name)), c("Method_Name")]
    this_day_all_data_out[this_out_row,c("Method_Name")] <- concatinate_vector_of_strings.fn(Method_Name_real) 
    rm(Method_Name_real)
    # PM25_Station_Name: input unique PM25_Station_Name
    PM25_Station_Name_real <- this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$PM25_Station_Name)), c("PM25_Station_Name")]
    this_day_all_data_out[this_out_row,c("PM25_Station_Name")] <- concatinate_vector_of_strings.fn(PM25_Station_Name_real) 
    rm(PM25_Station_Name_real)
    # Address: input unique Address
    Address_real <- this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$Address)), c("Address")]
    this_day_all_data_out[this_out_row,c("Address")] <- concatinate_vector_of_strings.fn(Address_real) 
    rm(Address_real)
    # State_Name: input unique State_Name
    State_Name_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$State_Name)), c("State_Name")])
    if (length(State_Name_real)>1) {stop("State_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
    if (length(State_Name_real)==0) {State_Name_real <- NA} 
    this_day_all_data_out[this_out_row,c("State_Name")] <- State_Name_real 
    rm(State_Name_real)
    # County_Name: input unique County_Name
    County_Name_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$County_Name)), c("County_Name")])
    if (length(County_Name_real)>1) {stop("County_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
    if (length(County_Name_real)==0) {County_Name_real <- NA} 
    this_day_all_data_out[this_out_row,c("County_Name")] <- County_Name_real
    rm(County_Name_real)
    
    # City_Name: input unique City_Name
    City_Name_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$City_Name)), c("City_Name")])
    if (length(City_Name_real)>1) {
      stop("City_Name doesn't match. Look at data/code and write more code")
      } else if (length(City_Name_real) == 0) {
        City_Name_real <- NA
      } # check that latitudes match
    this_day_all_data_out[this_out_row,c("City_Name")] <- as.character(City_Name_real) # input unique value
    rm(City_Name_real)
    #if (length(unique(this_unique_obs_in_day$City_Name))>1) {stop("City_Name doesn't match. Look at data/code and write more code")} # check that latitudes match
    # CBSA_Name: input unique CBSA_Name
    CBSA_Name_real <- unique(this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$CBSA_Name)), c("CBSA_Name")])
    if (length(CBSA_Name_real)>1) {
      stop("CBSA_Name doesn't match. Look at data/code and write more code")
      } else if (length(CBSA_Name_real)==0) {
        CBSA_Name_real <- NA
      }  # check that latitudes match
    this_day_all_data_out[this_out_row,c("CBSA_Name")] <- as.character(CBSA_Name_real) # input unique value
    rm(CBSA_Name_real)
    # Date_of_Last_Change: input unique Date_of_Last_Change
    var_interest <- "Date_of_Last_Change"
    all_Vars <- concatinate_within_column.fn(var_interest, this_unique_obs_in_day) 
    this_day_all_data_out[this_out_row,c(var_interest)] <- all_Vars # input composite of data
    rm(all_Vars,var_interest)
    # State_Abbrev: input unique State_Abbrev
    if (length(unique(this_unique_obs_in_day$State_Abbrev))>1) {stop("State_Abbrev doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("State_Abbrev")] <- as.character(unique(this_unique_obs_in_day$State_Abbrev)) # input unique value
    # Winter: input unique Winter
    if (length(unique(this_unique_obs_in_day$Winter))>1) {stop("Winter doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("Winter")] <- as.numeric(mean(this_unique_obs_in_day$Winter)) # input average 
    # Data_Source_Name_Display: input composite Data_Source_Name_Display; no check because they may be different
    var_interest <- "Data_Source_Name_Display"
    all_Vars <- concatinate_within_column.fn(var_interest, this_unique_obs_in_day) 
    this_day_all_data_out[this_out_row,c(var_interest)] <- all_Vars # input composite of data
    rm(all_Vars,var_interest)
    # Data_Source_Name_Short: input composite Data_Source_Name_Short
    var_interest <- "Data_Source_Name_Short"
    all_Vars <- concatinate_within_column.fn(var_interest, this_unique_obs_in_day) 
    this_day_all_data_out[this_out_row,c(var_interest)] <- all_Vars # input composite of data
    rm(all_Vars,var_interest)
    # Data_Source_Counter: input unique value, take average and multiply by -1
    #if (length(unique(this_unique_obs_in_day$Data_Source_Counter))>1) {stop("Data_Source_Counter don't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("Data_Source_Counter")] <- as.numeric(mean(this_unique_obs_in_day$Data_Source_Counter)*-1) # input average times -1
    # "Source_File": input unique "Source_File"
    var_interest <- "Source_File"
    all_Vars <- concatinate_within_column.fn(var_interest, this_unique_obs_in_day) 
    this_day_all_data_out[this_out_row,c(var_interest)] <- all_Vars # input composite of data
    rm(all_Vars,var_interest)
    
    # Composite_of_N_rows: sum the Composite_of_N_rows
    if (min(this_unique_obs_in_day$Composite_of_N_rows)<1) {stop("Composite_of_N_rows doesn't make sense. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("Composite_of_N_rows")] <- as.numeric(sum(this_unique_obs_in_day$Composite_of_N_rows)) # input average 
    # N_Negative_Obs: sum the N_Negative_Obs
    if (max(this_unique_obs_in_day$N_Negative_Obs)>0) {stop("N_Negative_Obs doesn't make sense. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("N_Negative_Obs")] <- as.numeric(sum(this_unique_obs_in_day$N_Negative_Obs)) # input average 
    # "flg.Lat": input unique "flg.Lat"
    if (length(unique(this_unique_obs_in_day$flg.Lat))>1) {stop("flg.Lat doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("flg.Lat")] <- as.character(unique(this_unique_obs_in_day$flg.Lat)) # input unique value
    # "flg.Lon: input unique flg.Lon
    if (length(unique(this_unique_obs_in_day$flg.Lon))>1) {stop("flg.Lon doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("flg.Lon")] <- as.character(unique(this_unique_obs_in_day$flg.Lon)) # input unique value
    # "Type: input unique Type
    if (length(unique(this_unique_obs_in_day$Type))>1) {stop("Type doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("Type")] <- as.character(unique(this_unique_obs_in_day$Type)) # input unique value
    # "flg.Type: input unique flg.Type
    if (length(unique(this_unique_obs_in_day$flg.Type))>1) {stop("flg.Type doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("flg.Type")] <- as.character(unique(this_unique_obs_in_day$flg.Type)) # input unique value
    # flg.Site_Num: input unique flg.Site_Num
    if (length(unique(this_unique_obs_in_day$flg.Site_Num))>1) {stop("flg.Site_Num doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("flg.Site_Num")] <- as.character(unique(this_unique_obs_in_day$flg.Site_Num)) # input unique value
    # flg.PM25_Obs: input unique flg.PM25_Obs
    #if (length(unique(this_unique_obs_in_day$flg.PM25_Obs))>1) {stop("flg.PM25_Obs doesn't match. Look at data/code and write more code")} # check that latitudes match
    #this_day_all_data_out[this_out_row,c("flg.PM25_Obs")] <- as.character(unique(this_unique_obs_in_day$flg.PM25_Obs)) # input unique value
    flg.PM25_Obs_real <- this_unique_obs_in_day[which(!is.na(this_unique_obs_in_day$flg.PM25_Obs)), c("flg.PM25_Obs")]
    this_day_all_data_out[this_out_row,c("flg.PM25_Obs")] <- concatinate_vector_of_strings.fn(flg.PM25_Obs_real) 
    rm(flg.PM25_Obs_real)
    # l.m.Ave..Air.Flw: input unique l.m.Ave..Air.Flw
    if (length(unique(this_unique_obs_in_day$l.m.Ave..Air.Flw))>1) {stop("l.m.Ave..Air.Flw doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("l.m.Ave..Air.Flw")] <- as.numeric(mean(this_unique_obs_in_day$l.m.Ave..Air.Flw)) # input average 
    # flg.AirFlw: input unique flg.AirFlw
    if (length(unique(this_unique_obs_in_day$flg.AirFlw))>1) {stop("flg.AirFlw doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("flg.AirFlw")] <- as.character(unique(this_unique_obs_in_day$flg.AirFlw)) # input unique value
    # Deg.C.Av.Air.Temp: input unique Deg.C.Av.Air.Temp
    if (length(unique(this_unique_obs_in_day$Deg.C.Av.Air.Temp))>1) {stop("Deg.C.Av.Air.Temp doesn't match. Look at data/code and write more code")} # check that latitudes match
    this_day_all_data_out[this_out_row,c("Deg.C.Av.Air.Temp")] <- as.numeric(mean(this_unique_obs_in_day$Deg.C.Av.Air.Temp)) # input average 
    # flg.AirTemp: input unique flg.AirTemp
    if (length(unique(this_unique_obs_in_day$flg.AirTemp))>1) {stop("flg.AirTemp doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("flg.AirFlw")] <- as.character(unique(this_unique_obs_in_day$flg.AirFlw)) # input unique value
    # X..Rel.Humidty: input unique X..Rel.Humidty
    if (length(unique(this_unique_obs_in_day$X..Rel.Humidty))>1) {stop("X..Rel.Humidty doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("X..Rel.Humidty")] <- as.numeric(mean(this_unique_obs_in_day$X..Rel.Humidty)) # input average 
    # flg.RelHumid: input unique flg.RelHumid
    if (length(unique(this_unique_obs_in_day$flg.RelHumid))>1) {stop("flg.RelHumid doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("flg.RelHumid")] <- as.character(unique(this_unique_obs_in_day$flg.RelHumid)) # input unique value
    # mbar.Barom.Press: input unique mbar.Barom.Press
    if (length(unique(this_unique_obs_in_day$mbar.Barom.Press))>1) {stop("mbar.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("mbar.Barom.Press")] <- as.numeric(mean(this_unique_obs_in_day$mbar.Barom.Press)) # input average 
    # flg.Barom.Press: input unique flg.Barom.Press
    if (length(unique(this_unique_obs_in_day$flg.Barom.Press))>1) {stop("flg.Barom.Press doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("flg.Barom.Press")] <- as.character(unique(this_unique_obs_in_day$flg.Barom.Press)) # input unique value
    # deg.C.Sensor..Int.AT: input unique deg.C.Sensor..Int.AT
    if (length(unique(this_unique_obs_in_day$deg.C.Sensor..Int.AT))>1) {stop("deg.C.Sensor..Int.AT doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("deg.C.Sensor..Int.AT")] <- as.numeric(mean(this_unique_obs_in_day$deg.C.Sensor..Int.AT)) # input average 
    # flg.deg.C.Sensor.Int.AT: input unique flg.deg.C.Sensor.Int.AT
    if (length(unique(this_unique_obs_in_day$flg.deg.C.Sensor.Int.AT))>1) {stop("flg.deg.C.Sensor.Int.AT doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("flg.deg.C.Sensor.Int.AT")] <- as.character(unique(this_unique_obs_in_day$flg.deg.C.Sensor.Int.AT)) # input unique value
    # X..Sensor.Int.RH: input unique X..Sensor.Int.RH
    if (length(unique(this_unique_obs_in_day$X..Sensor.Int.RH))>1) {stop("X..Sensor.Int.RH doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("X..Sensor.Int.RH")] <- as.numeric(mean(this_unique_obs_in_day$X..Sensor.Int.RH)) # input average 
    # flg.deg.C.Sensor.Int.A: input unique flg.deg.C.Sensor.Int.A
    if (length(unique(this_unique_obs_in_day$flg..SensorIntRH))>1) {stop("flg..SensorIntRH doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("flg..SensorIntRH")] <- as.character(unique(this_unique_obs_in_day$flg..SensorIntRH)) # input unique value
    # Wind.Speed.m.s: input unique Wind.Speed.m.s
    if (length(unique(this_unique_obs_in_day$Wind.Speed.m.s))>1) {stop("Wind.Speed.m.s doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("Wind.Speed.m.s")] <- as.numeric(mean(this_unique_obs_in_day$Wind.Speed.m.s)) # input average 
    # flg.WindSpeed: input unique flg.WindSpeed
    if (length(unique(this_unique_obs_in_day$flg.WindSpeed))>1) {stop("flg.WindSpeed doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("flg.WindSpeed")] <- as.character(unique(this_unique_obs_in_day$flg.WindSpeed)) # input unique value
    # Battery.Voltage.volts: input unique Battery.Voltage.volts
    if (length(unique(this_unique_obs_in_day$Battery.Voltage.volts))>1) {stop("Battery.Voltage.volts doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("Battery.Voltage.volts")] <- as.numeric(mean(this_unique_obs_in_day$Battery.Voltage.volts)) # input average 
    # flg.BatteryVoltage: input unique flg.BatteryVoltage
    if (length(unique(this_unique_obs_in_day$flg.BatteryVoltage))>1) {stop("flg.BatteryVoltage doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("flg.BatteryVoltage")] <- as.character(unique(this_unique_obs_in_day$flg.BatteryVoltage)) # input unique value
    # Alarm: input unique Alarm
    if (length(unique(this_unique_obs_in_day$Alarm))>1) {stop("Alarm doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("Alarm")] <- as.numeric(mean(this_unique_obs_in_day$Alarm)) # input average 
    # flg.Alarm: input unique flg.Alarm
    if (length(unique(this_unique_obs_in_day$flg.Alarm))>1) {stop("flg.Alarm doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("flg.Alarm")] <- as.character(unique(this_unique_obs_in_day$flg.Alarm)) # input unique value
    # InDayLatDiff: input unique InDayLatDiff
    if (length(unique(this_unique_obs_in_day$InDayLatDiff))>1) {stop("InDayLatDiff doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("InDayLatDiff")] <- as.numeric(mean(this_unique_obs_in_day$InDayLatDiff)) # input average 
    # InDayLonDiff: input unique InDayLonDiff
    if (length(unique(this_unique_obs_in_day$InDayLonDiff))>1) {stop("InDayLonDiff doesn't match. Look at data/code and write more code")} # check that values match
    this_day_all_data_out[this_out_row,c("InDayLonDiff")] <- as.numeric(mean(this_unique_obs_in_day$InDayLonDiff)) # input average 
    # PlottingColor: setting value specific to this if-statement; no checks since they may be different
    this_day_all_data_out[this_out_row,c("PlottingColor")] <- as.character(set_plot_color)
    
    } # for (this_out_row in 1:length(unique_conc_obs)) {
  Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = this_day_all_data_out)
  if (length(Check_data)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}
  rm(Check_data)
  if (class(this_day_all_data_out$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
  return(this_day_all_data_out)   # return value 
} # deduplicate.combine.eventtype.fn <- function(this_day_all_data_in) {
