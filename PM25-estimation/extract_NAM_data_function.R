extract_NAM_data.fn <- function(ProcessedData.directory, this_location_date_file, MeteoVarsMultiType, 
                                study_start_date, study_stop_date, forecast_times = 00, 
                                Model_in_use_abbrev =  "namanl") {
  # Input variables
    # ProcessedData.directory: location of source file that has the dates and locations for which you want the NAM data
    # this_location_date_file: file listing the dates (local) and locations where you want the NAM data for.
      # header should include: "Longitude", "Latitude", "Date"
    # MeteoVarsMultiType: should be loaded file: MeteoVariablesNAM.csv  
      # variables of interest. Use these commands in terminal to see what the variable names, levels, and units are:
      # wgrib 20080101_0000_000.grb -V > NAM_grib1.txt
      # wgrib2 20180202_0000_000.grb2 -V > NAM_grib2.txt
      # header: VariableNumber	VariableName	VariableCode	AtmosLevelName	AtmosLevelCode	Units	file_type	time frame	24-hr summary
    # study_start_date: 1st date of interest, e.g., as.Date("20080101",format="%Y%m%d")
    # study_stop_date: last date of interest, e.g., as.Date("20180830",format="%Y%m%d")
    # forecast_times <- 00 # reanalysis - anything else would be a forecast
    # Model_in_use_abbrev <-  "namanl" # NAM Analysis
    # file for _wNextDay is also called: this_location_date_file_wNextDay: same as this_location_date_file, but has the
      # next day for every date/location in this_location_date_file - this is to be 
      # able to handle local vs UTC time
  
  # #### Load Date/Locations of interest ####
  # print(this_location_date_file)
  # PM25DateLoc_orig <-read.csv(file.path(ProcessedData.directory,this_location_date_file),header=TRUE) # load the AQS file
  # PM25DateLoc_orig$Date <- as.Date(PM25DateLoc_orig$Date) # recognize date column as dates
  # PM25DateLoc <- add_next_day_date_loc.fn(PM25DateLoc_orig) # put in the day following each date 
  # #in the file at each location so that all of the data will be gathered when using UTC 
  
  #### Create data sets for each run time to put weather data into ####
  PM25DateLoc_0000 <- PM25DateLoc
  PM25DateLoc_0600 <- PM25DateLoc
  PM25DateLoc_1200 <- PM25DateLoc
  PM25DateLoc_1800 <- PM25DateLoc
  
  #### Cycle through all .grb files for processing ####
  theDate <- study_start_date # set date to beginning of study period before starting while loop
  while (theDate <= study_stop_date) { #Get data for "theDate" in loop
    print(theDate) # print current date in iteration # COMMENT
    
    # find the locations that need data for this date
    which_theDate <- which(PM25DateLoc$Date == theDate)
    length(which_theDate)
    #theDate <- as.Date("2018-07-02")
    
    #see rNOMADS.pdf page 5-6 example
    this_model.date <- format(theDate, format = "%Y%m%d") # get date in format YYYYmmdd - needed for rNOMADS functions
    print(this_model.date) # COMMENT
    
    list.available.models <- CheckNOMADSArchive(Model_in_use_abbrev, this_model.date) # list all model files available for this model and date
    print(list.available.models) # COMMENT
    
    available_times_of_day <- unique(list.available.models$model.run) # what times are available?
    print(available_times_of_day)
    
    #### is this a grib1 (.grb) or grib2 (.grb2) type of file? ####
    this_file_type <- which_type_of_grib_file.fn(list.available.models)
    
    # grab the list of relevant meteo variables for this file type from MeteoVars
    which_meteo <- which(MeteoVarsMultiType$file_type == this_file_type)
    MeteoVars <- MeteoVarsMultiType[which_meteo,]
    
    #### Cycle through the model runs on this Date (theDate) ####    
    # model.run = time of day
    for (model.run_long in available_times_of_day) {
      # model.run_long <- available_times_of_day[1]
      print(model.run_long)
      this_model.run <- substr(model.run_long,1,2)
      print(this_model.run)
      
      # Download archived model data from the NOMADS server - page 4 of rNOMADS.pdf
      this_model.info <- ArchiveGribGrab(abbrev = Model_in_use_abbrev, model.date = this_model.date, 
                                         model.run = this_model.run, preds = forecast_times,
                                         local.dir = NAM.directory, file.names = NULL, tidy = FALSE,
                                         verbose = TRUE, download.method = NULL, file.type = this_file_type)
      
      thisGribInfo <- GribInfo(grib.file = this_model.info[[1]]$file.name, file.type = this_file_type)
      print(thisGribInfo)
      
      print(thisGribInfo[["inventory"]])
      thisGribInfo[["grid"]]
      
      #### Cycle through meteo variables and pull out the data ####
      # grab all meteo variables for this file type
      
      for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
        # meteo_var_counter <- 2 # surface Temp
        
        # get variable full name
        thisMeteo_var_Name <- MeteoVars[meteo_var_counter,c("VariableName")]
        print(thisMeteo_var_Name)
        # get variable coded name
        thisMeteo_variable <- MeteoVars[meteo_var_counter,c("VariableCode")]
        print(thisMeteo_variable)
        # get variable level name
        thisMeteo_level <- MeteoVars[meteo_var_counter,c("AtmosLevelCode")]
        print(thisMeteo_level)
        # get variable units
        thisMeteo_units <- MeteoVars[meteo_var_counter,c("Units")]
        print(thisMeteo_units)
        
        # Load the data for this variable/level
        this_model.data <- ReadGrib(file.names = this_model.info[[1]]$file.name, levels = thisMeteo_level, variables = thisMeteo_variable,
                                    forecasts = NULL, domain = NULL, domain.type = "latlon",
                                    file.type = this_file_type, missing.data = NULL)
        
        for (this_PM25_row in which_theDate) { # cycle through the rows of dates locations that need data for this date
          # this_PM25_row <- which_theDate[1]
          print(PM25DateLoc[this_PM25_row,])
          this_lon <- PM25DateLoc[this_PM25_row,c("Longitude")]
          print(this_lon)
          this_lat <- PM25DateLoc[this_PM25_row,c("Latitude")]
          print(this_lat)
          #this_lat <- 40.037416
          #this_lon <- -105.228667
          
          this_profile <- BuildProfile(model.data = this_model.data, lon = this_lon, lat = this_lat, spatial.average = TRUE, points = 4)
          
          print(paste("The temperature at ",this_lat," ",this_lon," was ",
                      sprintf("%.0f", this_profile[[1]]$profile.data[1,1,1] - 273.15), " degrees Celsius."))
          
          this_meteo_value <- this_profile[[1]]$profile.data[1,1,1]
          print(paste(thisMeteo_var_Name,"at",thisMeteo_level,"is",this_meteo_value,thisMeteo_units,sep = " "))
          
          if (thisMeteo_variable == "TMP") { # show temperature in Celsius (display only - still input in K)
            this_TempC <- this_profile[[1]]$profile.data[1,1,1] - 273.15
            print(paste(this_TempC," C"))
            rm(this_TempC)
          } #  if (thisMeteo_variable == "TMP") { # show temperature in Celsius (display only - still input in K)
          
          if (model.run_long == "0000") { # input meteo value in appropriate matrix
            PM25DateLoc_0000[this_PM25_row,c(paste(as.character(thisMeteo_variable), as.character(thisMeteo_level)))] <- this_meteo_value
          } else if (model.run_long == "0600") {
            PM25DateLoc_0600[this_PM25_row,(cat(thisMeteo_variable,thisMeteo_level))] <- this_meteo_value
          } else if (model.run_long == "1200") {
            PM25DateLoc_1200[this_PM25_row,(cat(thisMeteo_variable,thisMeteo_level))] <- this_meteo_value
          } else if (model.run_long == "1800") {
            PM25DateLoc_1800[this_PM25_row,(cat(thisMeteo_variable,thisMeteo_level))] <- this_meteo_value
          } else { 
            error("invalid model.run_long - check code and data")
          } # if (model.run_long == "0000") { # input meteo value in appropriate matrix
        } # for (this_PM25_row in which_theDate) { # cycle through the rows of dates locations that need data for this date
        rm(this_PM25_row,this_model.data)
        rm(thisMeteo_var_Name,thisMeteo_variable,thisMeteo_level,thisMeteo_units)
      } # for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
      rm(meteo_var_counter)
      error("write more code") # COMMENT
      rm(model.run_long, model.run) # clear variables from this iteration
      file.remove(model.info[[1]]$file.name) # delete file that was downloaded
      
    } # for (model.run in available_times_of_day) {
    rm(model.run_long, which_theDate)
    theDate <- theDate +1 # iterate to the next day
  } # while (theDate <= study_stop_date) { #Get data for "theDate" in loop
  rm(theDate)
  
  #### Clear variables ####
  rm(study_start_date, study_stop_date, forecast_times, Model_in_use_abbrev, MeteoVars)
  rm(PM25DateLoc)
  
  #### Output 4 data frames to csv files ####
  error("still need to output csv files")
  
} # function