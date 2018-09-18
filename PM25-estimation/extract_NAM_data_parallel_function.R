extract_NAM_data.parallel.fn <- function(MeteoVarsMultiType, theDate, forecast_times = 00, this_model.run, #which_theDate,
                                         PM25DateLoc_time, Model_in_use_abbrev =  "namanl") {
  # PM25DateLoc_time_0000, PM25DateLoc_time_0600, PM25DateLoc_time_1200, PM25DateLoc_time_1800,
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
  
  #### Cycle through all .grb files for processing ####
#  theDate <- study_start_date # set date to beginning of study period before starting while loop
#  while (theDate <= study_stop_date) { #Get data for "theDate" in loop
    print(theDate) # print current date in iteration # COMMENT
    
   # # find the locations that need data for this date
    which_theDate <- which(PM25DateLoc_time$Date == theDate)
      #length(which_theDate)
    print(paste(length(which_theDate),"locations need weather data on",theDate,sep = " "))
    OneDay1ModRun <- PM25DateLoc_time[which_theDate,]
    rm(PM25DateLoc_time)
    #see rNOMADS.pdf page 5-6 example
    this_model.date <- format(theDate, format = "%Y%m%d") # get date in format YYYYmmdd - needed for rNOMADS functions
    #print(this_model.date) # COMMENT
    
    list.available.models <- CheckNOMADSArchive(Model_in_use_abbrev, this_model.date) # list all model files available for this model and date
    #print(list.available.models) # COMMENT
    
    available_times_of_day <- unique(list.available.models$model.run) # what times are available?
    #print(available_times_of_day)
    
    #### is this a grib1 (.grb) or grib2 (.grb2) type of file? ####
    this_file_type <- which_type_of_grib_file.fn(list.available.models)
    
    # grab the list of relevant meteo variables for this file type from MeteoVars
    #which_meteo <- which(MeteoVarsMultiType$file_type == this_file_type)
    which_meteo <- which(MeteoVarsMultiType$file_type == "grib2")
    MeteoVars <- MeteoVarsMultiType[which_meteo,]
    
    # Download archived model data from the NOMADS server - page 4 of rNOMADS.pdf
      this_model.info <- ArchiveGribGrab(abbrev = Model_in_use_abbrev, model.date = this_model.date, 
                                         model.run = this_model.run, preds = forecast_times,
                                         local.dir = NAM.directory, file.names = NULL, tidy = FALSE,
                                         verbose = TRUE, download.method = NULL, file.type = this_file_type)
      
      # Convert grib1 to grib2 if necessary and then run GribInfo
      thisGribInfo <- convert_grib1to2.fn(this_model.info,this_file_type)

      # Load the data for this variable/level
      this_model.data <- ReadGrib(file.names = paste(this_model.info[[1]]$file.name,".grb2",sep = ""), levels = MeteoVars$AtmosLevelCode, variables = MeteoVars$VariableCode,
                                  forecasts = NULL, domain = NULL, domain.type = "latlon",
                                  file.type = "grib2", missing.data = NULL)
            
      #for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
        # meteo_var_counter <- 2 # surface Temp
        
        # get variable full name
       # thisMeteo_var_Name <- MeteoVars[meteo_var_counter,c("VariableName")]
        #print(thisMeteo_var_Name)
        # get variable coded name
      #  thisMeteo_variable <- MeteoVars[meteo_var_counter,c("VariableCode")]
        #print(thisMeteo_variable)
        # get variable level name
       # thisMeteo_level <- MeteoVars[meteo_var_counter,c("AtmosLevelCode")]
        #print(thisMeteo_level)
        # get variable units
      #  thisMeteo_units <- MeteoVars[meteo_var_counter,c("Units")]
        #print(thisMeteo_units)
        
#        # Load the data for this variable/level
#        this_model.data <- ReadGrib(file.names = paste(this_model.info[[1]]$file.name,".grb2",sep = ""), levels = thisMeteo_level, variables = thisMeteo_variable,
#                                    forecasts = NULL, domain = NULL, domain.type = "latlon",
#                                    file.type = "grib2", missing.data = NULL)
        
        full_profile <- BuildProfile(model.data = this_model.data, lon = OneDay1ModRun$Longitude, lat = OneDay1ModRun$Latitude, spatial.average = TRUE, points = 4) # about 45 minutes to run 

        #for (this_PM25_row in which_theDate) { # cycle through the rows of dates locations that need data for this date
        for (profile_layer_counter in 1:dim(OneDay1ModRun)[1]) { # cycle through the rows of dates locations that need data for this date
          #print(paste("row",this_PM25_row,"of",dim(OneDay1ModRun)[1],sep = " "))
            # this_PM25_row <- which_theDate[1]
          #print(OneDay1ModRun[this_PM25_row,])
          #this_lon <- OneDay1ModRun[this_PM25_row,c("Longitude")]
          #print(this_lon)
          #this_lat <- OneDay1ModRun[this_PM25_row,c("Latitude")]
          #print(this_lat)
          
          #which_lat_layer <- which(this_profile[[1]]$Lat == this_lat)
          #this_profile[[1]]$lat[which_lat_layer]
          
          #which_lon_layer <- which(this_profile[[1]]$Lon == this_lon)
          #this_profile[[1]]$lon[which_lat_layer]
                    
          #this_profile <- BuildProfile(model.data = this_model.data, lon = this_lon, lat = this_lat, spatial.average = TRUE, points = 4)
          this_profile <- full_profile[[profile_layer_counter]]
          
          this_lat <- this_profile$location[2]
          print(this_lat)
          
          this_lon <- this_profile$location[1]
          print(this_lat)
          
          this_PM25_row <- which(OneDay1ModRun$Latitude == this_lat & OneDay1ModRun$Longitude == this_lon)
          print(OneDay1ModRun[this_PM25_row,])
          
          
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
          
          #which_var_col <- which(this_profile[[1]]$variables == thisMeteo_variable)
          which_var_col <- which(this_profile$variables == thisMeteo_variable)
          this_profile$variables[which_var_col]
          
          which_lev_row <- which(this_profile$levels == thisMeteo_level)
          this_profile$levels[which_lev_row]
          
          this_meteo_value <- this_profile$profile.data[which_lev_row,which_var_col,1]
          #print(paste(thisMeteo_var_Name,"at",thisMeteo_level,"is",this_meteo_value,thisMeteo_units,sep = " "))
          
          print(paste(thisMeteo_var_Name,"at",thisMeteo_level,"is",this_meteo_value,thisMeteo_units,"at",
                      this_lon,this_lat,"on",theDate,"at",this_model.run,"UTC",sep = " "))
          
          OneDay1ModRun[this_PM25_row,c(paste(as.character(thisMeteo_variable), as.character(thisMeteo_level)))] <- this_meteo_value
          
          rm(thisMeteo_var_Name,thisMeteo_variable,thisMeteo_level,thisMeteo_units)
          } # for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
          
        } # for (this_PM25_row in which_theDate) { # cycle through the rows of dates locations that need data for this date

#### Write output to file ####
      write.csv(OneDay1ModRun,file = file.path(ProcessedData.directory,paste(this_location_date_file,"_",as.character(theDate),"_",this_model.run,"UTC.csv",sep = "")),row.names = FALSE)

#### Clear variables ####
      rm(this_PM25_row,this_model.data)
      rm(meteo_var_counter)
      rm(this_model.run) # clear variables from this iteration
      file.remove(this_model.info[[1]]$file.name) # delete file that was downloaded
      file.remove(paste(this_model.info[[1]]$file.name,".grb2",sep = ""))      
  
} # function