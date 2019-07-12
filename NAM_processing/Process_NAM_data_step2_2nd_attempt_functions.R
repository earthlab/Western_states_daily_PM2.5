# Process_NAM_data_step2_2nd_attempt_functions.R
# day_counter <- issue_day_counters[2]
NAM_step2_attempt2_parallel.fn <- function(day_counter) {
  theDate <- as.Date(Date_vector[day_counter]) # the date of the current loop iteration
  print(theDate)
  # which runs for that day are missing data?
  which_this_day <- which(Issue_files$day_counter == day_counter)
  this_day_issues <- Issue_files[which_this_day, ]
  for (run_counter in 1:length(which_this_day)) { # loop through the 4 runs (time periods) per day
    print("run_counter")
    print(run_counter)
    this_model.run_step <- as.character(this_day_issues[run_counter, c("this_model.run")])
    this_model.run <- preserve_leading_zeros.fn(this_value_in = this_model.run_step, n_total_digits_needed = 2)
    print(this_model.run)
    # run function to extract NAM data (one run of one day)
    extract_NAM_data_attempt2.fn(ProcessedData.directory = ProcessedData.directory, #this_location_date_file = this_location_date_file,
                                 MeteoVarsMultiType = MeteoVarsMultiType, theDate = theDate, forecast_times = forecast_times, this_model.run = this_model.run,
                                 PM25DateLoc_time = Merged_Dates_Locations, Model_in_use_abbrev =  Model_in_use_abbrev, sub_folder = output_sub_folder, day_counter = day_counter)
  } # for (run_counter in 1:4) { # loop through the 4 runs (time periods) per day
  
} # end of NAM_step2_attempt2_parallel.fn function


extract_NAM_data_attempt2.fn <- function(ProcessedData.directory, #this_location_date_file,
                                         MeteoVarsMultiType, theDate, forecast_times = 00, this_model.run, 
                                         PM25DateLoc_time, Model_in_use_abbrev =  "namanl", sub_folder, 
                                         day_counter) {
  
  this_file <- file.path(ProcessedData.directory,NAM_folder,sub_folder,paste(sub_folder,"_",as.character(theDate),"_",this_model.run,"UTC_batch",batch_date,".csv",sep = ""))
  print(this_file) 
  #file.exists(this_file) # COMMENT
  
  # grab the list of relevant meteo variables for this file type from MeteoVars
  which_meteo <- which(MeteoVarsMultiType$file_type == "grib2") # get grib2 variable names - all manually downloaded files seem to be grib2
  MeteoVars <- MeteoVarsMultiType[which_meteo,] # matrix with just the relevant rows
  
  if (file.exists(this_file)) { # only run code if file doesn't already exist
    print(this_file)
    stop(print("already exists"))
  } else { # if (file.exists(this_file)) { # only run code if file doesn't already exist - file does not exist
    print("file does not already exist - need to generate file")
    print(paste("Start extract_NAM_data_attempt2.fn for",theDate,this_model.run,"UTC at",Sys.time(),sep = " "))  
    # find the locations with PM25 data for this date
    which_theDate <- which(Merged_Dates_Locations$Date == theDate) # find the locations that need data for this date
    print(paste(length(which_theDate),"PM25 observation locations need weather data on",theDate,sep = " "))
    OneDay1ModRun_PM25 <- Merged_Dates_Locations[which_theDate,] # data frame with just this date's information, all locations
    rm(which_theDate) # clear variable
    # find the prediction locations for this date
    OneDay1ModRun_Predict <- Prediction_Locations
    OneDay1ModRun_Predict$Date <- theDate # fill in this date for the prediction locations
    print(paste(dim(OneDay1ModRun_Predict)[1],"prediction locations need weather data on",theDate,sep = " "))
    OneDay1ModRun <- rbind(OneDay1ModRun_PM25,OneDay1ModRun_Predict)
    rm(OneDay1ModRun_PM25,OneDay1ModRun_Predict)
    print(paste("Overall,",dim(OneDay1ModRun)[1],"locations need weather data on",theDate,sep = " "))
    this_model.date <- format(theDate, format = "%Y%m%d") # get date in format YYYYmmdd - needed for rNOMADS functions
    # look for file in NAM data that was manually downloaded
    #guess_file_name_grb2 <- paste("namanl_218_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb2",sep = "")
    guess_file_name_grb2 <- paste("nam_218_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb2",sep = "")
    #guess_file_name_grb1 <- paste("namanl_218_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb",sep = "")
    guess_file_name_grb1 <- paste("nam_218_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb",sep = "")
    
    if (file.exists(file.path(ProcessedData.directory,NAM_folder,attempt2_data_source_subfolder,guess_file_name_grb2))) {
      HAS_file_name <- guess_file_name_grb2
      this_file_type <-  "grib2"
    } else if (file.exists(file.path(ProcessedData.directory,NAM_folder,attempt2_data_source_subfolder,guess_file_name_grb1))) {
      this_file_name <- guess_file_name_grb1
      this_file_type <-  "grib1"
      stop("expand code to handle grib1 files in local repository (files that were manually downloaded from NAM_HAS).")
    } else {
      HAS_file_name <- NA
      this_file_type <- NA
    }
    
    if (!is.na(HAS_file_name)) { # file is in the local repository
      print(paste("start copying ",HAS_file_name,"from local repository to NAM_data_orig folder"))
      local_source <- file.path(ProcessedData.directory,NAM_folder,attempt2_data_source_subfolder,HAS_file_name)
      HAS_file_w_path <- file.path(define_file_paths.fn("NAM.directory"),HAS_file_name)
      file.copy(from = local_source, to = HAS_file_w_path)
      thisGribInfo <- GribInfo(grib.file = HAS_file_w_path, file.type = this_file_type)
      
      if (file.exists(HAS_file_w_path) & length(thisGribInfo$inventory)>5) { # does converted file exist and it has more than 5 variables (should have lots)?
            # load the bounding box for the study
            bounding_box <- define_project_bounds.fn()
            bound_box_vec <- c(bounding_box$West_Edge, bounding_box$East_Edge, bounding_box$North_Edge, bounding_box$South_Edge)
            
            # Load the data for this variable/level in the study area
            print(paste("Start ReadGrib for",this_model.date,this_model.run,"UTC at",Sys.time(),sep = " "))
            if (this_file_type == "grib1") { # name of file depends on file type
              this_model.data <- ReadGrib(file.names = paste(this_model.info[[1]]$file.name,".grb2",sep = ""), levels = MeteoVars$AtmosLevelCode, variables = MeteoVars$VariableCode,
                                          forecasts = NULL, domain = bound_box_vec, domain.type = "latlon",
                                          file.type = "grib2", missing.data = NULL)
            } else if (this_file_type == "grib2") { # if (this_file_type == "grib1") { # name of file depends on file type
              #this_model.data <- ReadGrib(file.names = this_model.info[[1]]$file.name, levels = MeteoVars$AtmosLevelCode, variables = MeteoVars$VariableCode,
              #                            forecasts = NULL, domain = bound_box_vec, domain.type = "latlon",
              #                            file.type = "grib2", missing.data = NULL) 
              this_model.data <- ReadGrib(file.names = HAS_file_w_path, levels = MeteoVars$AtmosLevelCode, variables = MeteoVars$VariableCode,
                                          forecasts = NULL, domain = bound_box_vec, domain.type = "latlon",
                                          file.type = "grib2", missing.data = NULL) 
            } # if (this_file_type == "grib1") { # name of file depends on file type
            rm(bounding_box,bound_box_vec)
            # from rNOMADS.pdf:  domain - Include model nodes in the specified region: c(LEFT LON, RIGHT LON, NORTH LAT, SOUTH LAT). If NULL,
            # include everything. This argument works for GRIB2 only.
            
            # Build the Profile, i.e., extract variables at the points of interest
            print(paste("Start BuildProfile for",this_model.date,this_model.run,"UTC at",Sys.time(),sep = " "))
            full_profile <- BuildProfile(model.data = this_model.data, lon = OneDay1ModRun$Lon, lat = OneDay1ModRun$Lat, spatial.average = FALSE) # about X minutes to run, nearest model node
            # Cycle through the locations of interest and put meteo variables of interest into OneDay1MOdRun data frame
            print(paste("Start cycling through layers (locations) for",this_model.date,this_model.run,"UTC at",Sys.time(),sep = " "))
            for (profile_layer_counter in 1:dim(OneDay1ModRun)[1]) { # cycle through the rows of dates locations that need data for this date
              # Locate the data for this location from full_profile
              this_profile <- full_profile[[profile_layer_counter]] # grab all data for one location
              this_lat <- this_profile$location[2] # identify the latitude for this location
              this_lon <- this_profile$location[1] # identify the longitude for this location
              this_PM25_row <- which(OneDay1ModRun$Lat == this_lat & OneDay1ModRun$Lon == this_lon) # find this lat/lon in OneDay1ModRun
              
              # Cycle through meteo variables and pull out the data #
              #print(paste("Start meteo variables for location #",this_PM25_row,"of",dim(OneDay1ModRun)[1]," for",this_model.date,this_model.run,"UTC at",Sys.time(),sep = " "))
              for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
                # get meteo variable info
                thisMeteo_var_Name <- MeteoVars[meteo_var_counter,c("VariableName")] # get variable full name
                thisMeteo_variable <- MeteoVars[meteo_var_counter,c("VariableCode")] # get variable coded name
                thisMeteo_level <- MeteoVars[meteo_var_counter,c("AtmosLevelCode")] # get variable level name
                thisMeteo_units <- MeteoVars[meteo_var_counter,c("Units")] # get variable units
                which_var_col <- which(this_profile$variables == thisMeteo_variable) # locate the data for this variable
                which_lev_row <- which(this_profile$levels == thisMeteo_level) # locate the data for this atmospheric level
                this_meteo_value <- this_profile$profile.data[which_lev_row,which_var_col,1] # what is the value for this variable at this level?
                # uncomment next 2 lines to output variable value/information to console
                #print(paste(thisMeteo_var_Name,"at",thisMeteo_level,"is",this_meteo_value,thisMeteo_units,"at",
                #            this_lon,this_lat,"on",theDate,"at",this_model.run,"UTC",sep = " "))
                
                # input meteo value into OneDay1ModRun
                if (length(this_meteo_value)>0) {
                  OneDay1ModRun[this_PM25_row,c(paste(as.character(thisMeteo_variable), as.character(thisMeteo_level)))] <- this_meteo_value
                } else {
                  print(paste("*** ",thisMeteo_var_Name,"value has length of zero for date:",this_model.date,"run:",this_model.run," ***"))
                }
                
                rm(thisMeteo_var_Name,thisMeteo_variable,thisMeteo_level,thisMeteo_units) # clear variables
              } # for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
            } #for (profile_layer_counter in 1:dim(OneDay1ModRun)[1]) { # cycle through the rows of dates locations that need data for this date # for (this_PM25_row in which_theDate) { # cycle through the rows of dates locations that need data for this date
            
            # Write output to file #
            print(paste("Start outputting file to csv for",this_model.date,this_model.run,"UTC at",Sys.time(),sep = " "))
            write.csv(OneDay1ModRun,file = this_file,row.names = FALSE)
            
            # Delete NAM files ##
            if (file.exists(HAS_file_w_path)) { # check if file exists before trying to delete it
              file.remove(HAS_file_w_path) # delete file that was downloaded
            } # # check if file exists before trying to delete it
            
            rm(this_PM25_row,this_model.data,meteo_var_counter,this_model.run) # clear variables from this iteration
          } else if (file.exists(HAS_file_w_path) == FALSE) { # if (file.exists(HAS_file_name) & length(thisGribInfo$inventory)>5) { # does converted file exist and it has more than 5 variables (should have lots)?
            print("the converted file does not exist")
            sink(file = ReportFileName_Attempt2, append = TRUE)
            write.table(paste(day_counter,this_model.date,this_model.run,"The file does not exist.",sep = ","),row.names = FALSE, col.names = FALSE, sep = "", quote = FALSE)
            sink()
          } else {
            print("the converted file had insufficient data")
            # Delete NAM files #
            file.remove(this_model.info[[1]]$file.name) # delete file that was downloaded
            file.remove(paste(this_model.info[[1]]$file.name,".grb2",sep = ""))
            sink(file = ReportFileName, append = TRUE)
            write.table(paste(day_counter,this_model.date,this_model.run,"The converted file had insufficient data.",sep = ","),row.names = FALSE, col.names = FALSE, sep = "", quote = FALSE)
            sink()
          } # if (file.exists(HAS_file_name) & length(thisGribInfo$inventory)>5) { # does converted file exist and it has more than 5 variables (should have lots)?
        
    } else {
      stop("write code to make a different attempt to fill in missing NAM data. (It was not in the local repository.)")
    }
  } # if (file.exists(this_file)) { # only run code if file doesn't already exist
} # end of extract_NAM_data_attempt2.fn function
