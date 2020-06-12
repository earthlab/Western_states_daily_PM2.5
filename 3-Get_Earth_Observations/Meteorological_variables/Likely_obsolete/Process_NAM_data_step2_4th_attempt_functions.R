# Process_NAM_data_step2_4th_attempt_functions.R

NAM_step2_attempt4_parallel.fn <- function(issue_file_counter) {
  This_issue_file <- Issue_files3[issue_file_counter, ]
  day_counter <- This_issue_file$day_counter
  theDate <- as.Date(Date_vector[day_counter]) # the date of the current loop iteration
  print(theDate) # print date to screen
  this_model.run <- preserve_leading_zeros.fn(this_value_in = This_issue_file$this_model.run, n_total_digits_needed = 2)
  print(this_model.run) # print model run to screen
  # run function to extract NAM data (one run of one day)
  extract_wx_data_attempt4.fn(ProcessedData.directory = ProcessedData.directory, #this_location_date_file = this_location_date_file,
                               MeteoVarsMultiType = MeteoVarsMultiType, theDate = theDate, forecast_times = forecast_times, this_model.run = this_model.run,
                               PM25DateLoc_time = Merged_Dates_Locations, sub_folder = output_sub_folder, day_counter = day_counter) # Model_in_use_abbrev =  Model_in_use_abbrev,
} # end of NAM_step2_attempt2_parallel.fn function

guess_NOMADS_file_name.fn <- function(Model_in_use_abbrev = "namanl", this_file_type = 'grib2', include_clutter = "FALSE") { # determine what the file name of the file to be downloaded is likely to be
  if (Model_in_use_abbrev == "namanl") { # name of file depends on the model
      if (this_file_type == "grib2") {
        #guess_name_grb2_step <- paste("namanl_218_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb2",sep = "")
        guess_name_step <- paste("namanl_218_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb2",sep = "")
        name_clutter <- ""
      } else if (this_file_type == "grib1") {
        #guess_name_grb1_step <- paste("namanl_218_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb",sep = "")
        guess_name_step <- paste("namanl_218_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb",sep = "")
        name_clutter <- ""
      }
  } else if (Model_in_use_abbrev == "gfsanl") {
      if (this_file_type == "grib2") {
        guess_name_step <- paste("gfsanl_4_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb2",sep = "")
        name_clutter <- "<"
      } else if (this_file_type == "grib1") {
        guess_name_step <- paste("gfsanl_4_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb",sep = "")
        name_clutter <- "<i"
      }
  } else {
    stop(paste("Expand code in guess_NOMADS_file_name.fn in Process_NAM_data_step2_3rd_attempt.R to include",Model_in_use_abbrev))
  }
  
  if (include_clutter == TRUE) {
    guess_name <- paste(guess_name_step,name_clutter,sep = "")
  } else {
    guess_name <- guess_name_step
  }
  print(guess_name)
} # guess_NOMADS_file_name.fn <- function() { # determine what the file name of the file to be downloaded is likely to be
#guess_file_name <- paste("namanl_218_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb",sep = "")

# is this a grib1 (.grb) or grib2 (.grb2) type of file?
which_type_of_grib_file_multi_model.fn <- function(list.available.models, this_model.date, this_model.run, forecast_times, Model_in_use_abbrev = "namanl") { # is this a grib1 (.grb) or grib2 (.grb2) type of file?
  # if (Model_in_use_abbrev == "namanl") { # name of file depends on the model
  #   guess_name_grb2 <- paste("namanl_218_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb2",sep = "")
  #   guess_name_grb1 <- paste("namanl_218_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb",sep = "")
  # } else if (Model_in_use_abbrev == "gfsanl") {
  #   guess_name_grb2 <- paste("gfsanl_4_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb2<",sep = "")
  #   guess_name_grb1 <- paste("gfsanl_4_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb<i",sep = "")
  # } else {
  #   stop(paste("Expand code in which_type_of_grib_file_multi_model.fn in Process_NAM_data_step2_3rd_attempt.R to include",Model_in_use_abbrev))
  # }
  guess_name_grb2 <- guess_NOMADS_file_name.fn(Model_in_use_abbrev, this_file_type = 'grib2', include_clutter = "TRUE")
  print(guess_name_grb2)
  guess_name_grb1 <- guess_NOMADS_file_name.fn(Model_in_use_abbrev, this_file_type = 'grib1', include_clutter = "TRUE")

  if (guess_name_grb2 %in% list.available.models$file.name) { # check if the predicted file name for a grib2 file is present
    this_file_type <- "grib2"
  } else if (guess_name_grb1 %in% list.available.models$file.name) { # check if the predicted file name for a grib1 file is present
    this_file_type <- "grib1"
  } else { # neither of the predicted names were present, so try the old way of figuring out grib1 vs grib2
    stop("Expand code in which_type_of_grib_file_multi_model.fn in Process_NAM_data_step2_3rd_attempt.R for 'else' statement")
    # first_file_name <- as.character(list.available.models$file.name[[1]]) # grab first file name in list
    # last_character <- substr(first_file_name,nchar(first_file_name),nchar(first_file_name)) # find the last character in the file name - determines which type of file it is
    # if (last_character == "b") { # grib1 files
    #   #print("These are grib1 files")
    #   this_file_type <- "grib1"
    # } else if (last_character == "2") { # grib2 files
    #   #print("These are grib2 files")
    #   this_file_type <- "grib2"
    # } else {error("Unknown file type")} # check code
    # rm(first_file_name,last_character) # clear variables
  } # if (guess_name_grb2 %in% list.available.models$file.name) { # check if the predicted file name for a grib2 file is present
  return(this_file_type) # function output
} # end of which_type_of_grib_file.fn function

weed_multiple_files.fn <- function(this_model.info, Model_in_use_abbrev) { # deal with the circumstance in which multple files were downloaded by ArchiveGribGrab
  if (Model_in_use_abbrev == "gfsanl") {
    orig_urls <- this_model.info[[1]]$url
    grid_domains <- unlist(lapply(1:length(orig_urls), function(X){
      this_grid_domain <- substr(orig_urls[X],nchar(orig_urls[X])-23,nchar(orig_urls[X])-23)
    }))
    which_to_use <- which(grid_domains == "4") # We want to use grid "4" because it is 0.5 degrees resolution whereas grid "3" is 1 degree resolution 
    # see https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-forcast-system-gfs 
    name_to_keep <- substr(orig_urls[which_to_use],nchar(orig_urls[which_to_use])-30,nchar(orig_urls[which_to_use]))
    downloaded_filename <- substr(this_model.info[[1]]$file.name[which_to_use],29, nchar(this_model.info[[1]]$file.name[which_to_use]))
    file.rename(file.path(NAM.directory,downloaded_filename), file.path(NAM.directory,name_to_keep))
    which_to_delete <- which(grid_domains == "3")
    name_to_delete <- substr(this_model.info[[1]]$file.name[which_to_delete],29, nchar(this_model.info[[1]]$file.name[which_to_delete]))
    file.remove(file.path(NAM.directory,name_to_delete))
    print("Multiple files were downloaded. Keeping the grid 4 data, which is 0.5 degree resolution and deleting the grid 3 data which is 1 degree resolution.")
    print(paste("Re-naming",downloaded_filename,"to",name_to_keep,"and deleting",name_to_delete))
    print("For further info about the gfsanl model, see")
    print("https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-forcast-system-gfs")
  } else {
    stop("write more code in Process_NAM_data_step2_4th_attempt.R in weed_multiple_files.fn to include more models")
  } # if (Model_in_use_abbrev == "gfsanl") {
  return(name_to_keep) # output from function
} # end of weed_multiple_files.fn function

extract_wx_data_attempt4.fn <- function(ProcessedData.directory, #this_location_date_file,
                                         MeteoVarsMultiType, theDate, forecast_times = 00, this_model.run, 
                                         PM25DateLoc_time, sub_folder, day_counter) { # Model_in_use_abbrev =  "namanl", 
  
  this_file <- file.path(ProcessedData.directory,NAM_folder,sub_folder,paste(sub_folder,"_",as.character(theDate),"_",this_model.run,"UTC_batch",batch_date,".csv",sep = ""))
  print(this_file) 
  file.exists(this_file) # COMMENT
  
  if (file.exists(this_file)) { # only run code if file doesn't already exist
    print(this_file)
    print("already exists")
  } else {
    print("file does not already exist - need to generate file")
    print(paste("Start extract_wx_data_attempt4.fn for",theDate,this_model.run,"UTC at",Sys.time(),sep = " "))  
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
    # Determine file type   
    options(warn  =  1) # don't throw an error when there is a warning about there not being a file
    # try to find a model that has data for that day
    Models_to_try <-  c("namanl","rucanl","gfsanl","ruc13","ruc","meso-eta-hi","gfs-avn-hi","gfs4","rap252","rap130") 
    #Model_in_use_abbrev =  "namanl"
    model_found = 0
    for (m in 1:length(Models_to_try)) { # looking for a model that has data for the given date
      if (model_found == 0) { # only check if a model hasn't already been found
      Model_in_use_abbrev <- Models_to_try[m]
      print(paste("try model",Model_in_use_abbrev))
      list.available.models <- CheckNOMADSArchive_MMM(Model_in_use_abbrev, this_model.date) # list all model files available for this model and date
      #print(list.available.models)
        if (is.null(list.available.models$file.name) == FALSE & 
            paste(this_model.run,"00",sep = "") %in% list.available.models$model.run &
            preserve_leading_zeros.fn(forecast_times,3) %in% list.available.models$pred) { # check if this model had data for the given date
          print(paste(Model_in_use_abbrev,"has data for this date"))
          model_found <- 1 # a model with data has been found
          print(list.available.models)
        } else {
          rm(Model_in_use_abbrev)
        } # if (is.null(list.available.models$file.name) == FALSE) { # check if this model had data for the given date
      } # if (model_found == 0) { # only check if a model hasn't already been found
    } # for (m in 1:length(Models_to_try)) { # looking for a model that has data for the given date
    rm(model_found)
    sink(file = ReportFileName_Attempt4, append = TRUE)
    write.table(paste(day_counter,this_model.date,this_model.run,Model_in_use_abbrev,sep = ","),row.names = FALSE, col.names = FALSE, sep = "", quote = FALSE)
    sink()
    
    if (is.null(list.available.models$file.name) == FALSE) { # only run computations if there is model data for this day
      available_times_of_day <- unique(list.available.models$model.run) # what times are available?
      available_times_of_day_trunc <- unlist(lapply(available_times_of_day,function(x){substr(x,1,2)}))
      if (this_model.run %in% available_times_of_day_trunc) { # check if there is a model run for this model run (time of day)
        this_file_type <- which_type_of_grib_file_multi_model.fn(list.available.models, this_model.date, this_model.run, forecast_times,Model_in_use_abbrev)# is this a grib1 (.grb) or grib2 (.grb2) type of file?
        print(this_file_type) 
        # if (this_file_type == "grib1") { # determine the name of the file to be downloaded, depending on whether it is grib1 or grib2
        #   guess_file_name <- paste("namanl_218_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb",sep = "")
        # } else if (this_file_type == "grib2") { # if (this_file_type == "grib1") { # determine the name of the file to be downloaded, depending on whether it is grib1 or grib2
        #   guess_file_name <- paste("namanl_218_",this_model.date,"_",this_model.run,"00_00",forecast_times,".grb2",sep = "")
        # } # if (this_file_type == "grib1") { # determine the name of the file to be downloaded, depending on whether it is grib1 or grib2
        guess_file_name <- guess_NOMADS_file_name.fn(Model_in_use_abbrev = Model_in_use_abbrev, this_file_type = this_file_type, include_clutter = "TRUE")
        
        # grab the list of relevant meteo variables for this file type from MeteoVars
        #which_meteo <- which(MeteoVarsMultiType$file_type == "grib2") # get grib2 files because grib1 files will be converted to grib2
        #MeteoVars <- MeteoVarsMultiType[which_meteo,] # matrix with just the relevant rows
          if (Model_in_use_abbrev == "gfsanl") {
            this_source_file <- paste("MeteoVariables_gfsanl.csv")
            MeteoVarsMultiType <- read.csv(file.path(define_file_paths.fn("NAM_Code.directory"),this_source_file))
            rm(this_source_file)
          } else {
            stop("create variable dictionary for the current model")
          }
        # Download archived model data from the NOMADS server - page 4 of rNOMADS.pdf ~13 seconds
        print(paste("Start downloading",this_file_type,"file for",this_model.date,this_model.run,"UTC at",Sys.time(),sep = " "))
        # example url: https://nomads.ncdc.noaa.gov/data/namanl/200811/20081123/namanl_218_20081123_0600_000.grb
        if (guess_file_name %in% list.available.models$file.name) { # file not present in archive
          this_model.info <- ArchiveGribGrab_MMM(abbrev = Model_in_use_abbrev, model.date = this_model.date,
                                                 model.run = this_model.run, preds = forecast_times,
                                                 local.dir = NAM.directory, file.names = NULL, tidy = FALSE,
                                                 verbose = TRUE, download.method = NULL, file.type = this_file_type)
          
          if (length(this_model.info[[1]]$file.name)>1) {
            kept_filename <- weed_multiple_files.fn(this_model.info, Model_in_use_abbrev)
          }

          # Convert grib1 to grib2 if necessary and then run GribInfo
            if (this_file_type == "grib1") { # name of file depends on file type
            stop("Write code to get the right file name for grib1 to grib2 conversion. (Only need to write this code if we end up with grib1 files in attemp4 weather data processing.)")
            #keep print(paste("Start converting grib1 to grib2 for",this_model.date,this_model.run,"UTC at",Sys.time(),sep = " "))
            #keep thisGribInfo <- convert_grib1to2.fn(this_model.info,this_file_type)
            #keep converted_file <- file.path(uppermost.directory,"NAM_data_orig",paste(as.character(this_model.date),"_",this_model.run,"00_000.grb.grb2",sep = ""))
            } else if (this_file_type == "grib2") { # if (this_file_type == "grib1") { # name of file depends on file type
              thisGribInfo <- GribInfo(grib.file = file.path(NAM.directory,kept_filename), file.type = this_file_type)
              converted_file <- file.path(NAM.directory,kept_filename)
            } # if (this_file_type == "grib1") { # name of file depends on file type

          if (file.exists(converted_file) & length(thisGribInfo$inventory)>5) { # does converted file exist and it has more than 5 variables (should have lots)?
            # load the bounding box for the study
            bounding_box <- define_project_bounds.fn()
            bound_box_vec <- c(bounding_box$West_Edge, bounding_box$East_Edge, bounding_box$North_Edge, bounding_box$South_Edge)
            
            stop("pick up here - need to check on code since this is gfsanl instead of namanl - variable names or other things might be different")
            
            # Load the data for this variable/level in the study area
            print(paste("Start ReadGrib for",this_model.date,this_model.run,"UTC at",Sys.time(),sep = " "))
            if (this_file_type == "grib1") { # name of file depends on file type
              #this_model.data <- ReadGrib(file.names = paste(this_model.info[[1]]$file.name,".grb2",sep = ""), levels = MeteoVars$AtmosLevelCode, variables = MeteoVars$VariableCode,
              #                            forecasts = NULL, domain = bound_box_vec, domain.type = "latlon",
              #                            file.type = "grib2", missing.data = NULL)
            } else if (this_file_type == "grib2") { # if (this_file_type == "grib1") { # name of file depends on file type
              #this_model.data <- ReadGrib(file.names = this_model.info[[1]]$file.name, levels = MeteoVars$AtmosLevelCode, variables = MeteoVars$VariableCode,
              #                            forecasts = NULL, domain = bound_box_vec, domain.type = "latlon",
              #                            file.type = "grib2", missing.data = NULL) 
              
              #this_model.data <- ReadGrib(file.names = converted_file, levels = MeteoVars$AtmosLevelCode, variables = MeteoVars$VariableCode,
              #                            forecasts = NULL, domain = bound_box_vec, domain.type = "latlon",
              #                            file.type = "grib2", missing.data = NULL)
              
              
              this_model.data <- ReadGrib(file.names = converted_file, levels = MeteoVars$AtmosLevelCode, variables = MeteoVars$VariableCode,
                                          forecasts = NULL, #domain = bound_box_vec, domain.type = "latlon",
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
            #if (file.exists(this_model.info[[1]]$file.name)) { # check if file exists before trying to delete it
            #  file.remove(this_model.info[[1]]$file.name) # delete file that was downloaded
            #} # # check if file exists before trying to delete it
            #if (file.exists(paste(this_model.info[[1]]$file.name,".grb2",sep = ""))) { # # check if file exists before trying to delete it
            #  file.remove(paste(this_model.info[[1]]$file.name,".grb2",sep = ""))
            #} # check if file exists before trying to delete it
            file.remove(file.path(NAM.directory,kept_filename))
            
            # Clear variables #
            rm(this_PM25_row,this_model.data, this_model.info)
            rm(meteo_var_counter)
            rm(this_model.run) # clear variables from this iteration
          } else if (file.exists(converted_file) == FALSE) { # if (file.exists(converted_file) & length(thisGribInfo$inventory)>5) { # does converted file exist and it has more than 5 variables (should have lots)?
            print("the converted file does not exist")
            sink(file = ReportFileName, append = TRUE)
            write.table(paste(day_counter,this_model.date,this_model.run,"The converted file does not exist.",sep = ","),row.names = FALSE, col.names = FALSE, sep = "", quote = FALSE)
            sink()
          } else {
            print("the converted file had insufficient data")
            # Delete NAM files #
            file.remove(this_model.info[[1]]$file.name) # delete file that was downloaded
            file.remove(paste(this_model.info[[1]]$file.name,".grb2",sep = ""))
            sink(file = ReportFileName, append = TRUE)
            write.table(paste(day_counter,this_model.date,this_model.run,"The converted file had insufficient data.",sep = ","),row.names = FALSE, col.names = FALSE, sep = "", quote = FALSE)
            sink()
          } # if (file.exists(converted_file) & length(thisGribInfo$inventory)>5) { # does converted file exist and it has more than 5 variables (should have lots)?
        } else { # if (guess_file_name %in% list.available.models$file.name) { # file not present in archive
          print(paste("the requested file, ",guess_file_name, " is not in the archive."))
          sink(file = ReportFileName, append = TRUE)
          write.table(paste(day_counter,this_model.date,this_model.run,"The requested file is not in the archive.",sep = ","),row.names = FALSE, col.names = FALSE, sep = "", quote = FALSE)
          sink()
        } # if (guess_file_name %in% list.available.models$file.name) { # file not present in archive
      } else { # if (is.null(list.available.models$file.name) == FALSE) { # only run computations if there is model data for this day
        print(paste("there is data for this date, but not this model run",this_model.run))
        sink(file = ReportFileName, append = TRUE)
        write.table(paste(day_counter,this_model.date,this_model.run,paste("There is data for this date but not this model run."),sep = ","),row.names = FALSE, col.names = FALSE, sep = "", quote = FALSE)
        sink()
      } # if (this_model.run %in% available_times_of_day_trunc) { # check if there is a model run for this model run (time of day)
    } else {
      print(paste("there is no data for this date",this_model.date))
      sink(file = ReportFileName, append = TRUE)
      write.table(paste(day_counter,this_model.date,this_model.run,"There is no data for this date.",sep = ","),row.names = FALSE, col.names = FALSE, sep = "", quote = FALSE)
      sink()
    } # if (is.null(list.available.models$file.name) == FALSE) { # only run computations if there is model data for this day
  } # if (file.exists(this_file)) { # only run code if file doesn't already exist
} # end of extract_NAM_data.parallel.fn function
