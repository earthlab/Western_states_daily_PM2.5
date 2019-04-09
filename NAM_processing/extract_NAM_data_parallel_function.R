extract_NAM_data.parallel.fn <- function(ProcessedData.directory, this_location_date_file,
                                        MeteoVarsMultiType, theDate, forecast_times = 00, this_model.run, 
                                        PM25DateLoc_time, Model_in_use_abbrev =  "namanl", sub_folder) {
  
  #this_file <- file.path(ProcessedData.directory,sub_folder,paste(this_location_date_file,"_",as.character(theDate),"_",this_model.run,"UTC.csv",sep = ""))
  this_file <- file.path(ProcessedData.directory,NAM_folder,output_sub_folder,paste("NAM_Step2_",as.character(theDate),"_",this_model.run,"UTC_batch",batch_date,".csv",sep = ""))
  print(this_file)
  file.exists(this_file)
  
  if (file.exists(this_file)) { # only run code if file doesn't already exist
    print(this_file)
    print("already exists")
    
  } else {
    print("file does not already exist - need to generate file")
  
  # Input variables
    # ProcessedData.directory: location of source file that has the dates and locations for which you want the NAM data
    # this_location_date_file: name of file listing the dates (local) and locations where you want the NAM data for - just used for naming output file
    # MeteoVarsMultiType: should be loaded file: MeteoVariablesNAM.csv  
      # variables of interest. Use these commands in terminal to see what the variable names, levels, and units are:
      # wgrib 20080101_0000_000.grb -V > NAM_grib1.txt
      # wgrib2 20180202_0000_000.grb2 -V > NAM_grib2.txt
      # header: VariableNumber	VariableName	VariableCode	AtmosLevelName	AtmosLevelCode	Units	file_type	time frame	24-hr summary
    # theDate: is the date to be processed
    # forecast_times <- 00 # reanalysis - anything else would be a forecast
    # this_model.run: which run (time of day in UTC) is to be processed. The choices for NAM are 00, 06, 12, and 18 Z
    # PM25DateLoc_time: listing the dates (local) and locations where you want the NAM data for, probably loaded from 
      # this_location_date_file_NextDay.csv, or something similar, which is the same as this_location_date_file, but has the
      # next day for every date/location in this_location_date_file - this is to be 
      # able to handle local vs UTC time
      # header should include: "Longitude", "Latitude", "Date"
    # Model_in_use_abbrev <-  "namanl" # NAM Analysis
    # file for _wNextDay is also called: this_location_date_file_wNextDay: same as this_location_date_file, but has the
      # next day for every date/location in this_location_date_file - this is to be 
      # able to handle local vs UTC time
  # Output: a csv file with the meteo variables for all locations in PM25Date_Loc_time for this_model.run and theDate
  # See rNOMADS.pdf (google rNOMADS) for more information about several functions called in this function

  print(paste("Start extract_NAM_data_parallel_fn for",theDate,this_model.run,"UTC at",Sys.time(),sep = " "))  
  
    # set up the data frame for the meteo data (which will be output to a csv file)
  #print(theDate)
  #print(dim(theDate))
  #print(dim(PM25DateLoc_time$Date))
  #print(PM25DateLoc_time$Date)
  #which(PM25DateLoc_time$Date == theDate)
    #which_theDate <- which(PM25DateLoc_time$Date == theDate) # find the locations that need data for this date
    which_theDate <- which(Merged_Dates_Locations$Date == theDate) # find the locations that need data for this date
    print(paste(length(which_theDate),"locations need weather data on",theDate,sep = " "))
    #OneDay1ModRun <- PM25DateLoc_time[which_theDate,] # data frame with just this date's information, all locations
    OneDay1ModRun <- Merged_Dates_Locations[which_theDate,] # data frame with just this date's information, all locations
    #print(OneDay1ModRun)
    #rm(PM25DateLoc_time) #UNCOMMENT
    this_model.date <- format(theDate, format = "%Y%m%d") # get date in format YYYYmmdd - needed for rNOMADS functions

    #pause_seconds <- runif(1, 1, 300)
    #print(paste("pause_seconds = ",pause_seconds))
    #if (with_pause) {Sys.sleep(pause_seconds)}
    
    # # Determine file type   
    options(warn  =  1) # don't throw an error when there is a warning about there not being a file
    #list.available.models <- CheckNOMADSArchive(Model_in_use_abbrev, this_model.date) # list all model files available for this model and date
    list.available.models <- CheckNOMADSArchive_MMM(Model_in_use_abbrev, this_model.date) # list all model files available for this model and date
    #if (exists("list.available.models")) { # only run computations if there is model data
    if (is.null(list.available.models$file.name) == FALSE) { # only run computations if there is model data for this day
    available_times_of_day <- unique(list.available.models$model.run) # what times are available?
    available_times_of_day_trunc <- unlist(lapply(available_times_of_day,function(x){substr(x,1,2)}))
    if (this_model.run %in% available_times_of_day_trunc) { # check if there is a model run for this model run (time of day)
    this_file_type <- which_type_of_grib_file.fn(list.available.models) # is this a grib1 (.grb) or grib2 (.grb2) type of file?
    print(this_file_type) 
    # grab the list of relevant meteo variables for this file type from MeteoVars
    which_meteo <- which(MeteoVarsMultiType$file_type == "grib2") # get grib2 files because grib1 files will be converted to grib2
    MeteoVars <- MeteoVarsMultiType[which_meteo,] # matrix with just the relevant rows
    # Download archived model data from the NOMADS server - page 4 of rNOMADS.pdf ~13 seconds
      print(paste("Start downloading",this_file_type,"file for",this_model.date,this_model.run,"UTC at",Sys.time(),sep = " "))
      this_model.info <- ArchiveGribGrab_MMM(abbrev = Model_in_use_abbrev, model.date = this_model.date,
                                         model.run = this_model.run, preds = forecast_times,
                                         local.dir = NAM.directory, file.names = NULL, tidy = FALSE,
                                         verbose = TRUE, download.method = NULL, file.type = this_file_type)
      # Convert grib1 to grib2 if necessary and then run GribInfo
      print(paste("Start converting grib1 to grib2 for",this_model.date,this_model.run,"UTC at",Sys.time(),sep = " "))
      #thisGribInfo <- convert_grib1to2.fn(this_model.info,this_file_type)
      #if (with_pause) {
      #  #Sys.sleep(pause_seconds)
      #  wait(convert_grib1to2.fn(this_model.info,this_file_type),300)
      #} else {
      #  thisGribInfo <- convert_grib1to2.fn(this_model.info,this_file_type)
      #}
      thisGribInfo <- convert_grib1to2.fn(this_model.info,this_file_type)
      converted_file <- file.path(uppermost.directory,"NAM_data_orig",paste(as.character(this_model.date),"_",this_model.run,"00_000.grb.grb2",sep = ""))
      print(converted_file)
      file.exists(converted_file)
      if (file.exists(converted_file) & length(thisGribInfo$inventory)>5) { # does converted file exist and it has more than 5 variables (should have lots)?
      # load the bounding box for the study
      bounding_box <- define_project_bounds.fn()
      bound_box_vec <- c(bounding_box$West_Edge, bounding_box$East_Edge, bounding_box$North_Edge, bounding_box$South_Edge)

      # Load the data for this variable/level in the study area
      print(paste("Start ReadGrib for",this_model.date,this_model.run,"UTC at",Sys.time(),sep = " "))
      this_model.data <- ReadGrib(file.names = paste(this_model.info[[1]]$file.name,".grb2",sep = ""), levels = MeteoVars$AtmosLevelCode, variables = MeteoVars$VariableCode,
                                  forecasts = NULL, domain = bound_box_vec, domain.type = "latlon",
                                  file.type = "grib2", missing.data = NULL)
      rm(bounding_box,bound_box_vec)
      # from rNOMADS.pdf:  domain - Include model nodes in the specified region: c(LEFT LON, RIGHT LON, NORTH LAT, SOUTH LAT). If NULL,
      #include everything. This argument works for GRIB2 only.

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

          #### Cycle through meteo variables and pull out the data ####
          # grab all meteo variables for this file type
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
          OneDay1ModRun[this_PM25_row,c(paste(as.character(thisMeteo_variable), as.character(thisMeteo_level)))] <- this_meteo_value

          rm(thisMeteo_var_Name,thisMeteo_variable,thisMeteo_level,thisMeteo_units) # clear variables
          #} #if (exists(list.available.models)) { # only run computations if there is model data
          } # for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
        } #for (profile_layer_counter in 1:dim(OneDay1ModRun)[1]) { # cycle through the rows of dates locations that need data for this date # for (this_PM25_row in which_theDate) { # cycle through the rows of dates locations that need data for this date

#### Write output to file ####
      print(paste("Start outputting file to csv for",this_model.date,this_model.run,"UTC at",Sys.time(),sep = " "))
      #write.csv(OneDay1ModRun,file = file.path(ProcessedData.directory,sub_folder,paste(this_location_date_file,"_",as.character(theDate),"_",this_model.run,"UTC.csv",sep = "")),row.names = FALSE)
      write.csv(OneDay1ModRun,file = file.path(ProcessedData.directory,NAM_folder,sub_folder,paste(sub_folder,"_",as.character(theDate),"_",this_model.run,"UTC_batch",batch_date,".csv",sep = "")),row.names = FALSE)
      
#### Delete NAM files ####
      file.remove(this_model.info[[1]]$file.name) # delete file that was downloaded
      file.remove(paste(this_model.info[[1]]$file.name,".grb2",sep = ""))

#### Clear variables ####
      rm(this_PM25_row,this_model.data, this_model.info)
      rm(meteo_var_counter)
      rm(this_model.run) # clear variables from this iteration
      } else if (file.exists(converted_file) == FALSE) { # if (file.exists(converted_file) & length(thisGribInfo$inventory)>5) { # does converted file exist and it has more than 5 variables (should have lots)?
        print("the converted file does not exist")
      } else {
        print("the converted file had insufficient data")
        # Delete NAM files #
        file.remove(this_model.info[[1]]$file.name) # delete file that was downloaded
        file.remove(paste(this_model.info[[1]]$file.name,".grb2",sep = ""))
      } # if (file.exists(converted_file) & length(thisGribInfo$inventory)>5) { # does converted file exist and it has more than 5 variables (should have lots)?
      
      } else { # if (is.null(list.available.models$file.name) == FALSE) { # only run computations if there is model data for this day
        #print("converted file does not exist")
        print(paste("there is data for this date, but not this model run",this_model.run))
      } # if (this_model.run %in% available_times_of_day_trunc) { # check if there is a model run for this model run (time of day)
      } else {
        print(paste("there is no data for this date",this_model.date))
      } # if (is.null(list.available.models$file.name) == FALSE) { # only run computations if there is model data for this day
  } # if (file.exists(this_file)) { # only run code if file doesn't already exist
} # end of extract_NAM_data.parallel.fn function