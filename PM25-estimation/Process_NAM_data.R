# Process NAM data

print("run Define_directories.R before this script") 

#### Call Packages (Library) ####
library(rNOMADS)

#### Call Load Functions that I created ####
source(file.path(writingcode.directory,"add_next_day_date_loc_function.R"))

#### define constants ####
#study_start_date <- as.Date("20170101",format="%Y%m%d") # first date in study period
study_start_date <- as.Date("20080101",format="%Y%m%d") # first date in study period

#study_stop_date  <- as.Date("20180830",format="%Y%m%d") # last date in study period
study_stop_date  <- as.Date("20080103",format="%Y%m%d") # last date in study period

forecast_times <- 00 # reanalysis - anything else would be a forecast

# Select which model to use
Model_in_use_abbrev <-  "namanl" # NAM Analysis

#### Load list of meteorology variables of interest ####
this_source_file <- paste("MeteoVariablesNAM.csv")
MeteoVars <- read.csv(file.path(code.directory,this_source_file))
rm(this_source_file)

#### Load Date/Locations of PM2.5 Obs ####
this_source_file <- paste('Locations_Dates_of_PM25_Obs_DeDuplicate.csv',sep="")
print(this_source_file)

PM25DateLoc_temp <-read.csv(file.path(ProcessedData.directory,this_source_file),header=TRUE) # load the AQS file
rm(this_source_file)
PM25DateLoc_temp$Date <- as.Date(PM25DateLoc_temp$Date) # recognize date column as dates

PM25DateLoc <- add_next_day_date_loc.fn(PM25DateLoc_temp)
rm(PM25DateLoc_temp)

#### Create data sets for each run time to put weather data into ####
PM25DateLoc_0000 <- PM25DateLoc
PM25DateLoc_0600 <- PM25DateLoc
PM25DateLoc_1200 <- PM25DateLoc
PM25DateLoc_1800 <- PM25DateLoc

#### Cycle through all .grb files for processing 
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
  first_file_name <- as.character(list.available.models$file.name[[1]]) # grab first file name in list
  last_character <- substr(first_file_name,nchar(first_file_name),nchar(first_file_name)) # find the last character in the file name - determines which type of file it is
  if (last_character == "b") { # grib1 files
    print("These are grib1 files")
    this_file_type <- "grib1"
  } else if (last_character == "2") { # grib2 files
    print("These are grib2 files")
    this_file_type <- "grib2"
  } else {error("Unknown file type")} # check code
  
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
    for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
      # get variable coded name
      thisMeteo_variable <- MeteoVars[meteo_var_counter,c("VariableCode")]
      print(thisMeteo_variable)
      # get variable level name
      thisMeteo_level <- MeteoVars[meteo_var_counter,c("AtmosLevelCode")]
      print(thisMeteo_level)
      
      # Load the data for this variable/level
      this_model.data <- ReadGrib(file.names = this_model.info[[1]]$file.name, levels = thisMeteo_level, variables = thisMeteo_variable,
               forecasts = NULL, domain = NULL, domain.type = "latlon",
               file.type = "grib2", missing.data = NULL)
      
      for (this_PM25_row in which_theDate) {
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
        
        this_TempC <- this_profile[[1]]$profile.data[1,1,1] - 273.15
        
        
        if (model.run_long == "0000") { # input meteo value in appropriate matrix
          PM25DateLoc_0000[this_PM25_row,c(paste(as.character(thisMeteo_variable), as.character(thisMeteo_level)))] <- this_meteo_value
        } else if (model.run_long == "0600") {
          PM25DateLoc_0600[this_PM25_row,(cat(thisMeteo_variable,thisMeteo_level))] <- this_meteo_value
        } else if (model.run_long == "1200") {
          PM25DateLoc_1200[this_PM25_row,(cat(thisMeteo_variable,thisMeteo_level))] <- this_meteo_value
        } else if (model.run_long == "1800") {
          PM25DateLoc_1800[this_PM25_row,(cat(thisMeteo_variable,thisMeteo_level))] <- this_meteo_value
        } else { error("invalid model.run_long - check code and data")}
        
      }
      
      
      
    } #     for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
    
    
    #Temperature at 2 m above ground, analysis using GRIB
    #this_model.data <- ReadGrib(file.names = this_model.info[[1]]$file.name, levels, variables,
    #         forecasts = NULL, domain = NULL, domain.type = "latlon",
    #         file.type = "grib2", missing.data = NULL)
    
    #model.data <- ReadGrib(this_model.info[[1]]$file.name, c("2 m above ground"), c("TMP"))
    #model.data <- ReadGrib(model.info[[1]]$file.name, c("sfc"), c("TMP"))
    #Get surface temperature in Chapel Hill, NC
    #lat <- 35.907605
    #lon <- -79.052147
    #profile <- BuildProfile(model.data, Lon_interest_point, Lat_interest_point, TRUE)
    print(paste("The temperature in ",Location_Name," was ",
                sprintf("%.0f", profile[[1]]$profile.data[1,1,1] - 272.15), " degrees Celsius."))
    rm(abbrev, model.date, model.run, preds, list.available.models, model.info)
    rm(thisGribInfo, model.data, profile)
    
    
    error("write more code") # COMMENT
    rm(model.run_long, model.run) # clear variables from this iteration
    file.remove(model.info[[1]]$file.name) # delete file that was downloaded
    
    } # for (model.run in available_times_of_day) {
  rm(model.run_long)
  theDate <- theDate +1 # iterate to the next day
} # while (theDate <= study_stop_date) { #Get data for "theDate" in loop

for (this_year in start_study_year:stop_study_year) { # cycle through each year of NARR data
  print(paste("now processing data for ",this_year,sep = ""))

  # what are the file names for NARR data for this year?
  all_files_this_year <- list.files(path = file.path(NARR.directory,this_year,"."), pattern = NULL, all.files = FALSE, 
                              full.names = FALSE, recursive = FALSE,
                              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  print(all_files_this_year) # show files names
  print(paste("There are ",length(all_files_this_year)," files for year ",this_year,sep = ""))
  
  for (this_file_i in 1:length(all_files_this_year)) { # cycle through files within this year
    this_file_name <- all_files_this_year[this_file_i]
    print(paste("Processing file ",this_file_i," of ",length(all_files_this_year)," for ",this_year,": ",this_file_name,sep = ""))
    
    # find out what model, date, levels and variables are in grib file, see page 20 of https://cran.r-project.org/web/packages/rNOMADS/rNOMADS.pdf
    thisGribInfo <- GribInfo(file.path(NARR.directory,this_year,this_file_name),file.type = "grib1")
    
    # <- file.path(NARR.directory,this_year,this_file_name)
    #this_level <- "2 m above ground"
    pressure <- c(1, 2, 3, 5, 7,
                  10, 20, 30, 50, 70,
                  seq(100, 1000, by = 25))
    this_level <- paste(pressure, " mb", sep = "")
    this_variable <- "TMP"
    #forecasts = NULL, domain = NULL, domain.type = "latlon", 
    #         file.type = "grib1"
             #, missing.data = NULL
    #data_list_1_file <- ReadGrib(file.names = file.path(NARR.directory,this_year,this_file_name), levels = 1, variables = "DPT",file.type = "grib1") # see https://cran.r-project.org/web/packages/rNOMADS/rNOMADS.pdf
    data_list_1_file <- ReadGrib(file.names = file.path(NARR.directory,this_year,this_file_name), levels = this_level, variables = this_variable,file.type = "grib1") # see https://cran.r-project.org/web/packages/rNOMADS/rNOMADS.pdf
    
    print(data_list_1_file)
    
    MetaData <- data_list_1_file[[1]]
    DataValue <- data_list_1_file[[2]]
    DataVariables <- data_list_1_file[[3]]
    DataLevels <- data_list_1_file[[4]]
    DataGrbType <- data_list_1_file[[5]]
    
    variables_to_process <- c("PRMSL","TMP","DPT","RH","UGRD","VGRD","HPBL","PRES","PRATE")
    print(variables_to_process)
    
    
    model.parameters <- ParseModelPage(file.path(NARR.directory,this_year,this_file_name))
    
    all_variables <- model.parameters["levels"]
    
    # open file
    print("write code to open file")
    #ReadGrib(file.names, levels, variables,
    #         forecasts = NULL, domain = NULL, domain.type = "latlon",
    #         file.type = "grib2", missing.data = NULL) # see https://cran.r-project.org/web/packages/rNOMADS/rNOMADS.pdf
    
    load_data <- ReadGrib(file.names = file.path(NARR.directory,this_year,this_file_name), levels = "MSL", variables = "PRMSL") # see https://cran.r-project.org/web/packages/rNOMADS/rNOMADS.pdf
    
    #File_Data <- ReadGrib(file.names = file.path(NARR.directory,this_year,this_file_name), levels = 1, variables = RH,
    #         forecasts = NULL, domain = NULL, domain.type = "latlon",
    #         file.type = "grib2", missing.data = NULL) # see https://cran.r-project.org/web/packages/rNOMADS/rNOMADS.pdf
    
    # if getting this error: Error in plot.new() : figure margins too large
    # do this:
    # par("mar")
    # par(mar = c(1,1,1,1))
    # see also https://stackoverflow.com/questions/23050928/error-in-plot-new-figure-margins-too-large-scatter-plot
    
    # pull out the relevant bits of information
    print("write code to pull out the relevant bits of information")
    
    # clear variables before moving onto next file
    rm(this_file_name)  
  } # for (this_file_i in 1:length(all_files_this_year)) { # cycle through files within this year
  # clear variables before moving onto next year
  rm(all_files_this_year,this_file_i)
} # for (this_year in start_study_year:stop_study_year) { # cycle through each year of NARR data
rm(this_year)

#### Save output to csv file ####

#### End of file cleanup
rm(start_study_year,stop_study_year)
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
rm(writingcode.directory,computer_system)
