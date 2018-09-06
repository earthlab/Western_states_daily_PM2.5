# Process NAM data

print("run Define_directories.R before this script") 

#### Call Packages (Library) ####
library(rNOMADS)

#### define constants ####
#start_study_year <- 2018#2008
#stop_study_year <- 2018

study_start_date <- as.Date("20170101",format="%Y%m%d") # first date in study period
study_stop_date  <- as.Date("20180830",format="%Y%m%d") # last date in study period

forecast_times <- 00 # reanalysis - anything else would be a forecast

# Select which model to use
Model_in_use_abbrev <-  "namanl" # NAM Analysis

#### Load Date/Locations of PM2.5 Obs ####
this_source_file <- paste('Locations_Dates_of_PM25_Obs_DeDuplcate.csv',sep="")
print(this_source_file)

PM25DateLoc<-read.csv(file.path(this_source_file),header=TRUE) # load the AQS file


#### Cycle through all .grb files for processing 
theDate <- study_start_date # set date to beginning of study period before starting while loop
while (theDate <= study_stop_date) { #Get data for "theDate" in loop
  print(theDate) # print current date in iteration # COMMENT

  #see rNOMADS.pdf page 5-6 example
  model.date <- format(theDate, format = "%Y%m%d") # get date in format YYYYmmdd - needed for rNOMADS functions
  print(model.date) # COMMENT
  #model.date <- Date_interest #"20170101" #theDate #20140101
  preds <- forecast_times #00
  
  list.available.models <- CheckNOMADSArchive(Model_in_use_abbrev, model.date) # list all model files available for this model and date
  print(list.available.models) # COMMENT
  
  available_times_of_day <- unique(list.available.models$model.run) # what times are available?
  print(available_times_of_day)
  
  # is this a grib1 (.grb) or grib2 (.grb2) type of file?
  first_file_name <- as.character(list.available.models$file.name[[1]]) # grab first file name in list
  last_character <- substr(first_file_name,nchar(first_file_name),nchar(first_file_name)) # find the last character in the file name - determines which type of file it is
  if (last_character == "b") { # grib1 files
    print("These are grib1 files")
    this_file_type <- "grib1"
  } else if (last_character == "2") { # grib2 files
    print("These are grib2 files")
    this_file_type <- "grib2"
  } else {error("Unknown file type")} # check code
  
  #Temperature at 2 m above ground, analysis using GRIB
    
  # model.run = time of day
  for (model.run in available_times_of_day) {
    print(model.run)

    
    } # for (model.run in available_times_of_day) {
#  model.run <- Time_of_day 
  
  model.info <- ArchiveGribGrab(abbrev, model.date,
                                model.run, preds, file.type = "grib2")
  #model.info <- ArchiveGribGrab(abbrev, model.date,
  #                              model.run, preds, file.type = "grib1")
  
  thisGribInfo <- GribInfo(model.info[[1]]$file.name,file.type = "grib2")
  #thisGribInfo <- GribInfo(model.info[[1]]$file.name,file.type = "grib1")
  print(thisGribInfo[["inventory"]])
  thisGribInfo[["grid"]]
  
  model.data <- ReadGrib(model.info[[1]]$file.name, c("2 m above ground"), c("TMP"))
  #model.data <- ReadGrib(model.info[[1]]$file.name, c("sfc"), c("TMP"))
  #Get surface temperature in Chapel Hill, NC
  #lat <- 35.907605
  #lon <- -79.052147
  profile <- BuildProfile(model.data, Lon_interest_point, Lat_interest_point, TRUE)
  print(paste("The temperature in ",Location_Name," was ",
              sprintf("%.0f", profile[[1]]$profile.data[1,1,1] - 272.15), " degrees Celsius."))
  rm(abbrev, model.date, model.run, preds, list.available.models, model.info)
  rm(thisGribInfo, model.data, profile)

  theDate <- theDate +1 # iterate to the next day
}

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
