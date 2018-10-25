# NAM_processing_functions.R functions used for processing NAM data

##### Functions first used in Process_NAM_data_step1.R ####
# put in the day following each date in the file at each location so that all of the data will be gathered when using UTC
add_next_day_date_loc.fn <- function(PM25DateLoc_temp) { # put in the day following each date in the file at each location so that all of the data will be gathered when using UTC
  #PM25DateLoc_temp <- PM25DateLoc_orig # example input
  PM25DateLoc_NextDay <- PM25DateLoc_temp # copy input data to another dataframe
  PM25DateLoc_NextDay$Date <- PM25DateLoc_temp$Date+1 # shift all of the dates in new dataframe by one day
  PM25DateLoc_step1 <- rbind(PM25DateLoc_temp,PM25DateLoc_NextDay) # Combine the two dataframes together
  #PM25DateLoc <- PM25DateLoc_step1[!duplicated(PM25DateLoc_step1[,1:4]),] # get rid of any duplicate rows
  PM25DateLoc <- PM25DateLoc_step1[!duplicated(PM25DateLoc_step1[,1:dim(PM25DateLoc_step1)[2]]),] # get rid of any duplicate rows
  return(PM25DateLoc)
} # end of add_next_day_date_loc.fn function

#### Functions first used in Process_NAM_data_step2.R ####
# download grib1 to grib2 converters and make them usable in the docker
grb1to2_conversion_prep.fn <- function() {
  # this function needs to be done prior to trying to convert grib1 files to grib2 files
    # many thanks to Max Joseph for figuring these commands out
  # get the perl scripts
  system("wget ftp://ftp.cpc.ncep.noaa.gov/wd51we/grb1to2/grb1to2.pl")
  system("wget ftp://ftp.cpc.ncep.noaa.gov/wd51we/grb1to2/grib1to2_metadata.pl")
  system("wget ftp://ftp.cpc.ncep.noaa.gov/wd51we/grb1to2/global_template.g2")
  # create symbolic links to some executables
  system("ln -s /usr/local/bin/wgrib wgrib")
  system("ln -s /usr/local/bin/wgrib2 wgrib2")
  system("ln -s /usr/local/grib2/grib2/aux_progs/gmerge gmerge")
  system("ln -s /usr/local/grib2/grib2/aux_progs/smallest_grib2 smallest_grib2")
  # modify the perl script so that it is executable
  system("chmod +x grb1to2.pl")
} # end of grb1to2_conversion_prep.fn function


# is this a grib1 (.grb) or grib2 (.grb2) type of file?
which_type_of_grib_file.fn <- function(list.available.models) { # is this a grib1 (.grb) or grib2 (.grb2) type of file?
  first_file_name <- as.character(list.available.models$file.name[[1]]) # grab first file name in list
  last_character <- substr(first_file_name,nchar(first_file_name),nchar(first_file_name)) # find the last character in the file name - determines which type of file it is
  if (last_character == "b") { # grib1 files
    #print("These are grib1 files")
    this_file_type <- "grib1"
  } else if (last_character == "2") { # grib2 files
    #print("These are grib2 files")
    this_file_type <- "grib2"
  } else {error("Unknown file type")} # check code
  rm(first_file_name,last_character) # clear variables
  return(this_file_type) # function output
} # end of which_type_of_grib_file.fn function

# convert from grib1 to grib2 files
convert_grib1to2.fn <- function(this_model.info, this_file_type) {
  
  if (this_file_type == "grib1") { # convert grib1 files to grib2 and then run GribInfo
    
    # run the script to convert to grib2
    # ./grb1to2.pl namanl_218_20080101_0000_000.grb
    system(paste("./grb1to2.pl",this_model.info[[1]]$file.name,sep = " "))
    grib2_file_name <- paste(this_model.info[[1]]$file.name,".grb2",sep = "")
    
    thisGribInfo <- GribInfo(grib.file = grib2_file_name, file.type = "grib2")
    
    
  } else if (this_file_type == "grib2") { # run GribInfo
    
    thisGribInfo <- GribInfo(grib.file = this_model.info[[1]]$file.name, file.type = this_file_type)
    
  } else {
    error("Invalid this_file_type. It should be either grib1 or grib2")
  } # end if
  return(thisGribInfo)
} # end function


##### probably don't need this
# extract_NAM_data.fn <- function(date_i, date_vec,ProcessedData.directory, PM25DateLoc, output_file_name_sub, MeteoVarsMultiType, 
#                                 study_start_date, study_stop_date, forecast_times = 00, 
#                                 Model_in_use_abbrev =  "namanl") {
#   # Input variables
#   # ProcessedData.directory: location of source file that has the dates and locations for which you want the NAM data
#   # this_location_date_file: file listing the dates (local) and locations where you want the NAM data for.
#   # header should include: "Longitude", "Latitude", "Date"
#   # MeteoVarsMultiType: should be loaded file: MeteoVariablesNAM.csv  
#   # variables of interest. Use these commands in terminal to see what the variable names, levels, and units are:
#   # wgrib 20080101_0000_000.grb -V > NAM_grib1.txt
#   # wgrib2 20180202_0000_000.grb2 -V > NAM_grib2.txt
#   # header: VariableNumber	VariableName	VariableCode	AtmosLevelName	AtmosLevelCode	Units	file_type	time frame	24-hr summary
#   # study_start_date: 1st date of interest, e.g., as.Date("20080101",format="%Y%m%d")
#   # study_stop_date: last date of interest, e.g., as.Date("20180830",format="%Y%m%d")
#   # forecast_times <- 00 # reanalysis - anything else would be a forecast
#   # Model_in_use_abbrev <-  "namanl" # NAM Analysis
#   # file for _wNextDay is also called: this_location_date_file_wNextDay: same as this_location_date_file, but has the
#   # next day for every date/location in this_location_date_file - this is to be 
#   # able to handle local vs UTC time
#   
#   #### Create data sets for each run time (UTC) to put weather data into ####
#   PM25DateLoc_0000 <- PM25DateLoc
#   PM25DateLoc_0600 <- PM25DateLoc
#   PM25DateLoc_1200 <- PM25DateLoc
#   PM25DateLoc_1800 <- PM25DateLoc
#   
#   #  #### Cycle through all .grb files for processing ####
#   #  date_vec <- seq(as.Date(study_start_date), as.Date(study_stop_date), by="days")
#   #  print(date_vec)
#   #theDate <- study_start_date # set date to beginning of study period before starting while loop
#   #while (theDate <= study_stop_date) { #Get data for "theDate" in loop
#   #  for (date_i in 1:length(date_vec)) {
#   print(date_i)
#   theDate <- as.Date(date_vec[date_i])
#   print(theDate) # print current date in iteration # COMMENT
#   # find the locations that need data for this date
#   which_theDate <- which(PM25DateLoc$Date == theDate)
#   #length(which_theDate)
#   print(paste(length(which_theDate),"locations need weather data on",theDate,sep = " "))
#   
#   #see rNOMADS.pdf page 5-6 example
#   this_model.date <- format(theDate, format = "%Y%m%d") # get date in format YYYYmmdd - needed for rNOMADS functions
#   #print(this_model.date) # COMMENT
#   
#   list.available.models <- CheckNOMADSArchive(Model_in_use_abbrev, this_model.date) # list all model files available for this model and date
#   #print(list.available.models) # COMMENT
#   
#   available_times_of_day <- unique(list.available.models$model.run) # what times are available?
#   #print(available_times_of_day)
#   
#   #### is this a grib1 (.grb) or grib2 (.grb2) type of file? ####
#   this_file_type <- which_type_of_grib_file.fn(list.available.models)
#   
#   # grab the list of relevant meteo variables for this file type from MeteoVars
#   #which_meteo <- which(MeteoVarsMultiType$file_type == this_file_type)
#   which_meteo <- which(MeteoVarsMultiType$file_type == "grib2")
#   MeteoVars <- MeteoVarsMultiType[which_meteo,]
#   
#   #### Cycle through the model runs on this Date (theDate) ####    
#   # model.run = time of day
#   for (model.run_long in available_times_of_day) {
#     # model.run_long <- available_times_of_day[1]
#     #print(model.run_long)
#     this_model.run <- substr(model.run_long,1,2)
#     #print(this_model.run)
#     
#     # Download archived model data from the NOMADS server - page 4 of rNOMADS.pdf
#     this_model.info <- ArchiveGribGrab(abbrev = Model_in_use_abbrev, model.date = this_model.date, 
#                                        model.run = this_model.run, preds = forecast_times,
#                                        local.dir = NAM.directory, file.names = NULL, tidy = FALSE,
#                                        verbose = TRUE, download.method = NULL, file.type = this_file_type)
#     
#     # Convert grib1 to grib2 if necessary and then run GribInfo
#     thisGribInfo <- convert_grib1to2.fn(this_model.info,this_file_type)
#     #thisGribInfo <- GribInfo(grib.file = this_model.info[[1]]$file.name, file.type = this_file_type)
#     #print(thisGribInfo)
#     
#     #print(thisGribInfo[["inventory"]])
#     #thisGribInfo[["grid"]]
#     
#     #### Cycle through meteo variables and pull out the data ####
#     # grab all meteo variables for this file type
#     
#     for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
#       # meteo_var_counter <- 2 # surface Temp
#       
#       # get variable full name
#       thisMeteo_var_Name <- MeteoVars[meteo_var_counter,c("VariableName")]
#       #print(thisMeteo_var_Name)
#       # get variable coded name
#       thisMeteo_variable <- MeteoVars[meteo_var_counter,c("VariableCode")]
#       #print(thisMeteo_variable)
#       # get variable level name
#       thisMeteo_level <- MeteoVars[meteo_var_counter,c("AtmosLevelCode")]
#       #print(thisMeteo_level)
#       # get variable units
#       thisMeteo_units <- MeteoVars[meteo_var_counter,c("Units")]
#       #print(thisMeteo_units)
#       
#       # Load the data for this variable/level
#       this_model.data <- ReadGrib(file.names = paste(this_model.info[[1]]$file.name,".grb2",sep = ""), levels = thisMeteo_level, variables = thisMeteo_variable,
#                                   forecasts = NULL, domain = NULL, domain.type = "latlon",
#                                   file.type = "grib2", missing.data = NULL)
#       
#       for (this_PM25_row in which_theDate) { # cycle through the rows of dates locations that need data for this date
#         # this_PM25_row <- which_theDate[1]
#         #print(PM25DateLoc[this_PM25_row,])
#         this_lon <- PM25DateLoc[this_PM25_row,c("Lon")]#PM25DateLoc[this_PM25_row,c("Longitude")]
#         #print(this_lon)
#         this_lat <- PM25DateLoc[this_PM25_row,c("Lat")] #PM25DateLoc[this_PM25_row,c("Latitude")]
#         #print(this_lat)
#         #this_lat <- 40.037416
#         #this_lon <- -105.228667
#         
#         this_profile <- BuildProfile(model.data = this_model.data, lon = this_lon, lat = this_lat, spatial.average = TRUE, points = 4)
#         
#         #print(paste("The temperature at ",this_lat," ",this_lon," was ",
#         #            sprintf("%.0f", this_profile[[1]]$profile.data[1,1,1] - 273.15), " degrees Celsius."))
#         
#         this_meteo_value <- this_profile[[1]]$profile.data[1,1,1]
#         #print(paste(thisMeteo_var_Name,"at",thisMeteo_level,"is",this_meteo_value,thisMeteo_units,sep = " "))
#         
#         #print(paste(thisMeteo_var_Name,"at",thisMeteo_level,"is",this_meteo_value,thisMeteo_units,"at",
#         #            this_lon,this_lat,"on",theDate,"at",model.run_long,sep = " "))
#         
#         #if (thisMeteo_variable == "TMP") { # show temperature in Celsius (display only - still input in K)
#         #  this_TempC <- this_profile[[1]]$profile.data[1,1,1] - 273.15
#         #  print(paste(this_TempC," C"))
#         #  rm(this_TempC)
#         #} #  if (thisMeteo_variable == "TMP") { # show temperature in Celsius (display only - still input in K)
#         
#         if (model.run_long == "0000") { # input meteo value in appropriate matrix
#           PM25DateLoc_0000[this_PM25_row,c(paste(as.character(thisMeteo_variable), as.character(thisMeteo_level)))] <- this_meteo_value
#         } else if (model.run_long == "0600") {
#           PM25DateLoc_0600[this_PM25_row,c(paste(as.character(thisMeteo_variable), as.character(thisMeteo_level)))] <- this_meteo_value
#         } else if (model.run_long == "1200") {
#           PM25DateLoc_1200[this_PM25_row,c(paste(as.character(thisMeteo_variable), as.character(thisMeteo_level)))] <- this_meteo_value
#         } else if (model.run_long == "1800") {
#           PM25DateLoc_1800[this_PM25_row,c(paste(as.character(thisMeteo_variable), as.character(thisMeteo_level)))] <- this_meteo_value
#         } else { 
#           error("invalid model.run_long - check code and data")
#         } # if (model.run_long == "0000") { # input meteo value in appropriate matrix
#       } # for (this_PM25_row in which_theDate) { # cycle through the rows of dates locations that need data for this date
#       rm(this_PM25_row,this_model.data)
#       rm(thisMeteo_var_Name,thisMeteo_variable,thisMeteo_level,thisMeteo_units)
#     } # for (meteo_var_counter in 1:dim(MeteoVars)[1]) { # cycle through variables(levels) of interest
#     rm(meteo_var_counter)
#     rm(model.run_long, this_model.run) # clear variables from this iteration
#     file.remove(this_model.info[[1]]$file.name) # delete file that was downloaded
#     file.remove(paste(this_model.info[[1]]$file.name,".grb2",sep = ""))
#     
#   } # for (model.run in available_times_of_day) {
#   rm(which_theDate)
#   theDate <- theDate +1 # iterate to the next day
#   #} # while (theDate <= study_stop_date) { #Get data for "theDate" in loop
#   #rm(theDate)
#   
#   #### Clear variables ####
#   #rm(study_start_date, study_stop_date, forecast_times, Model_in_use_abbrev, MeteoVars)
#   #rm(PM25DateLoc)
#   
#   #### Output 4 data frames to csv files ####
#   #error("still need to output csv files")
#   write.csv(PM25DateLoc_0000,file = file.path(ProcessedData.directory,output_file_name_sub,paste(this_location_date_file,"_0000UTC.csv",sep = "")),row.names = FALSE)
#   write.csv(PM25DateLoc_0600,file = file.path(ProcessedData.directory,output_file_name_sub,paste(this_location_date_file,"_0600UTC.csv",sep = "")),row.names = FALSE)
#   write.csv(PM25DateLoc_1200,file = file.path(ProcessedData.directory,output_file_name_sub,paste(this_location_date_file,"_1200UTC.csv",sep = "")),row.names = FALSE)
#   write.csv(PM25DateLoc_1800,file = file.path(ProcessedData.directory,output_file_name_sub,paste(this_location_date_file,"_1800UTC.csv",sep = "")),row.names = FALSE)
#   
# } # function
