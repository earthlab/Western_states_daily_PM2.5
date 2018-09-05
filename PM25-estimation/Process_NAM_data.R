# Process NARR data

#### Call Packages (Library) ####
library(rNOMADS)

#### define constants ####
start_study_year <- 2008
stop_study_year <- 2018

#### Cycle through all .grb files for processing 
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
