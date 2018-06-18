# Process NARR data
rm(list = ls())
options(warn=2) # throw an error when there's a warning and stop the code from running further
#### Call Packages (Library) ####
library(rNOMADS)

#### define constants ####
#start_study_year <- 2008
#stop_study_year <- 2008 #2014
#NARR.directory <- file.path("home","rstudio","NARR")
#NARR.directory <- file.path("home","NARR")

#### Cycle through all .grb files for processing 
#for (this_year in start_study_year:stop_study_year) { # cycle through each year of NARR data
  this_year <- 2008
  print(paste("now processing data for ",this_year,sep = ""))

  # what are the file names for NARR data for this year?
  all_files_this_year <- list.files(path = "/home/rstudio/NARR/2008/.", pattern = NULL, all.files = FALSE, 
                              full.names = FALSE, recursive = FALSE,
                              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  print(all_files_this_year) # show files names
  print(paste("There are ",length(all_files_this_year)," files for year ",this_year,sep = ""))
  
  for (this_file_i in 1:length(all_files_this_year)) { # cycle through files within this year
    this_file_name <- all_files_this_year[this_file_i]
    print(paste("Processing file ",this_file_i," of ",length(all_files_this_year)," for ",this_year,": ",this_file_name,sep = ""))
    
    #model.parameters <- ParseModelPage()
    
    
    # open file
    print("write code to open file")
    File_Data <- ReadGrib(file.names = file.path("/home/rstudio/NARR/2008/.",this_file_name), levels = 1, variables = "RH",
             forecasts = NULL, domain = NULL, domain.type = "latlon",
             file.type = "grib2", missing.data = NULL) # see https://cran.r-project.org/web/packages/rNOMADS/rNOMADS.pdf
    
    # pull out the relevant bits of information
    print("write code to pull out the relevant bits of information")
    
    # clear variables before moving onto next file
    rm(this_file_name)  
  } # for (this_file_i in 1:length(all_files_this_year)) { # cycle through files within this year
  # clear variables before moving onto next year
  rm(all_files_this_year,this_file_i)
#} # for (this_year in start_study_year:stop_study_year) { # cycle through each year of NARR data
rm(this_year)

#### Save output to csv file ####

#### End of file cleanup
rm(start_study_year,stop_study_year)
rm(uppermost.directory,output.directory)
rm(working.directory,ProcessedData.directory,UintahData.directory,USMaps.directory,PCAPSData.directory)
rm(AQSData.directory,FMLE.directory,FireCache.directory,CARB.directory,UTDEQ.directory,NVDEQ.directory)
rm(writingcode.directory,computer_system)
