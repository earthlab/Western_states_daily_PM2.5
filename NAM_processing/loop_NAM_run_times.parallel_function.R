loop_NAM_run_times.parallel.fn <- function(day_counter,Date_vector, 
                                         ProcessedData.directory, this_location_date_file,
                                         MeteoVarsMultiType, forecast_times = 00,
                                         PM25DateLoc_time, Model_in_use_abbrev =  "namanl",
                                         sub_folder) {
  
  #set.seed(day_counter*day_counter*day_counter*100) # seed for random number generator
  set.seed(42) #COMMENT?
  theDate <- as.Date(Date_vector[day_counter]) # the date of the current loop iteration
  print(paste("loop_NAM_run_times.parallel.fn starting for",theDate,sep = " "))
  print("only doing run_counter = 4 (18 UTC)")
  #for (run_counter in 4) { # loop through the 4 runs (time periods) per day
  for (run_counter in 1:4) { # loop through the 4 runs (time periods) per day
    print("run_counter")
    print(run_counter)
    if (run_counter == 1) { # define the 4 time periods (UTC time stamp)
      this_model.run <- "00" # 00 UTC
    } else if (run_counter == 2) {
      this_model.run <- "06" # 06 UTC
    } else if (run_counter == 3) {
      this_model.run <- "12" # 12 UTC
    } else if (run_counter == 4) {
      this_model.run <- "18" # 18 UTC
    } # if (run_counter == 1) { # define the 4 time periods (UTC time stamp)
    print(this_model.run)
    # run function to extract NAM data (one run of one day)
    extract_NAM_data.parallel.fn(ProcessedData.directory = ProcessedData.directory, this_location_date_file = this_location_date_file,
                                 MeteoVarsMultiType = MeteoVarsMultiType, theDate = theDate, forecast_times = forecast_times, this_model.run = this_model.run, 
                                 PM25DateLoc_time = PM25DateLoc, Model_in_use_abbrev =  Model_in_use_abbrev, sub_folder)
    #return(paste(theDate,this_model.run)) # function output
  } # for (run_counter in 1:4) { # loop through the 4 runs (time periods) per day
} # end function