loop_NAM_run_times.parallel.fn <- function(day_counter) {
  theDate <- as.Date(Date_vector[day_counter]) # the date of the current loop iteration
  print(paste("loop_NAM_run_times.parallel.fn starting for",theDate,sep = " "))
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
    extract_NAM_data.parallel.fn(ProcessedData.directory = ProcessedData.directory, #this_location_date_file = this_location_date_file,
                                MeteoVarsMultiType = MeteoVarsMultiType, theDate = theDate, forecast_times = forecast_times, this_model.run = this_model.run,
                                PM25DateLoc_time = Merged_Dates_Locations, Model_in_use_abbrev =  Model_in_use_abbrev, sub_folder = output_sub_folder, day_counter = day_counter)
  } # for (run_counter in 1:4) { # loop through the 4 runs (time periods) per day
} # end function