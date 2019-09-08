#PM25_station_deduplicate_aves_parallel.fn() 
# this_location_i <- X
# this_location_i <- 368
# for a given location, de-duplicate by taking average of multiple obs at a location
PM25_station_deduplicate_aves_parallel.fn <- function(this_location_i) { # start function definition - this function handles the data for 1 location
  verbose_flag <- 1 # indicate whether to output text information about the station to the screen (0 means No)
  print(paste("this_location_i",this_location_i))
  this_lat <- Locations_input_mat3[this_location_i,"Lat"] # find the latitude for this_location_i
  this_lon <- Locations_input_mat3[this_location_i,"Lon"] # find the longitude for this_location_i
  which_this_location <- which(input_mat3$Lat == this_lat & input_mat3$Lon == this_lon) # find the rows of data with this location
  if (length(which_this_location)==0) { # rounding to given_digits didn't find a match, this can happen if there are fewer than given_digits decimal places  # outer nest
    print("round Lon to one fewer decimal place to match on location")
    which_this_location <- which(input_mat3$Lat == this_lat & round(input_mat3$Lon,(given_digits-1)) == round(this_lon,(given_digits-1))) # find the rows of data with this location
    if (length(which_this_location)==0) { # inner nest 1
      print("round Lat to one fewer decimal place to match on location")
      which_this_location <- which(round(input_mat3$Lat,(given_digits-1)) == round(this_lat,(given_digits-1)) & input_mat3$Lon == this_lon) # find the rows of data with this location
      if (length(which_this_location)==0) { # inner nest 2
        print("round Lat and Lon to one fewer decimal place to match on location")
        which_this_location <- which(round(input_mat3$Lat,(given_digits-1)) == round(this_lat,(given_digits-1)) & round(input_mat3$Lon,(given_digits-1)) == round(this_lon,(given_digits-1))) # find the rows of data with this location
        if (length(which_this_location)==0 & this_location_i != 6970 | this_location_i != 6971) { # inner nest 3 #if (length(which_this_location)==0) { # inner nest 3
          print("round Lat to one fewer decimal place and Lon to 2 fewer decimal places to match on location")
          which_this_location <- which(round(input_mat3$Lat,(given_digits-1)) == round(this_lat,(given_digits-1)) & round(input_mat3$Lon,(given_digits-2)) == round(this_lon,(given_digits-2))) # find the rows of data with this location
          if (length(which_this_location)==0) { # inner nest 4
            print("round Lat to 2 fewer decimal places and Lon to 2 fewer decimal places to match on location")
            which_this_location <- which(round(input_mat3$Lat,(given_digits-2)) == round(this_lat,(given_digits-2)) & round(input_mat3$Lon,(given_digits-2)) == round(this_lon,(given_digits-2))) # find the rows of data with this location
            if (length(which_this_location)==0) { # inner nest 5
              print("round Lat to 1 fewer decimal place and Lon to 3 fewer decimal places to match on location")
              which_this_location <- which(round(input_mat3$Lat,(given_digits-2)) == round(this_lat,(given_digits-2)) & round(input_mat3$Lon,(given_digits-3)) == round(this_lon,(given_digits-3))) # find the rows of data with this location
              if (length(which_this_location)==0) { # inner nest 6
                stop("Not finding lat/lon match in data. Look at data and maybe expand nested if statements in PM25_station_deduplicate_aves_parallel.fn")  
              } # innter nest 6
            } # innter nest 5
          } # innter nest 4
        } # inner nest 3
      } # inner nest 2
    } # inner nest 1
  } # outer nest
  this_location_data_step1 <- input_mat3[which_this_location, ] # isolate all of the data for this location into a data frame
  if (class(this_location_data_step1$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
  # check for and de-duplicate any rows that are complete repeats
  this_location_data_step2 <- this_location_data_step1[!duplicated(this_location_data_step1), ] # de-duplicate any rows that are complete repeats
  rm(this_location_data_step1, which_this_location) # clear variables
  Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = this_location_data_step2)
  if (length(Check_data)>0) {stop("***check_4_NAs.fn found questionable data. Investigate.***")}
  rm(Check_data)
  if (class(this_location_data_step2$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
  unique_days <- unique(this_location_data_step2$Date_Local) # create list of unique days in this data
  if (class(unique_days) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
  #unique_days_locations <- unique(this_location_data_step2[,c("Date_Local","Lat","Lon")]) # create list of unique day/location combinations

  input_mat4_aves <- data.frame(matrix(NA, nrow = length(unique_days), ncol = dim(input_mat3)[2])) # create data frame for input_mat_4_aves
  names(input_mat4_aves) <- colnames(input_mat3) # assign the header to input_mat_4_aves
  input_mat4_aves <- input_mat_change_data_classes.fn(input_mat4_aves) # set variable classes
  if (class(input_mat4_aves$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
  rstart_aves <- 1 # start row counter

  if (verbose_flag != 0) { # print text information about station to screen
    # describe this station and it's data
    print(paste("location_i ", this_location_i, ": (Lat ", unique(this_location_data_step2$Lat),", Lon ",
                unique(this_location_data_step2$Lon),") has ",
                dim(this_location_data_step2)[1]," rows of data among ",length(unique_days), 
                " unique days.",sep = ""))
    print("station names:")
    print(unique(this_location_data_step2$PM25_Station_Name))
  } # if (verbose_flag != 0) { # print text information about station to screen

  # determine whether there were multiple monitors ever operating at this site (or duplicate data)
  if (length(unique_days)==dim(this_location_data_step2)[1] &
     length(unique(this_location_data_step2$Data_Source_Name_Short))==1) { # determine whether there were multiple monitors ever operating at this site (or duplicate data)
    if (verbose_flag != 0) { # print text information about station to screen
      print("Each day of data for this station has only one monitor operating and there is no duplicate data.")
    } # if (verbose_flag != 0) { # print text information about station to screen

     # aves data
      #rstop_aves <- rstart_aves + dim(this_location_data_step2)[1]-1 # set end row counter
      #input_mat4_aves[rstart_aves:rstop_aves, ] <- this_location_data_step2 # input data directly into input_mat4_aves
      #rstart_aves <- rstop_aves+1 # update start row counter
      input_mat4_aves <- this_location_data_step2 # input data directly into input_mat4_aves
      Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = input_mat4_aves)
      if (length(Check_data)>0) {stop("***check_4_NAs.fn found questionable data. Investigate.***")}
      rm(Check_data)
      if (class(input_mat4_aves$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
      
  } else { # if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) there is duplicate data
    if (verbose_flag != 0) { # print text information about station to screen 
      print("there is duplicate data")
    } # if (verbose_flag != 0) { # print text information about station to screen
      # cycle through days
      for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
         this_day <- unique_days[this_day_i] # get the date
         which_this_day <- which(this_location_data_step2$Date_Local == this_day) # find the rows for this day
         this_day_all_data <- this_location_data_step2[which_this_day, ] # isolate data for this date and location
         
         if (verbose_flag != 0) { # print text information about station to screen
         print(paste("This location (Lat ", unique(this_location_data_step2$Lat),", Lon ",
                     unique(this_location_data_step2$Lon),") has ",
                     length(which_this_day)," rows of data on ",this_day,".",sep = ""))
         } # if (verbose_flag != 0) { # print text information about station to screen
          
         # call function of repeat entries of the same observations (usually event type is different)
          this_day_all_combined_true_dup  <- Combine_true_replicates_R.fn(this_day_all_data_in = this_day_all_data, this_day = this_day) # function to combine rows that are from the same source and have the same concentration (usually event type is the only/main difference)
          rm(this_day_all_data) # clear variable
          Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = this_day_all_combined_true_dup)
          if (length(Check_data)>0) {stop("***check_4_NAs.fn found questionable data. Investigate.***")}
          rm(Check_data)
          if (class(this_day_all_combined_true_dup$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
          
          # if setting indicate to prioritize 24 hr observations over hourly obs, do so here.
          if (de_duplication_method == "prioritize_24Hour_Obs") { # if setting indicate to prioritize 24 hr observations over hourly obs, do so here.
            #stop("write function to prioritize 24 hour obs")
            this_day_all_combined_true_dup_out  <- prioritize_daily_obs_over_hourly.fn(this_day_all_combined_true_dup_in = this_day_all_combined_true_dup)
            rm(this_day_all_combined_true_dup)
            this_day_all_combined_true_dup <- this_day_all_combined_true_dup_out
            rm(this_day_all_combined_true_dup_out)
            Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = this_day_all_combined_true_dup)
            if (length(Check_data)>0) {stop("***check_4_NAs.fn found questionable data. Investigate.***")}
            rm(Check_data)
            if (class(this_day_all_combined_true_dup$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
          } # if (de_duplication_method == "prioritize_24Hour_Obs") { # if setting indicate to prioritize 24 hr observations over hourly obs, do so here.
          
          if (verbose_flag != 0 & de_duplication_method == "prioritize_24Hour_Obs") { # print text information about station to screen
            print(paste("After prioritizing 24-hr data: This location (Lat ", unique(this_location_data_step2$Lat),", Lon ",
                        unique(this_location_data_step2$Lon),") has ",
                        dim(this_day_all_combined_true_dup)[1]," rows of data on ",this_day,".",sep = ""))
          } # if (verbose_flag != 0) { # print text information about station to screen
          
          # call function to fill in PM2.5 data
         # if (de_duplication_method == "averages") {
          output_list <- fill_input_mat_aves.fn(this_day_all_combined_true_dup = this_day_all_combined_true_dup, input_mat4_aves = input_mat4_aves, rstart_aves = rstart_aves, this_day = this_day) # call function to fill in PM2.5 data
          #} else if (de_duplication_method == "prioritize_24Hour_Obs") {
          #  stop("write function to prioritize 24 hour obs")
          #}
          rm(input_mat4_aves,rstart_aves) # clear old versions of variables, which will be replaced with the output from the function
      # get the variables out of the output_list from the function
      input_mat4_aves <- output_list[[1]] # extract input_mat4_aves from list output from function
      rstart_aves <- output_list[[2]] # extract rstart_aves from list output from function
      rm(output_list) # clear variable
      Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = input_mat4_aves[1:(rstart_aves-1), ])
      if (length(Check_data)>0) {stop("***Check_4_NAs.fn found questionable data. Investigate.***")}
      rm(Check_data)
      if (class(input_mat4_aves$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
     } # for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
  } # } else { # if (length(unique_days)==dim(this_location_data_step2)[1] & length(unique(this_location_data_step2$Data_Source_Name_Short))==1) there is duplicate data
  Check_data <- check_4_NAs.fn(no_NAs_allowed_cols = c("Lat","Lon","NewDatum","PM2.5_Obs","Date_Local","Year","Month","Day"), input_data = input_mat4_aves)
  if (length(Check_data)>0) {stop("***check_4_NAs.fn found questionable data. Investigate.***")}
  rm(Check_data)
  if (class(input_mat4_aves$Date_Local) != "Date") {stop("***class of Date_Local is not 'Date'. Investigate***")}
  return(input_mat4_aves)
} # end of loop_PM25_station_deduplicate.parallel.fn function
