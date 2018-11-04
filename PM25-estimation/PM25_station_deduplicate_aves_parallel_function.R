#PM25_station_deduplicate_aves_parallel.fn() 
# for a given station, de-duplicate by taking average of multiple obs at a location
PM25_station_deduplicate_aves_parallel.fn <- function(this_station_i) {  #, input_header, unique_EPA_Codes) {
  this_station <- unique_EPA_Codes[this_station_i,] # what is the code for this station?
  which_this_station <- which(known_EPA_Code_data$State_Code == this_station$State_Code &
                              known_EPA_Code_data$County_Code == this_station$County_Code &
                              known_EPA_Code_data$Site_Num == this_station$Site_Num) # find which rows in input_mat correspond to this station
  this_station_data <- known_EPA_Code_data[which_this_station,] # create data frame with this station's data
  rm(which_this_station) # clear variable
  unique_days <- unique(this_station_data$Date_Local) # create list of unique days in this data
  unique_days_locations <- unique(this_station_data[,c("Date_Local","Lat","Lon","Easting","Northing")])

    if (length(unique_days) != dim(unique_days_locations)[1]) { # check on data/code
      stop("location data not making sense")
      # data_sources <- unique(this_station_data$Data_Source_Name_Short)
      # print(paste("station_i = ",this_station_i,"; data sources:"))
      # print(data_sources)
      # if (data_sources[1] == "EPA_PM25") { # if station data is from EPA and other sources, use only the EPA version
      #   which_EPA <- which(this_station_data$Data_Source_Name_Short == "EPA_PM25")
      #   this_station_data_step <- this_station_data[which_EPA, ]
      #   rm(this_station_data)
      #   this_station_data <- this_station_data_step
      #   rm(this_station_data_step) 
      #   # how many unique days are in this data?
      #   unique_days <- unique(this_station_data$Date_Local)
      #   print("figure out why some of the IMPROVE sites have more days at EPA stations than EPA data - and write code to keep those")
     # } else {stop("check data and code")}
    } # if (length(unique_days) != dim(unique_days_locations)[1]) { # check on data/code

  input_mat5_aves <- data.frame(matrix(NA, nrow = length(unique_days), ncol = dim(known_EPA_Code_data)[2])) # create data frame for input_mat_5_aves
  names(input_mat5_aves) <- colnames(known_EPA_Code_data)#input_header # assign the header to input_mat_5_aves
  rstart_aves <- 1 # start counter

    # describe this station and it's data
    print(paste("station_i ", this_station_i, ": Station ", this_station$State_Code,"-",
                this_station$County_Code,"-",this_station$Site_Num," has ",
                dim(this_station_data)[1]," rows of data among ",length(unique_days), #length(which_this_station)," rows of data among ",length(unique_days),
                " unique days.",sep = ""))

  # determine whether there were multiple monitors ever operating at this site (or duplicate data)
  if (length(unique_days)==dim(this_station_data)[1] &
     length(unique(this_station_data$Data_Source_Name_Short))==1) { # determine whether there were multiple monitors ever operating at this site (or duplicate data)
     print("Each day of data for this station has only one monitor operating and there is no duplicate data.")

     # aves data
      rstop_aves <- rstart_aves + dim(this_station_data)[1]-1
      input_mat5_aves[rstart_aves:rstop_aves, ] <- this_station_data
      rstart_aves <- rstop_aves+1
  } else { # if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) there is duplicate data
       print("there is duplicate data")
      # cycle through days
      for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
         this_day <- unique_days[this_day_i] # get the date
         which_this_day <- which(this_station_data$Date_Local == this_day) # find the rows for this day
         this_day_all_data <- this_station_data[which_this_day, ]
         #print(paste("Station ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num," has ",
         #            length(which_this_day)," rows of data on ",this_day,".",sep = ""))
          
         # call function of repeat entries of the same observations (usually event type is different)
          # function to combine rows that are from the same source and have the same concentration (usually event type is the only/main difference)
          this_day_all_combined_true_dup  <- Combine_true_replicates_R.fn(this_day_all_data, this_day)
          rm(this_day_all_data)

          # call function to fill in PM2.5 data
          #output_list <- fill_input_mat_aves.fn(this_day_all_combined_true_dup, input_mat5_aves,rstart_aves, lat_tolerance_threshold,lon_tolerance_threshold, this_day)
          output_list <- fill_input_mat_aves.fn(this_day_all_combined_true_dup = this_day_all_combined_true_dup, input_mat5_aves = input_mat5_aves, rstart_aves = rstart_aves, lat_tolerance_threshold = lat_tolerance_threshold, lon_tolerance_threshold = lon_tolerance_threshold, this_day = this_day)      
          rm(input_mat5_aves,rstart_aves) # clear old versions of variables, which will be replaced with the output from the function
      # get the variables out of the output_list from the function
      input_mat5_aves <- output_list[[1]]
      rstart_aves <- output_list[[2]] 
      
     } # for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
  } # } else { # if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) there is duplicate data
  return(input_mat5_aves)
} # end of loop_PM25_station_deduplicate.parallel.fn function
