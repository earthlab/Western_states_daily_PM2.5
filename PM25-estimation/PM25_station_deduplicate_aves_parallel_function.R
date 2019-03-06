#PM25_station_deduplicate_aves_parallel.fn() 
# this_location_i <- X
# for a given location, de-duplicate by taking average of multiple obs at a location
PM25_station_deduplicate_aves_parallel.fn <- function(this_location_i) {  #, input_header, unique_EPA_Codes) {
  this_lat <- Locations_input_mat3[this_location_i,"Lat"] 
  this_lon <- Locations_input_mat3[this_location_i,"Lon"] 
  which_this_location <- which(input_mat3$Lat == this_lat & input_mat3$Lon == this_lon)
  this_location_data_step1 <- input_mat3[which_this_location, ]
  # check for and de-duplicate any rows that are complete repeats
  this_location_data_step2 <- this_location_data_step1[!duplicated(this_location_data_step1), ]
  rm(this_location_data_step1)
  rm(which_this_location)
  unique_days <- unique(this_location_data_step2$Date_Local) # create list of unique days in this data
  unique_days_locations <- unique(this_location_data_step2[,c("Date_Local","Lat","Lon")])#,"Easting","Northing")])

    if (length(unique_days) != dim(unique_days_locations)[1]) { # check on data/code
      stop("figure out if changes are needed for this section")
      #stop("location data not making sense")
      data_sources <- unique(this_station_data$Data_Source_Name_Short)
      print(paste("station_i = ",this_station_i,"; data sources:"))
      print(data_sources)
      if (data_sources[1] == "EPA_PM25") { # if station data is from EPA and other sources, use only the EPA version
         which_EPA <- which(this_station_data$Data_Source_Name_Short == "EPA_PM25")
         this_station_data_step <- this_station_data[which_EPA, ]
         rm(this_station_data)
         this_station_data <- this_station_data_step
         rm(this_station_data_step) 
         # how many unique days are in this data?
         unique_days <- unique(this_station_data$Date_Local)
         print("figure out why some of the IMPROVE sites have more days at EPA stations than EPA data - and write code to keep those")
      } else {stop("check data and code; location data not making sense (PM25_station_deduplication_aves_parallel_function.R, line 27)")}
    } # if (length(unique_days) != dim(unique_days_locations)[1]) { # check on data/code

  #input_mat5_aves <- data.frame(matrix(NA, nrow = length(unique_days), ncol = dim(known_EPA_Code_data)[2])) # create data frame for input_mat_5_aves
  input_mat4_aves <- data.frame(matrix(NA, nrow = length(unique_days), ncol = dim(input_mat3)[2])) # create data frame for input_mat_5_aves
  names(input_mat4_aves) <- colnames(input_mat3)#input_header # assign the header to input_mat_5_aves
  rstart_aves <- 1 # start counter

    # describe this station and it's data
    print(paste("location_i ", this_location_i, ": (Lat ", unique(this_location_data_step2$Lat),", Lon ",
                unique(this_location_data_step2$Lon),") has ",
                dim(this_location_data_step2)[1]," rows of data among ",length(unique_days), #length(which_this_station)," rows of data among ",length(unique_days),
                " unique days.",sep = ""))
    print("station names:")
    print(unique(this_location_data_step2$PM25_Station_Name))

  # determine whether there were multiple monitors ever operating at this site (or duplicate data)
  if (length(unique_days)==dim(this_location_data_step2)[1] &
     length(unique(this_location_data_step2$Data_Source_Name_Short))==1) { # determine whether there were multiple monitors ever operating at this site (or duplicate data)
     print("Each day of data for this station has only one monitor operating and there is no duplicate data.")

     # aves data
      rstop_aves <- rstart_aves + dim(this_location_data_step2)[1]-1
      input_mat4_aves[rstart_aves:rstop_aves, ] <- this_location_data_step2
      rstart_aves <- rstop_aves+1
  } else { # if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) there is duplicate data
       print("there is duplicate data")
      # cycle through days
      for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
         this_day <- unique_days[this_day_i] # get the date
         which_this_day <- which(this_location_data_step2$Date_Local == this_day) # find the rows for this day
         this_day_all_data <- this_location_data_step2[which_this_day, ]
         #print(paste("This location (Lat ", unique(this_location_data_step2$Lat),", Lon ",
        #             unique(this_location_data_step2$Lon),") has ",
        #             length(which_this_day)," rows of data on ",this_day,".",sep = ""))
          
         # call function of repeat entries of the same observations (usually event type is different)
          # function to combine rows that are from the same source and have the same concentration (usually event type is the only/main difference)
          this_day_all_combined_true_dup  <- Combine_true_replicates_R.fn(this_day_all_data, this_day)
          rm(this_day_all_data)

          # call function to fill in PM2.5 data
          output_list <- fill_input_mat_aves.fn(this_day_all_combined_true_dup = this_day_all_combined_true_dup, input_mat4_aves = input_mat4_aves, rstart_aves = rstart_aves, this_day = this_day)# lat_tolerance_threshold = lat_tolerance_threshold, lon_tolerance_threshold = lon_tolerance_threshold, this_day = this_day)      
          rm(input_mat4_aves,rstart_aves) # clear old versions of variables, which will be replaced with the output from the function
      # get the variables out of the output_list from the function
      input_mat4_aves <- output_list[[1]]
      rstart_aves <- output_list[[2]] 
      rm(output_list)
     } # for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
  } # } else { # if (length(unique_days)==dim(this_location_data_step2)[1] & length(unique(this_location_data_step2$Data_Source_Name_Short))==1) there is duplicate data
  return(input_mat4_aves)
} # end of loop_PM25_station_deduplicate.parallel.fn function
