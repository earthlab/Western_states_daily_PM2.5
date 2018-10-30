loop_PM25_station_deduplicate.parallel.fn <- function(this_station_i) {  #, input_header, unique_EPA_Codes) {
  print(this_station_i)
  #return(this_station_i)
  #### Loop through all stations with EPA codes ####
  print("starting loop through all stations for which we have EPA codes")
  #stop("start at this_station_i <- 15 ... crashes there")
  #for (this_station_i in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)
#  for (this_station_i in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)
    this_station <- unique_EPA_Codes[this_station_i,] # what is the code for this station?
    # find which rows in input_mat correspond to this station
    which_this_station <- which(known_EPA_Code_data$State_Code == this_station$State_Code &
                                  known_EPA_Code_data$County_Code == this_station$County_Code &
                                  known_EPA_Code_data$Site_Num == this_station$Site_Num)
    # create data frame with this station's data
    this_station_data <- known_EPA_Code_data[which_this_station,]
    rm(which_this_station)

    # how many unique days are in this data?
    unique_days <- unique(this_station_data$Date_Local)

    #unique_lat <- unique(this_station_data$Lat)
    unique_days_locations <- unique(this_station_data[,c("Date_Local","Lat","Lon","Easting","Northing")])

    if (length(unique_days) != dim(unique_days_locations)[1]) {
      print("location data not making sense")
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
      } else {stop("check data and code")}
    }

    # data just taking average of multiple obs at a location
    # create data frame for input_mat_4_aves
    input_mat4_aves <- data.frame(matrix(NA, nrow = length(unique_days), ncol = N_columns))
    names(input_mat4_aves) <- input_header # assign the header to input_mat_4_aves
    rstart_aves <- 1 # start counter

    # data that keeps data from co-located monitors separate and just combines data that are
    input_mat4_colocated <- data.frame(matrix(NA, nrow = 0, ncol = N_columns)) # create data frame for input_mat_4_aves
    names(input_mat4_colocated) <- input_header # assign the header to input_mat_4_aves
    rstart_colocated <- 1 # start counter

    #rm(N_columns) # clear variable

    #N_unique_days <- length(unique_days)
    #N_unique_days_locations <- length(unique_days_locations)

    # describe this station and it's data
    print(paste("station_i ", this_station_i, ": Station ", this_station$State_Code,"-",
                this_station$County_Code,"-",this_station$Site_Num," has ",
                dim(this_station_data)[1]," rows of data among ",length(unique_days), #length(which_this_station)," rows of data among ",length(unique_days),
                " unique days.",sep = ""))

    #sink() # stop outputting to sink file
    # print info to screen:
    #print(paste("station_i ", this_station_i, ": Station ", this_station$State_Code,"-",
    #            this_station$County_Code,"-",this_station$Site_Num," has ",
    #            dim(this_station_data)[1]," rows of data among ",length(unique_days),
    #            " unique days.",sep = ""))
    #sink(file = SinkFileName, append = TRUE, type = c("output","message"),split = FALSE)

    #rm(which_this_station)

    # determine whether there were multiple monitors ever operating at this site (or duplicate data)
    if (length(unique_days)==dim(this_station_data)[1] &
        length(unique(this_station_data$Data_Source_Name_Short))==1) { # determine whether there were multiple monitors ever operating at this site (or duplicate data)
      print("Each day of data for this station has only one monitor operating and there is no duplicate data.")

      # aves data
      rstop_aves <- rstart_aves + dim(this_station_data)[1]-1
      input_mat4_aves[rstart_aves:rstop_aves, ] <- this_station_data
      rstart_aves <- rstop_aves+1

      # colocated data
      rstop_colocated <- rstart_colocated + dim(this_station_data)[1]-1
      input_mat4_colocated[rstart_colocated:rstop_colocated, ] <- this_station_data
      rstart_colocated <- rstop_colocated+1

    } else { # if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) there is duplicate data
      print("there is duplicate data")
      print("figure out if this loop can be parallelized")
      # cycle through days
      for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
        # get data for this day
        this_day <- unique_days[this_day_i] # get the date
        # find the rows for this day
        which_this_day <- which(this_station_data$Date_Local == this_day)
        this_day_all_data <- this_station_data[which_this_day, ]
        #print(paste("Station ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num," has ",
        #            length(which_this_day)," rows of data on ",this_day,".",sep = ""))

        # call function of repeat entries of the same observations (usually event type is different)
        # function to combine rows that are from the same source and have the same concentration (usually event type is the only/main difference)
        #this_day_all_data_combine_true_duplicates  <- Combine_true_replicates_R.fn(this_day_all_data)
        this_day_all_combined_true_dup  <- Combine_true_replicates_R.fn(this_day_all_data)

        # call function to fill in PM2.5 data
        #output_list <- fill_in_aves_coloc_unique_PC_POC_MN.fn(this_day_all_data_combine_true_duplicates,input_mat4_aves,rstart_aves,input_mat4_colocated,rstart_colocated,lat_tolerance_threshold,lon_tolerance_threshold)
        output_list <- fill_in_aves_coloc_unique_PC_POC_MN.fn(this_day_all_combined_true_dup,
                                                              input_mat4_aves,rstart_aves,input_mat4_colocated,rstart_colocated,
                                                              lat_tolerance_threshold,lon_tolerance_threshold)

        # clear old versions of variables, which will be replaced with the output from the function
        rm(input_mat4_aves,rstart_aves,input_mat4_colocated,rstart_colocated)
        # get the variables out of the output_list from the function
        input_mat4_aves <- output_list[[1]]
        rstart_aves <- output_list[[2]]
        input_mat4_colocated <- output_list[[3]]
        rstart_colocated <- output_list[[4]]

        #rm(this_day, which_this_day, this_day_all_data, this_day_all_combined_true_dup, output_list)
        #rm(this_day_all_data)
      } # for (this_day_i in 1:length(unique_days)) { # for loop cycling through days relevant for this station
    } # } else { # if (length(unique_days)==dim(this_station_data)[1] & length(unique(this_station_data$Data_Source_Name_Short))==1) there is duplicate data
    #rm(this_station,this_station_data,unique_days) #UNCOMMENT
#  } # for (this_station_i in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)

    #### Write csv files ####
    # aves file
    #print("summary of input_mat4_aves output by DeDuplicate_ML_Input_File.R:")
    #summary(input_mat4_aves_full) # give summary of current state of data
    #print("file names still included")
    #unique(input_mat4_aves_full$Source_File)
    write.csv(input_mat4_aves,file = file.path(ProcessedData.directory,sub_folder,paste('PM25_Step5_part_',processed_data_version,'_de_duplicated_aves_ML_input_station_',this_station_i,'.csv',sep = "")),row.names = FALSE)

    # colocated file
    #print("summary of input_mat4_colocated output by DeDuplicate_ML_Input_File.R:")
    #summary(input_mat4_colocated_full) # give summary of current state of data
    #print("file names still included")
    #unique(input_mat4_colocated_full$Source_File)
    write.csv(input_mat4_colocated,file = file.path(ProcessedData.directory,sub_folder,paste('PM25_Step5_part_',processed_data_version,'_de_duplicated_colocated_ML_input_station_',this_station_i,'.csv',sep = "")),row.names = FALSE)

} # end of loop_PM25_station_deduplicate.parallel.fn function