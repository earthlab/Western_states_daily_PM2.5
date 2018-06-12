# function to reconcile location information that doesn't match for a given EPA site code
Reconcile_multi_LatLon_one_site.fn <- function(this_station,this_station_data,AQS_master_locations) { 
  # Load site listing file
  this_source_file <- "aqs_monitors.csv"
  AQS_master_locations<-read.csv(file.path(AQSData.directory,this_source_file),header=TRUE) # load the AQS file
  
  # # Create data frame with location meta data
  # All_sites_meta <- data.frame(matrix(NA, nrow = 1, ncol = 10)) # create data frame with only EPA codes
  # names(All_sites_meta) <- c("State_Code","County_Code","Site_Num","Data_Source_Name_Short","Datum","Lat","Lon","minDate","maxDate","ReplacedLocation") # create header
  # All_sites_meta$minDate <- as.Date(All_sites_meta$minDate,format = "%Y-%m-%d")
  # All_sites_meta$maxDate <- as.Date(All_sites_meta$minDate,format = "%Y-%m-%d")
  # All_sites_meta$ReplacedLocation <- 0 # set all values to zero initially and then replace with 1 if location values are replaced
  # #meta_row <- 0 # row counter
  
  # figure out how many unique EPA codes are in the data
  Codes_only_repeats <- data.frame(matrix(NA, nrow = dim(known_EPA_Code_data)[1], ncol = 3)) # create data frame with only EPA codes
  names(Codes_only_repeats) <- c("State_Code","County_Code","Site_Num") # create header
  Codes_only_repeats <- known_EPA_Code_data[,c("State_Code","County_Code","Site_Num")]
  unique_EPA_Codes <- Codes_only_repeats[!duplicated(Codes_only_repeats[,1:3]),]
  print(paste("There are ",dim(unique_EPA_Codes)[1]," unique EPA codes (i.e. stations) in the data. (This includes slightly into bordering states.)",sep = ""))
  rm(Codes_only_repeats)
  
  # cycle through EPA site numbers and fill in "Unknown" and NA Datums with info from site listing file
  for (this_station_i in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)
    # Create data frame with location meta data
    One_site_meta <- data.frame(matrix(NA, nrow = 1, ncol = 10)) # create data frame with only EPA codes
    names(One_site_meta) <- c("State_Code","County_Code","Site_Num","Data_Source_Name_Short","Datum","Lat","Lon","minDate","maxDate","ReplacedLocation") # create header
    One_site_meta$minDate <- as.Date(One_site_meta$minDate,format = "%Y-%m-%d")
    One_site_meta$maxDate <- as.Date(One_site_meta$minDate,format = "%Y-%m-%d")
    meta_row <- 0 # row counter
    
    print(this_station_i)
    this_station <- unique_EPA_Codes[this_station_i,]
    
    which_this_station <- which(known_EPA_Code_data$State_Code == this_station$State_Code & known_EPA_Code_data$County_Code == this_station$County_Code & known_EPA_Code_data$Site_Num == this_station$Site_Num)
    this_station_data <- known_EPA_Code_data[which_this_station,]
    
    # how many unique days are in this data?
    unique_sources <- unique(this_station_data$Data_Source_Name_Short)
    
    # what are the lat/lons for this site?
    unique_lats <- unique(this_station_data$PM2.5_Lat)
    print(unique_lats)
    unique_lons <- unique(this_station_data$PM2.5_Lon)
    print(unique_lons)
    # what are the datums for this data source?
    this_station_datums <- unique(this_station_data$Datum)
    print(this_station_datums)
    
    print(paste("station_i ",this_station_i,": Station ",this_station$State_Code,"-",this_station$County_Code,"-",this_station$Site_Num," has ",
                length(which_this_station)," rows of data among ",length(unique_sources)," sources with ",length(unique_lats)," values for latitude",
                " and ",length(unique_lons)," values for longitude. The data has ",length(this_station_datums)," values for datums.",sep = ""))
    print(unique_sources)

 # if (as.factor("UNKNOWN") %in% this_station_datums) {
 #    stop("write more code 'UNKNOWN' datum")
 #  }

  for (this_source_i in 1:length(unique_sources)) { # cycle through sources of data
    this_source <- unique_sources[this_source_i]
    print(this_source)
    which_this_site_source <- which(this_station_data$Data_Source_Name_Short==this_source)
    this_site_source <- this_station_data[which_this_site_source,]
    unique_lats_source <- unique(this_site_source$PM2.5_Lat)
    print(unique_lats_source)
    for (this_lat_i in 1:length(unique_lats_source)) { # cycle through individual latitude values for this source
    this_lat <- unique_lats_source[this_lat_i]
    print(this_lat)  
    which_site_source_lat <- which(this_site_source$PM2.5_Lat==this_lat)
    if (length(which_site_source_lat)==0) {stop("check data and code. which_site_source_lat has length = 0")}
    site_source_lat <- this_site_source[which_site_source_lat,]
    unique_datums_lats_source <- unique(site_source_lat$Datum)
    print(unique_datums_lats_source)
      for (this_datum_i in 1:length(unique_datums_lats_source)) { # cycle through datum values for this latitude value from this source
        this_datum <- unique_datums_lats_source[this_datum_i]
        print(this_datum)  
        if (is.na(this_datum)) { # check if the datum has value NA
          which_this_datum_lat_source <- which(is.na(site_source_lat$Datum))
        } else { # value is not NA
        which_this_datum_lat_source <- which(site_source_lat$Datum==this_datum)
        }
        if (length(which_this_datum_lat_source)==0) {stop("check data and code. which_this_datum_lat_source has length = 0")}
        this_datum_lat_source <- site_source_lat[which_this_datum_lat_source,]      
        
        meta_row <- meta_row + 1
        print(meta_row)
        One_site_meta[meta_row,c("State_Code","County_Code","Site_Num")] <- c(unique(this_datum_lat_source$State_Code),unique(this_datum_lat_source$County_Code),unique(this_datum_lat_source$Site_Num))
        One_site_meta[meta_row,c("Data_Source_Name_Short")] <- as.character(this_source)
        One_site_meta[meta_row,c("Datum")] <- as.character(unique(this_datum_lat_source$Datum))
        One_site_meta[meta_row,c("Lat")] <- as.numeric(as.character(this_lat))
        One_site_meta[meta_row,c("Lon")] <- as.numeric(as.character(unique(this_datum_lat_source$PM2.5_Lon)))
        One_site_meta[meta_row,c("minDate")] <- as.character(min(as.Date(this_datum_lat_source$Date_Local,format = "%Y-%m-%d")))
        One_site_meta[meta_row,c("maxDate")] <- as.character(max(as.Date(this_datum_lat_source$Date_Local,format = "%Y-%m-%d")))
   
  # if (as.factor(NA) %in% this_station_datums) {
  #   stop("write more code - NA datum")
  # }
  


} # for (this_datum_i in 1:length(unique_datums_lats_source)) { # cycle through datum values for this latitude value from this source
    } # for (this_lat_i in 1:length(unique_lats_source)) { # cycle through individual latitude values for this source  
  } # for (this_source_i in 1:length(unique_sources)) { # cycle through sources of data
  
    which_datums_NA_or_UNKNOWN <- which(is.na(One_site_meta$Datum) | One_site_meta$Datum == "UNKNOWN")
    print(which_datums_NA_or_UNKNOWN) 
    One_site_meta$ReplacedLocation <- 0 # set all values to zero initially and then replace with 1 if location values are replaced
    One_site_meta_new <- One_site_meta # create new meta data -data frame
    if (length(which_datums_NA_or_UNKNOWN)>0) { # there are datums with values NA
      print("There are datums with value NA. For these, the Datum, Lat, and Lon will be replaced with values from aqs_monitors.csv")
      which_in_master <- which(AQS_master_locations$State.Code==unique(One_site_meta$State_Code) & AQS_master_locations$County.Code==unique(One_site_meta$County_Code) & AQS_master_locations$Site.Number==unique(One_site_meta$Site_Num))
      length(which_in_master)
      if (length(which_in_master)==0) { # could the station be found in aqs_monitors.csv?
        print("station could not be located in aqs_monitors.csv")
        One_site_meta_new[which_datums_NA_or_UNKNOWN,c("ReplacedLocation")] <- -1 # negative 1 indicates that this station could not be found in aqs_monitors.csv
      } else { # if (length(which_in_master)==0) { # could the station be found in aqs_monitors.csv?
      master_datum <- unique(AQS_master_locations[which_in_master,c("Datum")])
      print(master_datum)
      if (length(master_datum)>1) {stop("check code and data - only expecting one value for master_datum")}
      master_lat <- unique(AQS_master_locations[which_in_master,c("Latitude")])
      print(master_lat)
      if (length(master_lat)>1) {stop("check code and data - only expecting one value for master_lat")}
      master_lon <- unique(AQS_master_locations[which_in_master,c("Longitude")])
      print(master_lon)
      if (length(master_lon)>1) {stop("check code and data - only expecting one value for master_lon")}
      
      
      One_site_meta_new[which_datums_NA_or_UNKNOWN,c("ReplacedLocation")] <- 1
      One_site_meta_new[which_datums_NA_or_UNKNOWN,c("Datum")] <- as.character(master_datum) # replace datum
      One_site_meta_new[which_datums_NA_or_UNKNOWN,c("Lat")] <- as.character(master_lat) 
      One_site_meta_new[which_datums_NA_or_UNKNOWN,c("Lon")] <- as.character(master_lon) 
      } # if (length(which_in_master)==0) { # could the station be found in aqs_monitors.csv?
    } # if (length(which_datums_NA_or_UNKNOWN)>1) { # there are datums with values NA
    
    rm(One_site_meta)
    
    if (this_station_i == 1) {
      All_sites_meta <- One_site_meta_new
    } else {
      All_sites_meta <- rbind(All_sites_meta,One_site_meta_new)
    }
    rm(One_site_meta_new)
  } # for (this_station_i in 1:dim(unique_EPA_Codes)[1]) { # cycle through stations (EPA codes)  
  
  #### Save cleaned file to .csv ####
  print("summary of All_sites_meta output by Reconcile_multi_LatLon_one_site_function.R:")
  summary(All_sites_meta) # give summary of current state of data
  write.csv(All_sites_meta,file = file.path(ProcessedData.directory,'All_sites_meta_Reconcile_multi_LatLon.csv'),row.names = FALSE)
  
  #output_list <- list(All_sites_meta,All_sites_meta_new,input_mat_step_out)   # return value 
  return(All_sites_meta)

} # function