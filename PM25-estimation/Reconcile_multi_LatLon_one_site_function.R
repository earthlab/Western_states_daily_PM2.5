# function to reconcile location information that doesn't match for a given EPA site code
Reconcile_multi_LatLon_one_site.fn <- function(this_station,this_station_data,input_mat_step) {
  # what are the data sources?
  unique_sources <- unique(this_station_data$Data_Source_Name_Short)
  print(unique_sources)
  
  
  
  # figure out how many unique EPA codes are in the data
  #One_site_sources_loc <- data.frame(matrix(NA, nrow = length(unique_sources), ncol = 7)) # create data frame with only EPA codes
  One_site_sources_loc <- data.frame(matrix(NA, nrow = 1, ncol = 7)) # create data frame with only EPA codes
  names(One_site_sources_loc) <- c("State_Code","County_Code","Site_Num","Data_Source_Name_Short","Datum","Lat","Lon") # create header
  
  for (this_source_i in 1:) {}
  
  
  
  One_site_sources_loc[,c("State_Code","County_Code","Site_Num")] <- this_station
  unique_EPA_Codes <- Codes_only_repeats[!duplicated(Codes_only_repeats[,1:3]),]
  print(paste("There are ",dim(unique_EPA_Codes)[1]," unique EPA codes (i.e. stations) in the data. (This includes slightly into bordering states.)",sep = ""))
  rm(Codes_only_repeats)
  
  
  # what are the lat/lons for this site?
  unique_lats <- unique(this_station_data$PM2.5_Lat)
  print(unique_lats)
  unique_lons <- unique(this_station_data$PM2.5_Lon)
  print(unique_lons)
  
  for (this_lat_i in 1:length(unique_lats)) { # cycle through latitudes in this data
    this_lat <- unique_lats[this_lat_i]
    print(this_lat)
  }
  
  
  return(input_mat_step)
  
}