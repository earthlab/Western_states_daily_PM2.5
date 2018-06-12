# function to reconcile location information that doesn't match for a given EPA site code
# input_mat_step <- known_EPA_Code_data
Reconcile_multi_LatLon_one_site.fn <- function(this_station,this_station_data,AQS_master_locations,input_mat_step) { #,input_mat_step) {
  # what are the data sources?
  unique_sources <- unique(this_station_data$Data_Source_Name_Short)
  print(unique_sources)
  
  # what are the lat/lons for this site?
  unique_lats <- unique(this_station_data$PM2.5_Lat)
  print(unique_lats)
  unique_lons <- unique(this_station_data$PM2.5_Lon)
  print(unique_lons)
  #unique_datums <- unique(this_station_data$Datum)
  #print(unique_datums)
  rm(unique_lats)
  # what are the datums for this data source?
  this_station_datums <- unique(this_station_data$Datum)
  print(this_station_datums)

  if (as.factor("UNKNOWN") %in% this_station_datums) {
    stop("write more code 'UNKNOWN' datum")
  }
  
  # figure out how many unique EPA codes are in the data
  #One_site_sources_loc <- data.frame(matrix(NA, nrow = length(unique_sources), ncol = 7)) # create data frame with only EPA codes
  One_site_meta <- data.frame(matrix(NA, nrow = 1, ncol = 10)) # create data frame with only EPA codes
  names(One_site_meta) <- c("State_Code","County_Code","Site_Num","Data_Source_Name_Short","Datum","Lat","Lon","minDate","maxDate","ReplacedLocation") # create header
  One_site_meta$minDate <- as.Date(One_site_meta$minDate,format = "%Y-%m-%d")
  meta_row <- 0
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
    #One_site_meta$minDate <- as.Date(One_site_meta$minDate,"%Y-%m-%d")
    #input_mat$Date_Local <- as.Date(input_mat$Date_Local,"%Y-%m-%d")
        One_site_meta[meta_row,c("maxDate")] <- as.character(max(as.Date(this_datum_lat_source$Date_Local,format = "%Y-%m-%d")))
    #meta_row
    #as.Date(One_site_meta[meta_row-1,c("maxDate")],format = "%Y-%m-%d")
    #as.Date(One_site_meta[meta_row,c("minDate")],format = "%Y-%m-%d")
    
    # if (meta_row>1) {
    # if (as.Date(One_site_meta[meta_row-1,c("maxDate")],format = "%Y-%m-%d")>as.Date(One_site_meta[meta_row,c("minDate")],format = "%Y-%m-%d")) {
    #   stop("overlapping dates among rows in meta data")
    # } # if (as.Date(One_site_meta[meta_row-1,c("maxDate")],format = "%Y-%m-%d")>as.Date(One_site_meta[meta_row,c("minDate")],format = "%Y-%m-%d")) {
    # } # if (meta_row>1) {
    
      } # for (this_datum_i in 1:length(unique_datums_lats_source)) { # cycle through datum values for this latitude value from this source
    } # for (this_lat_i in 1:length(unique_lats)) { # cycle through individual latitude values for this source
  } # for (this_source_i in 1:length(unique_sources)) { # cycle through sources of data

  # One_site_sources_loc[,c("State_Code","County_Code","Site_Num")] <- this_station
  # unique_EPA_Codes <- Codes_only_repeats[!duplicated(Codes_only_repeats[,1:3]),]
  # print(paste("There are ",dim(unique_EPA_Codes)[1]," unique EPA codes (i.e. stations) in the data. (This includes slightly into bordering states.)",sep = ""))
  # rm(Codes_only_repeats)


#  return(input_mat_step)

#### For datums that are NA, replace datum, lat, and lon with values from aqs_monotors.csv ####
  
  # if (as.factor(NA) %in% this_station_datums) {
  #   stop("write more code - NA datum")
  # }
  
which_datums_NA <- which(is.na(One_site_meta$Datum))
print(which_datums_NA)  
if (length(which_datums_NA)>1) { # there are datums with values NA
  print("There are datums with value NA. For these, the Datum, Lat, and Lon will be replaced with values from aqs_monitors.csv")
  which_in_master <- which(AQS_master_locations$State.Code==unique(One_site_meta$State_Code) & AQS_master_locations$County.Code==unique(One_site_meta$County_Code) & AQS_master_locations$Site.Number==unique(One_site_meta$Site_Num))
  length(which_in_master)
  master_datum <- unique(AQS_master_locations[which_in_master,c("Datum")])
  print(master_datum)
  if (length(master_datum)>1) {stop("check code and data - only expecting one value for master_datum")}
  master_lat <- unique(AQS_master_locations[which_in_master,c("Latitude")])
  print(master_lat)
  if (length(master_lat)>1) {stop("check code and data - only expecting one value for master_lat")}
  master_lon <- unique(AQS_master_locations[which_in_master,c("Longitude")])
  print(master_lon)
  if (length(master_lon)>1) {stop("check code and data - only expecting one value for master_lon")}

  One_site_meta_new <- One_site_meta # create new meta data -data frame
  One_site_meta_new[which_datums_NA,c("ReplacedLocation")] <- 1
  One_site_meta_new[which_datums_NA,c("Datum")] <- as.character(master_datum) # replace datum
  One_site_meta_new[which_datums_NA,c("Lat")] <- as.character(master_lat) 
  One_site_meta_new[which_datums_NA,c("Lon")] <- as.character(master_lon) 
  
} # if (length(which_datums_NA)>1) { # there are datums with values NA
  
  output_list <- list(One_site_meta,One_site_meta_new,input_mat_step_out)   # return value 
  return(output_list)

}