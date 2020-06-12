Replace_LatLonDatum_for_NA_UKNOWN.fn <- function(known_EPA_Code_data,All_sites_meta) { 
  
  # find the sites that need to have info replaced
  which_need_replace <- which(All_sites_meta$ReplacedLocation == 1)
  Replace_Sites_meta <- All_sites_meta[which_need_replace,]
  known_EPA_Code_data_new <- known_EPA_Code_data
  known_EPA_Code_data_new$PM2.5_Lat <- as.numeric(as.character(known_EPA_Code_data_new$PM2.5_Lat))
  known_EPA_Code_data_new$PM2.5_Lon <- as.numeric(as.character(known_EPA_Code_data_new$PM2.5_Lon))
  for (this_row_i in 1:dim(Replace_Sites_meta)[1]) { # cycle through rows meta data that need replacements
    print(paste(this_row_i," of ",dim(Replace_Sites_meta)[1]),sep = " ")
    this_row <- Replace_Sites_meta[this_row_i,] #COMMENT
    # find the rows in known_EPA_Codes (the main data frame)
    which_to_replace <- which(known_EPA_Code_data$State_Code == Replace_Sites_meta[this_row_i,c("State_Code")] & 
                              known_EPA_Code_data$County_Code == Replace_Sites_meta[this_row_i,c("County_Code")] & 
                              known_EPA_Code_data$Site_Num == Replace_Sites_meta[this_row_i,c("Site_Num")] &
                              known_EPA_Code_data$Data_Source_Name_Short == Replace_Sites_meta[this_row_i,c("Data_Source_Name_Short")])
    print(length(which_to_replace)) # COMMENT
    known_EPA_Code_data_new[which_to_replace,c("Datum")] <- Replace_Sites_meta[this_row_i,c("Datum")]
    known_EPA_Code_data_new[which_to_replace,c("PM2.5_Lat")] <- Replace_Sites_meta[this_row_i,c("Lat")]
    known_EPA_Code_data_new[which_to_replace,c("PM2.5_Lon")] <- Replace_Sites_meta[this_row_i,c("Lon")]

  } # for (this_row_i in 1:dim(Replace_Sites_meta)[1]) { # cycle through rows meta data that need replacements
  return(known_EPA_Code_data_new) # output from function
} # function