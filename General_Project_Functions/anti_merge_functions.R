# separate (anti_merge) datasets
anti_merge_lat_lon_data.fn <- function(Newer_data,Older_data) { #,latitude_col_s = "Lat",longitude_col_s = "Lon") { #,by_vars, round_digits) { #latitude_col_s = "Lat",longitude_col_s = "Lon") {
  Newer_data_Only_step <- Newer_data
  for (row_i in 1:dim(Older_data)[1]) {
    Older_row <- Older_data[row_i, ]
    this_lat <- Older_row$Lat
    N_dec_lat <- decimalplaces(this_lat)
    this_lon <- Older_row$Lon
    N_dec_lon <- decimalplaces(this_lon)    
    
    which_match <- which(round(Newer_data_Only_step$Lat,N_dec_lat) == round(this_lat,N_dec_lat) & 
                           round(Newer_data_Only_step$Lon,N_dec_lon) == round(this_lon,N_dec_lon))  
    
    if (length(which_match) == 1) { 
    Newer_data_Only_step[which_match, ] <- NA # replace 
    } else if (length(which_match) > 1) {
      step_newer <- Newer_data_Only_step[which_match, ]
      for (row_j in 1:length(which_match)) {
        N_dec_lat_new <- decimalplaces(step_newer[row_j,"Lat"])
        N_dec_lon_new <- decimalplaces(step_newer[row_j,"Lon"])
        if (N_dec_lat == N_dec_lat_new & N_dec_lon == N_dec_lon_new) {
          this_which_match <-  which_match[row_j]
          Newer_data_Only_step[this_which_match, ] <- NA # replace
          rm(this_which_match)
        } # if (N_dec_lat == N_dec_lat_new & N_dec_lon == N_dec_lon_new) {
        rm(N_dec_lat_new,N_dec_lon_new)
      } # for (row_j in 1:length(which_match)) {
      rm(step_newer)
    }
  }
  Newer_Only <- Newer_data_Only_step[complete.cases(Newer_data_Only_step), ]
  return(Newer_Only)
} # end of merge_time_static_data.fn function

anti_merge_LatLon_data.fn <- function(Newer_data,Older_data, this_cluster) { #,latitude_col_s = "Lat",longitude_col_s = "Lon") { #,by_vars, round_digits) { #latitude_col_s = "Lat",longitude_col_s = "Lon") {
  #Newer_data_Only_step <- Newer_data
  #for (row_i in 1:dim(Older_data)[1]) {
  #match_rows_out_step <- unlist(lapply(X = 1:dim(Older_data)[1], FUN = function(row_i) { # start lapply
  #match_rows_out_step <- unlist(parLapply(X = 1:dim(Older_data)[1], FUN = function(row_i) { # start lapply  
  
  #clusterExport(cl = this_cluster, varlist = c(Newer_data,Older_data),  envir = .GlobalEnv) # export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
  clusterExport(cl = this_cluster, varlist = c("Newer_data","Older_data"),  envir = environment()) # export functions and variables to parallel clusters (libaries handled with clusterEvalQ)
  
    match_rows_out_step <- unlist(parLapply(this_cluster,X = 1:dim(Older_data)[1], fun = function(row_i) { # start lapply    
    Older_row <- Older_data[row_i, ]
    this_lat <- Older_row$Lat
    N_dec_lat <- decimalplaces(this_lat)
    this_lon <- Older_row$Lon
    N_dec_lon <- decimalplaces(this_lon)    
    
    which_match <- which(round(Newer_data$Lat,N_dec_lat) == round(this_lat,N_dec_lat) & 
                           round(Newer_data$Lon,N_dec_lon) == round(this_lon,N_dec_lon))  
    
    if (length(which_match) == 1) { 
      #Newer_data[which_match, ] <- NA # replace 
      which_match_out <- which_match
    } else if (length(which_match) > 1) {
      step_newer <- Newer_data[which_match, ]
      for (row_j in 1:length(which_match)) {
        N_dec_lat_new <- decimalplaces(step_newer[row_j,"Lat"])
        N_dec_lon_new <- decimalplaces(step_newer[row_j,"Lon"])
        if (N_dec_lat == N_dec_lat_new & N_dec_lon == N_dec_lon_new) {
          this_which_match <-  which_match[row_j]
          #Newer_data[this_which_match, ] <- NA # replace
          which_match_out <- this_which_match
          rm(this_which_match)
        } else {# if (N_dec_lat == N_dec_lat_new & N_dec_lon == N_dec_lon_new) {
          which_match_out <- NA
        }
        rm(N_dec_lat_new,N_dec_lon_new)
      } # for (row_j in 1:length(which_match)) {
      rm(step_newer)
    } else {# } else if (length(which_match) > 1) {
      which_match_out <- NA
    } # if (length(which_match) == 1) { 
    return(which_match_out)
  })) # match_rows_out <- lapply(X = 1:dim(Older_data)[1]) { # start lapply
  
  list_keep <- which(c(1:dim(Newer_data)[1]) %!in% match_rows_out_step)
  
  #Newer_Only <- Newer_data[complete.cases(Newer_data), ]
  Newer_Only <- Newer_data[list_keep, ]
  return(Newer_Only)
} # end of merge_time_static_data.fn function