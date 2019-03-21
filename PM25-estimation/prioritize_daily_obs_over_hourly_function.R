# prioritize_daily_obs_over_hourly.fn function 
# this_day_all_combined_true_dup_in <- this_day_all_combined_true_dup
prioritize_daily_obs_over_hourly.fn <- function(this_day_all_combined_true_dup_in) { # function to prioritize daily obs over hourly
  verbose_flag <- 0 # indicate whether to output text information about the station to the screen (0 means No)
  
  # find the hourly and daily observation designations for this data
  obs_duration_indicator <- unique(this_day_all_combined_true_dup_in$Sample_Duration)
  if (verbose_flag != 0) {
  print(obs_duration_indicator)
  }
  
  # determine if there are any 24 hour observations
  which_24 <- which(obs_duration_indicator != "1 HOUR")
  
  
  # prioritize 24-hr obs and disregard hourly obs for this day/location
  if (length(which_24)>0) { # are there 24-hr obs?
    print("keep only 24-hr obs")
    this_day_all_combined_true_dup_out <- this_day_all_combined_true_dup_in[which_24, ] # keep only the 24-hour obs
  } else { # if (length(which_24)>0) { # are there 24-hr obs?
    print("keep all obs since they are only hourly")
    this_day_all_combined_true_dup_out <- this_day_all_combined_true_dup_in # keep all obs
  } # if (length(which_24)>0) { # are there 24-hr obs?
  
  print(unique(this_day_all_combined_true_dup_out$Sample_Duration))
  
   # unique(input_mat3$Sample_Duration)
  #[1] "24 HOUR"       "24-HR BLK AVG" "1 HOUR"   
  
  return(this_day_all_combined_true_dup_out)
} # end of prioritize_daily_obs_over_hourly.fn function - to prioritize daily obs over hourly
