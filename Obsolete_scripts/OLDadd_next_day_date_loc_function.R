# add_next_day_date_loc.fn function to add the next day to the list for every location and then de-duplicate
add_next_day_date_loc.fn <- function(PM25DateLoc_temp) { # start function
  PM25DateLoc_NextDay <- PM25DateLoc_temp # duplicate date/location data frame to new variable name
  PM25DateLoc_NextDay$Date <- PM25DateLoc_temp$Date+1 # shift all dates to the next day
  PM25DateLoc_step1 <- rbind(PM25DateLoc_temp,PM25DateLoc_NextDay) # combine the data frames with the day of interest and the next day
  PM25DateLoc <- PM25DateLoc_step1[!duplicated(PM25DateLoc_step1[,1:4]),] # get rid of any duplicates (which happens any time a monitor runs for 2 consecutive days in the same location)
  return(PM25DateLoc) # output from function
} # end add_next_day_date_loc.fn function
