add_next_day_date_loc.fn <- function(PM25DateLoc_temp) {
  
  #PM25DateLoc_temp <- PM25DateLoc_orig
  
  PM25DateLoc_NextDay <- PM25DateLoc_temp
  
  PM25DateLoc_NextDay$Date <- PM25DateLoc_temp$Date+1

  PM25DateLoc_step1 <- rbind(PM25DateLoc_temp,PM25DateLoc_NextDay)
  
  PM25DateLoc <- PM25DateLoc_step1[!duplicated(PM25DateLoc_step1[,1:4]),]
  return(PM25DateLoc)
} # end function

#PM25DateLoc_NextDay <- data.frame(matrix(NA,nrow=dim(PM25DateLoc_temp)[1],ncol=dim(PM25DateLoc_temp)[2])) # create data frame for input_mat1
#names(PM25DateLoc_NextDay) <- colnames(PM25DateLoc_temp) # assign the header to input_mat1
#PM25DateLoc_NextDay$Date <- as.Date(PM25DateLoc_NextDay$Date)

#PM25DateLoc_step1 <- data.frame(matrix(NA,nrow=dim(PM25DateLoc_temp)[1]*2,ncol=dim(PM25DateLoc_temp)[2])) # create data frame for input_mat1
#names(PM25DateLoc_step1) <- colnames(PM25DateLoc_temp) # assign the header to input_mat1
#PM25DateLoc_step1$Date <- as.Date(PM25DateLoc_step1$Date)

# new_row_start <- 1
# new_row_stop <- 2
# for (old_row_counter in 1:dim(PM25DateLoc_step1)[1]) { #UNCOMMENT
# #for (old_row_counter in 1:5000) { #COMMENT
# # print("*** BE SURE TO INCLUDE ENTIRE LOOP IN add_next_day_date_loc_function.R WHEN RUNNING ALL DATA ****") #COMMENT  
#   old_row <- PM25DateLoc_temp[old_row_counter,]
#   #print(old_row)
#   next_day <- old_row
#   next_day$Date <- old_row$Date+1
#   #print(next_day)
#   
#   PM25DateLoc_step1[new_row_start,] <- old_row
#   PM25DateLoc_step1[new_row_stop,] <- next_day
#   new_row_start <- new_row_start +2
#   new_row_stop <- new_row_stop +2
#   
# } # for (old_row_counter in 1:dim(PM25DateLoc)[1]) {
# 


