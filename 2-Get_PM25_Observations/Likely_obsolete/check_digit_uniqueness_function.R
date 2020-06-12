check_digit_uniqueness.fn <- function(link_projections,given_digits) { # start function 
  
  for (this_row_i in 1:dim(link_projections)[1]) { # cycle through rows of the reprojected data and check if given_digits is long enough to get unique vaues in link_projectsions
    print(paste("row",this_row_i,"of",dim(link_projections)[1],sep = " "))
    
    which_match <- which(link_projections$Old.Lat >= link_projections[this_row_i,c("Old.Lat")]-given_digits &
                         link_projections$Old.Lat <= link_projections[this_row_i,c("Old.Lat")]+given_digits &
                         link_projections$Old_Lon >= link_projections[this_row_i,c("Old_Lon")]-given_digits &
                         link_projections$Old_Lon <= link_projections[this_row_i,c("Old_Lon")]+given_digits &
                         link_projections$Old.Datum == link_projections[this_row_i,c("Old.Datum")])
print(length(which_match))
print(link_projections[which_match,])

if (length(which_match) != 1) {stop(paste("given_digits set to ",given_digits,
                                          " is not enough digits to get unique values in link_projections matrix",sep = ""))}
  
} # for
} # function