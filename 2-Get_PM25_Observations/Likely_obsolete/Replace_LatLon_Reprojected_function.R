# Replace_LatLon_Reprojected.fn
Replace_LatLon_Reprojected.fn <- function(input_mat2,link_projections,Reprojected_datum,given_digits) { # start function 
  
#### create new input_mat3 for the new location information to go into ####
input_header <-  c(colnames(input_mat2),'Northing','Easting')
print(input_header) 
N_columns <- length(input_header) # how many columns are in header?
mat_nrow=dim(input_mat2)[1]
input_mat3 <- data.frame(matrix(NA,nrow=mat_nrow,ncol=N_columns)) # create data frame for input_mat1
names(input_mat3) <- input_header # assign the header to input_mat1
input_mat3[,1:(N_columns-2)] <- input_mat2
rm(N_columns,mat_nrow)

#input_mat3$PM2.5_Lat <- as.character(input_mat3$PM2.5_Lat)
#link_projections$Old.Lat <- as.character(link_projections$Old.Lat)

# Add factor level to datums in input_mat3
levels(input_mat3$Datum) <- c(levels(input_mat3$Datum),Reprojected_datum)
levels(link_projections$Old.Datum) <- c(levels(link_projections$Old.Datum),Reprojected_datum)

for (this_row_i in 1:dim(link_projections)[1]) { # cycle through rows of the reprojected data and replace values in input_mat3
  print(paste("row",this_row_i,"of",dim(link_projections)[1],sep = " "))
  # which rows in input_mat3 correspond to this lat/lon?
  which_this_lat_lon <- which(input_mat3$PM2.5_Lat >= link_projections[this_row_i,c("Old.Lat")]-given_digits &
                                input_mat3$PM2.5_Lat <= link_projections[this_row_i,c("Old.Lat")]+given_digits &
                                input_mat3$PM2.5_Lon >= link_projections[this_row_i,c("Old_Lon")]-given_digits &
                                input_mat3$PM2.5_Lon <= link_projections[this_row_i,c("Old_Lon")]+given_digits &
                                input_mat3$Datum == link_projections[this_row_i,c("Old.Datum")])
  
  
  

  length(which_this_lat_lon)
  if (length(which_this_lat_lon)==0) {stop("check data and code - no match found in input_mat3")}
  
  # Replace location info in input_mat3
  input_mat3[which_this_lat_lon,c("PM2.5_Lat")] <- link_projections[this_row_i,c("Latitude")]
  input_mat3[which_this_lat_lon,c("PM2.5_Lon")] <- link_projections[this_row_i,c("Longitude")]    
  input_mat3[which_this_lat_lon,c("Datum")] <- as.factor(Reprojected_datum)
  input_mat3[which_this_lat_lon,c("Northing")] <- link_projections[this_row_i,c("Northing")]
  input_mat3[which_this_lat_lon,c("Easting")] <- link_projections[this_row_i,c("Easting")]
}
  
# Did any locations not get replaced?
which_Loc_not_replaced <- which(input_mat3$Datum!=Reprojected_datum)
length(which_Loc_not_replaced)
if (length(which_Loc_not_replaced) != 0) {stop("check data and code, not all locations have been replaced.")}
#summary(input_mat3[which_Loc_not_replaced,])
#Not_replaced <- input_mat3[which_Loc_not_replaced,]
#first_lat <- Not_replaced[1,c("PM2.5_Lat")]
#print(first_lat)

return(input_mat3)
  
} # Replace_LatLon_Reprojected.fn <- function(this_day_all_data_in) { # start function 