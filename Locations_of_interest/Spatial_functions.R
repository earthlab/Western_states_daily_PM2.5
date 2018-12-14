# functions for mapping and handling spatial data

# create data frame with Lon, Lat, and Datum for the centroids of the SpatialPolygonData put in
create_centroid_df.fn <- function(InterestMapGeom){
  # InterestMapGeom <- WestCountymapGeom
#county_centroids_mat <- centroid(WestCountymapGeom) # https://www.rdocumentation.org/packages/geosphere/versions/1.5-5/topics/centroid
InterestArea_centroids_mat <- centroid(InterestMapGeom) # https://www.rdocumentation.org/packages/geosphere/versions/1.5-5/topics/centroid
this_header <- c("Lon","Lat","Datum")
InterestArea_centroids_step <- data.frame(matrix(NA, nrow = dim(InterestArea_centroids_mat)[1], ncol = length(this_header))) # create data frame for input_mat1
names(InterestArea_centroids_step) <- this_header # assign the header to data frame
InterestArea_centroids_step[,1:2] <- InterestArea_centroids_mat # fill in centroid info
InterestArea_centroids_step$Datum <- this_datum
InterestArea_centroids_for_coord <- InterestArea_centroids_step
return(InterestArea_centroids_for_coord)
} # end of create_centroid_df.fn function

# fill in Easting/Northing
fill_in_EastingNorthing_from_LatLon.fn <- function(InterestArea_centroids_for_coord) {
  # InterestArea_centroids_for_coord <- county_centroids_for_coord2
InterestArea_centroids_orig <- InterestArea_centroids_for_coord
coordinates(InterestArea_centroids_for_coord)<- c("Lon", "Lat")
proj4string(InterestArea_centroids_for_coord)<- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
albers<- spTransform(InterestArea_centroids_for_coord, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
Albers<- coordinates(albers)
colnames(Albers)<- c("Easting", "Northing")
#county_centroids <- cbind(county_centroids_step, Albers)
centroids_df <- cbind(InterestArea_centroids_orig, Albers)
row.names(centroids_df)<- c()
 return(centroids_df)
} # end of fill_in_EastingNorthing_from_LatLon.fn function