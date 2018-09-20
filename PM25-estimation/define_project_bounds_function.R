define_project_bounds.fn <- function() {
  # define bounding box for study area (lat/lon)
  North_Edge <- 50
  South_Edge <- 25
  West_Edge <- -126
  East_Edge <- -101 # about 78 km east of eastern edge of Colorado
  bounding_box <- data.frame(matrix(NA,nrow=1,ncol=4)) # create data frame 
  header <- c("North_Edge","South_Edge","West_Edge","East_Edge") # define header
  names(bounding_box) <- header # assign the header to matrix
  # fill in assigned values
  bounding_box$North_Edge <- North_Edge
  bounding_box$South_Edge <- South_Edge
  bounding_box$West_Edge <- West_Edge
  bounding_box$East_Edge <- East_Edge
  return(bounding_box)
}
# Bounds Gina used for NED data
#UL: 49.710378, -129.226343
#LR: 31.042818, -99.589304