library(raster)

#Merge Locations & Dates

subset_batches<- function(bigger, smaller, filename){ #Use location-date files here
  smaller$ID<- rep(1, dim(smaller)[1])
  big_minus_small<- merge(bigger, smaller, by = c("Lat", "Lon", "Date"), all.x = TRUE)
  difference_pos<- which(is.na(big_minus_small$ID))
  final<- big_minus_small[difference_pos,]
  
  write.csv(final[,1:3], paste0("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\", filename, "_Locations-Dates.csv"),
            row.names = FALSE)
  
  #Merge Locations only
  LOC_final<- unique(final[,1:2])
  
  write.csv(LOC_final, paste0("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\", filename, "_Locations.csv"),
            row.names = FALSE)
  
  #Project to Albers
  monitors<- SpatialPoints(final[,c(2,1)])
  proj4string(monitors)<- CRS( "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
  Monitors<- spTransform(monitors, CRS( "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  Easting<- Monitors@coords[, "Lon"]
  Northing<- Monitors@coords[, "Lat"]
  
  Projected<- cbind(final[,1:3], Easting, Northing)
  
  write.csv(Projected, paste0("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\", filename, "_Projected_Locations_Dates.csv"),
            row.names = FALSE)
}


#Read in part_e and part_b
part_b<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\PM25_Step3_part_b_Locations_Dates_Projected (1).csv")

part_e<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\PM25_Step3_part_e_Locations_Dates_NAD83.csv")

subset_batches(part_e, part_b, "Part_e_minus_b")

#Part F...

part_f<- read.csv("C:\\Users\\ellen\\Downloads\\PM25_Step3_part_f_Locations_Dates_NAD83.csv")

subset_batches(part_f, part_e, "Part_f_minus_e")

