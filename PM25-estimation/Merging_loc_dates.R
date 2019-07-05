
#Merge Locations & Dates

subset_batches<- function(bigger, smaller, filename){ #Use location-date files here
  big_minus_small<- merge(bigger, smaller, by = c("Lat", "Lon", "Date"), all.x = TRUE)
  difference_pos<- which(is.na(big_minus_small$Northing))
  final<- big_minus_small[difference_pos,]
  
  write.csv(final[,1:3], paste0("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\", filename, "_Locations-Dates.csv"),
            row.names = FALSE)
  
  #Merge Locations only
  LOC_final<- unique(final[,1:2])
  
  write.csv(LOC_final, paste0("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\", filename, "_Locations.csv"),
            row.names = FALSE)
}


#Read in part_e and part_b
part_b<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\PM25_Step3_part_b_Locations_Dates_Projected (1).csv")

part_e<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\PM25_Step3_part_e_Locations_Dates_NAD83.csv")

subset_batches(part_e, part_b, "Part_e_minus_b")

#Part F...

part_f<- read.csv("C:\\Users\\ellen\\Downloads\\PM25_Step3_part_f_Locations_Dates_NAD83.csv")

subset_batches(part_f, part_e, "Part_f_minus_e")

