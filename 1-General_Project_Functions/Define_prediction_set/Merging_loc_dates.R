#Read in part_e and part_b
part_b<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\PM25_Step3_part_b_Locations_Dates_Projected (1).csv")

part_e<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\PM25_Step3_part_e_Locations_Dates_NAD83.csv")

#Merge Locations & Dates

e_minus_b<- merge(part_e, part_b, by = c("Lat", "Lon", "Date"), all.x = TRUE)

difference_pos<- which(is.na(e_minus_b$Northing))

final<- e_minus_b[difference_pos,]

write.csv(final[,1:3], "C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\e_minus_b_Locations_Dates.csv",
          row.names = FALSE)

#Merge Locations only

LOC_final<- unique(final[,1:2])

write.csv(LOC_final, "C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\e_minus_b_Locations.csv",
          row.names = FALSE)


