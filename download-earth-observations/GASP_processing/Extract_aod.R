library(dismo)
library(rgdal)
library(raster)
library(date)

#GASP format: GASP_2008.091_avg.tif
toDate<- function(slice){
  year<- substr(slice, 6, 9)
  jd<- substr(slice, 11, 13)
  date<- as.Date(as.numeric(jd) - 1, origin = paste(year,"-01-01", sep = ""))
  return(date)
}

#Get monitor locations
monitors<- read.csv("C:\\Users\\elco2649\\Documents\\GASP_EC2\\Projected_locations_with_dates_part_a.csv")
row.names(monitors)<- c()
coords<- monitors[,c("Lon", "Lat")]
coordinates(coords)<- c("Lon", "Lat")
proj4string(coords)<- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

#Create output df
output<- data.frame(Latitude=double(),
                    Longitude=double(), 
                    Datum=character(),
                    Date=as.Date(character()),
                    Lat=double(),
                    Lon=double(),
                    Northing=double(), 
                    Easting=double(),
                    AOD=double(),
                    stringsAsFactors=FALSE)

#Loop through TIFs
TIFpath = "C:\\Users\\elco2649\\Documents\\GASP_EC2\\rasters\\"
out.file<-""
file.names <- dir(TIFpath, pattern =".tif")
print(Sys.time())

for(i in 1:length(file.names)){
  print(file.names[i])
  date<- toDate(file.names[i])
  
  mont_dates<- as.Date(monitors$Date)
  pos<- which(mont_dates == date)
  day_monts<- coords[pos,]
  
  file <- raster(paste(TIFpath, file.names[i], sep = ""))
  
  aod<- extract(file, day_monts)
  #print(head(aod))
  newdata<- cbind(monitors[pos,], aod)
  
  output<- rbind(output, newdata)
  
}

output<- output[,-1]
colnames(output)<- c("Latitude", "Longitude", "Datum", "Date", "Lat", "Lon", "Northing", "Easting", "AOD")

#Write output to csv
write.csv(output, "C:\\Users\\elco2649\\Documents\\GASP_EC2\\rasters\\GASP_extracted.csv")

print(Sys.time())
