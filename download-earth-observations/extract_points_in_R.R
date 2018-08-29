library(dismo)
library(rgdal)
library(raster)
library(date)

toDate<- function(slice){
  year<- substr(slice, 1, 4)
  jd<- substr(slice, 6, 8)
  date<- as.Date(as.numeric(jd) - 1, origin = paste(year,"-01-01", sep = ""))
  return(date)
}

#Get monitor locations
#all_monts<- read.csv("C:\\Users\\elco2649\\Documents\\Reproject_monitors\\Final_monitors.csv")
#monitors<- unique(all_monts[c("Easting", "Northing")])
monitors<- read.csv("C:\\Users\\elco2649\\Documents\\MAIAC\\Locations_Dates_of_PM25_Obs_DeDuplicate.csv")
row.names(monitors)<- c()
coords<- monitors[c("Longitude", "Latitude")]
albers<- project(as.matrix(coords), "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
colnames(albers)<- c("Easting", "Northing")

#Create output df
output<- data.frame(Latitude=double(),
                    Longitude=double(), 
                    Datum=character(),
                    Date=as.Date(character()),
                    Easting=double(),
                    Northing=double(),
                    AOD=double(),
                    stringsAsFactors=FALSE)

#Loop through TIFs
TIFpath = "C:\\Users\\elco2649\\Documents\\GASP_EC2\\rasters\\"
out.file<-""
file.names <- dir(TIFpath, pattern =".tif")

for(i in 1:length(file.names)){
  date<- toDate(file.names[i])
  
  mont_dates<- as.Date(monitors$Date)
  pos<- which(mont_dates == date)
  day_monts<- albers[pos,]
  
  file <- raster(paste(TIFpath, file.names[i], sep = ""))
  
  aod<- data.frame(day_monts, extract(file, day_monts))
  #print(head(aod))
  newdata<- cbind(monitors[pos,], aod)
  
  output<- rbind(output, newdata)
  
}

colnames(output)<- c("Latitude", "Longitude", "Datum", "Date", "Easting", "Northing", "AOD")

#Write output to csv
write.csv(output, "C:\\Users\\elco2649\\Documents\\GASP_EC2\\rasters\\GASP_extracted.csv")

