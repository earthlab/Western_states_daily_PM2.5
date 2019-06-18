#After retrieving the hourly value CSVs from http://www.airmonitoring.utah.gov/dataarchive/archpm25.htm

library(rgdal)

#All station info:
stations<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\UT-PM2.5-stations.csv")

extract_UT_data<- function(filename, year){
  S1<- names(read.csv(filename))[-1]
  raw1<- read.csv(filename, skip = 2)
  matched<- match(S1, stations$Name)
  
  days<- sort(rep(1:365, 24)) #366 for 2016
  daily<- cbind(unlist(apply(raw1[,-1], MARGIN = 2, function(y){
    aggregate(x = y, by = list(days), FUN = mean)[,2] })))
  
  Daily<- data.frame(Date = seq.Date(as.Date(paste0(year, "-1-1")), as.Date(paste0(year, "-12-31")), by = "day"), daily)
  
  data<- Daily[,1:2]
  names(data)<- c("Date", "X")
  for(j in 3:dim(Daily)[2]){
    new<- Daily[,c(1,j)]
    names(new)<- c("Date", "X")
    data<- rbind(data, new)
  }
  
  data$station.ID<- as.vector(sapply(matched, function(y){rep(stations[y,1],365)})) #366 for 2016
  Data<- merge(data, stations, by.x = "station.ID", by.y = names(stations)[1])
  
  sputm <- SpatialPoints(Data[,c("UTM.Easting", "UTM.Northing")], proj4string=CRS("+proj=utm +zone=12S +datum=WGS84")) 
  spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
  Coords<- as.data.frame(spgeo@coords)
  names(Coords)<- c("Longitude", "Latitude")
  
  DATA<- cbind(Data, Coords)
  write.csv(DATA, paste0("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\UT-PM2.5-", year,".csv"),
            row.names = FALSE)
}


extract_UT_data("C:\\Users\\ellen\\Downloads\\2015-PM2.5.csv", 2015)
extract_UT_data("C:\\Users\\ellen\\Downloads\\2016-PM2.5.csv", 2016)
extract_UT_data("C:\\Users\\ellen\\Downloads\\2017-PM2.5.csv", 2017)
extract_UT_data("C:\\Users\\ellen\\Downloads\\2018-PM2.5.csv", 2018)


