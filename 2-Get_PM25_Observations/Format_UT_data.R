#After retrieving the hourly value CSVs from http://www.airmonitoring.utah.gov/dataarchive/archpm25.htm

library(rgdal)

#All station info:
stations<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\UT-PM2.5-stations.csv")

extract_UT_data<- function(filename, year){
  if(year %in% c(2008, 2012, 2016)){
    numdays<- 366
  }else{
    numdays<- 365
  }
  
  #Read in data
  S1<- names(read.csv(filename))[-1]
  raw1<- as.matrix(read.csv(filename, skip = 2))
  #Prepare for merging (later on)
  matched<- match(S1, stations$Name)
  
  #Get data in useable format
  days<- sort(rep(1:numdays, 24)) #365 for 2016
  station.ID<- as.vector(sapply(matched, function(y){rep(stations[y,1],numdays*24)})) #365 for 2016
  all_data<- as.numeric(as.vector(raw1[,-1]))
  ALL<- cbind(all_data, station.ID, days)
  ALL<- ALL[-which(ALL[,1] < 0),]
  #Calculate number of non-negative observations per 
  Freq<- table(interaction(ALL[,3], ALL[,2]))
  #Aggregate into 24-hour average
  daily<- aggregate(all_data ~ days + station.ID, data = ALL, FUN = mean)
  Freq2<- interaction(daily$days, daily$station.ID)
  pos<- which(as.character(names(Freq)) %in% as.character(Freq2))
  Freq3<- Freq[pos]
  daily$Daily<- sapply(daily$days, function(y){format(as.Date(y, origin = paste0(year-1,"-12-31")), "%Y-%m-%d")})
  data<- daily[,c(2,4,3)]
  names(data)<- c("Station", "Date", "PM2.5")
  data$Num_Obs<- Freq3

  #Merge with station information
  Data<- merge(data, stations, by.x = "Station", by.y = names(stations)[1])
  
  #Get lat,lon from UTM
  sputm <- SpatialPoints(Data[,c("UTM.Easting", "UTM.Northing")], proj4string=CRS("+proj=utm +zone=12S +datum=WGS84")) 
  spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
  Coords<- as.data.frame(spgeo@coords)
  names(Coords)<- c("Longitude", "Latitude")
  
  DATA<- cbind(Data, Coords)
  write.csv(DATA, paste0("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Spatial_Processing\\UT-PM2.5-", year,".csv"),
            row.names = FALSE)
}

#Run 
for(y in 2008:2018){
  extract_UT_data(paste0("C:\\Users\\ellen\\Downloads\\", y, "-PM2.5.csv"), y)
}
