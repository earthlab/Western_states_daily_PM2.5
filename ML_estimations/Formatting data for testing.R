# Load the required libraries
library(caret)
library(caretEnsemble)

mayData<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_PM25_Step4_part_e_de_duplicated_aves_compiled_2019-05-15.csv")
#dim() -> 280136, 75

#Variables
names(mayData)

DATA<- mayData[ , -which(names(mayData) %in% c("NewDatum","Date","A_100", "C_100","A_250", "C_250","A_500", "C_500", 
                                               "A_1000", "C_1000", "TimeZone", "DecimalDatewYear", "DecimalDate"))]

#Obtain state from lat-lon: https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
  # Also reference lat-lon values from: https://gist.github.com/jakebathman/719e8416191ba14bb6e700fc2d5fccc5

library(sp)
library(maps)
library(maptools)

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  NApos<- which(is.na(indices))
  
  for(p in NApos){
    if(pointsDF[p,2]<= 41.9952){
      if(pointsDF[p,1] <= -114.1315){
        indices[p]<- which(stateNames == "california")
      }else if(pointsDF[p,1] <= -109.0475){
        indices[p]<- which(stateNames == "arizona")
      }else{
        indices[p]<- which(stateNames == "new mexico")
      }
    }else if((pointsDF[p,2] > 41.9952)& (pointsDF[p,2]<= 46.2891) ){
        indices[p]<- which(stateNames == "oregon")
    }else if(pointsDF[p,2] > 46.2891){
      if(pointsDF[p,1] <= -117.2372){
        indices[p]<- which(stateNames == "washington")
      }else if(pointsDF[p,1] <= -116.0458){
        indices[p]<- which(stateNames == "idaho")
      }else{
        indices[p]<- which(stateNames == "montana")
      }
    }
  }
  
  # Return the state names of the Polygons object containing each point
  stateNames[indices]
}

DATA$State<- latlong2state(as.data.frame(DATA[,c("Longitude", "Latitude")]))

# windows()
# map("state")
# points(DATA$Longitude, DATA$Latitude, col = as.factor(DATA$State), pch = 16)

names(DATA)

#Get day of year

non_leap<- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
leap<- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

DOY<- function(YMD){ # YMD is a vector containing year, month, day
  
  #Figure out if it is a leap year or not
  if(YMD[1] %in% c(2008, 2012, 2016)){
    nums<- leap
  }else{
    nums<- non_leap
  }
  
  if(YMD[2] == 1){
    return(YMD[3])
  }else{
    return(sum(nums[1:(YMD[2]-1)]) + YMD[3])
  }
}

my_DOY<- apply(DATA[,c("Year", "Month", "Day")], MARGIN = 1, FUN = DOY)

DATA$CosDOY<- cos(2*pi*my_DOY/365)

DATA<- DATA[,-which(names(DATA) %in% c("Year", "Day"))]

#Get rid of NAs:
my_inds<- apply(DATA, MARGIN = 1, FUN = function(x){sum(is.na(x)) == 0})

write.csv(DATA[my_inds,], "C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_5-15-2019.csv", row.names = FALSE)

############################################################################
DATA<- read.csv("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_6-10-2019.csv") #update to 6-10 after running cubist tests

badVars<- c( "APCP.surface",  "WEASD.surface")
DATA<- DATA[-c(apply(DATA, MARGIN = 1, FUN = function(x){sum(is.na(x)) > 0}), which(DATA$HPBL.surface == min(DATA$HPBL.surface))),
            -which(names(DATA) %in% badVars)]

set_2010<- DATA[which(DATA$Year == 2010),] #132860, 71
set_2017<- DATA[which(DATA$Year == 2017),] #125288, 71

set_2008<- DATA[which(DATA$Year == 2008),] #112757, 71
set_2009<- DATA[which(DATA$Year == 2009),] #121373, 71
set_2011<- DATA[which(DATA$Year == 2011),] #137338, 71
set_2012<- DATA[which(DATA$Year == 2012),] #145372, 71
set_2013<- DATA[which(DATA$Year == 2013),] #151029, 71
set_2014<- DATA[which(DATA$Year == 2014),] #150334, 71
set_2015<- DATA[which(DATA$Year == 2015),] #104064, 71
set_2016<- DATA[which(DATA$Year == 2016),] #106862, 71

#Separate into fire, non-fire for all years separately:
write_FNF<- function(year){
  Set<- DATA[which(DATA$Year == year),] #get(paste0("set_", year))
  fire_pos<- which(rowSums(Set[,sapply(names(Set), FUN = function(x){str_detect(x, "Fire_Count")})]) > 0)
  Fire<- Set[fire_pos,]
  row.names(Fire)<- c()
  
  NotFire<- Set[-fire_pos,-which(sapply(names(Set), FUN = function(x){str_detect(x, "Fire_Count")}))]
  row.names(NotFire)<- c()
  
  write.csv(Fire, paste0("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Fire_", year, ".csv"), row.names = FALSE)
  write.csv(NotFire, paste0("C:\\Users\\elco2649\\Documents\\Machine Learning\\ML_input_Not-Fire_", year, ".csv"), row.names = FALSE)
  }

for(y in 2008:2017){
  print(y)
  write_FNF(y)
}




