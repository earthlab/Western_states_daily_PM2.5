##Libraries:
library(dplyr)
library(multidplyr)
library(lubridate)
library(stringr)
library(future.apply)
library(parallel)
library(stringr)

Stat<- read.csv("All_stationary.csv")

## Get temporal variables:

dates<- seq.Date(as.Date(paste0("2008-01-01")), as.Date(paste0("2018-12-31")), by = "day")
n_days<- length(dates)

Year<- sapply(dates, function(y) as.numeric(format(y, "%Y")))
Month<- sapply(dates, function(y) as.numeric(format(y, "%m")))
Day<- sapply(dates, function(y) as.numeric(format(y, "%d")))
DayOfWeek<- sapply(dates, function(x){wday(as.POSIXct(x))})
Season<- sapply(Month, function(m){ifelse(m %in% c(3:5), "spring",
                                                ifelse(m %in% c(6:8), "summer",
                                                       ifelse(m %in% c(9:11), "fall", "winter")))})
DOY<- apply(cbind(Year, Month, Day), MARGIN = 1, FUN = DOY)
CosDOW<- cos(2*pi*DayOfWeek/7)
CosDOY<- cos(2*pi*DOY/365)
CosMonth<- cos(2*pi*Month/12)
Mid_Study<- (Year > 2012)&(Year <= 2016)
Late_Study<- Year > 2016

DF<- data.frame(Date=dates, Year, Season, DayOfWeek, CosDOW, CosDOY, CosMonth, Mid_Study, Late_Study)

write.csv(DF, "Temporal_variables.csv", row.names=FALSE)

##Get region:

State<- c("california", "nevada", "colorado", "utah", "new mexico", "arizona",
          "washington", "oregon", "idaho", "montana", "wyoming")

Region<- c(rep("Southwest",2), rep("Four Corners", 4), rep("Northwest",2), 
           rep("Northern Mountain States",3))

Regions<- data.frame(State, Region)
write.csv(Regions, "Regions.csv", row.names=FALSE)

##Reduce active fire variables:

agg_AF<- function(i, DF){
  lag<- DF[,sapply(c("25km", "50km", "100km", "500km"), function(x) paste0("Fires_lag",i, "_", x))]
  lag[,1]<- lag[,1]*(1/25)
  lag[,2]<- (lag[,2] - lag[,1])*(1/50)
  lag[,3]<- (lag[,3] - lag[,2])*(1/100)
  lag[,4]<- (lag[,4] - lag[,3])*(1/500)
  return(as.vector(rowSums(lag)))
}

for(state in State[3:11]){
  AF<- read.csv(paste0("~/State_data/AF_", state, ".csv"))[,c("Lon", "Lat", "Date",
                                                              sapply(0:7, function(x){
                                                                sapply(c(25, 50, 100, 500),
                                                                       function(y){
                                                                         paste0("Fires_lag",x,"_",
                                                                                y,"km")
                                                                       })}))]
  Lag0<- agg_AF(0, AF)
  Lag1<- agg_AF(1, AF)
  Lag2<- agg_AF(2, AF)
  Lag3<- agg_AF(3, AF)
  Lag4<- agg_AF(4, AF)
  Lag5<- agg_AF(5, AF)
  Lag6<- agg_AF(6, AF)
  Lag7<- agg_AF(7, AF)
  
  final_DF<- data.frame(AF[,c("Lon", "Lat", "Date")], Lag0, Lag1, Lag2, Lag3, Lag4, Lag5, Lag6, Lag7)
  write.csv(final_DF, paste0("~/State_data/AF_FINAL_", state, ".csv"))
  print(state)
}


