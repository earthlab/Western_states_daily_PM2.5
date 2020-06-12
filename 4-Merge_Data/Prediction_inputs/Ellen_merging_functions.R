
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

Years<- 2008:2018

write_state<- function(DF, name){
  # #For AF:
  # Stat$Lon<- round(Stat$Lon, 4)
  # Stat$Lat<- round(Stat$Lat, 4)
  # DF$Lon<- round(DF$Lon, 4)
  # DF$Lat<- round(DF$Lat, 4)
  # # DF$Index<- 1:dim(DF)[1]
  # 
  merged<- inner_join(Stat[,c(1:6,16)], DF, by = c("Lon", "Lat"))
  # merged<- DF
  
  states<- unique(merged$State)

  for(s in states){
    if(1==1){
      # inds<- merged[which(merged$State==s), "Index"]
      write.csv(merged[which(merged$State==s),], 
                paste0("~/State_data/",name,"_",s,".csv"), row.names = FALSE)
    }
  }
}

library(sp)
library(maps)
library(maptools)

west<- c("wyoming", "california", "utah", "arizona", "new mexico", "oregon", "washington",
         "nevada", "colorado", "montana", "idaho")


latlong2state <- function(pointsDF){
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
  #Fill in points based on this: https://gist.github.com/jakebathman/719e8416191ba14bb6e700fc2d5fccc5
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

  return_obj<- stateNames[indices]
  for(r in 1:length(return_obj)){
    if(! return_obj[r] %in% west){
      if(pointsDF$Lat[r] >= 44.3563){
        return_obj[r]<- "montana"
      }else if(pointsDF$Lat[r] >= 40.9986){
        return_obj[r]<- "wyoming"
      }else if(pointsDF$Lat[r] >= 36.9949){
        return_obj[r]<- "colorado"
      }else{
        return_obj[r]<- "new mexico"
      }
    }
  }

  # Return the state names of the Polygons object containing each point
  return_obj
}

##CMAQ:
library("FNN")

knn_per_state<- function(state, DF){
  these_locs<- Stat[which(Stat$State == state),c("Lon", "Lat")]
  KNN_locs<- data.frame(Lon=double(), Lat=double(), CM_row=double())
  for(l in 1:dim(these_locs)[1]){
    data<- DF[,1]
    lon<- as.numeric(sapply(names(data), function(x){strsplit(x,"_")[[1]][1]}))
    names(lon)<- c()
    lat<- as.numeric(sapply(names(data), function(x){strsplit(x,"_")[[1]][2]}))
    names(lat)<- c()
    Lon<- round(lon, 5)
    Lat<- round(lat, 5)
    Data<- cbind(Lon, Lat)
    
    row<- these_locs[l,]
    knn_ind<- get.knnx(Data[,c("Lon", "Lat")], t(c(row[[1]], row[[2]])), k = 1)$nn.index
    KNN_locs[l,]<- c(row[[1]], row[[2]], knn_ind)
  }
  write.csv(KNN_locs, paste0("~/CMAQ_data/CMAQ_h_locs_", state, ".csv"), row.names=FALSE)
  print(state)
}

#---------------------------

dates<- seq.Date(as.Date(paste0("2008-01-01")), as.Date(paste0("2018-12-31")), by = "day")
n_days<- length(dates)

get_CMAQ_08_12<- function(state){
  KNN_locs<- read.csv(paste0("~/CMAQ_data/CMAQ_h_locs_", state, ".csv"))
  
  CM_df<- data.frame(Lon = double(), Lat = double(), Date = as.Date(character()), 
                     CMAQ_pm = double())

  for(i in 1:dim(KNN_locs)[1]){
    CM_df<- rbind(CM_df, data.frame(Lon = rep(KNN_locs[i,1],n_days), Lat = rep(KNN_locs[i,2],n_days),
                                    Date = dates,
                                    CMAQ_pm = round(X1[KNN_locs[i,3],],4)))
  }

  write.csv(CM_df, paste0("~/CMAQ_data/CMAQ_h_", state, ".csv"), row.names = FALSE)
  print(state)
}

##Second batch of CMAQ (2013-2016):


knn_per_state2<- function(state, DF){ #DF = CM_locs
  these_locs<- Stat[which(Stat$State == state),c("Lon", "Lat")]
  KNN_locs<- data.frame(Lon=double(), Lat=double(), CM_row=double())
  for(l in 1:dim(these_locs)[1]){
    row<- these_locs[l,]
    knn_ind<- get.knnx(DF[,c("Lon", "Lat")], t(c(row[[1]], row[[2]])), k = 1)$nn.index
    KNN_locs[l,]<- c(row[[1]], row[[2]], knn_ind)
  }
  write.csv(KNN_locs, paste0("~/CMAQ_data/CMAQ_h2_locs_", state, ".csv"), row.names=FALSE)
  print(state)
}


dates<- seq.Date(as.Date(paste0("2008-01-01")), as.Date(paste0("2012-12-31")), by = "day")
n_days<- length(dates)

get_CMAQ_13_16<- function(state, DF, big_DF, year){#DF = CM_locs
  KNN_locs<- read.csv(paste0("~/CMAQ_data/CMAQ_h2_locs_", state, ".csv"))
  Lon<- DF$Lon
  Lat<- DF$Lat
  Data<- cbind(Lon, Lat)
  
  big_Data<- as.data.frame(big_DF)
  
  CM_df<- data.frame(Lon = double(), Lat = double(), Date = as.Date(character()), 
                     CMAQ_pm = double())
  
  for(i in 1:dim(KNN_locs)[1]){
    new<- big_Data[which((big_Data$Lon==Data[KNN_locs[i,3],"Lon"])&
                           (big_Data$Lat==Data[KNN_locs[i,3],"Lat"])),
                   c("Lon", "Lat", "Date", "Conc")]
    names(new)[4]<- "CMAQ_pm"
    CM_df<- rbind(CM_df, new)
  }
  
  write.csv(CM_df, paste0("~/CMAQ_data/CMAQ_h2_", state, "_", year, ".csv"), row.names = FALSE)
  print(state)
}
