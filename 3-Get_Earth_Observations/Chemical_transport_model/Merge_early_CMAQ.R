### Author: Ellen Considine

load("CMAQ_08_12.RData")

dim(X1) # 97416  1827

#Each column is a day between 2008-01-01 and 2012-12-31
#Each row is a lat-lon location

#MERGE with other data:
library("FNN") # has get.kknx function

merge_CMAQ<- function(row){
  
  date<- as.character(row[[1]])
  data<- X1[,date]
  
  lon<- as.numeric(sapply(names(data), function(x){strsplit(x,"_")[[1]][1]}))
  names(lon)<- c()
  lat<- as.numeric(sapply(names(data), function(x){strsplit(x,"_")[[1]][2]}))
  names(lat)<- c()
  Lon<- round(lon, 4)
  Lat<- round(lat, 4)
  # plot(Lon, Lat, cex = 0.01, pch = 16)
  
  Data<- cbind(Lon, Lat, PM2.5 = as.vector(data))
  #Get nearest neighbor value:
  NN_val<- Data[get.knnx(Data[,c("Lon", "Lat")], 
                         t(c(row[[2]], row[[3]])), 
                         k = 1)$nn.index, 3]
  
  return(as.vector(NN_val))
}

merge_many_CMAQ<- function(pos, N){
  CMAQ<- c()
  for(i in pos){
    CMAQ<- append(CMAQ, merge_CMAQ(early_data[i,c("Date","Lon","Lat")]))
  }
  write.csv(CMAQ, paste0("CMAQ_", N, ".csv"), row.names = FALSE)
  return(CMAQ)
}

#Read in data:
load("data.RData")

early_data<- DATA[which(DATA$Year %in% 2008:2012),]

#Run through data set, parallelized:
library(future.apply)

ncores<- detectCores()-1
uniq_seq<- seq(1,dim(early_data)[1], length.out = ncores + 1)
loc_list<- c()
for(j in 1:ncores){
  uniq_locs<- round(uniq_seq[j]):round(uniq_seq[j+1]-1)
  loc_list<- append(loc_list, list(uniq_locs))
}

options(future.globals.maxSize= 2000*1024^2)

plan(multiprocess, workers = ncores)

this_list<- future_lapply(1:length(loc_list), function(j){merge_many_CMAQ(loc_list[[j]],j)})

save.image("Merged_CMAQ.RData")

load("Merged_CMAQ.RData")

all_CMAQ<- unlist(this_list)

library("FNN") # has get.kknx function

last<- merge_CMAQ(early_data[dim(early_data)[1],c("Date","Lon","Lat")])
all_CMAQ<- append(all_CMAQ, last)

Early_data<- cbind(early_data, all_CMAQ)

Early_data$Mid_Study<- (Early_data$Year > 2011)&(Early_data$Year <= 2015)
Early_data$Late_Study<- Early_data$Year > 2015
Early_data$Year<- as.factor(Early_data$Year)
Early_data$Int_Year_State_Mid<- interaction(Early_data$Mid_Study, Early_data$Region)
Early_data$Int_Year_State_Late<- interaction(Early_data$Late_Study, Early_data$Region)

write.csv(Early_data, "Data_with_CMAQ_08-12.csv", row.names = FALSE)

