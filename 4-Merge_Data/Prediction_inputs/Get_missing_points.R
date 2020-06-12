library("FNN")
library("fields")

Stat1<- read.csv("All_stationary.csv")
Stat2<- read.csv("All_stationary2.csv")
Stat<- rbind(Stat1, Stat2)
rm(list=c("Stat1", "Stat2"))

missing<- read.csv("Final_missing_stationary.csv")

#KNN:
KNN<- data.frame(KNN_dist=double(), Lon=double(), Lat=double(), State_FIPS=double(), County_FIPS=double(),
                 Tract_code=double(), ZCTA5_code=double())

for(i in 1:dim(missing)[1]){
  knn_ind<- get.knnx(Stat[,c("Lon", "Lat")], 
                     t(c(missing[i,"Lon"], missing[i,"Lat"])), k = 1)$nn.index
  dist<- rdist.earth(Stat[knn_ind,c("Lon", "Lat")], 
                     t(c(missing[i,"Lon"], missing[i,"Lat"])), miles=TRUE)
  KNN[i,]<- c(dist, Stat[knn_ind, c("Lon", "Lat", "State_FIPS", "County_FIPS", 
                                    "Tract_code", "ZCTA5_code")])
}





