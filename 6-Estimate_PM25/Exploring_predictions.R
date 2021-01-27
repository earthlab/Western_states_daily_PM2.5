library(stringr)
library(ggplot2)
library(Hmisc)
library(FNN)
library(dplyr)

load("~/ML_inputs.RData")
input_locs<- distinct(DF_no_CMAQ[,c("Lon", "Lat")])

Stat<- read.csv("~/FINAL_Stat.csv")

State<- c("nevada", "colorado", "utah", "new mexico", "arizona",
          "washington", "oregon", "idaho", "montana", "wyoming", "california")

#Visually located census tracts downtown for each city:
CTs<- data.frame(Place=c("Las Vegas", "Denver", "Salt Lake", "Albuquerque", "Phoenix",
                         "Seattle", "Portland", "Boise", "Missoula", "Cheyenne", "San Francisco", "Los Angeles"), 
                 County_FIPS=c(3, 31, 35, 1, 13, 33, 51, 1,
                               63, 21, 75, 37), Tract_code=c(000523,000905,102000,000119,109002,
                                                             001100,001900,000303,001000,000800,030301, 212410))

#Now find closest monitoring locations:
lons<- c()
lats<- c()

use<- c(1, 3, 1, 2, 2, 3, 1, 3, 1, 3, 1, 1) #closest with more than 3000 obs

for(i in 1:11){
  print(State[i])
  
  nearest_ind<- get.knnx(input_locs, Stat[which((Stat$State == State[i])&(Stat$County_FIPS == CTs[i,2])&
                                                  (Stat$Tract_code == CTs[i,3])),c("Lon", "Lat")], k = 3)$nn.index
  
  lons<- append(lons, input_locs[nearest_ind[use[which(State == State[i])]], "Lon"])
  lats<- append(lats, input_locs[nearest_ind[use[which(State == State[i])]], "Lat"])
}

## Add data for second CA location:
nearest_ind<- get.knnx(input_locs, Stat[which((Stat$State == "california")&(Stat$County_FIPS == CTs[12,2])&
                                                (Stat$Tract_code == CTs[12,3])),c("Lon", "Lat")], k = 3)$nn.index
lons<- append(lons, input_locs[nearest_ind[use[12]], "Lon"])
lats<- append(lats, input_locs[nearest_ind[use[12]], "Lat"])


#Get closest predictions to these monitor locations:
Nearest_ind<- get.knnx(Stat[,c("Lon", "Lat")], data.frame(lons, lats), k = 1)$nn.index
pred_lons<- Stat[Nearest_ind, "Lon"]
pred_lats<- Stat[Nearest_ind, "Lat"]


##Make time series, without CMAQ:

pdf("~/CA-NV-CA_time-series_final.pdf", 8, 11) ## Change file name depending on the states for which you're making a figure
par(mfrow=c(3,1))

for(i in c(1,11,12)){ ## Change between c(1, 11, 12), c(2, 9, 10), c(3, 4, 5), and c(6, 7, 8) to make Figures 5-8
  if(i == 12){
    s<- "california"
    print(s)
  }else{
    s<- State[i]
    print(s)
    load(paste0("Predictions/Ensemble_preds_no_CMAQ_", s, ".RData"))
  }
  
  DF<- as.data.frame(DF)
  DF$Ens_pred[which(DF$Ens_pred < 0)]<- 0
  
  state_pos<- i
  this_data<- DF[which((DF$Lon == round(pred_lons[state_pos],4))
                       &(DF$Lat == pred_lats[state_pos])),]
  these_obs<- DF_no_CMAQ[which((DF_no_CMAQ$Lon == round(lons[state_pos],4))&(DF_no_CMAQ$Lat == lats[state_pos])), c("PM2.5_Obs", "Date")]
  
  these_obs<- aggregate(PM2.5_Obs ~ Date, data = these_obs, mean)
  
  both<- inner_join(this_data[,c("Date", "Ens_pred")], these_obs, by = "Date")
  
  my_max<-  max(max(these_obs$PM2.5_Obs), max(this_data$Ens_pred))
  plot(these_obs$Date, these_obs$PM2.5_Obs, 
       xlab = "Date", ylab = "", pch = 16, cex = 0.75, 
       ylim=c(0, my_max), cex.lab = 1.5)
  title(ylab=expression(paste("PM"[2.5], " Estimates and Observations (", mu, "g/", m^3, ")")),
        line = 2, cex.lab = 1.5)
  points(this_data$Date, this_data$Ens_pred, col = "red", pch = 16, cex = 0.75)
  if(s == "new mexico"){
    this_state<- "New Mexico"
  }else{
    this_state<- capitalize(s)
  }
  text(x = min(these_obs$Date) + 900, y = 0.9*my_max, cex = 1.5,
    paste(CTs[state_pos, "Place"],this_state, sep=", "))
  rmse<- round(sqrt(mean((both$Ens_pred - both$PM2.5_Obs)^2)),1)
  r2<-  round((cor(both$Ens_pred, both$PM2.5_Obs))^2,3)
  
  text(x = min(these_obs$Date) + 800, y = 0.8*my_max, cex = 1.5,
       bquote(RMSE == .(rmse) ~mu* "g/"* m^3* "," ~ R^2 == .(r2) ))
  
}
dev.off()


##############################
### Calculate correlations for each monitor: 
Preds<- c()
Obs<- c()
for(s in State){
  print(s)
  obs_locs<- distinct(DF_with_CMAQ[which(DF_with_CMAQ$State == s),c("Lon", "Lat")])
  nearest_inds<- get.knnx(Stat[,c("Lon", "Lat")], obs_locs, k = 1)$nn.index
  pred_ref<- Stat[nearest_inds, c("Lon", "Lat")]
  pred_ref$Index<- 1:dim(pred_ref)[1]
  obs_locs$Index<- 1:dim(pred_ref)[1]
  obs_locs$Dist<- get.knnx(Stat[,c("Lon", "Lat")], obs_locs[,1:2], k = 1)$nn.dist
  these_obs<- inner_join(obs_locs, DF_with_CMAQ[,c("Lon", "Lat", "Date", "PM2.5_Obs")])
  
  load(paste0("~/Predictions/Ensemble_preds_with_CMAQ_", s, "_2.RData"))
  pred_ref$Lon<- round(pred_ref$Lon, 4)
  these_preds<- inner_join(pred_ref, DF[,c("Lon", "Lat", "Date", "Ens_pred")]) 
  
  Both<- inner_join(these_obs, these_preds, by = c("Index", "Date"))
  num_obs<- as.vector(table(Both$Index))
  enough_obs<- Both[which(Both$Index %in% which(num_obs > 0)),]
  enough_obs$Ens_pred[which(enough_obs$Ens_pred < 0)]<- 0
  
  Preds<- append(Preds, enough_obs$Ens_pred)
  Obs<- append(Obs, enough_obs$PM2.5_Obs)
  print(sqrt(mean((enough_obs$PM2.5_Obs - enough_obs$Ens_pred)^2)))
  print((cor(enough_obs$PM2.5_Obs, enough_obs$Ens_pred))^2)
  dists<- get.knnx(Stat[,c("Lon", "Lat")], obs_locs[,1:2], k = 1)$nn.dist
  print(summary(dists))
}

sqrt(mean((Obs - Preds)^2))
(cor(Obs, Preds))^2

rm(list=setdiff(ls(), c("Preds", "Obs")))
save.image("~/Monitor_level_values.RData")

#FVO plots:
png(filename = "FVO_with_CMAQ.png")
plot(Obs, Preds, main = "Nearest Prediction to Each Observation, Without CMAQ",
     xlab = "Observations (ug/m^3)", ylab = "Predictions (ug/m^3)")
abline(0,1)
dev.off()
