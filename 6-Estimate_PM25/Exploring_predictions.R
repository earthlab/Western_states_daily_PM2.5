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

# LOIs<- data.frame(Place=c("Las Vegas", "Denver", "Salt Lake", "Albuquerque", "Phoenix",
#                           "Seattle", "Portland", "Boise", "Missoula", "Cheyenne", "San Francisco"), 
#                   County_FIPS=c(3, 31, 35, 1, 13, 33, 51, 15,
#                                 63, 21, 75))
CTs<- data.frame(Place=c("Las Vegas", "Denver", "Salt Lake", "Albuquerque", "Phoenix",
                         "Seattle", "Portland", "Boise", "Missoula", "Cheyenne", "San Francisco"), 
                 County_FIPS=c(3, 31, 35, 1, 13, 33, 51, 15,
                               63, 21, 75), Tract_code=c(000106,000905,112002,000119,115500,
                                                         011700,001900,950200,001000,000600,030301))

lons<- c()
lats<- c()

# county_lons<- c()
# county_lats<- c()

for(i in 1:11){
  # county_lons<- append(county_lons, Stat[which((Stat$State == State[i])&(Stat$County_FIPS == LOIs[i,2])&
  #              is.na(Stat$Tract_code) & is.na(Stat$ZCTA5_code)),"Lon"])
  # county_lats<- append(county_lats, Stat[which((Stat$State == State[i])&(Stat$County_FIPS == LOIs[i,2])&
  #                                                is.na(Stat$Tract_code) & is.na(Stat$ZCTA5_code)),"Lat"])
  
  # nearest_ind<- get.knnx(input_locs, Stat[which((Stat$State == State[i])&(Stat$County_FIPS == LOIs[i,2])&
  #                                   is.na(Stat$Tract_code) & is.na(Stat$ZCTA5_code)),c("Lon", "Lat")], k = 1)$nn.index
  nearest_ind<- get.knnx(input_locs, Stat[which((Stat$State == State[i])&(Stat$County_FIPS == CTs[i,2])&
                                                  (Stat$Tract_code == CTs[i,3])),c("Lon", "Lat")], k = 1)$nn.index
  
  lons<- append(lons, input_locs[nearest_ind, "Lon"])
  lats<- append(lats, input_locs[nearest_ind, "Lat"])
}

##Without CMAQ:
for(s in State[-8]){
  print(s)
  load(paste0("~/Predictions/Ensemble_preds_no_CMAQ_", s, "_2.RData"))
  DF$Month<- sapply(DF$Date, function(x) as.numeric(strsplit(as.character(x),"-")[[1]][2]))
  DF$Month<- as.factor(DF$Month)
  
  DF$Year<- sapply(DF$Date, function(x) as.numeric(strsplit(as.character(x),"-")[[1]][1]))
  DF$Year<- as.factor(DF$Year)
  
  DF<- as.data.frame(DF)
  DF$Ens_pred[which(DF$Ens_pred < 0)]<- 0
  
  # png(filename = paste0("~/Plots/Pred_vs_Month_", s, ".png"))
  # p <- ggplot(DF, aes(x=Month, y=Ens_pred)) + 
  #   geom_boxplot() + ggtitle(toupper(s)) + labs(y = "Ensemble Estimate of PM2.5")
  # print(p)
  # dev.off()
  # 
  # png(filename = paste0("~/Plots/Pred_vs_Year_", s, ".png"))
  # p <- ggplot(DF, aes(x=Year, y=Ens_pred)) + 
  #   geom_boxplot() + ggtitle(toupper(s)) + labs(y = "Ensemble Estimate of PM2.5")
  # print(p)
  # dev.off()
  
  # county_of_interest<- as.vector(LOIs[which(State == s),])
  county_of_interest<- as.vector(CTs[which(State == s),])
  # this_data<- DF[which((DF$County_FIPS == county_of_interest[1,2])&
  #                        is.na(DF$Tract_code) & is.na(DF$ZCTA5_code)),]
  this_data<- DF[which((DF$County_FIPS == county_of_interest[1,2])&
                         (DF$Tract_code == county_of_interest[1,3])),]
  
  these_obs<- DF_no_CMAQ[which((DF_no_CMAQ$Lon == round(lons[which(State == s)],4))&(DF_no_CMAQ$Lat == lats[which(State == s)])), c("PM2.5_Obs", "Date")]
  
  png(filename = paste0("~/Plots/", county_of_interest[1,1],"_", s, "_time-series_closest-CT.png"), width = 960)
  # p <- ggplot(this_data, aes(x=Date, y=Ens_pred)) + geom_point() + ggtitle(paste(county_of_interest[1,1],capitalize(s), sep=", ")) + labs(y = "Ensemble Estimate of PM2.5") + geom_point(these_obs, aes(x=Date, y=PM2.5_Obs), colour="red")
  # print(p)
  plot(this_data$Date, this_data$Ens_pred, main = paste(county_of_interest[1,1],capitalize(s), "no CMAQ", sep=", "), 
       xlab = "Date", ylab = "Ensemble Estimate of PM2.5", pch = 16, cex = 0.75)
  points(these_obs$Date, these_obs$PM2.5_Obs, col = "red", pch = 16, cex = 0.75)
  dev.off()
  
}

##With CMAQ:

for(s in State){
  print(s)
  load(paste0("~/Predictions/Ensemble_preds_with_CMAQ_", s, "_2.RData"))
  DF$Month<- sapply(DF$Date, function(x) as.numeric(strsplit(as.character(x),"-")[[1]][2]))
  DF$Month<- as.factor(DF$Month)
  
  DF$Year<- sapply(DF$Date, function(x) as.numeric(strsplit(as.character(x),"-")[[1]][1]))
  DF$Year<- as.factor(DF$Year)
  
  DF<- as.data.frame(DF)
  DF$Ens_pred[which(DF$Ens_pred < 0)]<- 0
  
  # png(filename = paste0("~/Plots/Pred_vs_Month_CMAQ_", s, ".png"))
  # p <- ggplot(DF, aes(x=Month, y=Ens_pred)) + 
  #   geom_boxplot() + ggtitle(toupper(s)) + labs(y = "Ensemble Estimate of PM2.5")
  # print(p)
  # dev.off()
  # 
  # png(filename = paste0("~/Plots/Pred_vs_Year_CMAQ_", s, ".png"))
  # p <- ggplot(DF, aes(x=Year, y=Ens_pred)) + 
  #   geom_boxplot() + ggtitle(toupper(s)) + labs(y = "Ensemble Estimate of PM2.5")
  # print(p)
  # dev.off()
  
  county_of_interest<- as.vector(LOIs[which(State == s),])
  this_data<- DF[which((DF$County_FIPS == county_of_interest[1,2])&
                         is.na(DF$Tract_code) & is.na(DF$ZCTA5_code)),]
  
  these_obs<- DF_with_CMAQ[which((DF_with_CMAQ$Lon == lons[which(State == s)])&(DF_with_CMAQ$Lat == lats[which(State == s)])), c("PM2.5_Obs", "Date")]
  
  png(filename = paste0("~/Plots/", county_of_interest[1,1],"_CMAQ_", s, "_time-series_2.png"), width = 960)
  # p <- ggplot(this_data, aes(x=Date, y=Ens_pred)) + geom_point() + ggtitle(paste(county_of_interest[1,1],capitalize(s), sep=", ")) + labs(y = "Ensemble Estimate of PM2.5")
  # print(p)
  plot(this_data$Date, this_data$Ens_pred, main = paste(county_of_interest[1,1],capitalize(s), "with CMAQ", sep=", "), 
       xlab = "Date", ylab = "Ensemble Estimate of PM2.5", pch = 16, cex = 0.75)
  points(these_obs$Date, these_obs$PM2.5_Obs, col = "red", pch = 16, cex = 0.75)
  dev.off()
  
}
