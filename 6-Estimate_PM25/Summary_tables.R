library(stringr)
library(dplyr)

State<- c("nevada", "colorado", "utah", "new mexico", "arizona",
          "washington", "oregon", "idaho", "montana", "wyoming", "california")

season_ref<- data.frame(Season = c(rep("spring", 3), rep("summer", 3), 
                        rep("fall", 3), rep("winter", 3)),
                        Month = c(3:12,1:2))

Year<- c()
Season<- c()
Preds<- c()

sink("Summary_tables_state.txt")
for(s in State){
  load(paste0("~/Predictions/Ensemble_preds_no_CMAQ_", s, "_2.RData"))
  print(paste("State:", s))
  print(paste("N =", dim(DF)[1]))
  print(paste("Mean =", mean(DF$Ens_pred)))
  print(quantile(DF$Ens_pred))
  print(paste("Percent negative =", round(sum(DF$Ens_pred<0)/length(DF$Ens_pred),5)))

  DF$Year<- sapply(DF$Date, function(x) as.numeric(strsplit(as.character(x),"-")[[1]][1]))
  Year<- append(Year, DF$Year)
  DF$Month<- sapply(DF$Date, function(x) as.numeric(strsplit(as.character(x),"-")[[1]][2]))
  
  DF<- inner_join(DF[,c("Month", "Ens_pred")], season_ref, by = "Month")
  Season<- append(Season, DF$Season)
  Preds<- append(Preds, DF$Ens_pred)
  
  rm(DF)
  
}
sink()
save.image("For_summary_tables.RData")

sink("Summary_tables_season.txt")
seasons<- c("fall", "spring", "summer", "winter")
for(S in c(2,3,1,4)){
  print(paste("Season:", seasons[S]))
  these<- which(Season == S)
  print(paste("N =", length(these)))
  print(paste("Mean =", mean(Preds[these])))
  print(quantile(Preds[these]))
  print(paste("Percent negative =", round(sum(Preds[these]<0)/length(these),5)))
  
}
sink()

sink("Summary_tables_year.txt")
for(y in 2008:2018){
  print(paste("Year:", y))
  these<- which(Year == y)
  print(paste("N =", length(these)))
  print(paste("Mean =", mean(Preds[these])))
  print(quantile(Preds[these]))
  print(paste("Percent negative =", round(sum(Preds[these]<0)/length(these),5)))
  
}
sink()




