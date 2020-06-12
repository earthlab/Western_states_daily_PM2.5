source("~/Scripts/Data_prep.R")
rm(list=setdiff(ls(), c("Both", "CMAQ_Both"))) 

table_format<- data.frame(Mean=double(), Min=double(),
                          Q1=double(), Median=double(), Q3=double(), Max=double())

states<- sort(unique(Both$State))
years<- 2008:2018
seasons<- sort(unique(Both$Season))

by_state<- table_format
for(s in 1:length(states)){
  PM<- Both[which(Both$State == states[s]), "PM2.5_Obs"]
  by_state[s,]<- c(round(mean(PM),3), as.vector(quantile(PM, probs=seq(0,1,length.out = 5))))
}
by_state$State<- states

write.csv(by_state, "~/Results/PM25_by_state.csv", row.names=FALSE)

by_year<- table_format
for(y in 1:length(years)){
  PM<- Both[which(Both$Year == years[y]), "PM2.5_Obs"]
  by_year[y,]<- c(round(mean(PM),3), as.vector(quantile(PM, probs=seq(0,1,length.out = 5))))
}
by_year$Year<- years
write.csv(by_year, "~/Results/PM25_by_year.csv", row.names=FALSE)
  
by_season<- table_format
for(s in 1:length(seasons)){
  PM<- Both[which(Both$Season == seasons[s]), "PM2.5_Obs"]
  by_season[s,]<- c(round(mean(PM),3), as.vector(quantile(PM, probs=seq(0,1,length.out = 5))))
}
by_season$Season<- seasons
write.csv(by_season, "~/Results/PM25_by_season.csv", row.names=FALSE)
  