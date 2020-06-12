##Read in data:
source("~/Scripts/Data_prep.R")
train<- rbind(high_train, low_train)
test<- rbind(high_test, low_test)

train_results<- read.csv("~/Results/All_train_preds.csv")
test_results<- read.csv("~/Results/All_test_preds.csv")
CMAQ_train_results<- read.csv("~/Results/CMAQ_train_preds.csv")
CMAQ_test_results<- read.csv("~/Results/CMAQ_test_preds.csv")


## Set results
level_results<- function(df, df_ref, lev, type){
  this_data<- df[which(df_ref$PM2.5_Obs < lev),]
  if(type == "RMSE"){
    return(round(sqrt(mean((this_data$Preds - this_data$Obs)^2)),4))
  }else{
    return(round((cor(this_data$Preds, this_data$Obs))^2,4))
  }
}

level_RMSE<- matrix(0, nrow = 4, ncol = length(PM2.5_levels))
level_R2<- matrix(0, nrow = 4, ncol = length(PM2.5_levels))
for(p in 1:length(PM2.5_levels)){
  level_RMSE[1,p]<- level_results(train_results, train, PM2.5_levels[p], "RMSE")
  level_R2[1,p]<- level_results(train_results, train, PM2.5_levels[p], "R2")
  level_RMSE[2,p]<- level_results(test_results, test, PM2.5_levels[p], "RMSE")
  level_R2[2,p]<- level_results(test_results, test, PM2.5_levels[p], "R2")
  level_RMSE[3,p]<- level_results(CMAQ_train_results, CMAQ_train, PM2.5_levels[p], "RMSE")
  level_R2[3,p]<- level_results(CMAQ_train_results, CMAQ_train, PM2.5_levels[p], "R2")
  level_RMSE[4,p]<- level_results(CMAQ_test_results, CMAQ_test, PM2.5_levels[p], "RMSE")
  level_R2[4,p]<- level_results(CMAQ_test_results, CMAQ_test, PM2.5_levels[p], "R2")
}
Level_RMSE<- as.data.frame(level_RMSE)
row.names(Level_RMSE)<- c("All years, training", "All years, testing",
                          "With CMAQ, training", "With CMAQ, testing")
colnames(Level_RMSE)<- paste("Below", PM2.5_levels, sep = "_")
write.csv(Level_RMSE, "~/Results/March_RMSE_by_level.csv")

Level_R2<- as.data.frame(level_R2)
row.names(Level_R2)<- c("All years, training", "All years, testing",
                          "With CMAQ, training", "With CMAQ, testing")
colnames(Level_R2)<- paste("Below", PM2.5_levels, sep = "_")
write.csv(Level_R2, "~/Results/March_R2_by_level.csv")

##Spatio-temporal:
subset_results<- function(df, df_ref, subset_var, subset, type){
  this_data<- df[which(df_ref[,subset_var] == subset),]
  if(type == "RMSE"){
    return(round(sqrt(mean((this_data$Preds - this_data$Obs)^2)),4))
  }else{
    return(round((cor(this_data$Preds, this_data$Obs))^2,4))
  }
}

Years<- 2008:2018
year_RMSE<- matrix(0, nrow = 4, ncol = length(Years))
year_R2<- matrix(0, nrow = 4, ncol = length(Years))
for(p in 1:length(Years)){
  year_RMSE[1,p]<- subset_results(train_results, train, "Year", Years[p], "RMSE")
  year_R2[1,p]<- subset_results(train_results, train, "Year", Years[p], "R2")
  year_RMSE[2,p]<- subset_results(test_results, test, "Year", Years[p], "RMSE")
  year_R2[2,p]<- subset_results(test_results, test, "Year", Years[p], "R2")
  year_RMSE[3,p]<- subset_results(CMAQ_train_results, CMAQ_train, "Year", Years[p], "RMSE")
  year_R2[3,p]<- subset_results(CMAQ_train_results, CMAQ_train, "Year", Years[p], "R2")
  year_RMSE[4,p]<- subset_results(CMAQ_test_results, CMAQ_test, "Year", Years[p], "RMSE")
  year_R2[4,p]<- subset_results(CMAQ_test_results, CMAQ_test, "Year", Years[p], "R2")
}
Year_RMSE<- as.data.frame(year_RMSE)
row.names(Year_RMSE)<- c("All years, training", "All years, testing",
                          "With CMAQ, training", "With CMAQ, testing")
colnames(Year_RMSE)<- Years
write.csv(Year_RMSE, "~/Results/March_RMSE_by_year.csv")

Year_R2<- as.data.frame(year_R2)
row.names(Year_R2)<- c("All years, training", "All years, testing",
                         "With CMAQ, training", "With CMAQ, testing")
colnames(Year_R2)<- Years
write.csv(Year_R2, "~/Results/March_R2_by_year.csv")


Seasons<- c("fall", "winter", "spring", "summer")
season_RMSE<- matrix(0, nrow = 4, ncol = length(Seasons))
season_R2<- matrix(0, nrow = 4, ncol = length(Seasons))
for(p in 1:length(Seasons)){
  season_RMSE[1,p]<- subset_results(train_results, train, "Season", Seasons[p], "RMSE")
  season_R2[1,p]<- subset_results(train_results, train, "Season", Seasons[p], "R2")
  season_RMSE[2,p]<- subset_results(test_results, test, "Season", Seasons[p], "RMSE")
  season_R2[2,p]<- subset_results(test_results, test, "Season", Seasons[p], "R2")
  season_RMSE[3,p]<- subset_results(CMAQ_train_results, CMAQ_train, "Season", Seasons[p], "RMSE")
  season_R2[3,p]<- subset_results(CMAQ_train_results, CMAQ_train, "Season", Seasons[p], "R2")
  season_RMSE[4,p]<- subset_results(CMAQ_test_results, CMAQ_test, "Season", Seasons[p], "RMSE")
  season_R2[4,p]<- subset_results(CMAQ_test_results, CMAQ_test, "Season", Seasons[p], "R2")
}
Season_RMSE<- as.data.frame(season_RMSE)
row.names(Season_RMSE)<- c("All years, training", "All years, testing",
                         "With CMAQ, training", "With CMAQ, testing")
colnames(Season_RMSE)<- Seasons
write.csv(Season_RMSE, "~/Results/March_RMSE_by_season.csv")

Season_R2<- as.data.frame(season_R2)
row.names(Season_R2)<- c("All years, training", "All years, testing",
                           "With CMAQ, training", "With CMAQ, testing")
colnames(Season_R2)<- Seasons
write.csv(Season_R2, "~/Results/March_R2_by_season.csv")


States<- unique(train$State)
state_RMSE<- matrix(0, nrow = 4, ncol = length(States))
state_R2<- matrix(0, nrow = 4, ncol = length(States))
for(p in 1:length(States)){
  state_RMSE[1,p]<- subset_results(train_results, train, "State", States[p], "RMSE")
  state_R2[1,p]<- subset_results(train_results, train, "State", States[p], "R2")
  state_RMSE[2,p]<- subset_results(test_results, test, "State", States[p], "RMSE")
  state_R2[2,p]<- subset_results(test_results, test, "State", States[p], "R2")
  state_RMSE[3,p]<- subset_results(CMAQ_train_results, CMAQ_train, "State", States[p], "RMSE")
  state_R2[3,p]<- subset_results(CMAQ_train_results, CMAQ_train, "State", States[p], "R2")
  state_RMSE[4,p]<- subset_results(CMAQ_test_results, CMAQ_test, "State", States[p], "RMSE")
  state_R2[4,p]<- subset_results(CMAQ_test_results, CMAQ_test, "State", States[p], "R2")
}
State_RMSE<- as.data.frame(state_RMSE)
row.names(State_RMSE)<- c("All years, training", "All years, testing",
                           "With CMAQ, training", "With CMAQ, testing")
colnames(State_RMSE)<- States
State_RMSE<- State_RMSE[,1:11]
write.csv(State_RMSE, "~/Results/March_RMSE_by_state.csv")

State_R2<- as.data.frame(state_R2)
row.names(State_R2)<- c("All years, training", "All years, testing",
                         "With CMAQ, training", "With CMAQ, testing")
colnames(State_R2)<- States
State_R2<- State_R2[,1:11]
write.csv(State_R2, "~/Results/March_R2_by_state.csv")




