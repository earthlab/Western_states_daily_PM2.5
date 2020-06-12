# data<- DATA_std[,c(1:17,50:53, 55, 58, 61, 64:83)]
load("Normalized_data_small.RData")
source("Base_ML_functions.R")
source("Test_subsets.R")

categorical<- which(names(DATA) %in% c("Date", "Year", "State", "Season", "Binary_fire",
                                       "Region", "Mid_Study", "Late_Study"))
stats<- preProcess(DATA[,-categorical], method = c("center", "scale"))

# reg_levels<- seq(35, 75, 10)
reg_levels<- seq(15,30)

sink("Investigating_H-L_cutpoints_2.txt")

for(lev in reg_levels){
  print(paste("Cut point =", lev))
  
  high_train<- data[tr_pos,][which(DATA$PM2.5_Obs[tr_pos] >= lev),]
  low_train<- data[tr_pos,][which(DATA$PM2.5_Obs[tr_pos] < lev),]
  high_test<- data[-tr_pos,][which(DATA$PM2.5_Obs[-tr_pos] >= lev),]
  low_test<- data[-tr_pos,][which(DATA$PM2.5_Obs[-tr_pos] < lev),]
  
  print("High:")
  high_model<- run_ranger(high_train, "high")
  saveRDS(high_model, paste0("Cut-point_level-,", lev, "_high.rds")) #for the first round, I messed this up
  print(ML_to_CSV(high_model, high_test))
  print(varImp(high_model))
  
  print("Low:")
  low_model<- run_ranger(low_train, "low")
  saveRDS(low_model, paste0("Cut-point_level-,", lev, "_low.rds"))
  print(ML_to_CSV(low_model, low_test))
  
  print("-----------------------------------------------------")
}

sink()

