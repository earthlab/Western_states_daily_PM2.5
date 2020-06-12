source("~/Scripts/Data_prep.R")
source("~/Scripts/Base_ML_functions.R")

sink("~/Results/All_years_individual.txt")

print("High:")
print("Ranger:")
high_ranger<- run_ranger(high_train)
saveRDS(high_ranger, "~/Models/All_years_high_ranger.rds")
print(ML_to_CSV(high_ranger, high_test))
print("XGBT:")
high_xgbt<- run_xgbt(high_train)
saveRDS(high_xgbt, "~/Models/All_years_high_xgbt.rds")
print(ML_to_CSV(high_xgbt, high_test))

print("Low:")
print("Ranger:")
low_ranger<- run_ranger(low_train)
saveRDS(low_ranger, "~/Models/All_years_low_ranger.rds")
print(ML_to_CSV(low_ranger, low_test))
print("XGBT:")
low_xgbt<- run_xgbt(low_train)
saveRDS(low_xgbt, "~/Models/All_years_low_xgbt.rds")
print(ML_to_CSV(low_xgbt, low_test))

sink()


#CMAQ analysis:
sink("~/Results/CMAQ_years_individual.txt")

print("High:")
print("Ranger:")
CMAQ_high_ranger<- run_ranger(CMAQ_high_train)
saveRDS(CMAQ_high_ranger, "~/Models/CMAQ_years_high_ranger.rds")
print(ML_to_CSV(CMAQ_high_ranger, CMAQ_high_test))
print("XGBT:")
CMAQ_high_xgbt<- run_xgbt(CMAQ_high_train)
saveRDS(CMAQ_high_xgbt, "~/Models/CMAQ_years_high_xgbt.rds")
print(ML_to_CSV(CMAQ_high_xgbt, CMAQ_high_test))

rm(list = c("CMAQ_high_ranger", "CMAQ_high_xgbt"))
gc()

print("Low:")
print("Ranger:")
CMAQ_low_ranger<- run_ranger(CMAQ_low_train)
saveRDS(CMAQ_low_ranger, "~/Models/CMAQ_years_low_ranger.rds")
print(ML_to_CSV(CMAQ_low_ranger, CMAQ_low_test))
print("XGBT:")
CMAQ_low_xgbt<- run_xgbt(CMAQ_low_train)
saveRDS(CMAQ_low_xgbt, "~/Models/CMAQ_years_low_xgbt.rds")
print(ML_to_CSV(CMAQ_low_xgbt, CMAQ_low_test))

sink()

