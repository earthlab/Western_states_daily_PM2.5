source("~/Scripts/Data_prep.R")
source("~/Scripts/Base_ML_functions.R")

set.seed(321)
tr_pos<- sample(1:(dim(data)[1]),round(dim(data)[1]*0.9), replace = FALSE)

train<- with_AF[tr_pos,]
test<- with_AF[-tr_pos,]

sink("~/Results/All_years_individual_old-AF.txt")

print("Ranger:")
ranger_model<- run_ranger(train)
saveRDS(ranger_model, "~/Models/All_years_ranger_old-AF.rds")
print(ML_to_CSV(ranger_model, test))

print("XGBT:")
xgbt_model<- run_xgbt(train)
saveRDS(xgbt_model, "~/Models/All_years_xgbt_old-AF.rds")
print(ML_to_CSV(xgbt_model, test))

sink()

