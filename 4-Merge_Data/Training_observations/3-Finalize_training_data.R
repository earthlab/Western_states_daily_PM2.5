library(dplyr)

load("~/Final_ML_data3.RData")
rm(list=setdiff(ls(), c("data", "CMAQ_data")))

DF_no_CMAQ<- distinct(data)
DF_with_CMAQ<- distinct(CMAQ_data)

save.image("~/ML_inputs.RData")