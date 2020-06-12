source("Base_ML_functions.R")
source("Test_subsets.R")

#Additional indicators:
DATA$Mid_Study<- (DATA$Year > 2011)&(DATA$Year <= 2015)
DATA$Late_Study<- DATA$Year > 2015
DATA$Year<- as.factor(DATA$Year)

region<- c()
for(i in 1:dim(DATA)[1]){
  if(DATA[i, "State"] %in% c("california", "nevada")){
    region<- append(region, "Southwest")
  }else if(DATA[i, "State"] %in% c("colorado", "utah", "new mexico",
                                   "arizona")){
    region<- append(region, "Four Corners")
  }else if(DATA[i, "State"] %in% c("washington", "oregon")){
    region<- append(region, "Northwest")
  }else if(DATA[i, "State"] %in% c("idaho", "montana", "wyoming")){
    region<- append(region, "Northern Mountain States")
  }else{
    region<- append(region, "Texas")
  }
}
DATA$Region<- region

#Reduce active fire variables:

agg_AF<- function(i){
  lag<- DATA[,sapply(c("25km", "50km", "100km", "500km"), function(x) paste0("Fires_lag",i, "_", x))]
  lag[,1]<- lag[,1]*(1/25)
  lag[,2]<- (lag[,2] - lag[,1])*(1/50)
  lag[,3]<- (lag[,3] - lag[,2])*(1/100)
  lag[,4]<- (lag[,4] - lag[,3])*(1/500)
  return(as.vector(rowSums(lag)))
}

DATA$Lag0<- agg_AF(0)
DATA$Lag1<- agg_AF(1)
DATA$Lag2<- agg_AF(2)
DATA$Lag3<- agg_AF(3)
DATA$Lag4<- agg_AF(4)
DATA$Lag5<- agg_AF(5)
DATA$Lag6<- agg_AF(6)
DATA$Lag7<- agg_AF(7)

#Normalize:
categorical<- which(names(DATA) %in% c("Date", "Year", "State", "Season", "Binary_fire",
                                       "Region", "Mid_Study", "Late_Study"))

stats<- preProcess(DATA[,-categorical], method = c("center", "scale"))

DATA_std<- DATA
counter<- 1
for(j in 1:dim(DATA)[2]){
  if(! j %in% categorical){
    DATA_std[,j]<- (DATA[,j] - stats$mean[counter])/stats$std[counter]
    counter<- counter + 1
  }
}

set.seed(321)
n<- round(dim(DATA)[1]*train_sizes)
tr_pos<- sample(1:(dim(DATA_std)[1]),n, replace = FALSE)
train<- DATA_std[tr_pos,]
test<- DATA_std[-tr_pos,]

rm(list=setdiff(ls(), c("DATA", "DATA_std", "tr_pos", "train", "test")))

# save.image("Normalized_data2.RData")
load("Normalized_data2.RData")

#Keep only new AF variables and "Both" highway variables:

data<- DATA_std[,c(1:17,50:53, 55, 58, 61, 64:83)]

train<- data[tr_pos,]
test<- data[-tr_pos,]

save.image("Normalized_data_small.RData")

sink("DATA_normalized.txt")
std_model<- run_ranger(train)
saveRDS(std_model, "Std_90pct_Seed321.rds")
ML_to_CSV(std_model, test)

print("-------------------------------------------------")
print("Testing by year and state subset:")
test_subsets(std_model, test)

sink()

std_preds<- data.frame(predict(std_model, test))
preds<- (std_preds[,1]*stats$std["PM2.5_Obs"]) + stats$mean["PM2.5_Obs"]
obs<- (test$PM2.5_Obs*stats$std["PM2.5_Obs"]) + stats$mean["PM2.5_Obs"]

preds<- preds[which(obs < 400)]
obs<- obs[which(obs < 400)]

sqrt(mean((preds-obs)^2)) #RMSE
cor(preds, obs)^2 #R^2 

varImp(std_model)

plot(obs, preds, xlab = "Observed", ylab = "Fitted")

##Other algs:
sink("DATA_normalized_xgbt.txt")

std_model<- run_ranger(train)
saveRDS(std_model, "Std_90pct_Seed321.rds")
std_model_x<- run_xgbt(train)
saveRDS(std_model_x, "Std_90pct_xgbt_Seed321.rds")

ML_to_CSV(std_model, test)

print("-------------------------------------------------")
print("Testing by year and state subset:")
test_subsets(std_model, test)

sink()

sink("DATA_normalized_cubist.txt")
std_model2<- run_cubist(train)
saveRDS(std_model2, "Std_90pct_cubist.rds")
ML_to_CSV(std_model2, test)

print("-------------------------------------------------")
print("Testing by year and state subset:")
test_subsets(std_model2, test)

sink()


############ Testing other models:

#Fire, not fire:

fire<- DATA[which(DATA$Binary_fire),]
not_fire<- DATA[which(DATA$Binary_fire == FALSE),]

set.seed(321)
f_n<- round(dim(fire)[1]*train_sizes)
f_tr_pos<- sample(1:(dim(fire)[1]),f_n, replace = FALSE)
f_train<- fire[f_tr_pos,]
f_test<- fire[-f_tr_pos,]

nf_n<- round(dim(not_fire)[1]*train_sizes)
nf_tr_pos<- sample(1:(dim(not_fire)[1]),nf_n, replace = FALSE)
nf_train<- not_fire[nf_tr_pos,]
nf_test<- not_fire[-nf_tr_pos,]

sink("Fire_Not-Fire2.txt")

print("Fire:")
fire_model<- run_ranger(f_train)
saveRDS(fire_model, "Fire2_90pct.rds")
ML_to_CSV(fire_model, f_test)

F_test<- f_test[which(f_test$PM2.5_Obs < 1000),]
ML_to_CSV(fire_model, F_test)

small_F_test<- f_test[which(f_test$PM2.5_Obs < 500),]
ML_to_CSV(fire_model, small_F_test)

print("Not fire:")
not_fire_model<- run_ranger(nf_train)
saveRDS(not_fire_model, "Not-Fire2_90pct.rds")
ML_to_CSV(not_fire_model, nf_test)

# NF_test<- nf_test[which(nf_test$PM2.5_Obs < 1000),] #no different

# for(ts in train_sizes){
#   print(paste("Training set size:", round(ts*dim(DATA)[1])))
#   model<- run_ranger(1-ts)
#   name<- "first_run_all_vars"
#   saveRDS(model, paste0("ranger_",name,".rds"))
# }

sink()

preds<- data.frame(predict(fire_model, f_test))
plot(f_test$PM2.5_Obs, preds[,1], xlab= "Observed", ylab= "Fitted")

preds2<- data.frame(predict(not_fire_model, nf_test))
plot(nf_test$PM2.5_Obs, preds2[,1], xlab= "Observed", ylab= "Fitted")

########### Smaller subsets:
fire_17<- DATA[which((DATA$Year == 2017)&(DATA$Binary_fire)),]
not_fire_17<- DATA[which((DATA$Year == 2017)&(DATA$Binary_fire == FALSE)),]

set.seed(321)
f_n_17<- round(dim(fire_17)[1]*train_sizes)
f_tr_pos_17<- sample(1:(dim(fire_17)[1]),f_n_17, replace = FALSE)
f_train_17<- fire_17[f_tr_pos_17,]
f_test_17<- fire_17[-f_tr_pos_17,]

nf_n_17<- round(dim(not_fire_17)[1]*train_sizes)
nf_tr_pos_17<- sample(1:(dim(not_fire_17)[1]),nf_n_17, replace = FALSE)
nf_train_17<- not_fire_17[nf_tr_pos_17,]
nf_test_17<- not_fire_17[-nf_tr_pos_17,]

sink("Fire_Not-Fire_2017.txt")

print("Fire:")
fire_model_17<- run_ranger(f_train_17)
saveRDS(fire_model_17, "Fire_2017_90pct.rds")
ML_to_CSV(fire_model_17, f_test_17)

F_test_17<- f_test_17[which(f_test_17$PM2.5_Obs < 500),]
ML_to_CSV(fire_model_17, F_test_17)

print("Not fire:")
not_fire_model_17<- run_ranger(nf_train_17)
saveRDS(not_fire_model_17, "Not-Fire_2017_90pct.rds")
ML_to_CSV(not_fire_model_17, nf_test_17)

sink()

preds_17<- data.frame(predict(fire_model_17, f_test_17))
plot(f_test_17$PM2.5_Obs, preds_17[,1], xlab= "Observed", ylab= "Fitted")

preds2_17<- data.frame(predict(not_fire_model_17, nf_test_17))
plot(nf_test_17$PM2.5_Obs, preds2_17[,1], xlab= "Observed", ylab= "Fitted")

############################

##Test high and low:

DATA<- DATA[which(DATA$PM2.5_Obs < 850),] 

high<- DATA[which(DATA$PM2.5_Obs > quantile(DATA$PM2.5_Obs, probs = 0.999)),] #cutoff = 113.5
low<- DATA[which(DATA$PM2.5_Obs <= quantile(DATA$PM2.5_Obs, probs = 0.999)),]

set.seed(321)
h_n<- round(dim(high)[1]*train_sizes)
h_tr_pos<- sample(1:(dim(high)[1]),h_n, replace = FALSE)
h_train<- high[h_tr_pos,]
h_test<- high[-h_tr_pos,]

l_n<- round(dim(low)[1]*train_sizes)
l_tr_pos<- sample(1:(dim(low)[1]),l_n, replace = FALSE)
l_train<- low[l_tr_pos,]
l_test<- low[-l_tr_pos,]

sink("High_Low_0.txt")

print("High:")
high_model<- run_ranger(h_train)
saveRDS(high_model, "High0.rds")
ML_to_CSV(high_model, h_test)

low_model<- run_ranger(l_train)
saveRDS(low_model, "Low0.rds")
ML_to_CSV(low_model, l_test)

print("-------------------------------------------------")
print("Testing by year and state subset:")
print("High:")
test_subsets(high_model, h_test)
print("Low:")
test_subsets(low_model, l_test)

sink()

h_preds<- data.frame(predict(H0, h_test))
l_preds<- data.frame(predict(L0, l_test))

plot(h_test$PM2.5_Obs, h_preds[,1], xlab = "Observed", ylab = "Fitted", main = "High")
plot(l_test$PM2.5_Obs, l_preds[,1], xlab = "Observed", ylab = "Fitted", main = "Low")

#### Classifying first:
load("Classifying_first.RData")

low_train<- DATA[tr_pos,][which(class_train$High_Low == "Low"), -74]
high_train<- DATA[tr_pos,][which(class_train$High_Low == "High"), -74]
low_test<- DATA[-tr_pos,][which(test_preds[,1] == "Low"),]
high_test<- DATA[-tr_pos,][which(test_preds[,1] == "High"),]

sink("High_Low_class-first.txt")

print("High:")
high_model<- run_ranger(high_train)
saveRDS(high_model, "High_class-first.rds")
ML_to_CSV(high_model, high_test)

low_model<- run_ranger(low_train)
saveRDS(low_model, "Low_class-first.rds")
ML_to_CSV(low_model, low_test)

print("-------------------------------------------------")
print("Testing by year and state subset:")
print("High:")
test_subsets(high_model, high_test)
print("Low:")
test_subsets(low_model, low_test)

sink()

high_preds<- data.frame(predict(high_model, high_test))
plot(high_test$PM2.5_Obs, high_preds[,1], xlab = "Observed", ylab = "Fitted")

## subset of variables for high model (from classification)
high_train_subset<- high_train[,c("Lon", "Lat", "PM2.5_Obs", "PRES_surface",
                                  "elevation", "RH_2m", "TMP_2m", "CosDOY",
                                  "MAIAC_AOD", "Pop_density", "NLCD_5km", 
                                  "Date", "NLCD_10km", "DPT_2m", "CosMonth",
                                  "Fires_lag7_50km", "NLCD_1km", "Both_500", "ndvi")]
high_train_subset2<- high_train[,c("NLCD_10km", "NLCD_5km", "Pop_density",
                                   "MAIAC_AOD", "Fires_lag7_50km", "PRES_surface",
                                   "NLCD_1km", "CosDOY", "Fires_lag7_100km",
                                   "Fires_lag7_25km", "PM2.5_Obs", "Lat", #Date
                                   "elevation", "TMP_2m", "HPBL_surface",
                                   "ndvi", "CosMonth", "DPT_2m", "Year", "Lon")]
high_stats<- preProcess(high_train_subset2, method = c("center", "scale"))
high_subset_std<- high_train_subset2
for(j in 1:dim(high_subset_std)[2]){
  high_subset_std[,j]<- (high_train_subset2[,j] - high_stats$mean[j])/high_stats$std[j]
}


high_subset_model<- run_ranger(high_subset_std)
# LM<- lm(PM2.5_Obs ~ ., high_train_subset)
# summary(LM)

high_preds<- data.frame(predict(high_subset_model, high_test))
sqrt(mean((high_preds[,1] - high_test$PM2.5_Obs)^2)) #81.4182
(cor(high_preds[,1], high_test$PM2.5_Obs))^2 #0.3873

plot(high_test$PM2.5_Obs, high_preds[,1])


############ Classification with threshold of 35:
model35<- readRDS("Classifying_thresh-35.rds")
test_preds <- data.frame(predict(model35, class_test))

low_train<- data[tr_pos,][which(class_train$High_Low == "Low"), -45]
high_train<- data[tr_pos,][which(class_train$High_Low == "High"), -45]
low_test<- data[-tr_pos,][which(test_preds[,1] == "Low"),]
high_test<- data[-tr_pos,][which(test_preds[,1] == "High"),]

sink("High_Low_class_thresh-35.txt")

print("High:")
high_model<- run_ranger(high_train)
saveRDS(high_model, "High_class_thresh-35.rds")
ML_to_CSV(high_model, high_test)

low_model<- run_ranger(low_train)
saveRDS(low_model, "Low_class_thresh-35.rds")
ML_to_CSV(low_model, low_test)

print("-------------------------------------------------")
print("Testing by year and state subset:")
print("High:")
test_subsets(high_model, high_test)
print("Low:")
test_subsets(low_model, low_test)

sink()

high_preds<- data.frame(predict(high_model, high_test))
compare<- cbind(high_preds[,1], high_test[,"PM2.5_Obs"])
compare<- apply(compare, MARGIN = 2, 
                function(y){y*stats$std["PM2.5_Obs"] + stats$mean["PM2.5_Obs"]})

plot(compare[,2], compare[,1], xlab = "Observed", ylab = "Fitted")

low_preds<- data.frame(predict(low_model, low_test))
compare<- cbind(low_preds[,1], low_test[,"PM2.5_Obs"])
compare<- apply(compare, MARGIN = 2, 
                function(y){y*stats$std["PM2.5_Obs"] + stats$mean["PM2.5_Obs"]})

plot(compare[,2], compare[,1], xlab = "Observed", ylab = "Fitted")
plot(compare[,2], compare[,1], xlab = "Observed", ylab = "Fitted", xlim = c(0,200))
