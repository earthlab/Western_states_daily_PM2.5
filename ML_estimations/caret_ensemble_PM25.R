# Code reference: https://rpubs.com/zxs107020/370699

# Load the required libraries
library(caret)
library(caretEnsemble)

# data<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Machine Learning\\ML_input_test.csv")
data<- read.csv("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Machine Learning\\clean_ML_input_testing1.csv")

#####skip next part if using cleaned input file from Melissa

DATA<- data[,c("PM2.5_Obs", "Date", "Latitude", "Longitude", "A_100", "C_100", "Both_100",
               "A_250", "C_250", "Both_250","A_500", "C_500", "Both_500",
               "A_1000", "C_1000", "Both_1000","AOD", "MAIAC_AOD","HPBL.surface",
               "TMP.2.m.above.ground","RH.2.m.above.ground", "DPT.2.m.above.ground",
               "APCP.surface", "WEASD.surface", "SNOWC.surface", "UGRD.10.m.above.ground",
               "VGRD.10.m.above.ground", "PRMSL.mean.sea.level", "PRES.surface", "DZDT.850.mb",
               "DZDT.700.mb", "elevation", "NLCD")]

DATA_2<- DATA[1:149, c("PM2.5_Obs", #"Date", "Latitude", "Longitude",
                           "AOD", "MAIAC_AOD",
                           # "HPBL.surface", "TMP.2.m.above.ground","RH.2.m.above.ground",
                           "elevation", "NLCD")] #2008 through 2010

DATA_2<- DATA[, c("PM2.5_Obs", #"Date", "Latitude", "Longitude",
                       "AOD", "MAIAC_AOD",
                       "HPBL.surface", "TMP.2.m.above.ground","RH.2.m.above.ground",
                       "elevation", "NLCD")] #too many are NA


na_pos<- c()
for(j in 1:dim(DATA_2)[1]){
  if(sum(is.na(DATA_2[j,])) > 0){
    na_pos<- c(na_pos, j)
  }
}

DATA_2<- DATA_2[-na_pos,]

n<- round(dim(DATA_2)[1]*0.1)
test_pos<- sample(1:(dim(DATA_2)[1]),n, replace = FALSE)

test<- DATA_2[test_pos,]
train<- DATA_2[-test_pos,]


######## start here if using cleaned input file from Melissa

DATA_2<- data[1:4500, c("PM2.5_Obs", "AOD", "MAIAC_AOD", "elevation", "NLCD"
                        ,"TMP.2.m.above.ground", "RH.2.m.above.ground", "HPBL.surface"
                        ,"Longitude", "Latitude"
                        )]

n<- round(dim(DATA_2)[1]*0.1)
test_pos<- sample(1:(dim(DATA_2)[1]),n, replace = FALSE)

test<- DATA_2[test_pos,]
train<- DATA_2[-test_pos,]


#Baseline:
LM<- lm(PM2.5_Obs ~ ., data = DATA_2)
summary(LM)


# Model to predict PM2.5
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid", 
                        savePredictions = "final", index = createResample(train$PM2.5_Obs, 10),
                        verboseIter = TRUE)

# List of algorithms to use in ensemble
alg_list <- c("glm", "bam", "gbm", "ranger", "superpc", "mlpML", "gaussprRadial", "svmRadial")
inds<- c(3,4,8)
alt_algs<- c("xgbTree", "glmnet", "bagEarth")
glmList<- c("glm", "glmnet")

#Set up for our variables
LIST <- caretList(PM2.5_Obs ~ ., data = train, trControl = control, tuneLength = 1,
                       methodList = glmList, metric = "RMSE") #could also use "MSE"


# Results
res <- resamples(LIST)
summary(res) # look at RMSE (lowest) and R squared (highest)


# #Run random selection:
# n_algs<- 3
# combos<- combn(x = alg_list, m = n_algs)
# numCombos<- choose(length(alg_list),n_algs)
# for(i in 1:numCombos){
#   print(i)
#   print(paste0(combos[i], ", ", combos[i+1],", ", combos[i+2]))
# }
# 

# Stack 
stackControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, 
                                #use less reps if for random forest?
                             savePredictions = TRUE, verboseIter = TRUE)

stack <- caretStack(LIST, method = "gbm", metric = "RMSE", trControl = stackControl)

stack$error

# Predict for test set
stack_test_preds <- data.frame(predict(stack, test))
compare<- cbind(stack_test_preds, test$PM2.5_Obs)

sqrt(mean((compare[,1] - compare[,2])^2)) #RMSE

#################################################################
#Write the above as a function...

runAlgs<- function(dataset, algList){
  #Make train and test sets
  n<- round(dim(dataset)[1]*0.1)
  test_pos<- sample(1:(dim(dataset)[1]),n, replace = FALSE)
  test<- dataset[test_pos,]
  train<- dataset[-test_pos,]
  
  #Make list
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid", 
                          savePredictions = "final", index = createResample(train$PM2.5_Obs, 10),
                          verboseIter = TRUE)
  LIST <- caretList(PM2.5_Obs ~ ., data = train, trControl = control, tuneLength = 1,
                    methodList = algList, metric = "RMSE")
  
  res <- resamples(LIST)
  
  #Make stack
  stackControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                               #use less reps if for random forest?
                               savePredictions = TRUE, verboseIter = TRUE)
  
  stack <- caretStack(LIST, method = "ranger", metric = "RMSE", trControl = stackControl)
  
  print(summary(res))
  print(stack$error)
  
  stack_test_preds <- data.frame(predict(stack, test[,-(which(names(test)=="PM2.5_Obs"))]))
  compare<- cbind(stack_test_preds, test$PM2.5_Obs)
  print(paste("Test set error =", sqrt(mean((compare[,1] - compare[,2])^2))))
  
}


##################################################################
#Geographic and Temporal Subsets

#Temporal
date_split<- strsplit(as.character(data$Date), split="-")
all<- unlist(date_split)

Year<- as.numeric(all[seq(1,length(all)-2, 3)])
Month<- as.numeric(all[seq(2,length(all)-1, 3)])
Day<- as.numeric(all[seq(3,length(all), 3)])

data<- cbind(Year, Month, Day, data[,c("PM2.5_Obs", "AOD", "MAIAC_AOD", "elevation", "NLCD"
                                       ,"TMP.2.m.above.ground", "RH.2.m.above.ground", "HPBL.surface"
                                       ,"Longitude", "Latitude")])

Summer<- data[data$Month %in% 6:8,] #147516
Fall<- data[data$Month %in% 9:11,] #121449 
Winter<- data[data$Month %in% c(12,1,2),] #81636
Spring<- data[data$Month %in% 3:5,] #100605

#Geographic

Lat_thresh<- mean(range(data$Latitude))
Lon_thresh<- mean(range(data$Longitude))

NW<- data[(data$Latitude > Lat_thresh) & (data$Longitude < Lon_thresh),] #148511 obs
SW<- data[(data$Latitude < Lat_thresh) & (data$Longitude < Lon_thresh),] #196809 obs
SE<- data[(data$Latitude < Lat_thresh) & (data$Longitude > Lon_thresh),] #57792 obs
NE<- data[(data$Latitude > Lat_thresh) & (data$Longitude > Lon_thresh),] #48094 obs


#Specific combos: 

#Summer 2008 in the SW
Sum08_SW<- SW[(SW$Month %in% 6:8) & (SW$Year == 2008),]

small_Sum08_SW<- Sum08_SW[1:4500,]
algs<- c("gbm", "ranger", "svmRadial")

runAlgs(small_Sum08_SW[,-(1:3)], algs)

#Summer 2008 in the NW
Sum08_NW<- NW[(NW$Month %in% 6:8) & (NW$Year == 2008),]

small_Sum08_NW<- Sum08_NW[1:4500,]
algs<- c("gbm", "ranger", "svmRadial")

runAlgs(small_Sum08_NW[,-(1:3)], algs)

#Sanity check:
LM<- lm(PM2.5_Obs ~ ., data = small_Sum08_SW[,-(1:3)])
summary(LM)

LM2<- lm(PM2.5_Obs ~ ., data = small_Sum08_NW[,-(1:3)])
summary(LM2)


#Summer 2008-2010 in the SW
Sum_SW<- SW[(SW$Month %in% 6:8),]

small_Sum_SW<- Sum_SW[1:4500,]
algs<- c("gbm", "ranger", "svmRadial")

runAlgs(small_Sum_SW[,-(1:3)], algs)

#Summer 2008-2010 in the NW
Sum_NW<- NW[(NW$Month %in% 6:8),]

small_Sum_NW<- Sum_NW[1:4500,]
algs<- c("gbm", "ranger", "svmRadial")

runAlgs(small_Sum_NW[,-(1:3)], algs)

#Whole Year 2008 in the SW
algs<- c("gbm", "ranger", "svmRadial")
SW_08<- SW[(SW$Year == 2008),]
runAlgs(SW_08[1:4500,-(1:3)], algs)


#Summer 2008-2010 in the SE
Sum_SE<- SE[(SE$Month %in% 6:8),]

small_Sum_SE<- Sum_SE[1:4500,]
algs<- c("gbm", "ranger", "svmRadial")

runAlgs(small_Sum_SE[,-(1:3)], algs)

#Summer 2008-2010 in the NE
Sum_NE<- NE[(NE$Month %in% 6:8),]

small_Sum_NE<- Sum_NE[1:4500,]
algs<- c("gbm", "ranger", "svmRadial")

runAlgs(small_Sum_NE[,-(1:3)], algs)
