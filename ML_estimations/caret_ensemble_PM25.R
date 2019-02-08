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
#small subset for code development

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


#Run random selection:
n_algs<- 3
combos<- combn(x = alg_list, m = n_algs)
numCombos<- choose(length(alg_list),n_algs)
for(i in 1:numCombos){
  print(i)
  print(paste0(combos[i], ", ", combos[i+1],", ", combos[i+2]))
}


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


