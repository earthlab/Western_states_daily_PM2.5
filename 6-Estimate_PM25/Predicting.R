library(caret)
library(caretEnsemble)

ranger_model<- readRDS("~/Models/Full_ranger.rds")

names(ranger_model$trainingData)
