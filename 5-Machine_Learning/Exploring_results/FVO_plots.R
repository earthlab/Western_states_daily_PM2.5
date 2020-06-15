##SPATIAL

#Read in data:
GLM_train<- read.csv("~/Results/March_All_train_preds_spatial.csv")
GLM_test<- read.csv("~/Results/March_All_test_preds_spatial.csv")

CMAQ_GLM_train<- read.csv("~/Results/March_CMAQ_train_preds_spatial.csv")
CMAQ_GLM_test<- read.csv("~/Results/March_CMAQ_test_preds_spatial.csv")

#Make plots:
png(filename = "FVO_Non-CMAQ_Training.png")
plot(GLM_train$obs, GLM_train$pred, xlab = "Observed", ylab = "Predicted",
     main = "Non-CMAQ 10-Fold CV on 90% Training Set", xlim = c(0,900), ylim = c(0,600))
abline(a=0, b=1)
dev.off()

png(filename = "FVO_Non-CMAQ_Testing.png")
plot(GLM_test$Obs, GLM_test$Preds, xlab = "Observed", ylab = "Predicted",
     main = "Non-CMAQ 10% Test Set", xlim = c(0,900), ylim = c(0,600))
abline(a=0, b=1)
dev.off()

png(filename = "FVO_CMAQ_Training.png")
plot(CMAQ_GLM_train$obs, CMAQ_GLM_train$pred, xlab = "Observed", ylab = "Predicted",
     main = "CMAQ 10-Fold CV on 90% Training Set", xlim = c(0,900), ylim = c(0,600))
abline(a=0, b=1)
dev.off()

png(filename = "FVO_CMAQ_Testing.png")
plot(CMAQ_GLM_test$Obs, CMAQ_GLM_test$Preds, xlab = "Observed", ylab = "Predicted",
     main = "CMAQ 10% Test Set", xlim = c(0,900), ylim = c(0,600))
abline(a=0, b=1)
dev.off()


##RANDOM

#Read in data:
GLM_train<- read.csv("~/Results/March_All_train_preds.csv")
GLM_test<- read.csv("~/Results/March_All_test_preds.csv")

CMAQ_GLM_train<- read.csv("~/Results/March_CMAQ_train_preds.csv")
CMAQ_GLM_test<- read.csv("~/Results/March_CMAQ_test_preds.csv")

#Make plots:
png(filename = "FVO_Non-CMAQ_Training_random.png")
plot(GLM_train$obs, GLM_train$pred, xlab = "Observed", ylab = "Predicted",
     main = "Non-CMAQ 10-Fold CV on 90% Training Set", xlim = c(0,900), ylim = c(0,600))
abline(a=0, b=1)
dev.off()

png(filename = "FVO_Non-CMAQ_Testing_random.png")
plot(GLM_test$Obs, GLM_test$Preds, xlab = "Observed", ylab = "Predicted",
     main = "Non-CMAQ 10% Test Set", xlim = c(0,900), ylim = c(0,600))
abline(a=0, b=1)
dev.off()

png(filename = "FVO_CMAQ_Training_random.png")
plot(CMAQ_GLM_train$obs, CMAQ_GLM_train$pred, xlab = "Observed", ylab = "Predicted",
     main = "CMAQ 10-Fold CV on 90% Training Set", xlim = c(0,900), ylim = c(0,600))
abline(a=0, b=1)
dev.off()

png(filename = "FVO_CMAQ_Testing_random.png")
plot(CMAQ_GLM_test$Obs, CMAQ_GLM_test$Preds, xlab = "Observed", ylab = "Predicted",
     main = "CMAQ 10% Test Set", xlim = c(0,900), ylim = c(0,600))
abline(a=0, b=1)
dev.off()

