# ML_PM25_estimation_step2.R - make predictions of PM2.5 from trained models created in step1

# load files created in ML_PM25_estimation_step1.R, which trained the model

# set up parallel code again

# in the wrapper that cycles through models:
# make predictions with the data
PM25_prediction <- predict(this_model, PM25_obs_shuffled) # predict on the full data set
print('change code to make predictions on the locations of interest instead of locations of monitors')


### Calculation: predicted PM2.5
this_model_output$fit # display fit information about this_model_output
predicted<-predict(this_model_output,newdata=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)]) # find predictions based on the rfe fit. https://www.rdocumentation.org/packages/pathClass/versions/0.9.4/topics/predict.rfe
# predicted is in log scale of PM2.5

plot(x=FinalInputData$logpm25,y=predicted,xlim=c(0,7),ylim=c(0,7))
abline(0,1) # add a straight line to plot; https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/abline
rsq<-this_model_output$results[which(this_model_output$results$RMSE==min(this_model_output$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
mtext(paste(this_model_run_name," Log Predicted vs Observed Plot",sep = ""),side=3)

### Calculation: exp(predicted), i.e., undo the log in the predicted data
this_model_output.pm25pred=exp(predicted)

plot(x=FinalInputData$Monitor_PM25,y=this_model_output.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
abline(0,1) # add a straight line to plot; https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/abline
rsq<-this_model_output$results[which(this_model_output$results$RMSE==min(this_model_output$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=250,y=50)
mtext(side=3,text=paste(this_model_run_name_display," Predicted vs Expected Plot - Regular Scale",sep = ""))

### Calculation: calculate residuals (in log-land)
this_model_output.resids<-FinalInputData[,52]-predicted

plot(predicted,this_model_output.resids,ylab="Residuals",xlab="logged PM2.5")
abline(h=0)
mtext(side=3,text=paste(this_model_run_name_display," Residual Plot",sep = ""))

BlandAltman(x=FinalInputData$logpm25,y=predicted, gui=FALSE, bandsOn=TRUE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2,
            main="Random Forest Bland-Altman plot")


