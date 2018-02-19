# this code based on Colleen Reid's 2008 wildfire study, and uses that data

# this code based on Colleen Reid's 2008 wildfire study, and uses that data

# set working directory (setwd) http://rfunction.com/archives/1001
uppermost.directory="/home/rstudio" # on AWS
working.directory=uppermost.directory # on AWS
setwd(working.directory)
output.directory=file.path(working.directory,"Code_Outputs")
Data.directory <- file.path(working.directory,"Data_files")

#cat("SET MEMORY LIMIT\n") # is this necessary?
#memory.limit(1000000000) # commenting on AWS

# Download latest version of R: https://cran.r-project.org/bin/windows/base/
# The code is written for R version 3.4.3
# explanation of data types in R: http://resbaz.github.io/2014-r-materials/lessons/01-intro_r/data-structures.html

# sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
SinkFileName=file.path(output.directory,"AllforCaret_ML.txt")
sink(file =SinkFileName, append = FALSE, type = c("output","message"),
     split = FALSE)

cat("Code and R output for 29vars_alldata_withConfounders_2018.R \n \n")
############################################################################
cat("Title: 30vars.r \n")
cat("Author: Colleen Reid \n")
cat("Original Date: October 19, 2012 \n")
cat("Latest Update: May 29, 2013 \n")
cat(" comments added and minor edits by Melissa May Maestas, November 2017 \n")
cat("This program uses the 'rfe' function in the 'caret' package to do various \n")
cat("models on my data allowing the covariates to be chosen and the other parameters \n")
cat("of each model to be chosen through cross-validation and then the optimal model \n")
cat("picked by use of the MSE. \n \n")
############################################################################

cat("INSTALL PACKAGES \n")
# install.packages command installs packages from repositories or local files. https://www.rdocumentation.org/packages/utils/versions/3.4.1/topics/install.packages	http://blogs.helsinki.fi/bioinformatics-viikki/documentation/getting-started-with-r-programming/installing-r-packages-bioconductor/
# install Generalized Boosted Regression Models 
# useful website: https://cran.r-project.org/src/contrib/Archive/gbm/, more info here: https://crantastic.org/packages/gbm
# useful website: How should I deal with ?package 'xxx' is not available (for R version x.y.z)? warning? https://stackoverflow.com/questions/25721884/how-should-i-deal-with-package-xxx-is-not-available-for-r-version-x-y-z-wa/35762802
# setRepositories() # chose all of them
# it asked what CRAN mirror to use - I chose "USA(CA 1)[https]" from list
install.packages( pkgs= "gbm") # looks like it worked and installed gbm_2.1.3.zip
# install caret
# original: install.packages("C:/R_packages/caret_5.15-023.tar.gz", repos = NULL, type="source")
install.packages( pkgs= "caret") # looks like it worked and installed caret_6.0-77.zip and dependency files
# installing packages for which the library command didn't initially work:
install.packages(pkgs= "randomForest")
install.packages(pkgs= "polspline")
# http://www2.uaem.mx/r-mirror/web/packages/ResearchMethods/ResearchMethods.pdf
# http://www2.uaem.mx/r-mirror/web/packages/ResearchMethods/
# ResearchMethods 'http://www2.uaem.mx/r-mirror/src/contrib/ResearchMethods_1.4.tar.gz'
install.packages(pkgs="irr")
install.packages(pkgs="gplots")
install.packages(pkgs="ellipse")
install.packages(pkgs= "http://www2.uaem.mx/r-mirror/src/contrib/ResearchMethods_1.4.tar.gz", repos = NULL, type="source")
install.packages(pkgs="reshape")
install.packages(pkgs="earth")
install.packages(pkgs="foreach")
install.packages(pkgs="e1071") # needed for treebagFuncs
install.packages(pkgs="glmnet")
install.packages(pkgs="elasticnet") # pick up writing code here

cat("\n")

cat("Library statements \n")
# library: loading/attaching and listing of packages https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/library
library(randomForest) 
library(polspline)
library(foreign)
library(tcltk)
library(ResearchMethods) 
library(reshape) 
library(kernlab) 
library(foreach) 
library(caret) # not sure if it loaded ok
library(earth) 
library(gbm) 
library(e1071) # needed for treebagFuncs
library(glmnet) 
cat("\n \n") # add extra space

print('pick up writing code here - load FinalInputData file')
this_source_file <- "FinalInputDataFile.csv"
FinalInputData <- read.csv(file.path(Data.directory,this_source_file),header=TRUE) 

# loop through each of the machine learning methods and create plots

for(this_ML_method_counter in 1:2){  
  print(paste("this_ML_method_counter = ",this_ML_method_counter))

if(this_ML_method_counter==1){
    #######################
	#### try rfFuncs
	#######################	
	# Resources:
	# https://topepo.github.io/caret/recursive-feature-elimination.html
	# https://www.rdocumentation.org/packages/caret/versions/6.0-77/topics/rfe
	# https://www.harding.edu/fmccown/r/
	# http://www.cookbook-r.com/Graphs/Output_to_a_file/

	this_model_run_name_short="rf1" # string version of what you name the model output two lines down
	this_model_run_name_display="RF1" # string of how you want the model name displayed (with spaces, capitalization, etc.)
	set.seed(272)
	this_model_output<-rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
#not sure what this is for:# rfeControl <- rfeControl(functions=rfFuncs,method="cv",number=10,saveDetails=FALSE,verbose=TRUE,returnResamp="final")) # rfe = recursive feature elimination
	rf1 <- this_model_output # put it in named variable for saving to file 
	# Save ML model to file to be used in other scripts
	# Saving on object in RData format # http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata
	save(rf1, file = file.path(ML_Output.directory,paste(this_model_run_name_short,"_model.RData",sep = ""))
	# Save multiple objects
	#save(data1, data2, file = "data.RData")
	# To load the data again
	#load("data.RData")
	rm(rf1)
} 
else if(this_ML_method_counter==2){
   	####################################
	# treebagFuncs
	####################################

	this_model_run_name_short="treebag1" # string version of what you name the model output two lines down
	this_model_run_name_display="Treebag1" # string of how you want the model name displayed (with spaces, capitalization, etc.)
	set.seed(272)
	this_model_output<-rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
#not sure what this is for:# rfeControl=rfeControl(functions=treebagFuncs,method="cv",number=10,saveDetails=FALSE,verbose=TRUE,returnResamp="final"))
	treebag1 <- this_model_output # put it in named variable for saving to file 
	# Save ML model to file to be used in other scripts
	save(treebag1, file = file.path(ML_Output.directory,paste(this_model_run_name_short,"_model.RData",sep = ""))
	rm(treebag1)
} 
else if(this_ML_method_counter==3){
   	####################################
	# gbmtrainonly
	####################################
print('pick up writing code here, copying from 29vars file')
	this_model_run_name_short="treebag1" # string version of what you name the model output two lines down
	this_model_run_name_display="Treebag1" # string of how you want the model name displayed (with spaces, capitalization, etc.)
	set.seed(272)
	this_model_output<-rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
#not sure what this is for:# rfeControl=rfeControl(functions=treebagFuncs,method="cv",number=10,saveDetails=FALSE,verbose=TRUE,returnResamp="final"))
	treebag1 <- this_model_output # put it in named variable for saving to file 
	# Save ML model to file to be used in other scripts
	save(treebag1, file = file.path(ML_Output.directory,paste(this_model_run_name_short,"_model.RData",sep = ""))
	rm(treebag1)
} 

this_model_output # display summary of output from this model run
predictors(this_model_output) # list which predictors were used in the final model. https://www.rdocumentation.org/packages/caret/versions/6.0-77/topics/predictors

### plot this_model_run_name
FigFileName=file.path(output.directory,paste(this_model_run_name_short,"_RMSEvNVariables.pdf",sep = "")) # define file name for the figure to be created
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new() # clear the plot to have a clean canvas to draw on
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(this_model_output)# ,axes=F, ann=T, cex.lab=0.8, lwd=2)
# Make x axis tick marks without labels
# axis(1, lab=F)
title(main = paste(this_model_run_name_display," RMSE vs Number of Variables",sep = ""))
dev.off() # stop writing to pdf file
remove(FigFileName)
sink() # stop putting text into SinkFileName
LatexFileName=file.path(output.directory,paste(this_model_run_name_short,"Images.tex",sep = "")) # Start file for latex code images
sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE)
cat(paste("\n\\subsection{",this_model_run_name_display," Images}",sep = ""))
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat(paste("\\includegraphics[width=0.77\\textwidth]{","this_model_run_name_short","_RMSEvNVariables.pdf} \n",sep = "")) 
cat(paste("\\caption{\\label{fig:","this_model_run_name_short","RMSEvNVar}","this_model_run_name_display"," RMSE vs variable number.} \n",sep = "")) 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Plot this_model_run, Rsquared
FigFileName=file.path(output.directory,paste(this_model_run_name,"_Rsquared.pdf",sep = ""))
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(this_model_output, metric="Rsquared")
title(main = paste(this_model_run_name," Rsquared by Variable",sep = ""))
dev.off() # stop input to pdf document
remove(FigFileName)
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat(paste("\\includegraphics[width=0.77\\textwidth]{","this_model_run_name_short","_Rsquared.pdf} \n",sep=""))
cat(paste("\\caption{\\label{fig:",this_model_run_name_short,"Rsquared}","this_model_run_name_display"," Rsquared vs variable number.} \n",sep = "")) 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE)# resume putting output into SinkFileName

### Calculation: predicted PM2.5
this_model_output$fit # display fit information about this_model_output
predicted<-predict(this_model_output,newdata=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)]) # find predictions based on the rfe fit. https://www.rdocumentation.org/packages/pathClass/versions/0.9.4/topics/predict.rfe
# predicted is in log scale of PM2.5

### Plot this_model_output, Log(Predicted vs Observed PM2.5)
FigFileName=file.path(output.directory,paste(this_model_run_name,"_LogObsVsPredict.pdf",sep = ""))
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(x=FinalInputData$logpm25,y=predicted,xlim=c(0,7),ylim=c(0,7))
abline(0,1) # add a straight line to plot; https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/abline
rsq<-this_model_output$results[which(this_model_output$results$RMSE==min(this_model_output$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
mtext(paste(this_model_run_name," Log Predicted vs Observed Plot",sep = ""),side=3)
dev.off() # stop input to pdf document
remove(FigFileName)
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat(paste("\\includegraphics[width=0.77\\textwidth]{",this_model_run_name_short,"_LogObsVsPredict.pdf} \n",sep = ""))
cat(paste("\\caption{\\label{fig:",this_model_run_name_short,"LogObsVsPredict}Observed vs Predicted PM2.5, log scale.} \n",sep = "")) 
cat("\\end{figure} \n \n")
sink()
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Calculation: exp(predicted), i.e., undo the log in the predicted data
this_model_output.pm25pred=exp(predicted)

### Plot this_model_output, Predicted vs Observed PM2.5
FigFileName=file.path(output.directory,paste(this_model_run_name_short,"_ObsVsPredict.pdf",sep = ""))
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(x=FinalInputData$Monitor_PM25,y=this_model_output.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
abline(0,1) # add a straight line to plot; https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/abline
rsq<-this_model_output$results[which(this_model_output$results$RMSE==min(this_model_output$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=250,y=50)
mtext(side=3,text=paste(this_model_run_name_display," Predicted vs Expected Plot - Regular Scale",sep = ""))
dev.off() # stop input to pdf document
remove(FigFileName)
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat(paste("\\includegraphics[width=0.77\\textwidth]{",this_model_run_name,"_ObsVsPredict.pdf} \n",sep = ""))
cat(paste("\\caption{\\label{fig:",this_model_run_name_short,"ObsVsPredict}Observed vs Predicted PM2.5.} \n",sep = "")) 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Calculation: calculate residuals (in log-land)
this_model_output.resids<-FinalInputData[,52]-predicted

### Plot this_model_output, Log Residuals PM2.5
FigFileName=file.path(output.directory,paste(this_model_run_name_short,"_LogResids.pdf",sep = ""))
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(predicted,this_model_output.resids,ylab="Residuals",xlab="logged PM2.5")
abline(h=0)
mtext(side=3,text=paste(this_model_run_name_display," Residual Plot",sep = ""))
dev.off() # stop writing to pdf file
remove(FigFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat(paste("\\includegraphics[width=0.77\\textwidth]{",this_model_run_short,"_LogResids.pdf} \n",sep = ""))
cat(paste("\\caption{\\label{fig:",this_model_run_short,"LogResids}Log Residuals of PM2.5 (observed minus predicted).} \n",sep = "")) 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Plot this_model_output, BlandAltman PM2.5
FigFileName=file.path(output.directory,paste(this_model_output,"_BlandAltman.pdf",sep = ""))
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
BlandAltman(x=FinalInputData$logpm25,y=predicted, gui=FALSE, bandsOn=TRUE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2,
            main="Random Forest Bland-Altman plot")
dev.off() # stop writing to pdf file
remove(FigFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat(paste("\\includegraphics[width=0.77\\textwidth]{",this_model_run_name_short,"_BlandAltman.pdf} \n",sep = ""))
cat(paste("\\caption{\\label{fig:",this_model_run_name_short,"BlandAltman}",this_model_run_name_display," Bland Altman.} \n",sep = ""))
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

# Clear variables from this_model_output before moving on to next method
remove(this_model_output,this_model_output.pm25pred,this_model_output.resids,rsq,predicted,LatexFileName,this_model_run_name_short,this_model_run_name_display)
ls()

print("write code to save each ML output")

}






