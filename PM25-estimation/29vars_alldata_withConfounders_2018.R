# set working directory (setwd) http://rfunction.com/archives/1001
#uppermost.directory="F:" # on laptop
uppermost.directory="/home/rstudio" # on AWS

#working.directory=file.path(uppermost.directory,"Programs","PM25_Surface_Project") # on laptop
working.directory=uppermost.directory # on AWS

setwd(working.directory)
output.directory=file.path(working.directory,"Code_Outputs")

#.libPaths("D://R_Packages_Local") # work laptop
#.libPaths("C://R_Packages_Local") # home laptop


#cat("SET MEMORY LIMIT\n") # is this necessary?
#memory.limit(1000000000) # commenting on AWS

# Download latest version of R: https://cran.r-project.org/bin/windows/base/
# The code is written for R version 3.4.3
# explanation of data types in R: http://resbaz.github.io/2014-r-materials/lessons/01-intro_r/data-structures.html

# sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
SinkFileName=file.path(output.directory,"Data_Processing.txt")
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

cat("read in the datasets \n")
long<-read.csv(file.path(working.directory,"AllforCaret.csv"),header=TRUE) # office laptop # load AllforCaret.csv data and put it in variable called "long"; and keep header names
cat("size of 'long' variable: ")
cat(dim(long))
cat("\n \n")

cat("remove all STI monitors that are not the ebams \n")
sti<-long[which(long$EPA_or_STI=="STI"),] # find the sti monitors and put them in a variable called "sti"
	cat("total number of sti monitors: ",dim(sti)[1]," \n")
sti2remove<-sti[which(sti$uniqueID>20000),] # figure out which sti monitors have a uniqueID above 20000 and put them in a variable called sti2remove
	cat("number of sti monitors to remove (uniqueID>20000): ",dim(sti2remove)[1]," \n")
sti2keep<-sti[which(sti$uniqueID<20000),] # figure out which sti monitors have a uniqueID less than 20000 and put them in a variable called sti2keep
	cat("number of sti monitors to keep (uniqueID<20000): ",dim(sti2keep)[1]," \n")
# check that the number of remove plus the number of rows to keep matches the original size
try(if(dim(sti)[1] != dim(sti2remove)[1]+dim(sti2keep)[1]) stop("sti variable sizes do not make sense")) # throw an error message if matrix sizes don't match up
nostiStep<-long[which(long$EPA_or_STI!="STI"),] # create variable nosti which is the portion of "long" that doesn't have STI in it
	cat("number of non-sti monitors: ",dim(nostiStep),"\n")
nosti<-rbind(nostiStep,sti2keep) # combine the data without sti with the portion of sti with uniqueID<20000
	cat("size of non-sti and kept sti data in variable named 'nosti': ",dim(nosti),"\n")
# check that sizes of variable make sense
try(if(dim(long)[1]!= dim(sti2remove)[1]+dim(sti2keep)[1]+dim(nostiStep)[1]) stop("check variable sizes"))
try(if(dim(long)[1]!=dim(nosti)[1]+dim(sti2remove)[1] ) stop("check variable size"))
# clear un-needed variables
remove('long','nostiStep','sti','sti2keep','sti2remove')

# changed from original to remove missing rows of data before removing the outliers
#make this data set have no missing data for the complete case analysis
nomissing <- nosti[ which(nosti$Monitor_PM25!='NA'), ]
	cat("size of data after removing rows with missing PM2.5 monitor data: ",dim(nomissing)," \n")
	remove('nosti')
ls()
nostimiss<- nomissing[ which(nomissing$STI_PM25!='NA'), ]
	cat("size of data after removing rows with missing 'STI_PM25' data: ",dim(nostimiss)," \n")
	remove('nomissing')
ls()
noemissmiss<-nostimiss[ which(nostimiss$Emissdist!='NA'),]
	cat("size of data after removing rows with missing 'Emissdist' data: ",dim(noemissmiss)," \n")
	remove('nostimiss')
ls()
nonemiss<-noemissmiss[which(noemissmiss$WRF_PM25!='NA'),]
	cat("size of data after removing rows with missing 'WRF_PM25' data: ",dim(nonemiss)," \n")
	remove('noemissmiss')
ls()

cat("remove outliers \n")
nostiAlllogpm25gt0<-nonemiss[which(nonemiss$logpm25>0),]
	cat("size of data after removing rows not satisfying logpm25>0: ",dim(nostiAlllogpm25gt0)," \n")
	remove('nonemiss')
nostiAlllogpm25lt6<-nostiAlllogpm25gt0[which(nostiAlllogpm25gt0$logpm25<6),]
	cat("size of data after removing data not meeting logpm25<6': ",dim(nostiAlllogpm25lt6)[1],"\n")
	remove(nostiAlllogpm25gt0)

cat("remove June 20 and July 31 of 2008 from data \n")
nostiAlllogpm25lt6$Date<-as.Date(nostiAlllogpm25lt6$Date) # make R recognize this data as date information
nostiNoJune20<-nostiAlllogpm25lt6[which("2008-06-20"<=nostiAlllogpm25lt6$Date),] # remove data from June 20, 2008
	cat("size of data after removing rows from June 20, 2008 ",dim(nostiNoJune20)," \n")
	remove(nostiAlllogpm25lt6)
FinalInputData<-nostiNoJune20[which("2008-07-31">=nostiNoJune20$Date),]
cat("size of 'FinalInputData' after July 31, 2008: ",dim(FinalInputData),"\n")
	remove(nostiNoJune20)
	ls()
#######################
#### try rfFuncs
#######################	
set.seed(272)
rf1<-rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
         rfeControl=rfeControl(functions=rfFuncs,method="cv",number=10,saveDetails=FALSE,verbose=TRUE,returnResamp="final")) # rfe = recursive feature elimination

rf1 # display 'Recursive feature selection'
predictors(rf1) # list which predictors were used in the final model. https://www.rdocumentation.org/packages/caret/versions/6.0-77/topics/predictors

#https://topepo.github.io/caret/recursive-feature-elimination.html
#https://www.rdocumentation.org/packages/caret/versions/6.0-77/topics/rfe
# https://www.harding.edu/fmccown/r/
# http://www.cookbook-r.com/Graphs/Output_to_a_file/

### plot "rf1"
RF1RF1FileName=file.path(output.directory,"rf1_RMSEvNVariables.pdf")
pdf(file=RF1RF1FileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(rf1)# ,axes=F, ann=T, cex.lab=0.8, lwd=2)
# Make x axis tick marks without labels
# axis(1, lab=F)
title(main = "RF1 RMSE vs Number of Variables")
#title(main="Autos", col.main="red", font.main=4)
dev.off() # stop writing to pdf file
remove(RF1RF1FileName)
sink() # stop putting text into SinkFileName
LatexFileName=file.path(output.directory,"RF1Images.tex") # Start file for latex code images
sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE)
cat("\n\\subsection{RF1 Images}")
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{rf1_RMSEvNVariables.pdf} \n")
cat("\\caption{\\label{fig:rf1RMSEvNVar}RF1 RMSE vs variable number.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Plot rf1, Rsquared
RF1RsquaredFileName=file.path(output.directory,"rf1_Rsquared.pdf")
pdf(file=RF1RsquaredFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(rf1, metric="Rsquared")
title(main = "RF1 Rsquared by Variable")
dev.off() # stop input to pdf document
remove(RF1RsquaredFileName)
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{rf1_Rsquared.pdf} \n")
cat("\\caption{\\label{fig:rf1Rsquared}RF1 Rsquared vs variable number.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE)# resume putting output into SinkFileName

### Calculation: predicted PM2.5
rf1$fit # display fit information about rf1
predicted<-predict(rf1,newdata=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)]) # find predictions based on the rfe fit. https://www.rdocumentation.org/packages/pathClass/versions/0.9.4/topics/predict.rfe
# predicted is in log scale of PM2.5

### Plot rf1, Log(Predicted vs Observed PM2.5)
RF1LogPredVsObsFileName=file.path(output.directory,"rf1_LogObsVsPredict.pdf")
pdf(file=RF1LogPredVsObsFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(x=FinalInputData$logpm25,y=predicted,xlim=c(0,7),ylim=c(0,7))
abline(0,1) # add a straight line to plot; https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/abline
rsq<-rf1$results[which(rf1$results$RMSE==min(rf1$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
mtext("Random Forest Model Log Predicted vs Observed Plot",side=3)
#title(main = "Predicted vs Observed PM2.5")
dev.off() # stop input to pdf document
remove(RF1LogPredVsObsFileName)
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{rf1_LogObsVsPredict.pdf} \n")
cat("\\caption{\\label{fig:rf1LogObsVsPredict}Observed vs Predicted PM2.5, log scale.} \n") 
cat("\\end{figure} \n \n")
sink()
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Calculation: exp(predicted), i.e., undo the log in the predicted data
rf1.pm25pred=exp(predicted)

### Plot rf1, Predicted vs Observed PM2.5
RF1PredVsObsFileName=file.path(output.directory,"rf1_ObsVsPredict.pdf")
pdf(file=RF1PredVsObsFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(x=FinalInputData$Monitor_PM25,y=rf1.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
abline(0,1) # add a straight line to plot; https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/abline
rsq<-rf1$results[which(rf1$results$RMSE==min(rf1$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=250,y=50)
mtext(side=3,text="Random Forest Model Predicted vs Expected Plot - Regular Scale")
dev.off() # stop input to pdf document
remove(RF1PredVsObsFileName)
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{rf1_ObsVsPredict.pdf} \n")
cat("\\caption{\\label{fig:rf1ObsVsPredict}Observed vs Predicted PM2.5.} \n") 
cat("\\end{figure} \n \n")
sink()
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Calculation: calculate residuals (in log-land)
rf1.resids<-FinalInputData[,52]-predicted

### Plot rf1, Log Residuals PM2.5
RF1LogResidsFileName=file.path(output.directory,"rf1_LogResids.pdf")
pdf(file=RF1LogResidsFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(predicted,rf1.resids,ylab="Residuals",xlab="logged PM2.5")
abline(h=0)
mtext(side=3,text="Random Forest Model Residual Plot")
dev.off() # stop writing to pdf file
remove(RF1LogResidsFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{rf1_LogResids.pdf} \n")
cat("\\caption{\\label{fig:rf1LogResids}Log Residuals of PM2.5 (observed minus predicted).} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Plot rf1, BlandAltman PM2.5
RF1BlandAltmanFileName=file.path(output.directory,"rf1_BlandAltman.pdf")
pdf(file=RF1BlandAltmanFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
BlandAltman(x=FinalInputData$logpm25,y=predicted, gui=FALSE, bandsOn=TRUE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2,
            main="Random Forest Bland-Altman plot")
dev.off() # stop writing to pdf file
remove(RF1BlandAltmanFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{rf1_BlandAltman.pdf} \n")
cat("\\caption{\\label{fig:rf1BlandAltman}Rf1 Bland Altman.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

# Clear variables from RF1 before moving on to next method
remove(rf1,rf1.pm25pred,rf1.resids,rsq,predicted,LatexFileName)
ls()

### Calculation: exp(predicted), i.e., undo the log in the predicted data
#rf1.pm25pred<-exp(predicted)
#rf1.pm25pred=exp(predicted)
#### Plot rf1, Residuals PM2.5 - Not sure if the labeling matches the math
#RF1ResidsFileName=file.path(output.directory,"rf1_Resids.pdf")
#pdf(file=RF1ResidsFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
#plot.new()
#par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
#plot(FinalInputData$Monitor_PM25,rf1.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
#abline(0,1)
#mtext(side=3,text="Random Forest Model Predicted-Expected Plot - Regular Scale")
#dev.off() # stop input to pdf document
#sink() # stop input to SinkFileName document
#sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
#cat("\n\\begin{figure} \n")
#cat("\\centering \n")
#cat("\\includegraphics[width=0.77\\textwidth]{rf1_Resids.pdf} \n")
#cat("\\caption{\\label{fig:rf1Resids}Residuals of PM2.5 (observed minus predicted).} \n") 
#cat("\\end{figure} \n \n")
#sink()
#sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

####################################
# treebagFuncs
####################################

#try treebagFuncs
set.seed(272)
treebag1<-rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
         rfeControl=rfeControl(functions=treebagFuncs,method="cv",number=10,saveDetails=FALSE,verbose=TRUE,returnResamp="final"))

treebag1
predictors(treebag1)

### plot(treebag1)
FigFileName=file.path(output.directory,"treebag1_RMSEvNVariables.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(treebag1)# ,axes=F, ann=T, cex.lab=0.8, lwd=2)
title(main = "Treebag1: RMSE vs Number of Variables")
dev.off() # stop writing to pdf file
remove(FigFileName) # delete pdf file name variable
sink() # stop putting text into SinkFileName
LatexFileName=file.path(output.directory,"Treebag1Images.tex") # Start file for latex code images
sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE)
cat("\n\\subsection{Treebag1 Images}")
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{treebag1_RMSEvNVariables.pdf} \n")
cat("\\caption{\\label{fig:treebag1RMSEvNVar}Treebag1 RMSE vs variable number.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### plot(treebag1, metric="Rsquared")
FigFileName=file.path(output.directory,"treebag1_Rsquared.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(treebag1, metric="Rsquared")
title(main = "Treebag1: Rsquared by Variable")
dev.off() # stop input to pdf document
remove(FigFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{treebag1_Rsquared.pdf} \n")
cat("\\caption{\\label{fig:treebag1Rsquared}Treebag1 Rsquared vs variable number.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE)# resume putting output into SinkFileName

### Calculation: predicted PM2.5
treebag1$fit # display fit information about treebag1
predicted<-predict(treebag1,newdata=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)]) # find predictions based on the rfe fit.
# predicted is in log scale of PM2.5

### Plot treebag1, Log(Predicted vs Observed PM2.5)
FigFileName=file.path(output.directory,"treebag1_LogObsVsPredict.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(x=FinalInputData$logpm25,y=predicted,xlim=c(0,7),ylim=c(0,7))
abline(0,1) # add a straight line to plot; https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/abline
rsq<-treebag1$results[which(treebag1$results$RMSE==min(treebag1$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
mtext("Treebag1 Model Log Predicted vs Observed Plot",side=3)
dev.off() # stop input to pdf document
remove(FigFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{treebag1_LogObsVsPredict.pdf} \n")
cat("\\caption{\\label{fig:treebag1LogObsVsPredict}Observed vs Predicted PM2.5, log scale.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Calculation: exp(predicted), i.e., undo the log in the predicted data
treebag1.pm25pred=exp(predicted)

### Plot treebag1, Predicted vs Observed PM2.5
FigFileName=file.path(output.directory,"treebag1_ObsVsPredict.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(x=FinalInputData$Monitor_PM25,y=treebag1.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
abline(0,1) # add a straight line to plot; https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/abline
rsq<-treebag1$results[which(treebag1$results$RMSE==min(treebag1$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=250,y=50)
mtext(side=3,text="Treebag1 Model: Predicted vs Expected Plot - Regular Scale")
dev.off() # stop input to pdf document
remove(FigFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{treebag1_ObsVsPredict.pdf} \n")
cat("\\caption{\\label{fig:treebag1ObsVsPredict}Observed vs Predicted PM2.5.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Calculation: calculate residuals (in log-land)
treebag1.resids<-FinalInputData[,52]-predicted

### Plot treebag1, Log Residuals PM2.5
FigFileName=file.path(output.directory,"treebag1_LogResids.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(predicted,treebag1.resids,ylab="Residuals",xlab="logged PM2.5")
abline(h=0)
mtext(side=3,text="Treebag1: Residual Plot")
dev.off() # stop writing to pdf file
remove(FigFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{treebag1_LogResids.pdf} \n")
cat("\\caption{\\label{fig:treebag1LogResids}Log Residuals of PM2.5 (observed minus predicted).} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Plot treebag1, BlandAltman PM2.5
FigFileName=file.path(output.directory,"treebag1_BlandAltman.pdf")
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
cat("\\includegraphics[width=0.77\\textwidth]{treebag1_BlandAltman.pdf} \n")
cat("\\caption{\\label{fig:treebag1BlandAltman}Treebag1 Bland Altman.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

# Clear variables from Treebag1 before moving on to next method
remove(treebag1,treebag1.pm25pred,treebag1.resids,rsq,predicted,LatexFileName)
ls()

####################################
# gbmtrainonly
####################################

set.seed(272)
# original # gbmGrid <- expand.grid(interaction.depth = (1:9),n.trees = (1:15)*100,shrinkage = 0.1)
# MMM added n.minobsinnode = 10
gbmGrid <- expand.grid(interaction.depth = (1:9),n.trees = (1:15)*100,shrinkage = 0.1,n.minobsinnode = 10) # https://www.rdocumentation.org/packages/base/versions/3.4.3/topics/expand.grid
gbmtrainonly<-train(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],method = "gbm",
                    trControl=trainControl(method="cv",number=10,verbose=TRUE,returnResamp="final"),tuneGrid=gbmGrid)

gbmtrainonly
predictors(gbmtrainonly)

#summary(gbmtrainonly)
### plot summary(gbmtrainonly)
FigFileName=file.path(output.directory,"gbmtrainonly_summary.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
summary(gbmtrainonly)
title(main = "gbmtrainonly: Summary")
dev.off() # stop writing to pdf file
remove(FigFileName) # delete pdf file name variable
sink() # stop putting text into SinkFileName
LatexFileName=file.path(output.directory,"GBMImages.tex") # Start file for latex code images
sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE)
cat("\n\\subsection{GBM Images}")
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{gbmtrainonly_summary.pdf} \n")
cat("\\caption{\\label{fig:gbmtrainonlysummary}Summary of gbmtrainonly.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### pick up writing code here

# original #gbmGrid2 <- expand.grid(.interaction.depth = c(5,7,9),.n.trees = c(100,300,500),
#                        .shrinkage = 0.1,n.minobsinnode = 10) # MMM changed .nminobsinnode = 10 to  n.minobsinnode = 10

gbmGrid2 <- expand.grid(interaction.depth = c(5,7,9),n.trees = c(100,300,500),
                        shrinkage = 0.1,n.minobsinnode = 10) # MMM changed .nminobsinnode = 10 to  n.minobsinnode = 10, got rid of extra periods
gbm1 <- rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
            rfeControl = rfeControl(functions = caretFuncs,method="cv",number=10,
            verbose=TRUE,returnResamp="all"),
            method = "gbm",tuneGrid=gbmGrid2) # MMM changed gbmGrid to gbmGrid2

gbm1
predictors(gbm1)

### plot(gbm1)
FigFileName=file.path(output.directory,"gbm1_RMSEvNVariables.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(gbm1)# ,axes=F, ann=T, cex.lab=0.8, lwd=2)
title(main = "GBM1: RMSE vs Number of Variables")
dev.off() # stop writing to pdf file
remove(FigFileName) # delete pdf file name variable
sink() # stop putting text into SinkFileName
LatexFileName=file.path(output.directory,"GBM1Images.tex") # Start file for latex code images
sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE)
cat("\n\\subsection{GBM1 Images}")
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{gbm1_RMSEvNVariables.pdf} \n")
cat("\\caption{\\label{fig:gbm1RMSEvNVar}GBM1 RMSE vs variable number.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### plot(gbm1, metric="Rsquared")
FigFileName=file.path(output.directory,"gbm1_Rsquared.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(gbm1, metric="Rsquared")
title(main = "GBM1: Rsquared by Variable")
dev.off() # stop input to pdf document
remove(FigFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{gbm1_Rsquared.pdf} \n")
cat("\\caption{\\label{fig:gbm1Rsquared}GBM1 Rsquared vs variable number.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE)# resume putting output into SinkFileName

### Calculation: predicted PM2.5
gbm1$fit # display fit information about gbm1
predicted=gbm1$fit$finalModel$fit
# predicted is in log scale of PM2.5

### Plot gbm1, Log(Predicted vs Observed PM2.5)
FigFileName=file.path(output.directory,"gbm1_LogObsVsPredict.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(x=FinalInputData$logpm25,y=predicted,xlim=c(0,6),ylim=c(0,6))
abline(0,1) # add a straight line to plot; https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/abline
rsq<-gbm1$results[which(gbm1$results$RMSE==min(gbm1$results$RMSE)),3]
text(labels=paste("CV R-squared=",round(rsq,4)),x=4,y=1)
mtext("GBM1 Model Log Predicted vs Observed Plot",side=3)
dev.off() # stop input to pdf document
remove(FigFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{gbm1_LogObsVsPredict.pdf} \n")
cat("\\caption{\\label{fig:gbm1LogObsVsPredict}Observed vs Predicted PM2.5, log scale.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Calculation: exp(predicted), i.e., undo the log in the predicted data
gbm1.pm25pred=exp(predicted)

### Plot gbm1, Predicted vs Observed PM2.5
FigFileName=file.path(output.directory,"gbm1_ObsVsPredict.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(x=FinalInputData$Monitor_PM25,y=gbm1.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
abline(0,1) # add a straight line to plot; https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/abline
rsq<-gbm1$results[which(gbm1$results$RMSE==min(gbm1$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=250,y=50)
mtext(side=3,text="GBM1 Model: Predicted vs Expected Plot - Regular Scale")
dev.off() # stop input to pdf document
remove(FigFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{gbm1_ObsVsPredict.pdf} \n")
cat("\\caption{\\label{fig:gbm1ObsVsPredict}Observed vs Predicted PM2.5.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Calculation: calculate residuals (in log-land)
gbm1.resids<-FinalInputData[,52]-predicted

### Plot gbm1, Log Residuals PM2.5
FigFileName=file.path(output.directory,"gbm1_LogResids.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(predicted,gbm1.resids,ylab="Residuals",xlab="logged PM2.5")
abline(h=0)
mtext(side=3,text="GBM1: Residual Plot")
dev.off() # stop writing to pdf file
remove(FigFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{gbm1_LogResids.pdf} \n")
cat("\\caption{\\label{fig:gbm1LogResids}Log Residuals of PM2.5 (observed minus predicted).} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### Plot gbm1, BlandAltman PM2.5
FigFileName=file.path(output.directory,"gbm1_BlandAltman.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
BlandAltman(x=FinalInputData$logpm25,y=predicted, gui=FALSE, bandsOn=TRUE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2,
            main="GBM Bland-Altman plot")
dev.off() # stop writing to pdf file
remove(FigFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{gbm1_BlandAltman.pdf} \n")
cat("\\caption{\\label{fig:gbm1BlandAltman}GBM1 Bland Altman.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

# Clear variables from GBM1 before moving on to next method
remove(gbm1,gbm1.pm25pred,gbm1.resids,rsq,predicted,LatexFileName)
remove(gbmGrid,gbmGrid2,gbmtrainonly)
ls()

####################################
# glmnet
# This section of code is commented because it fails with this error 
#Error in { : 
#    task 1 failed - "x should be a matrix with 2 or more columns"
####################################

#try glmnet
# rfe documentation: https://www.rdocumentation.org/packages/caret/versions/6.0-78/topics/rfe
#set.seed(272)
#glmnet1<- rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
#                  rfeControl = rfeControl(functions = caretFuncs,method="cv",number=10,saveDetails=FALSE,verbose=TRUE,returnResamp="final"),
#                  method = "glmnet") ## pass options to train()#
#
#glmnet1
#predictors(glmnet1)
#plot(glmnet1)
#glmnet1$fit
#predicted<-predict(glmnet1,newdata=nonemiss[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)])
#plot(x=nonemiss$logpm25,y=predicted,xlim=c(0,7),ylim=c(0,7))   #need to fix this
#abline(0,1)
#rsq<-glmnet1$results[which(glmnet1$results$RMSE==min(glmnet1$results$RMSE)),3]
#text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
#mtext("glmnet Predicted Observed Plot",side=3)
#glmnet1.pm25pred<-exp(predicted)
#glmnet1.resids<-nonemiss[,52]-predicted
#plot(nonemiss$Monitor_PM25,glmnet1.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
#abline(0,1)
#mtext(side=3,text="glmnet Model Predicted-Expected Plot - Regular Scale")
#plot(predicted,glmnet1.resids,ylab="Residuals",xlab="logged PM2.5")
#abline(h=0)
#mtext(side=3,text="glmnet Model Residual Plot")
#BlandAltman(x=nonemiss$logpm25,y=predicted, gui=FALSE, bandsOn=TRUE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2, main="glmnet Bland-Altman plot")

####################################
# earth
####################################
#earth
set.seed(272)
earth1<- rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
                  rfeControl = rfeControl(functions = caretFuncs,method="cv",number=10,
                                          saveDetails=FALSE,verbose=TRUE,returnResamp="final"),
                  ## pass options to train()
                  method = "earth")
### pick up testing code here
earth1
predictors(earth1)

### plot(earth1)
FigFileName=file.path(output.directory,"earth1_RMSEvNVariables.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(earth1)# ,axes=F, ann=T, cex.lab=0.8, lwd=2)
title(main = "Earth1: RMSE vs Number of Variables")
dev.off() # stop writing to pdf file
remove(FigFileName) # delete pdf file name variable
sink() # stop putting text into SinkFileName
LatexFileName=file.path(output.directory,"Earth1Images.tex") # Start file for latex code images
sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE)
cat("\n\\subsection{Earth1 Images}")
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{earth1_RMSEvNVariables.pdf} \n")
cat("\\caption{\\label{fig:earth1RMSEvNVar}Earth1 RMSE vs variable number.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

### plot(earth1, metric="Rsquared")
FigFileName=file.path(output.directory,"earth1_Rsquared.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(earth1, metric="Rsquared")
title(main = "Earth1: Rsquared by Variable")
dev.off() # stop input to pdf document
remove(FigFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{earth1_Rsquared.pdf} \n")
cat("\\caption{\\label{fig:earth1Rsquared}Earth1 Rsquared vs variable number.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE)# resume putting output into SinkFileName

### Calculation: predicted PM2.5
earth1$fit # display fit information about earth1
predicted<-predict(earth1,newdata=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)]) # find predictions based on the rfe fit.
# predicted is in log scale of PM2.5

#### pick up writing code here

plot(x=nonemiss$logpm25,y=predicted,xlim=c(0,7),ylim=c(0,7))
abline(0,1)
rsq<-earth1$results[which(earth1$results$RMSE==min(earth1$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
mtext("earth (MARS) Predicted Observed Plot",side=3)

### Plot treebag1, Log(Predicted vs Observed PM2.5)
FigFileName=file.path(output.directory,"treebag1_LogObsVsPredict.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
plot.new()
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
plot(x=FinalInputData$logpm25,y=predicted,xlim=c(0,7),ylim=c(0,7))
abline(0,1) # add a straight line to plot; https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/abline
rsq<-treebag1$results[which(treebag1$results$RMSE==min(treebag1$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
mtext("Treebag1 Model Log Predicted vs Observed Plot",side=3)
dev.off() # stop input to pdf document
remove(FigFileName) # delete pdf file name variable
sink() # stop input to SinkFileName document
sink(file = LatexFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting text into latex document
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat("\\includegraphics[width=0.77\\textwidth]{treebag1_LogObsVsPredict.pdf} \n")
cat("\\caption{\\label{fig:treebag1LogObsVsPredict}Observed vs Predicted PM2.5, log scale.} \n") 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName



earth1.pm25pred<-exp(predicted)
earth1.resids<-nonemiss[,52]-predicted
plot(nonemiss$Monitor_PM25,earth1.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
abline(0,1)
mtext(side=3,text="earth Model Predicted-Expected Plot - Regular Scale")
plot(predicted,earth1.resids,ylab="Residuals",xlab="logged PM2.5")
abline(h=0)
mtext(side=3,text="earth Model Residual Plot")
BlandAltman(x=nonemiss$logpm25,y=predicted, gui=FALSE, bandsOn=TRUE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2, main="earth Bland-Altman plot")

#lasso
set.seed(272)
# pick up writing code here - doesn't seem to be working right
lasso1<- rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
                  rfeControl = rfeControl(functions = caretFuncs,method="cv",number=10,
                                          saveDetails=FALSE,verbose=TRUE,returnResamp="final"),
                  ## pass options to train()
                  method = "lasso")
lasso1
plot(lasso1)
predictors(lasso1)
lasso1$fit
predicted<-predict(lasso1,nonemiss[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)])
plot(x=nonemiss$logpm25,y=predicted,xlim=c(0,7),ylim=c(0,7))
abline(0,1)
rsq<-lasso1$results[which(lasso1$results$RMSE==min(lasso1$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
mtext("lasso Predicted Observed Plot",side=3)
lasso1.pm25pred<-exp(predicted)
lasso1.resids<-nonemiss[,52]-predicted
plot(nonemiss$Monitor_PM25,lasso1.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
abline(0,1)
mtext(side=3,text="lasso Model Predicted-Expected Plot - Regular Scale")
plot(predicted,lasso1.resids,ylab="Residuals",xlab="logged PM2.5")
abline(h=0)
mtext(side=3,text="lasso Model Residual Plot")
BlandAltman(x=nonemiss$logpm25,y=predicted, gui=FALSE, bandsOn=TRUE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2, main="lasso Bland-Altman plot")

#svmRadial
set.seed(272)
# tested this line:
svmRadial1<- rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
                  rfeControl = rfeControl(functions = caretFuncs,method="cv",number=10,
                                          saveDetails=FALSE,verbose=TRUE,returnResamp="final"),
                  ## pass options to train()
                  method = "svmRadial")
svmRadial1
plot(svmRadial1)
predictors(svmRadial1)
svmRadial1$fit
predicted<-predict(svmRadial1,nonemiss[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)])
plot(x=nonemiss$logpm25,y=predicted,xlim=c(0,7),ylim=c(0,7))
abline(0,1)
rsq<-svmRadial1$results[which(svmRadial1$results$RMSE==min(svmRadial1$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
mtext("svm Radial Predicted Observed Plot",side=3)
svmRadial1.pm25pred<-exp(predicted)
svmRadial1.resids<-nonemiss[,52]-predicted
plot(nonemiss$Monitor_PM25,svmRadial1.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
abline(0,1)
mtext(side=3,text="svmRadial Model Predicted-Expected Plot - Regular Scale")
plot(predicted,svmRadial1.resids,ylab="Residuals",xlab="logged PM2.5")
abline(h=0)
mtext(side=3,text="svmRadial Model Residual Plot")
BlandAltman(x=nonemiss$logpm25,y=predicted, gui=FALSE, bandsOn=TRUE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2, main="svmRadial Bland-Altman plot")

#gaussprRadial
set.seed(272)
# tested this line
gaussprRadial1<- rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
                  rfeControl = rfeControl(functions = caretFuncs,method="cv",number=10,
                                          saveDetails=FALSE,verbose=TRUE,returnResamp="final"),
                  ## pass options to train()
                  method = "gaussprRadial")
gaussprRadial1
plot(gaussprRadial1)
predictors(gaussprRadial1)
gaussprRadial1$fit
predicted<-predict(gaussprRadial1,nonemiss[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)])
plot(x=nonemiss$logpm25,y=predicted,xlim=c(0,7),ylim=c(0,7))
abline(0,1)
rsq<-gaussprRadial1$results[which(gaussprRadial1$results$RMSE==min(gaussprRadial1$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
mtext("Gaussian Processes Radial Predicted Observed Plot",side=3)
gaussprRadial1.pm25pred<-exp(predicted)
gaussprRadial1.resids<-nonemiss[,52]-predicted
plot(nonemiss$Monitor_PM25,gaussprRadial1.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
abline(0,1)
mtext(side=3,text="gaussprRadial Model Predicted-Expected Plot - Regular Scale")
plot(predicted,gaussprRadial1.resids,ylab="Residuals",xlab="logged PM2.5")
abline(h=0)
mtext(side=3,text="gaussprRadial Model Residual Plot")
BlandAltman(x=nonemiss$logpm25,y=predicted, gui=FALSE, bandsOn=TRUE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2, main="gaussprRadial Bland-Altman plot")

#glm
set.seed(272)
#tested this line
glm1<- rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
                  rfeControl = rfeControl(functions = caretFuncs,method="cv",number=10,
                                          saveDetails=FALSE,verbose=TRUE,returnResamp="final"),
                  ## pass options to train()
                  method = "glm")
glm1
predictors(glm1)
plot(glm1)
glm1$fit
predicted<-predict(glm1,newdata=nonemiss[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)])
plot(x=nonemiss$logpm25,y=predicted,xlim=c(0,7),ylim=c(0,7))
abline(0,1)
rsq<-glm1$results[which(glm1$results$RMSE==min(glm1$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
mtext("glm Predicted Observed Plot",side=3)
glm1.pm25pred<-exp(predicted)
glm1.resids<-nonemiss[,52]-predicted
plot(nonemiss$Monitor_PM25,glm1.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
abline(0,1)
mtext(side=3,text="glm Model Predicted-Expected Plot - Regular Scale")
plot(predicted,glm1.resids,ylab="Residuals",xlab="logged PM2.5")
abline(h=0)
mtext(side=3,text="glm Model Residual Plot")
BlandAltman(x=nonemiss$logpm25,y=predicted, gui=FALSE, bandsOn=TRUE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2, main="glm Bland-Altman plot")

#knn
set.seed(272)
#tested this line
knn1<- rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
                  rfeControl = rfeControl(functions = caretFuncs,method="cv",number=10,
                                          saveDetails=FALSE,verbose=TRUE,returnResamp="final"),
                  ## pass options to train()
                  method = "knn")
knn1
plot(knn1)
predictors(knn1)
knn1$fit
predicted<-predict(knn1,newdata=nonemiss[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)])
plot(x=nonemiss$logpm25,y=predicted,xlim=c(0,7),ylim=c(0,7))
abline(0,1)
rsq<-knn1$results[which(knn1$results$RMSE==min(knn1$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
mtext("knn Predicted Observed Plot",side=3)
knn1.pm25pred<-exp(predicted)
knn1.resids<-nonemiss[,52]-predicted
plot(nonemiss$Monitor_PM25,knn1.pm25pred,xlab="Observed PM2.5",ylab="Predicted PM2.5")
abline(0,1)
mtext(side=3,text="knn Model Predicted-Expected Plot - Regular Scale")
plot(predicted,knn1.resids,ylab="Residuals",xlab="logged PM2.5")
abline(h=0)
mtext(side=3,text="knn Model Residual Plot")
BlandAltman(x=nonemiss$logpm25,y=predicted, gui=FALSE, bandsOn=TRUE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2, main="knn Bland-Altman plot")

#try gamFuncs
set.seed(272)
# tested this line
gam1<-rfe(x=FinalInputData[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)],y=FinalInputData[,52],sizes=c(1:29),
         rfeControl=rfeControl(functions=gamFuncs,method="cv",number=10,saveDetails=FALSE,verbose=TRUE,returnResamp="final"))
gam1
predictors(gam1)
plot(gam1)
plot(gam1, metric="Rsquared")
gam1$fit
predicted<-predict(gam1,newdata=nonemiss[,c(9,10,23,25:30,32,34,36,38,39,41,43,58:61,63,64,67,70:75)])
plot(x=nonemiss$logpm25,y=predicted[,1],xlim=c(0,7),ylim=c(0,7))
abline(0,1)
rsq<-gam1$results[which(gam1$results$RMSE==min(gam1$results$RMSE)),3]
text(labels=paste("R-squared=",round(rsq,4)),x=5,y=1)
mtext("GAM Model Predicted Observed Plot",side=3)
gam1.pm25pred<-exp(predicted)
gam1.resids<-nonemiss[,52]-predicted
plot(nonemiss$Monitor_PM25,gam1.pm25pred[,1],xlab="Observed PM2.5",ylab="Predicted PM2.5")
abline(0,1)
mtext(side=3,text="GAM Model Predicted-Expected Plot - Regular Scale")
plot(predicted,gam1.resids[,1],ylab="Residuals",xlab="logged PM2.5")
abline(h=0)
mtext(side=3,text="GAM Model Residual Plot")
BlandAltman(x=nonemiss$logpm25,y=predicted[,1], gui=FALSE, bandsOn=TRUE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2,
            main="GAM Bland-Altman plot")
dev.off() # https://www.rdocumentation.org/packages/grDevices/versions/3.4.3/topics/dev
save.image(file="C:/Forest_Fire_Project/ExposureAssess_SuperLearner/VariableSelection_rfe_caretPackage/nonemiss_29vars_WithConfounders_data.Rdata")
sink()


