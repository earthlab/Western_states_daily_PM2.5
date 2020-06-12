# this code based on Colleen Reid's 2008 wildfire study, and uses that data

# set working directory (setwd) http://rfunction.com/archives/1001
uppermost.directory="/home/rstudio" # on AWS
working.directory=uppermost.directory # on AWS
setwd(working.directory)
output.directory=file.path(working.directory,"Code_Outputs")

#cat("SET MEMORY LIMIT\n") # is this necessary?
#memory.limit(1000000000) # commenting on AWS

# Download latest version of R: https://cran.r-project.org/bin/windows/base/
# The code is written for R version 3.4.3
# explanation of data types in R: http://resbaz.github.io/2014-r-materials/lessons/01-intro_r/data-structures.html

# sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
SinkFileName=file.path(output.directory,"Project2008_Wildfire_create_final_ML_input.txt")
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
long<-read.csv(file.path(working.directory,"AllforCaret.csv"),header=TRUE) # load AllforCaret.csv data and put it in variable called "long"; and keep header names
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
# check that the number of removed plus the number of rows to keep matches the original size
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

# Save Input file to be used in machine learning (in other scripts)
write.csv(FinalInputData, file = "FinalInputDataFile.csv")