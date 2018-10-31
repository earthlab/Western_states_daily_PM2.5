# clean_AllforCaret.R

cat("Code and R output for clean_AllforCaret.R \n \n")
############################################################################
cat("Title: clean_AllforCaret.R \n")
cat("Author: Melissa Maestas \n")
cat("Original Date: October 15, 2018 \n")
cat("Latest Update: October 15, 2018 \n")
cat("clean the AllforCaret.csv file provided by Colleen Reid (Northern CA, 2008) \n")
############################################################################

processed_data_version <- "practice"

cat("read in the datasets \n")
long<-read.csv(file.path(ProcessedData.directory,"AllforCaret.csv"),header=TRUE) # load AllforCaret.csv data and put it in variable called "long"; and keep header names
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

#file_sub_label <- paste("AllforCaret_cleaned_StepPractice_",Sys.Date(),"_part_",processed_data_version,sep = "")
file_sub_label <- paste("AllforCaret_cleaned_StepPractice_part_",processed_data_version,sep = "")

write.csv(FinalInputData,file = file.path(ProcessedData.directory,paste(file_sub_label,'.csv',sep = "")),row.names = FALSE)
