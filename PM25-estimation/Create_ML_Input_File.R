# Process data from Seth Lyman

# define directories
uppermost.directory="/home/rstudio" # on AWS
working.directory=uppermost.directory # on AWS
setwd(working.directory)
output.directory=file.path(working.directory,"Code_Outputs")
ProcessedData.directory=file.path(working.directory,"Processed_Data")
StartData.directory=file.path(working.directory,"Uintah Basin PM2.5")

# sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
SinkFileName=file.path(output.directory,"Lyman_Data_Processing.txt")
sink(file =SinkFileName, append = FALSE, type = c("output","message"),
     split = FALSE)

cat("Code and R output for Process_Lyman_data.R \n \n")
############################################################################
cat("Title: Process_Lyman_data.R \n")
cat("Author: Melissa May Maestas \n")
cat("Original Date: January 23, 2018 \n")
cat("Latest Update: January 23, 2018 \n")
cat("This program reads in and process the Uintah Basin PM2.5 data provided by Seth Lyman. \n")
############################################################################

cat("INSTALL PACKAGES \n")

# Start Input file for machine learning
input_header= c('ID','POC','Parameter','Method','Winter','Year','Month','Day','PM2.5_Obs','PM2.5_Lat','PM2.5_Lon','PM25_Station_Name')
N_columns=length(input_header)
input_mat1=data.frame(matrix(NA,nrow=10,ncol=N_columns))
names(input_mat1)=input_header

##### Fill in Lyman Uintah Basin data
UBdata<-read.csv(file.path(StartData.directory,"FinalPM2.5_multiyear_thruwint2017_sheet1.csv"),header=TRUE) 
row_start=1
row_stop=dim(UBdata)[1]
#for (this_column in UBdata[c("Roosevelt..24hr.avg.PM2.5.","Vernal..24hr.avg.PM2.5.")]){
#for(i in 1:10) {
for(this_column in 5:7){  
  #print(paste("The year is", year))
  #which( colnames(df)=="b" )
  print(paste("Column number = ",this_column))
  #this_name=names(UBdata[this_columnl])
  this_name=colnames(UBdata)[this_column]
  print(paste(this_name))
  input_mat1[row_start:row_stop,c('Winter')]=UBdata[,"Winter."]
  input_mat1[row_start:row_stop,c('Year')]=UBdata[,"year"]
  input_mat1[row_start:row_stop,c('PM2.5_Obs')]=UBdata[,this_column]
  input_mat1[row_start:row_stop,c('PM25_Station_Name')]=this_name
  row_start=row_stop+1
  row_stop=row_start+dim(UBdata)[1]-1
}

################################################

#for (year in UBdata[c(2010,2011,2012,2013,2014,2015)]){
#  print(paste("The year is", year))
#}
#which( colnames(df)=="b" )
# 
# input_mat1=matrix(NA, 4, 0)
# 
# m2 <- cbind(1, 1:4)
# colnames(m2, do.NULL = FALSE)
# colnames(m2) <- c("x","Y")
# 
# input_mat1=data.frame()
# 
# 
# 
# data.frame(..., row.names = NULL, check.rows = FALSE,
#            check.names = TRUE, fix.empty.names = TRUE,
#            stringsAsFactors = default.stringsAsFactors())
# 
# 
# col_headings <- c('heading1','heading2', ....,'heading_c')
# names(your_dataframe) <- col_headings
# 
# input_header= c('ID','POC','Parameter','Method','Winter','Year','Month','Day','PM2.5_Obs','PM2.5_Lat','PM2.5_Lon','PM25_Station_Name')
# input_mat1=data.frame(row.names=input_header)
# 
# try_mat=data.frame(matrix(NA, nrow = 2, ncol = 3))