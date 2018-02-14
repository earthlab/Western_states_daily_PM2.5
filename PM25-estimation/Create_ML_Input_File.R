# Create input file for Machine Learning estimation of PM2.5 for the western US, 2008-2014

# To clear all variables and start fresh:
# rm(list = ls())

# define directories
uppermost.directory="/home/rstudio" # on AWS
working.directory=uppermost.directory # on AWS
setwd(working.directory)
output.directory=file.path(working.directory,"Code_Outputs")
ProcessedData.directory=file.path(working.directory,"Processed_Data")
StartData.directory=file.path(working.directory,"PM25_Uintah_Basin")
USMaps.directory=file.path(working.directory,"Shapefiles_for_mapping","cp_2016_us_state_500k")
PCAPSData.directory=file.path(working.directory,"PM25_PCAPS_Salt_Lake")
AQSData.directory=file.path(working.directory,"AQS_Daily_Summaries")
start_study_year <- 2008
stop_study_year <- 2014
# sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
SinkFileName=file.path(output.directory,"Lyman_Data_Processing.txt")
sink(file =SinkFileName, append = FALSE, type = c("output","message"),
     split = FALSE)

cat("Code and R output for Process_Lyman_data.R \n \n")
############################################################################
cat("Title: Create_ML_Input_File.R \n")
cat("Author: Melissa May Maestas \n")
cat("Original Date: January 23, 2018 \n")
cat("Latest Update: February 11, 2018 \n")
cat("This program reads in and process the Uintah Basin PM2.5 data provided by Seth Lyman. \n")
############################################################################

cat("INSTALL PACKAGES \n")
#install.packages(pkgs="maps")
#install.packages(pkgs="mapproj")
install.packages('ggplot2')
install.packages(pkgs="ggmap")
install.packages(pkgs="rgdal")
install.packages(pkgs="rgeos")
install.packages(pkgs="maptools")
install.packages(pkgs="dplyr")
install.packages(pkgs="tidyr")
#install.packages(pkgs="tmap")
#install.packages('leaflet')


cat("Libraries")
#library(maps)
#library(mapproj)
library(ggplot2)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)
#library(tmap)


######################## Start Input file for machine learning#######################
input_header= c('ID','Parameter','Method','Winter','RDates','Year','Month','Day','PM2.5_Obs','PM2.5_Lat','PM2.5_Lon','PM25_Station_Name','Source_File','Data_Source_Counter')
N_columns=length(input_header)
input_mat1=data.frame(matrix(NA,nrow=10,ncol=N_columns))
names(input_mat1)=input_header

############################## Pull in AQS data #################
data_source_counter=0
row_start=1 # start row counter
ParameterCode_vec <- cbind(88101,88502)

# cycle through files
for(this_year in start_study_year:stop_study_year){     # cycle through years
  for(this_ParamCode in ParameterCode_vec){ # cycle through Parameter Codes
    this_source_file <- paste('daily_',as.character(this_ParamCode),'_',as.character(this_year),'.csv',sep="")
    print(this_source_file)
    # load the AQS file
    ThisAQSdata<-read.csv(file.path(AQSData.directory,this_source_file),header=TRUE) 
    
    # isolate data in study states
    #class(ThisAQSdata$State.Code)
    ThisAQSdata_StudyStates <- ThisAQSdata[which(ThisAQSdata$State.Code==4|ThisAQSdata$State.Code==6|ThisAQSdata$State.Code==8|ThisAQSdata$State.Code==16|ThisAQSdata$State.Code==30|ThisAQSdata$State.Code==32|ThisAQSdata$State.Code==35|ThisAQSdata$State.Code==41|ThisAQSdata$State.Code==49|ThisAQSdata$State.Code==53|ThisAQSdata$State.Code==56), ]
    rm(ThisAQSdata)
    #unique(ThisAQSdata_StudyStates$State.Name)
    row_stop <- row_start+dim(ThisAQSdata_StudyStates)[1]-1
    
    # input data source counter - indicates if this is EPA data or field data, etc.
    input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
    
    # input dates
    new_col_number <- length(ThisAQSdata_StudyStates)+1
    ThisAQSdata_StudyStates[,new_col_number] <- as.Date(ThisAQSdata_StudyStates[,c("Date.Local")],"%Y-%m-%d") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
    colnames(ThisAQSdata_StudyStates)[new_col_number] <- "R_Dates"
    input_mat1[row_start:row_stop,c("RDates")] <- format(ThisAQSdata_StudyStates[,c("R_Dates")],"%Y-%m-%d")
    rm(new_col_number)
    
    # input station names into input_mat1
    AQSStations <- ThisAQSdata_StudyStates[,c("Local.Site.Name")]
    #print(AQSStations)
    AQSstationsChar <- as.character(AQSStations)
    #print(AQSstationsChar)
    input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- AQSstationsChar
    rm(AQSStations,AQSstationsChar)
    
    # input lat and lon
    input_mat1[row_start:row_stop,c("PM2.5_Lat")] <- ThisAQSdata_StudyStates[,c('Latitude')]
    input_mat1[row_start:row_stop,c("PM2.5_Lon")] <- ThisAQSdata_StudyStates[,c('Longitude')]
    
    # input PM2.5 concentration
    input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- ThisAQSdata_StudyStates[,c("Arithmetic.Mean")]

    # input source file name
    input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
    
    # input parameter code and method name
    input_mat1[row_start:row_stop,c("Parameter")] <- this_ParamCode
    input_mat1[row_start:row_stop,c("Method")] <- ThisAQSdata_StudyStates[,c("Method.Name")]
    
    # update row counter
    row_start=row_stop+1
    
    # clear variables before moving on to next iteration of loop
    rm(this_source_file,ThisAQSdata_StudyStates)
  } # for(this_POC in POC_vec){ # cycle through POC
} # for(this_year in start_study_year:stop_study_year){     # cycle through years

rm(ParameterCode_vec,this_year,this_ParamCode)

####### Fill in Lyman Uintah Basin data ########################
data_source_counter=data_source_counter+1
this_source_file <- "FinalPM2.5_multiyear_thruwint2017_sheet1_dates.csv"
print(this_source_file)

# load data
UBdata<-read.csv(file.path(StartData.directory,this_source_file),header=TRUE) 

# handle date information
new_col_number <- length(UBdata)+1 # figure out how many columns are in UBdata and then add 1
UBdata[,new_col_number] <- as.Date(UBdata[,c("Dates")],"%m/%d/%Y") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
colnames(UBdata)[new_col_number] <- "R_Dates"
rm(new_col_number)

# load file with lat/lon for Uintah Basin stations
UBLocations <- read.csv(file.path(StartData.directory,"FinalPM2.5_multiyear_thruwint2017_GISsheet.csv"),header=TRUE)

#row_start <- 1
row_stop=row_start+dim(UBdata)[1]-1

for(this_column in 6:15){  
  print(paste("Column number = ",this_column))
  this_name=colnames(UBdata)[this_column]
  print(this_name)
  #print(paste(this_name)) #COMMENT
  
  # input data source counter - indicates if this is EPA data or field data, etc.
  input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
  
  # input station names into input_mat1
  input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- this_name
  
  # input PM2.5 concentration
  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- UBdata[,this_column]
  
  # input source file name
  input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
  print(UBdata[,"R_Dates"])
  #input_mat1[row_start:row_stop,c('RDates')] <- as.Date(UBdata[,c("Dates")],"%m/%d/%Y")#UBdata[,"R_Dates"]
  
  # input dates
  input_mat1[row_start:row_stop,c('RDates')] <- format(UBdata[,c("R_Dates")], "%Y-%m-%d")

  # input lat and lon
  if(this_name=="Roosevelt..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[1,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[1,c('long')]
  } else if(this_name=="Vernal..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[2,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[2,c('long')]
  } else if(this_name=="Ouray..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[3,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[3,c('long')]
  } else if(this_name=="Red.Wash..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[4,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[4,c('long')]
  } else if(this_name=="Myton..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[5,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[5,c('long')]
  } else if(this_name=="RabbitMtn..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[6,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[6,c('long')]
  } else if(this_name=="Horsepool..24hr.avg.PM2.5."){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[7,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[7,c('long')]
  } else if(this_name=="Ft..Duchesne"){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[8,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[8,c('long')]
  } else if(this_name=="Randlett"){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[9,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[9,c('long')]
  } else if(this_name=="Rangely"){
    input_mat1[row_start:row_stop,c('PM2.5_Lat')] <- UBLocations[10,c('lat')]
    input_mat1[row_start:row_stop,c('PM2.5_Lon')] <- UBLocations[10,c('long')]
  } else {
    stop(1, call. = TRUE, domain = NULL)
    geterrmessage("Loop should not have called this path in the if-statement")
  }
  
  # input other information
  input_mat1[row_start:row_stop,c('Winter')] <- UBdata[,"Winter."]
  input_mat1[row_start:row_stop,c('Year')] <- UBdata[,"year"]
  
  row_start <- row_stop+1
  row_stop <- row_start+dim(UBdata)[1]-1
}
rm(this_column,this_name,this_source_file)
rm(UBdata,UBLocations)

############################# Fill in Salt Lake City PCAPS data ############################
data_source_counter=data_source_counter+1
# load the data file
this_source_file <- "MiniVol_data_dates.csv"
PCAPSdata<-read.csv(file.path(PCAPSData.directory,this_source_file),header=TRUE) 
row_stop <- row_start+dim(PCAPSdata)[1]-1

# handle date information
new_col_number <- length(PCAPSdata)+1
PCAPSdata[,new_col_number] <- as.Date(PCAPSdata[,c("Dates")],"%m/%d/%Y") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
colnames(PCAPSdata)[new_col_number] <- "R_Dates"

# input station names
PCAPSstations <- PCAPSdata[,c('Location')]
print(PCAPSstations)
PCAPSstationsChar <- as.character(PCAPSstations)
print(PCAPSstationsChar)
input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- PCAPSstationsChar
rm(PCAPSstations,PCAPSstationsChar)

# load file containing lat/lon info for PCAPS sites
PCAPSLocations<-read.csv(file.path(PCAPSData.directory,"PCAPS_Site_Locations.csv"),header=TRUE) 

# input PM2.5 concentration
this_column <- which(colnames(PCAPSdata)=="ug.m3")
  print(paste("Column number = ",this_column))
  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- PCAPSdata[,this_column]
  
  # input dates
   input_mat1[row_start:row_stop,c('RDates')] <- format(PCAPSdata[,c("R_Dates")], "%Y-%m-%d")
   input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file

   # input data source counter - indicates if this is EPA data or field data, etc.
   input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter
   
   # input lat/lon information  
for(this_row in row_start:row_stop){     
   
  this_name <- input_mat1[this_row,c('PM25_Station_Name')]
  print(this_name)
 
  if(this_name=="Hawthorne"){
    input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[2,c('Latitude')]
    input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[2,c('Longitude')] 
  } 
  else if(this_name=="2nd Ave"){
    input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[4,c('Latitude')]
    input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[4,c('Longitude')]
  }
  else if(this_name=="9th Ave"){
    input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[5,c('Latitude')]
    input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[5,c('Longitude')]   
  }
  else if(this_name=="Hilltop"){
    input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[7,c('Latitude')]
    input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[7,c('Longitude')]   
  } 
  else if(this_name=="5400 ft"){
    input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[8,c('Latitude')]
    input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[8,c('Longitude')]   
  } 
  else if(this_name=="5680 ft"){
    input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[9,c('Latitude')]
    input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[9,c('Longitude')]   
  } 
  else if(this_name=="5820 ft"){
    input_mat1[this_row,c('PM2.5_Lat')] <- PCAPSLocations[10,c('Latitude')]
    input_mat1[this_row,c('PM2.5_Lon')] <- PCAPSLocations[10,c('Longitude')]   
  } 
  else {
    stop(1, call. = TRUE, domain = NULL)
    geterrmessage("Loop should not have called this path in the if-statement")
  }
}
   
   rm(new_col_number,this_column,this_name,this_source_file,this_row) 
  row_start <- row_stop+1
  #row_stop=row_start+dim(PCAPSdata)[1]-1
rm(PCAPSdata,PCAPSLocations,PCAPSstationsChar,PCAPSstations)

############################# Fill in data from Federal Land Managers ######################



############################# map locations #########################
# Resources for mapping
# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

# map boundaries of western US states
USmap=readOGR(dsn=file.path(USMaps.directory),layer = "cb_2016_us_state_500k")
#USmap=readOGR(dsn="/home/rstudio/Shapefiles_for_mapping/cp_2016_us_state_500k",layer = "cb_2016_us_state_500k")

# COMMENT following lines
head(USmap@data,n=2)
sapply(USmap@data,class)
USmap$ALAND_NUM <- as.numeric(as.character(USmap$ALAND))
mean(USmap$ALAND_NUM)
nrow(USmap)
ncol(USmap)
#plot(USmap)

# have R recognize state FP's as numerical values (in a new column)
USmap$STATEFP_NUM <- as.numeric(as.character(USmap$STATEFP))

# display the State FP values and state abbreviations next to each other
USmap@data[,c("STATEFP_NUM","STUSPS")]

# find the 11 western states included in the study
WestUSmap=USmap@data[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53,]
print(WestUSmap)

# start file for map
FigFileName=file.path(output.directory,"MapPM25_All_Obs.pdf")
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
#plot.new()
WestUSmapGeom=USmap[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53,]
plot(WestUSmapGeom)

# cycle through each data source (EPA and various field campaigns) and plot each in a different color
for(this_data_source_counter in 0:data_source_counter){     
 print(this_data_source_counter) 

  # isolate data from this data source (in loop iteration) 
  This_data <- input_mat1[which(input_mat1$Data_Source_Counter==this_data_source_counter), ]
  
  # find unique locations in data https://stats.stackexchange.com/questions/6759/removing-duplicated-rows-data-frame-in-r
  repeated_locations=This_data[,c("PM2.5_Lat","PM2.5_Lon")]
  duplicated(repeated_locations)
  repeated_locations[duplicated(repeated_locations), ]
  non_repeat_locations <- repeated_locations[!duplicated(repeated_locations), ]
  #plot(non_repeat_locations[,2],non_repeat_locations[,1])

  if(this_data_source_counter==0){
    points(non_repeat_locations[,2],non_repeat_locations[,1],col="black",cex=.6) # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
  } 
  else if(this_data_source_counter==1){
    points(non_repeat_locations[,2],non_repeat_locations[,1],col="red",cex=.6)
  }
  else if(this_data_source_counter==2){
    points(non_repeat_locations[,2],non_repeat_locations[,1],col="green",cex=.6)
  }
  else {
    stop(1, call. = TRUE, domain = NULL)
    geterrmessage("Loop should not have called this path in the if-statement")
  }
  rm(This_data)
} # for(this_data_source_counter in 0:data_source_counter){    



par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
#summary(gbmtrainonly)
title(main = "All PM2.5 Observation Locations")
dev.off() # stop writing to pdf file
remove(FigFileName) # delete pdf file name variable

## find unique locations in data https://stats.stackexchange.com/questions/6759/removing-duplicated-rows-data-frame-in-r
#repeated_locations=input_mat1[,c("PM2.5_Lat","PM2.5_Lon")]
#duplicated(repeated_locations)
#repeated_locations[duplicated(repeated_locations), ]
#non_repeat_locations <- repeated_locations[!duplicated(repeated_locations), ]
#plot(non_repeat_locations[,2],non_repeat_locations[,1])


#plot(non_repeat_locations[,2],non_repeat_locations[,1])

#sel <- USmap$STATEFP_NUM==6
#plot(USmap[sel,])


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