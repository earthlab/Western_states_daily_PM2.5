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
AQSData.directory=file.path(working.directory,"AQS Daily Summaries")
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
input_header= c('ID','POC','Parameter','Method','Winter','RDates','Year','Month','Day','PM2.5_Obs','PM2.5_Lat','PM2.5_Lon','PM25_Station_Name','Source_File')
N_columns=length(input_header)
input_mat1=data.frame(matrix(NA,nrow=10,ncol=N_columns))
names(input_mat1)=input_header

####### Fill in Lyman Uintah Basin data ########################
#UBdata<-read.csv(file.path(StartData.directory,"FinalPM2.5_multiyear_thruwint2017_sheet1.csv"),header=TRUE) 
this_source_file <- "FinalPM2.5_multiyear_thruwint2017_sheet1_dates.csv"
#UBdata<-read.csv(file.path(StartData.directory,"FinalPM2.5_multiyear_thruwint2017_sheet1_dates.csv"),header=TRUE) 
UBdata<-read.csv(file.path(StartData.directory,this_source_file),header=TRUE) 

new_col_number <- length(UBdata)+1 # figure out how many columns are in UBdata and then add 1
UBdata[,new_col_number] <- as.Date(UBdata[,c("Dates")],"%m/%d/%Y") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
colnames(UBdata)[new_col_number] <- "R_Dates"
# load file with lat/lon for Uintah Basin stations
UBLocations <- read.csv(file.path(StartData.directory,"FinalPM2.5_multiyear_thruwint2017_GISsheet.csv"),header=TRUE)

row_start <- 1
row_stop=dim(UBdata)[1]
#for (this_column in UBdata[c("Roosevelt..24hr.avg.PM2.5.","Vernal..24hr.avg.PM2.5.")]){
for(this_column in 6:15){  
  #print(paste("The year is", year))
  #which( colnames(df)=="b" )
  print(paste("Column number = ",this_column))
  #this_name=names(UBdata[this_columnl])
  this_name=colnames(UBdata)[this_column]
  print(this_name)
  #print(paste(this_name)) #COMMENT
  input_mat1[row_start:row_stop,c('Winter')] <- UBdata[,"Winter."]
  input_mat1[row_start:row_stop,c('Year')] <- UBdata[,"year"]
  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- UBdata[,this_column]
  input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- this_name
  input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
  print(UBdata[,"R_Dates"])
  input_mat1[row_start:row_stop,c('RDates')] <- as.Date(UBdata[,c("Dates")],"%m/%d/%Y")#UBdata[,"R_Dates"]
  input_mat1[row_start:row_stop,c('RDates')] <- format(UBdata[,c("R_Dates")], "%Y-%m-%d")

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
  
  #15
  
  row_start <- row_stop+1
  row_stop <- row_start+dim(UBdata)[1]-1
}
rm(new_col_number,this_column,this_name,this_source_file)

############################# Fill in Salt Lake City PCAPS data ############################


this_source_file <- "MiniVol_data_dates.csv"
PCAPSdata<-read.csv(file.path(PCAPSData.directory,this_source_file),header=TRUE) 
row_stop <- row_start+dim(PCAPSdata)[1]-1

new_col_number <- length(PCAPSdata)+1
PCAPSdata[,new_col_number] <- as.Date(PCAPSdata[,c("Dates")],"%m/%d/%Y") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
colnames(PCAPSdata)[new_col_number] <- "R_Dates"
#new_col_number <- length(PCAPSdata)+1

PCAPSstations <- PCAPSdata[,c('Location')]
print(PCAPSstations)
#PCAPSstations[] <- lapply(PCAPSstations,as.character)
PCAPSstationsChar <- as.character(PCAPSstations)
print(PCAPSstationsChar)
input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- PCAPSstationsChar

# load file containing lat/lon info for PCAPS sites
PCAPSLocations<-read.csv(file.path(PCAPSData.directory,"PCAPS_Site_Locations.csv"),header=TRUE) 


#for(this_column in 6:15){  
this_column <- which(colnames(PCAPSdata)=="ug.m3")
  print(paste("Column number = ",this_column))
  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- PCAPSdata[,this_column]
   input_mat1[row_start:row_stop,c('RDates')] <- format(PCAPSdata[,c("R_Dates")], "%Y-%m-%d")
   input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
  
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
   
   rm(new_col_number,this_column,this_name,this_source_file) 
  row_start=row_stop+1
  #row_stop=row_start+dim(PCAPSdata)[1]-1

############################## Pull in AQS data #################
  POC_vec <- cbind(88101,88502)
  
  for(this_year in start_study_year:stop_study_year){     
    for(this_POC in POC_vec){
  this_source_file <- cat('daily_',as.string(this_POC),'_',as.character(this_year),'.csv')
  
  # look up trim
  
  }
  }
  this_source_file <- "MiniVol_data_dates.csv"
  PCAPSdata<-read.csv(file.path(PCAPSData.directory,this_source_file),header=TRUE) 
  row_stop <- row_start+dim(PCAPSdata)[1]-1
  
  new_col_number <- length(PCAPSdata)+1
  PCAPSdata[,new_col_number] <- as.Date(PCAPSdata[,c("Dates")],"%m/%d/%Y") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
  colnames(PCAPSdata)[new_col_number] <- "R_Dates"
  #new_col_number <- length(PCAPSdata)+1
  
  PCAPSstations <- PCAPSdata[,c('Location')]
  print(PCAPSstations)
  #PCAPSstations[] <- lapply(PCAPSstations,as.character)
  PCAPSstationsChar <- as.character(PCAPSstations)
  print(PCAPSstationsChar)
  input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- PCAPSstationsChar
  
  # load file containing lat/lon info for PCAPS sites
  PCAPSLocations<-read.csv(file.path(PCAPSData.directory,"PCAPS_Site_Locations.csv"),header=TRUE) 

  this_column <- which(colnames(PCAPSdata)=="ug.m3")
  print(paste("Column number = ",this_column))
  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- PCAPSdata[,this_column]
  input_mat1[row_start:row_stop,c('RDates')] <- format(PCAPSdata[,c("R_Dates")], "%Y-%m-%d")
  input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file
  
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
  
  rm(new_col_number,this_column,this_name,this_source_file) 
  row_start=row_stop+1
  #row_stop=row_start+dim(PCAPSdata)[1]-1
  

############################# plot locations

# find unique locations in data https://stats.stackexchange.com/questions/6759/removing-duplicated-rows-data-frame-in-r
#a <- c(rep("A", 3), rep("B", 3), rep("C",2))
#b <- c(1,1,2,4,1,1,2,2)
#df <-data.frame(a,b)
repeated_locations=input_mat1[,c("PM2.5_Lat","PM2.5_Lon")]

#duplicated(df)
#[1] FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE
duplicated(repeated_locations)

#> df[duplicated(df), ]
#a b
#2 A 1
#6 B 1
#8 C 2
repeated_locations[duplicated(repeated_locations), ]

#> df[!duplicated(df), ]
#a b
#1 A 1
#3 A 2
#4 B 4
#5 B 1
#7 C 2
non_repeat_locations <- repeated_locations[!duplicated(repeated_locations), ]

plot(non_repeat_locations[,2],non_repeat_locations[,1])

#### Resources for mapping
# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

#USmap=readOGR(dsn = USMaps.directory,layer = "cp_2016_us_state_500k")
# fix so the path isn't hard-coded
USmap=readOGR(dsn="/home/rstudio/Shapefiles_for_mapping/cp_2016_us_state_500k",layer = "cb_2016_us_state_500k")
head(USmap@data,n=2)
#mean(USmap$STATENS)
sapply(USmap@data,class)
USmap$STATEFP_NUM <- as.numeric(as.character(USmap$STATEFP))
USmap$ALAND_NUM <- as.numeric(as.character(USmap$ALAND))



mean(USmap$ALAND_NUM)
nrow(USmap)
ncol(USmap)

#plot(USmap)
USmap@data[,c("STATEFP_NUM","STUSPS")]
# find the 11 western states included in the study
WestUSmap=USmap@data[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53,]
print(WestUSmap)

WestUSmapGeom=USmap[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53,]
plot(WestUSmapGeom)
points(non_repeat_locations[,2],non_repeat_locations[,1],col="red",cex=.6) # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
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