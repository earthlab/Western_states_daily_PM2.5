# # plot the input_mat1 File

rm(list = ls())
options(warn=2) # throw an error when there's a warning and stop the code from running further

#### define directories and constants ####
uppermost.directory="D:/S3_bucket_image/" # without docker
#uppermost.directory="/home/rstudio"  # with docker
working.directory=uppermost.directory 
setwd(working.directory)
#output.directory=file.path(working.directory,"Code_Outputs")
#output.directory=file.path(working.directory,"estimate-pm25","LaTeX_documentation","Code_Outputs")
#output.directory=file.path("C:","Users","mema2636","MMM_GitHub","estimate-pm25","PM25-estimation","LaTeX_documentation","Code_Outputs")
#output.directory=file.path("C:","estimate-pm25","LaTeX_documentation","Code_Outputs")
output.directory=file.path("C:","Users","mema2636","MMM_GitHub","estimate-pm25","LaTeX_documentation","Code_Outputs")

ProcessedData.directory=file.path(working.directory,"Processed_Data")
StartData.directory=file.path(working.directory,"PM25_Uintah_Basin")
USMaps.directory=file.path(working.directory,"Shapefiles_for_mapping","cp_2016_us_state_500k")
PCAPSData.directory=file.path(working.directory,"PM25_PCAPS_Salt_Lake")
AQSData.directory=file.path(working.directory,"AQS_Daily_Summaries")
FMLE.directory=file.path(working.directory,"Federal_Land_Manager_Environmental_Database")
FireCache.directory=file.path(working.directory,"Fire_Cache_Smoke_DRI")
start_study_year <- 2008
stop_study_year <- 2014

# # define directories
#uppermost.directory="/home/rstudio" # on AWS
#working.directory=uppermost.directory # on AWS
#setwd(working.directory)
#output.directory=file.path(working.directory,"Code_Outputs")
#output.directory=file.path(working.directory,"estimate-pm25","LaTeX_documentation","Code_Outputs")
#ProcessedData.directory=file.path(working.directory,"Processed_Data")
#USMaps.directory=file.path(working.directory,"Shapefiles_for_mapping","cp_2016_us_state_500k")

# define study years
#start_study_year <- 2008
#stop_study_year <- 2014

start_study_date <- as.Date("2008-01-01","%Y-%m-%d")
stop_study_date <- as.Date("2014-12-31","%Y-%m-%d")

##### Create Sink output file ####
# start sink for output
SinkFileName=file.path(output.directory,"Plot_ML_Input_File_sink.txt")
sink(file =SinkFileName, append = FALSE, type = c("output","message"),
     split = FALSE)

#### Call Packages (Library) ####
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

#### Read in Data file ####

# load input_mat1 from file
#input_mat1 <- read.csv(file.path(ProcessedData.directory,'combined_ML_input.csv'),header=TRUE)
input_mat1 <- read.csv(file.path(ProcessedData.directory,'cleaned_ML_input.csv'),header=TRUE)

print('structure: str() of input_mat1')
str(input_mat1)

# indicate which column should be interpreted as dates
input_mat1$RDates <- as.Date(input_mat1$RDates,"%Y-%m-%d")
input_mat1$Date_Local <- as.Date(input_mat1$Date_Local,"%Y-%m-%d")

#write.csv(input_mat1,file = file.path(ProcessedData.directory,'combined_ML_input.csv'))

#### Tell R which columns to recognize as factors ####

input_mat1$State_Code <- factor(input_mat1$State_Code) # state code should be a factor variable

input_mat1$County_Code <- factor(input_mat1$County_Code) # county code should be a factor variable

input_mat1$Site_Num <- factor(input_mat1$Site_Num) # Site number should be a factor variable

input_mat1$Parameter_Code <- factor(input_mat1$Parameter_Code) # Parameter code should be a factor variable

input_mat1$Winter <- factor(input_mat1$Winter) # winter is a categorical variable, so it should be a factor

input_mat1$Month <- factor(input_mat1$Month) # month should be a categorical variable, so it should be a factor

input_mat1$Day <- factor(input_mat1$Day) # month should be a categorical variable, so it should be a factor

input_mat1$l.m.Ave..Air.Flw <- as.numeric(input_mat1$l.m.Ave..Air.Flw) # air flow should be numerical

input_mat1$Deg.C.Av.Air.Temp <- as.numeric(input_mat1$Deg.C.Av.Air.Temp) # temperature should be numerical

print('fill in the data type for the rest of the variables')

#### Summarize data as a whole ####
print('structure: str() of input_mat1')
str(input_mat1)

print('top few rows of input_mat1:')
head(input_mat1)

print('last few rows of input_mat1:')
tail(input_mat1)

summary(input_mat1)

summary(input_mat1$PM2.5_Obs)

############################# map locations #########################
## Names for figure ##
FigFileName_nopath <- paste("MapPM25_All_Sites",sep = "")
this_image_file_name <- "All_Monitor_Locations"
subsection_name <- "All PM2.5 Monitor Locations"
this_fig_title <- "All PM2.5 Observation Locations"
fig_label <- "MapPM25Loc"
fig_caption <- "Map of locations of PM2.5 observations."
jpg_or_pdf <- "pdf"

print(FigFileName_nopath)
print(this_image_file_name)
print(subsection_name)
print(this_fig_title)
print(fig_label)
print(fig_caption)
print(jpg_or_pdf)

# Resources for mapping
# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
print('still need to put in the latex code for this plot')
# map boundaries of western US states
USmap=readOGR(dsn=file.path(USMaps.directory),layer = "cb_2016_us_state_500k")

# COMMENT following lines
#head(USmap@data,n=2)
#sapply(USmap@data,class)
#USmap$ALAND_NUM <- as.numeric(as.character(USmap$ALAND))
#mean(USmap$ALAND_NUM)
#nrow(USmap)
#ncol(USmap)
#plot(USmap)

# have R recognize state FP's as numerical values (in a new column)
USmap$STATEFP_NUM <- as.numeric(as.character(USmap$STATEFP))

# display the State FP values and state abbreviations next to each other
USmap@data[,c("STATEFP_NUM","STUSPS")]

# find the 11 western states included in the study
WestUSmap=USmap@data[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==38|USmap$STATEFP_NUM==46|USmap$STATEFP_NUM==31|USmap$STATEFP_NUM==20|USmap$STATEFP_NUM==40|USmap$STATEFP_NUM==48,]
print(WestUSmap)

# start file for map
FigFileName_extension <- as.character(paste(FigFileName_nopath,".",jpg_or_pdf,sep = ""))
FigFileName <- file.path(output.directory,FigFileName_extension)
print(FigFileName)
if (jpg_or_pdf=="pdf") {
pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
}  else if (jpg_or_pdf=="jpg") {
jpeg(file=FigFileName) # start jpg document to put figure into
  } else {stop("invalid option for image file type")}
plot.new() # clear the plot to have a clean canvas to draw on

## Do plotting/mapping
WestUSmapGeom=USmap[USmap$STATEFP_NUM==4|USmap$STATEFP_NUM==6|USmap$STATEFP_NUM==8|USmap$STATEFP_NUM==16|USmap$STATEFP_NUM==30|USmap$STATEFP_NUM==32|USmap$STATEFP_NUM==35|USmap$STATEFP_NUM==49|USmap$STATEFP_NUM==56|USmap$STATEFP_NUM==41|USmap$STATEFP_NUM==53|USmap$STATEFP_NUM==38|USmap$STATEFP_NUM==46|USmap$STATEFP_NUM==31|USmap$STATEFP_NUM==20|USmap$STATEFP_NUM==40|USmap$STATEFP_NUM==48,]
plot(WestUSmapGeom)

# cycle through each data source (EPA and various field campaigns) and plot each in a different color
for(this_data_source_counter in 0:max(input_mat1[,c("Data_Source_Counter")])){     
  print(this_data_source_counter) 
  
  # isolate data from this data source (in loop iteration) 
  This_data <- input_mat1[which(input_mat1$Data_Source_Counter==this_data_source_counter), ]
  
  # do a basic check of the data
  summary(This_data)
  
  # find unique locations in data https://stats.stackexchange.com/questions/6759/removing-duplicated-rows-data-frame-in-r
  repeated_locations=This_data[,c("PM2.5_Lat","PM2.5_Lon")]
  #duplicated(repeated_locations)
  #repeated_locations[duplicated(repeated_locations), ]
  non_repeat_locations <- repeated_locations[!duplicated(repeated_locations), ]
  rm(repeated_locations)
  #plot(non_repeat_locations[,2],non_repeat_locations[,1])
  
  if(this_data_source_counter==0){
    points(non_repeat_locations[,2],non_repeat_locations[,1],col="black",cex=.3) # http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
  legend_names <- as.character(unique(This_data$Data_Source_Name_Display))
    } else if(this_data_source_counter==1){
    points(non_repeat_locations[,2],non_repeat_locations[,1],col="red",cex=0.6)
      legend_names <- c(legend_names,as.character(unique(This_data$Data_Source_Name_Display)))
    } else if(this_data_source_counter==2){
    points(non_repeat_locations[,2],non_repeat_locations[,1],col="darkgoldenrod",cex=0.8)
      legend_names <- c(legend_names,as.character(unique(This_data$Data_Source_Name_Display)))
  } else if(this_data_source_counter==3){
    points(non_repeat_locations[,2],non_repeat_locations[,1],col="green",cex=0.6)
    legend_names <- c(legend_names,as.character(unique(This_data$Data_Source_Name_Display)))
  } else if(this_data_source_counter==4){
    points(non_repeat_locations[,2],non_repeat_locations[,1],col="blue",cex=0.6)
    legend_names <- c(legend_names,as.character(unique(This_data$Data_Source_Name_Display)))
  }   else {
    stop(1, call. = TRUE, domain = NULL)
    geterrmessage("Loop should not have called this path in the if-statement")
  }
  rm(This_data,non_repeat_locations) # UNCOMMENT
} # for(this_data_source_counter in 0:data_source_counter){    

legend("bottomleft", # position
       legend = legend_names, 
       col = c("black","red","darkgoldenrod","green","blue"),
       pch = 1,
       title = "Data Source",
       cex = 0.56,
       bty = "n") # border
rm(legend_names)
## Code to finish figure and write latex code
par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
#summary(gbmtrainonly)
title(main = this_fig_title)
dev.off() # stop writing to pdf file
remove(FigFileName) # delete pdf file name variable
sink() # stop putting text into SinkFileName
LatexFileName=file.path(output.directory,paste("Rgenerated_Images",this_image_file_name,".tex",sep = "")) # Start file for latex code images
sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE)
cat(paste("\n\\subsection{",subsection_name,"}",sep = ""))
cat("\n\\begin{figure} \n")
cat("\\centering \n")
cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",FigFileName_nopath,".",jpg_or_pdf,"} \n",sep = "")) 
cat(paste("\\caption{\\label{fig:",fig_label,"}",fig_caption,"} \n",sep = "")) 
cat("\\end{figure} \n \n")
sink() # stop writing to latex file
sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
rm(FigFileName_nopath,this_image_file_name,subsection_name,fig_label,fig_caption,jpg_or_pdf,this_fig_title)
rm(LatexFileName,FigFileName_extension,FigFileName)
rm(this_data_source_counter)

######## Loop through data sources and do a series of plots #####
#https://www.stat.berkeley.edu/classes/s133/saving.html
# this_data_source_counter <- 0
#this_data_source_counter <- 4
for(this_data_source_counter in 0:max(input_mat1[,c("Data_Source_Counter")])){    
  #if (this_data_source_counter!=1){
  print('still need to pull in Fed Land Management Database concentrations')

  print(this_data_source_counter) 
  
  # isolate data from this data source (in loop iteration) 
  This_data <- input_mat1[which(input_mat1$Data_Source_Counter==this_data_source_counter), ]
  This_Data_Source_Name_Short <- unique(This_data[,c("Data_Source_Name_Short")])
  This_Data_Source_Name_Display <- unique(This_data[,c("Data_Source_Name_Display")])
  print(This_Data_Source_Name_Display)
  
  summary(This_data)
  
## start with a basic time series plot:
  ## Names for figure ##
  FigFileName_nopath <- paste(This_Data_Source_Name_Short,"_time_series",sep = "")
  this_image_file_name <- paste(This_Data_Source_Name_Short,"DataSummary",sep = "")
  subsection_name <- paste(This_Data_Source_Name_Display," Plots",sep = "")
  this_fig_title <- paste(This_Data_Source_Name_Display," Time Series",sep = "")
  fig_label <- paste(This_Data_Source_Name_Short,"TS",sep = "")
  fig_caption <- paste(This_Data_Source_Name_Display," time series.",sep = "")
  jpg_or_pdf <- "jpg"
  
  # start file for map
  FigFileName_extension <- as.character(paste(FigFileName_nopath,".",jpg_or_pdf,sep = "")) # define file name for the figure to be created
  FigFileName <- file.path(output.directory,FigFileName_extension)
  print(FigFileName)
  if (jpg_or_pdf=="pdf") {
    pdf(file=FigFileName, height = 3.5, width = 5, onefile=FALSE) # start pdf document to put figure into
  }  else if (jpg_or_pdf=="jpg") {
    jpeg(file=FigFileName) # start jpg document to put figure into
  } else {stop("invalid option for image file type")}
  plot.new() # clear the plot to have a clean canvas to draw on
  
  ## Do plotting/mapping
  plot(x=This_data$Date_Local,y=This_data$PM2.5_Obs,xlim=c(start_study_date,stop_study_date))
  
  ## Code to finish figure and write latex code
  par(mar=c(4.2, 3.8, 1, 0.2)) # trim off extra white space (bottom, left, top, right)
  #summary(gbmtrainonly)
  title(main = this_fig_title)
  dev.off() # stop writing to pdf file
  remove(FigFileName) # delete pdf file name variable
  sink() # stop putting text into SinkFileName
  LatexFileName=file.path(output.directory,paste("Rgenerated_Images",this_image_file_name,".tex",sep = "")) # Start file for latex code images
  sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE)
  cat(paste("\n\\subsection{",subsection_name,"}",sep = ""))
  cat("\n\\begin{figure} \n")
  cat("\\centering \n")
  cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",FigFileName_nopath,".",jpg_or_pdf,"} \n",sep = "")) 
  cat(paste("\\caption{\\label{fig:",fig_label,"}",fig_caption,"} \n",sep = "")) 
  cat("\\end{figure} \n \n")
  sink() # stop writing to latex file
  sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName
  rm(FigFileName_nopath,this_image_file_name,subsection_name,fig_label,fig_caption,jpg_or_pdf,this_fig_title)
  rm(LatexFileName,FigFileName_extension)
  rm(this_data_source_counter)
  
  # plot(this_model_output)# ,axes=F, ann=T, cex.lab=0.8, lwd=2)
  # Make x axis tick marks without labels
  # axis(1, lab=F)
  #title(main = paste(This_Data_Source_Name_Display," Time Series",sep = ""))
  #dev.off() # stop writing to pdf file
  #remove(FigFileName)
  #sink() # stop putting text into SinkFileName
  #LatexFileName=file.path(output.directory,paste("Rgenerated_Images",This_Data_Source_Name_Short,".tex",sep = "")) # Start file for latex code images
  #sink(file = LatexFileName, append = FALSE, type = c("output","message"),split = FALSE)
  #cat(paste("\n\\subsection{",This_Data_Source_Name_Display," Plots}",sep = ""))
  #cat("\n\\begin{figure} \n")
  #cat("\\centering \n")
  #cat(paste("\\includegraphics[width=0.77\\textwidth]{Code_Outputs/",FigFileName_nopath,"} \n",sep = "")) 
  #cat(paste("\\caption{\\label{fig:",This_Data_Source_Name_Short,"TS}",This_Data_Source_Name_Display," time series.} \n",sep = "")) 
#  cat("\\end{figure} \n \n")
#  sink() # stop writing to latex file
 # sink(file =SinkFileName, append = TRUE, type = c("output","message"),split = FALSE) # resume putting output into SinkFileName

  ## get some stats about the data
  N_data_points <- dim(This_data)[1]
  find_high_points <- which(This_data$PM2.5_Obs>200)
  N_points_gt200 <- length(find_high_points)
  
  High_points <- This_data[find_high_points,]
  
  print('write code to plot high data points')
  summary(High_points)  
  rm(This_data,This_Data_Source_Name_Display,This_Data_Source_Name_Short)
#  }
} # for(this_data_source_counter in 0:max(input_mat1[,c("Data_Source_Counter")])){  

#### Look at the high points in the data set as a whole ####

# get some stats about the data
High_points <- input_mat1[which(input_mat1$PM2.5_Obs>=200), ]

High_points <- subset(input_mat1,PM2.5_Obs>=200)

print('write code to plot high data points')
summary(High_points)

Ordered_High_points <- High_points[order(High_points$PM2.5_Obs),]


print('get the high points of the data as a whole and look at it')


