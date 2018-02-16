# Code to handle Federal_Land_Manager_Environmental_Database data

# The original data file is quite large, so the purpose of this 
# script is to save a smaller file with just the information we'll need for our project.
uppermost.directory="/home/rstudio" # on AWS
working.directory=uppermost.directory # on AWS
FMLE.directory=file.path(working.directory,"Federal_Land_Manager_Environmental_Database")

install.packages('data.table')
library(data.table)
############################# Fill in data from Federal Land Managers ######################
#this_source_file <- "Federal_Land_Manager_Env_Database_201821321512474Iw0s1t.txt" 
this_source_file <- "Federal_Land_Manager_IMPROVE_RHR_III_2018215163723451I10uur.csv"
print(this_source_file)

# load FMLE data
#FMLEdata.data <- fread(file.path(FMLE.directory,this_source_file), header = T, skip = 3591,nrows=10)
#FMLEdata.data <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 3592)
#FMLEdata.data <- fread(file.path(FMLE.directory,this_source_file), skip = 3576,nrows=10, sep = ",",blank.lines.skip = T)
#FMLEdata.header <- fread(file.path(FMLE.directory,this_source_file),skip = 3575,nrows=1, sep = ",",blank.lines.skip = T)

#FMLEdata.header <- cbind("Dataset","SiteCode","POC","Date","SiteName","Latitude","Longitude","Elevation","State","CountyFIPS","EPACode","88502:Val","88502:Method","88502:Unc","88502:Mdl","88502:Unit","88502:StatusFlag","88502:Flag1","88502:Flag2","88502:Flag3	88502:Flag4	88502:Flag5	88502:AuxValue1	88502:AuxValue2	MF	MF:Val	MF	MF:Method	MF	MF:Unc	MF	MF:Mdl	MF	MF:Unit	MF	MF:StatusFlag	MF	MF:Flag1	MF	MF:Flag2	MF	MF:Flag3	MF	MF:Flag4	MF	MF:Flag5	MF	MF:AuxValue1	MF	MF:AuxValue2	RCFM:Val	RCFM:Method	RCFM:Unc	RCFM:Mdl	RCFM:Unit	RCFM:StatusFlag	RCFM:Flag1	RCFM:Flag2	RCFM:Flag3	RCFM:Flag4	RCFM:Flag5	RCFM:AuxValue1	RCFM:AuxValue2	BE_MASS_PM25:Val	BE_MASS_PM25:Method	BE_MASS_PM25:Unc	BE_MASS_PM25:Mdl	BE_MASS_PM25:Unit	BE_MASS_PM25:StatusFlag	BE_MASS_PM25:Flag1	BE_MASS_PM25:Flag2	BE_MASS_PM25:Flag3	BE_MASS_PM25:Flag4	BE_MASS_PM25:Flag5	BE_MASS_PM25:AuxValue1	BE_MASS_PM25:AuxValue2	FRM_MASS_PM25:Val	FRM_MASS_PM25:Method	FRM_MASS_PM25:Unc	FRM_MASS_PM25:Mdl	FRM_MASS_PM25:Unit	FRM_MASS_PM25:StatusFlag	FRM_MASS_PM25:Flag1	FRM_MASS_PM25:Flag2	FRM_MASS_PM25:Flag3	FRM_MASS_PM25:Flag4	FRM_MASS_PM25:Flag5	FRM_MASS_PM25:AuxValue1	FRM_MASS_PM25:AuxValue2

#FMLEdata.data <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 214)
FMLEdata.data <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 230,sep = ",",blank.lines.skip = F)


# load data information (top several lines of file)
FMLEdata.summary <- read.csv(file.path(FMLE.directory,this_source_file),header = F,nrows = 44)

# load the listing of data sets
FMLEdata.datasets <- read.csv(file.path(FMLE.directory,this_source_file),header = T,skip = 44 ,nrows = 6)

# load the listing of all sites
FMLEdata.sites <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 54,nrows = 3492)

# load the Parameters data
FMLEdata.parameters <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 3550,nrows = 9)

# load flag information
FMLEdata.flags <- read.csv(file.path(FMLE.directory,this_source_file), header = T, skip = 3571, nrows = 17)


# # load the FMLE file
#FMLEheaders <- read.csv(file.path(FMLE.directory,this_source_file),skip = 3000, header = F, nrows = 1, as.is = T)
#FMLEdata <- read.csv(file.path(FMLE.directory,this_source_file),skip = 3001,header = F)
#colnames(FMLEdata) <- headers

#FMLEdata<-read.csv(file.path(FMLE.directory,this_source_file),header=TRUE) 

## isolate data in study states
##class(ThisAQSdata$State.Code)
#ThisAQSdata_StudyStates <- ThisAQSdata[which(ThisAQSdata$State.Code==4|ThisAQSdata$State.Code==6|ThisAQSdata$State.Code==8|ThisAQSdata$State.Code==16|ThisAQSdata$State.Code==30|ThisAQSdata$State.Code==32|ThisAQSdata$State.Code==35|ThisAQSdata$State.Code==41|ThisAQSdata$State.Code==49|ThisAQSdata$State.Code==53|ThisAQSdata$State.Code==56), ]
#rm(ThisAQSdata)
#unique(ThisAQSdata_StudyStates$State.Name)
row_stop <- row_start+dim(FMLEdata)[1]-1

# input data source counter - indicates if this is EPA data or field data, etc.
input_mat1[row_start:row_stop,c("Data_Source_Counter")] <- data_source_counter

# input dates
new_col_number <- length(FMLEdata)+1
#FMLEdata[,new_col_number] <- as.Date(FMLEdata[,c("Date.Local")],"%Y-%m-%d") # add column at end of UB data and fill it with dates in format R will recognize https://www.statmethods.net/input/dates.html
#colnames(FMLEdata)[new_col_number] <- "R_Dates"
#input_mat1[row_start:row_stop,c("RDates")] <- format(FMLEdata[,c("R_Dates")],"%Y-%m-%d")
#rm(new_col_number)

# # input station names into input_mat1
#  AQSStations <- FMLEdata[,c("Local.Site.Name")]
#  #print(AQSStations)
#  AQSstationsChar <- as.character(AQSStations)
#  #print(AQSstationsChar)
#  input_mat1[row_start:row_stop,c('PM25_Station_Name')] <- AQSstationsChar
#  rm(AQSStations,AQSstationsChar)

# input lat and lon
input_mat1[row_start:row_stop,c("PM2.5_Lat")] <- FMLEdata[,c('Latitude')]
input_mat1[row_start:row_stop,c("PM2.5_Lon")] <- FMLEdata[,c('Longitude')]

#  # input PM2.5 concentration
#  input_mat1[row_start:row_stop,c('PM2.5_Obs')] <- FMLEdata[,c("Arithmetic.Mean")]

#  # input source file name
#  input_mat1[row_start:row_stop,c('Source_File')] <- this_source_file

# # input parameter code and method name
#  input_mat1[row_start:row_stop,c("Parameter")] <- this_ParamCode
#  input_mat1[row_start:row_stop,c("Method")] <- FMLEdata[,c("Method.Name")]

# update row counter
row_start=row_stop+1

# clear variables before moving on to next iteration of loop
rm(this_source_file,FMLEdata)


rm(ParameterCode_vec,this_year,this_ParamCode)

