# Process data from Seth Lyman

# # define directories
# uppermost.directory="/home/rstudio" # on AWS
# working.directory=uppermost.directory # on AWS
# setwd(working.directory)
# output.directory=file.path(working.directory,"Code_Outputs")
# ProcessedData.directory=file.path(working.directory,"Processed_Data")
#StartData.directory=file.path(working.directory,"Uintah Basin PM2.5")
StartData.directory=file.path(working.directory,"PM25_Uintah_Basin")
# 
# # sink command sends R output to a file. Don't try to open file until R has closed it at end of script. https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/sink
# SinkFileName=file.path(output.directory,"Lyman_Data_Processing.txt")
# sink(file =SinkFileName, append = FALSE, type = c("output","message"),
#      split = FALSE)

cat("Code and R output for Process_Lyman_data.R \n \n")
############################################################################
cat("Title: Process_Lyman_data.R \n")
cat("Author: Melissa May Maestas \n")
cat("Original Date: January 23, 2018 \n")
cat("Latest Update: January 23, 2018 \n")
cat("This program reads in and process the Uintah Basin PM2.5 data provided by Seth Lyman. \n")
############################################################################

cat("INSTALL PACKAGES \n")


cat("Read in excel file from Seth Lyman \n")
UBdata<-read.csv(file.path(StartData.directory,"FinalPM2.5_multiyear_thruwint2017_sheet1.csv"),header=TRUE) 

UBLocations <- read.csv(file.path(StartData.directory,"FinalPM2.5_multiyear_thruwint2017_GISsheet.csv"),header=TRUE)

# add columns for latitude and longitude to go in UBdata
#UBdata_loc <- cbind(UBdata, latitude,longitude,Elevation) 
#UBdata[,Latitude] <- NA
# add Latitude collumn
new_col_number <- length(UBdata)+1
UBdata[,new_col_number] <- NA # add column at end of UB data and fill it with NA
colnames(UBdata)[new_col_number] <- "Latitude"
new_col_number <- length(UBdata)+1
UBdata[,new_col_number] <- NA # add column at end of UB data and fill it with NA

# ############################### obsolete below ? ###################
# # Attempt 1
# # https://www.r-bloggers.com/read-excel-files-from-r/
# # "gdata package"
# require(gdata)
# #df = read.xls ("myfile.xlsx"), sheet = 1, header = TRUE)
# df = read.xls (file.path(StartData.directory,"FinalPM2.5_multiyear_thruwint2017.xlsx"), sheet = 1, header = TRUE)
# 
# # Attempt 2
# # https://www.r-bloggers.com/read-excel-files-from-r/
# # "XLConnect package"
# require(XLConnect)
# #wb = loadWorkbook("myfile.xlsx")
# wb = loadWorkbook(file.path(StartData.directory,"FinalPM2.5_multiyear_thruwint2017.xlsx"))
# df = readWorksheet(wb, sheet = "Sheet1", header = TRUE)
# 
# # Attempt 3
# # https://www.r-bloggers.com/read-excel-files-from-r/
# # "xlsx package"
# install.packages(pkgs="xlsx") 
# require(xlsx)
# read.xlsx("myfile.xlsx", sheetName = "Sheet1")
# read.xlsx2("myfile.xlsx", sheetName = "Sheet1")
# 
# require(xlsx)
# coln = function(x) { # A function to see column numbers
#   y = rbind(seq(1, ncol(x)))
#   colnames(y) = colnames(x)
#   rownames(y) = "col.number"
#   return(y)
# }
# data = read.xlsx2("myfile.xlsx", 1) # open the file 
# coln(data) # check the column numbers you want to have as factors
# x = 3 # Say you want columns 1-3 as factors, the rest numeric
# data = read.xlsx2("myfile.xlsx", 1, 
#                   colClasses = c(rep("character", x), rep("numeric", ncol(data)-x+1))
# )
# 
# # Attempt 4
# source("https://gist.github.com/schaunwheeler/5825002/raw/3526a15b032c06392740e20b6c9a179add2cee49/xlsxToR.r")
# #xlsxToR = function("myfile.xlsx", header = TRUE)
# xlsxToR = function(file.path(StartData.directory,"FinalPM2.5_multiyear_thruwint2017.xlsx"), header = TRUE)
# 
# 
# 
