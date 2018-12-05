# Process_NAM_data_step4.R calculate daily values combining the 4 timesteps for each day

library(lubridate) # https://cran.r-project.org/web/packages/lubridate/lubridate.pdf
library(lutz)

# load the data created in step 3, which has all of the observations for the 4 timesteps per day in one data frame
part_number <- "bc"
Step3_NAM_data <- read.csv(file.path(ProcessedData.directory,sub_folder,paste("NAM_Step3_part_",part_number,".csv",sep = ""))) # open data file

#Step3_NAM_data[1,c("Time.UTC")]
#as.character(Step3_NAM_data[1,c("Date")])
#ymd_h(paste(Step3_NAM_data[3007767,c("Date")],Step3_NAM_data[3007767,c("Time.UTC")]), tz = "UTC")

# add a column indicating the time stamp in UTC
Step3_NAM_data$UTC.Date.Time <- ymd_h(paste(Step3_NAM_data$Date,Step3_NAM_data$Time.UTC), tz = "UTC")

# add a column indicating the time zone for each observation
unique_locations <- unique(Step3_NAM_data[,c("Lat","Lon")])
unique_locations$TimeZone <- NA
Step3_NAM_data$TimeZone <- NA

# do we need a daylight savings time indicator? - see page 14

# add a column indicating the time in the relevant time zone - see page 58-72
Step3_NAM_data$Local.Date.Time <- with_tz(time = Step3_NAM_data$UTC.Date.Time, tzone = Step3_NAM_data$TimeZone)
Step3_NAM_data$Local.Date <- date(Step3_NAM_data$Local.Date.Time)

# write step 4 data to csv file