# Process_NAM_data_step4.R calculate daily values combining the 4 timesteps for each day

library(lubridate) # https://cran.r-project.org/web/packages/lubridate/lubridate.pdf

# load the data created in step 3, which has all of the observations for the 4 timesteps per day in one data frame
part_number <- "bc"
Step3_NAM_data <- read.csv(file.path(ProcessedData.directory,sub_folder,paste("NAM_Step3_part_",part_number,".csv",sep = ""))) # open data file


