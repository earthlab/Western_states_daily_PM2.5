
# define data of interest
day_interest <- as.Date("2008-01-01")
State_Code_interest <- 6
County_Code_interest <- 41

this_source_file <- paste("PM25_Step1_part_",processed_data_version,".csv",sep = "") # define file name
Step1_data <- read.csv(file.path(ProcessedData.directory,sub_folder,this_source_file),header=TRUE, stringsAsFactors=FALSE) # read step 3 full data file
Step1_data <- input_mat_change_data_classes.fn(input_mat3) # set variable classes

# find data of interest
which_data <- which(Step1_data$Date_Local == day_interest & Step1_data$State_Code == State_Code_interest & Step1_data$County_Code == County_Code_interest)
Step1_data_interest <- Step1_data[which_data, ]
write.csv(Step1_data_interest,file = file.path(ProcessedData.directory,sub_folder,paste('PM25_Step1_part_',processed_data_version,'_example_duplicates.csv',sep = "")),row.names = FALSE) # Write csv file
