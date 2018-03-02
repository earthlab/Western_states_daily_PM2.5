# Clean ML Input file

# AQS data
# some data is hourly data, averaged into 24-hour blocks and some is just one data point for 24 hours
# need to get rid of the hourly data that doesn't have at least min_hourly_obs_daily measurements
all_sample_durations <- unique(ThisAQSdata_step[,c("Sample.Duration")])
print(all_sample_durations[1])
#ThisAQSdata_24HR <- ThisAQSdata_step[which(ThisAQSdata_step$Sample.Duration=="24 HOUR"|ThisAQSdata_step$Sample.Duration=="24-HR BLK AVG"),]
ThisAQSdata_24HR <- ThisAQSdata_step[which(ThisAQSdata_step$Sample.Duration=="24-HR BLK AVG"|ThisAQSdata_step$Sample.Duration=="24 HOUR"),]
twenty4hr_sample_durations <- unique(ThisAQSdata_24HR[,c("Sample.Duration")])
print(twenty4hr_sample_durations)
ThisAQSdata_1HR <- ThisAQSdata_step[which(ThisAQSdata_step$Sample.Duration=="1 HOUR"),]
rm(all_sample_durations,twenty4hr_sample_durations)

# check that all rows are accounted for
if (dim(ThisAQSdata_24HR)[1]+dim(ThisAQSdata_1HR)[1]!=dim(ThisAQSdata_step)[1]){stop('check code - not all rows of data accounted for')}
rm(ThisAQSdata_step)

# check that there are enough observations in the 1-hr data
ThisAQSdata_1HR_enough <- ThisAQSdata_1HR[which(ThisAQSdata_1HR$Observation.Count>=min_hourly_obs_daily),]
print(cat(dim(ThisAQSdata_1HR)[1]-dim(ThisAQSdata_1HR_enough)[1]," data points removed because there were fewer than ",min_hourly_obs_daily," observations in the daily data in ",this_source_file,"."))

# recombine the 24-hr and 1-hr data
ThisAQSdata <- rbind(ThisAQSdata_24HR,ThisAQSdata_1HR_enough)
rm(ThisAQSdata_24HR,ThisAQSdata_1HR,ThisAQSdata_1HR_enough)

# Fire Cache data
# rule out readings with missing longitude data
date_this_lon_data_step3 <-as.numeric(as.character(date_all_Fire_Cache_data_step2[,c(" Deg    GPS     Lon. ")]))
find_this_data_rows_step3 <- which(date_this_lon_data_step3>=-180)
date_all_Fire_Cache_data_step3 <- date_all_Fire_Cache_data_step2[find_this_data_rows_step3,]
rm(date_this_lon_data_step3,find_this_data_rows_step3,date_all_Fire_Cache_data_step2)
# rule out readings with negative battery voltage
date_this_batt_volt <-as.numeric(as.character(date_all_Fire_Cache_data_step3[,c("volts Battery Voltage")]))
find_this_data_rows <- which(date_this_batt_volt>=0)
date_all_Fire_Cache_data <- date_all_Fire_Cache_data_step3[find_this_data_rows,]
rm(date_this_batt_volt,date_all_Fire_Cache_data_step3)
#rm(date_this_conc_data,find_this_data_rows_step,date_all_Fire_Cache_data_step)

print('check if there are at least min_hourly_obs_daily hourly observations, otherwise a daily value will not be computed')
if (length(find_this_data_rows)>=min_hourly_obs_daily){
}
