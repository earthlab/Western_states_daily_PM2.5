
#### Clear all variables and start fresh ####
rm(list  =  ls())
options(warn  =  2) # throw an error when there's a warning and stop the code from running further
# make sure it isn't outputing text or figures to any files
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which  =  dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {
sink.number()

#### Simplified test ####
Model_in_use_abbrev <-  "namanl"
Archive_file_type <- "grib2"
Lat_interest_point <- 40.037416
Lon_interest_point <- -105.228667
Location_Name <- "Boulder Municiple Airport, CO"
forecast_times <- 00 # reanalysis - anything else would be a forecast
Time_of_day <- 18 # 1800 UTC
Date_interest <- 20180702 #20080202

# meteorological variable of interest
Meteo_var <- "TMP" #temp at 2 m
Meteo_Var_Name <- "Surface Temperatures"
Meteo_level <- "2 m above ground"

list.available.models <- CheckNOMADSArchive(abbrev = Model_in_use_abbrev, model.date = Date_interest)
print(list.available.models)

## Not run:
this_model.info <- ArchiveGribGrab(abbrev = Model_in_use_abbrev, model.date = Date_interest,
                              model.run = Time_of_day, preds = forecast_times, file.type = "grib2")

thisGribInfo <- GribInfo(this_model.info[[1]]$file.name,file.type = "grib2")
print(thisGribInfo[["inventory"]])
thisGribInfo[["grid"]]

this_model.data <- ReadGrib(file.names = this_model.info[[1]]$file.name, levels = Meteo_level, variables = Meteo_var,
                            forecasts = NULL, domain = NULL, domain.type = "latlon",
                            file.type = "grib2", missing.data = NULL)

profile <- BuildProfile(model.data = this_model.data, lon = Lon_interest_point, lat = Lat_interest_point, spatial.average = TRUE, points = 4)
print(paste("The temperature in ",Location_Name," was ",
            sprintf("%.0f", profile[[1]]$profile.data[1,1,1] - 273.15), " degrees Celsius."))
