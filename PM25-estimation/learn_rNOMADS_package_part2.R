# learn_rNOMADS_package_part2.R
# modifying code in learn_rNOMADS_package.R - which are based on rNOMADS.pdf
# See https://cran.r-project.org/web/packages/rNOMADS/rNOMADS.pdf

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

#### Define relevant constants ####
# geographic extent: roughly Colorado's geographic extent 
Lat_interest_LB <-  37 #32
Lat_interest_UB <-  41 #37
Lon_interest_LB <-  102 #74
Lon_interest_UB <-  109 # 84
Area_Name <-  "Colorado"
Model_in_use_abbrev_nonArchive <- "gfs_0p50" #"nam_conusnest" #"gfs_0p50"
Model_in_use_abbrev <-  "namanl"
Archive_file_type <- "grib1"

# point of interest: Boulder, Colorado
# Near-ish Boulder
Lat_interest_point <- 40
Lon_interest_point <- -105
Lon_interest_point_positive <- 105
Location_Name <- "Boulder, CO"

# forecast times
forecast_times <- 00 # reanalysis - anything else would be a forecast
Time_of_day <- 18 # 1800 UTC

# Dates of interest
Date_interest <- 20180202 #20080202
start_study_year <- 2008
stop_study_year <- 2018

# meteorological variable of interest
Meteo_var <- "tmp2m" #temp at 2 m
Meteo_Var_Name <- "Surface Temperatures"

#### rNOMADS.pdf page 3 example - not for archived models modified to western US ####
#Getting temperature for Colorado, USA,
#6-12 hours ago depending on when the latest model run was.
#Then make a contour plot of the surface temperature.
#We use GrADS-DODS here for compatibility.
#Using the Global Forecast System 0.5x0.5 model
## Not run:
urls.out <- GetDODSDates(abbrev = Model_in_use_abbrev_nonArchive)
model.url <- tail(urls.out$url, 1) #Get most recent model date
#Get most recent model run
model.runs <- GetDODSModelRuns(model.url)
model.run <- tail(model.runs$model.run, 1)
#Get ground temperature for the 6 hour prediction
variable <- Meteo_var
time <- c(2,2) #6 hour prediction
lon.dom <- seq(0, 360, by = 0.5) #domain of longitudes in model
lat.dom <- seq(-90, 90, by = 0.5) #domain of latitudes in model
lon <- which((lon.dom >= 360 - Lon_interest_UB) & (lon.dom <= 360 - Lon_interest_LB)) - 1 #NOMADS indexes start at 0
lat <- which((lat.dom <= Lat_interest_UB) & (lat.dom >= Lat_interest_LB)) - 1 #NOMADS indexes start at 0
model.data.surface <- DODSGrab(model.url, model.run, variable, time, c(min(lon), max(lon)),
                               c(min(lat), max(lat)))
#Make results into arrays
model.array.surface <- ModelGrid(model.data.surface, c(0.5, 0.5))

#Make a contour plot of the temperature around North Carolina, USA:
contour(x = model.array.surface$x - 360, y = model.array.surface$y,
        model.array.surface$z[1,1,,] - 273.15, xlab = "Longitude", ylab = "Latitude",
        main = paste(Area_Name," ",Meteo_Var_Name," for ",
                     model.array.surface$fcst.date, "UTC in Celsius"))

# clear variables
rm(urls.out, model.url, model.runs, model.run, variable, time)
rm(lon.dom, lat.dom, lon, lat, model.data.surface, model.array.surface)
## End(Not run)

#### rNOMADS.pdf page 5-6 example - modified to Boulder, CO ####
#An example for the Global Forecast System
#Get data for January 1 2014
#Temperature at 2 m above ground
#3 hour prediction
# using GRIB
abbrev <- Model_in_use_abbrev #"namanl" #"gfsanl" #"namanl" #
model.date <- Date_interest #20140101
model.run <- Time_of_day #forecast_times #00
preds <- forecast_times #00

list.available.models <- CheckNOMADSArchive(abbrev, model.date)
print(list.available.models)

## Not run:
model.info <- ArchiveGribGrab(abbrev, model.date,
                              model.run, preds, file.type = "grib2")
#model.info <- ArchiveGribGrab(abbrev, model.date,
#                              model.run, preds, file.type = "grib1")

thisGribInfo <- GribInfo(model.info[[1]]$file.name,file.type = "grib2")
#thisGribInfo <- GribInfo(model.info[[1]]$file.name,file.type = "grib1")
print(thisGribInfo[["inventory"]])
thisGribInfo[["grid"]]

model.data <- ReadGrib(model.info[[1]]$file.name, c("2 m above ground"), c("TMP"))
#model.data <- ReadGrib(model.info[[1]]$file.name, c("sfc"), c("TMP"))
#Get surface temperature in Chapel Hill, NC
#lat <- 35.907605
#lon <- -79.052147
profile <- BuildProfile(model.data, Lon_interest_point, Lat_interest_point, TRUE)
print(paste("The temperature in ",Location_Name," was ",
             sprintf("%.0f", profile[[1]]$profile.data[1,1,1] - 272.15), " degrees Celsius."))
rm(abbrev, model.date, model.run, preds, list.available.models, model.info)
rm(thisGribInfo, model.data, profile)

##################################
# #An example for the NAM
# #Get data for January 1 2014
# #Temperature at 2 m above ground
# #3 hour prediction
# # using GRIB
# abbrev <- Model_in_use_abbrev #"gfsanl"
# model.date <- 20140101
# model.run <- 00#06
# preds <- forecast_times#3
# ## Not run:
# model.info <- ArchiveGribGrab(abbrev, model.date,
#                               model.run, preds, file.type = "grib1") #"grib2")
# #model.data <- ReadGrib(model.info[[1]]$file.name, c("2 m above ground"), c("TMP"))
# model.data <- ReadGrib(file.names = model.info[[1]]$file.name, levels = c("2 m above gnd"),
#          variables = c("TMP"), forecasts = forecast_times, #domain = NULL, 
#          domain.type = "latlon",
#          file.type = "grib1")#, missing.data = NULL)
# 
# #Get surface temperature in Chapel Hill, NC
# #lat <- 35.907605
# #lon <- -79.052147
# profile <- BuildProfile(model.data, Lon_interest_point, Lat_interest_point, TRUE)
# print(paste0("The temperature prediction in Chapel Hill was ",
#              sprintf("%.0f", profile[[1]]$profile.data[1,1,1] - 272.15), " degrees Celsius."))
# ## End(Not run)
# 
# #--------
# abbrev <- "namanl" # "gfsanl" #
# model.date <- 20140101
# model.run <- 00 #06
# preds <- 00  #3
# 
# list.available.models <- CheckNOMADSArchive(abbrev, model.date)
# print(list.available.models)
# ## Not run:
# model.info <- ArchiveGribGrab(abbrev, model.date,
#                               model.run, preds, file.type = "grib1")
# model.data <- ReadGrib(model.info[[1]]$file.name, c("sfc"), c("TMP"))
# #Get surface temperature in Chapel Hill, NC
# lat <- 35.907605
# lon <- -79.052147
# profile <- BuildProfile(model.data, lon, lat, TRUE)
# print(paste0("The temperature prediction in Chapel Hill was ",
#              sprintf("%.0f", profile[[1]]$profile.data[1,1,1] - 272.15), " degrees Celsius."))
# ## End(Not run)

#### rNOMADS.pdf page 7-8 example ####
#Get temperature profile in Chapel Hill, NC.
#First, define each location
#lon <- -79.052094
#lat <- 35.907562
#Get second to latest GFS 0.5 model, use analysis forecast
#(this ensures the data's fully up on NOMADS)
## Not run:
model.url <- CrawlModels(abbrev = Model_in_use_abbrev_nonArchive, depth = 2)[2]
pred <- ParseModelPage(model.url)$pred[1]
## End(Not run)
#Get levels
pressure <- c(1, 2, 3, 5, 7,
              10, 20, 30, 50, 70,
              seq(100, 1000, by = 25))
levels <- paste(pressure, " mb", sep = "")
#Variables - temperature and height only
variables <- c("TMP", "HGT")
## Not run:
#grib.info <- GribGrab(model.url, pred, levels, variables,
#                      model.domain = c(-85, -75, 37, 32))
grib.info <- GribGrab(model.url, pred, levels, variables,
                      model.domain = c((-1)*Lon_interest_UB, (-1)*Lat_interest_LB, Lat_interest_UB, Lat_interest_LB))
grib.data <- ReadGrib(grib.info[[1]]$file.name, levels, variables)
profile <- BuildProfile(grib.data, Lon_interest_point, Lat_interest_point, TRUE, points = 8)
plot(profile[[1]]$profile.data[,2, 1] - 272.15,
     profile[[1]]$profile.data[,1, 1], xlab = "Temperature (C)",
     ylab = "Height (m)", main = paste("Temperature Profile above ",Location_Name))

rm(model.url, pred, pressure, levels, variables, grib.info, grib.data, profile)

## End(Not run)

#### rNOMADS.pdf page 9 example ####
#Check availability for NAM for January first of every year in study period
for (year in start_study_year:stop_study_year) {
model.date <- paste(year,"0101",sep = "")#20080101 #Date_interest#NULL #paste0(format(Sys.time(), "%Y"), "0101")
nam.available.models <- CheckNOMADSArchive(Model_in_use_abbrev, model.date)
print(nam.available.models)
}

#### rNOMADS.pdf page 10 example ####
#Get the latest 5 instances
#for the Global Forecast System 0.5 degree model
## Not run: 
urls.out <- CrawlModels(abbrev = "gfs_0p50", depth = 5)

#### rNOMADS.pdf page 11-12 example ####
#An example for the Global Forecast System 0.5 degree model
#Make a world temperature map for the latest model run
## Not run:
#Figure out which model is most recent
model.urls <- GetDODSDates("gfs_0p50")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)
#Download worldwide temperature data at 2 m
variable <- "tmp2m"
time <- c(0, 0) #Analysis run, index starts at 0
lon <- c(0, 719) #All 720 longitude points
lat <- c(0, 360) #All 361 latitude points
model.data <- DODSGrab(latest.model, latest.model.run,
                       variable, time, lon, lat)
#Make it into a nice array and plot it
model.grid <- ModelGrid(model.data, c(0.5, 0.5))
image(model.grid$z[1,1,,])
## End(Not run)

# DODS = Distributed Oceanographic Data System

NOMADSRealTimeList("grib")
NOMADSArchiveList("grib")
NOMADSRealTimeList("dods")
NOMADSArchiveList("dods")

#### rNOMADS.pdf page 15 example ####
#An example for the Global Forecast System 0.5 degree model
#Get the latest model url and date
abbrev <- "gfs_0p50"
## Not run:
urls.out <- GetDODSDates(abbrev)
print(paste("Most recent model run:",tail(urls.out$date, 1)))
#Get model dates from the GFS archive
abbrev <- Model_in_use_abbrev#"gfs-avn-hi"
#DIDN'T WORK: urls.out <- GetDODSDates(abbrev, archive = TRUE, request.sleep = 1)
## End(Not run)

#### rNOMADS.pdf page 16-17 example ####
#An example for the Global Forecast System 0.5 degree model
#Get some information about the latest model url and date, real time server
#abbrev <- "nam"#Model_in_use_abbrev#"gfs_0p50"
abbrev <- "gfs_0p50"
## Not run:
urls.out <- GetDODSDates(abbrev)
#urls.out <- GetDODSDates(abbrev = Model_in_use_abbrev, archive = TRUE, request.sleep = 1)
model.url <- tail(urls.out$url, 1)
model.runs <- GetDODSModelRuns(model.url)
model.info <- GetDODSModelRunInfo(model.url, tail(model.runs$model.run, 1))
print(model.info)
## End(Not run)

#### rNOMADS.pdf page 18 example ####
#An example for the Global Forecast System 0.5 degree model
#Get the latest model url and date, real time server
abbrev <- "gfs_0p50"
## Not run:
urls.out <- GetDODSDates(abbrev)
model.url <- tail(urls.out$url, 1)
model.runs <- GetDODSModelRuns(model.url)
print(paste("Latest model run", tail(model.runs$model.run.info, 1)))
## End(Not run)
#DIDN'T WORK: Get model dates from the GFS analysis archive
#abbrev <- "gfsanl"
#model.url <- NOMADSArchiveList("dods", abbrev = abbrev)$url
#model.url <- NOMADSArchiveList("grib", abbrev = abbrev)$url
## Not run:
#model.runs <- GetDODSModelRuns(model.url)
#print(model.runs$model.run.info)
## End(Not run)

#### rNOMADS.pdf page 19-20 example ####
#An example for the Global Forecast System 0.5 degree model
#Get the latest model url
## Not run:
urls.out <- CrawlModels(abbrev = "gfs_0p50", depth = 1)
#Get a list of forecasts, variables and levels
model.parameters <- ParseModelPage(urls.out[1])
#Figure out which one is the 6 hour forecast
#provided by the latest model run
#(will be the forecast from 6-12 hours from the current date)
my.pred <- model.parameters$pred[grep("06$", model.parameters$pred)]
#What region of the atmosphere to get data for
levels <- c("2 m above ground", "800 mb")
#What data to return
variables <- c("TMP", "RH") #Temperature and relative humidity
#Get the data
grib.info <- GribGrab(urls.out[1], my.pred, levels, variables)
#Extract the data
model.data <- ReadGrib(grib.info[[1]]$file.name, levels, variables)
#Reformat it
model.grid <- ModelGrid(model.data, c(0.5, 0.5))
#Show an image of world temperature at ground level
image(model.grid$z[2, 1,,])
## End(Not run)

#### rNOMADS.pdf page 21 example ####
## Not run:
#An example for the Global Forecast System 0.5 degree model
#Get the second latest model url, for stability
urls.out <- CrawlModels(abbrev = "gfs_0p50", depth = 2)
#Get a list of forecasts, variables and levels
model.parameters <- ParseModelPage(urls.out[2])
#Figure out which one is the 6 hour forecast
#provided by the latest model run
#(will be the forecast from 6-12 hours from the current date)
my.pred <- model.parameters$pred[grep("06$", model.parameters$pred)]
#What region of the atmosphere to get data for
levels <- c("2 m above ground", "800 mb")
#What data to return
variables <- c("TMP", "RH") #Temperature and relative humidity
#Get the data
grib.info <- GribGrab(urls.out[2], my.pred, levels, variables)
#Print out the inventory - it should match the requested data
grib.inv <- GribInfo(grib.info[[1]]$file.name, "grib2")
## End(Not run)

#### rNOMADS.pdf page 22 example ####
#Find model runs for the
#GFS 0.5x0.5 model
## Not run:
urls.out <- LinkExtractor(
  "http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p50.pl")
print(urls.out)
## End(Not run)

#### rNOMADS.pdf page 23 example ####
zonal.wind <- c(35.5, -2)
meridional.wind <- c(-5, 15)
winds <- MagnitudeAzimuth(zonal.wind, meridional.wind)
print(winds$magnitude)
print(winds$azimuth)

#### rNOMADS.pdf page 25 example ####
## Not run:
#Get some example data
urls.out <- CrawlModels(abbrev = "gfs_0p50", depth = 1)
model.parameters <- ParseModelPage(urls.out[1])
levels <- c("2 m above ground", "100 mb")
variables <- c("TMP", "RH") #Temperature and relative humidity
grib.info <- GribGrab(urls.out[1], model.parameters$pred[1], levels, variables)
#Extract the data
model.data <- ReadGrib(grib.info[[1]]$file.name, levels, variables)
#Make it into an array
gfs.array <- ModelGrid(model.data, c(0.5, 0.5))
#What variables and levels we have
print(gfs.array$levels)
print(gfs.array$variables)
#Find minimum temperature at the ground surface, and where it is
min.temp <- min(gfs.array$z[2, 1,,] - 273.15)
sprintf("%.1f", min.temp) #in Celsius
ti <- which(gfs.array$z[2, 1,,] == min.temp + 273.15, arr.ind = TRUE)
lat <- gfs.array$y[ti[1,2]] #Lat of minimum temp
lon <- gfs.array$x[ti[1,1]] #Lon of minimum temp
#Find maximum temperature at 100 mb atmospheric pressure
max.temp <- max(gfs.array$z[1, 1,,]) - 273.15
sprintf("%.1f", max.temp) #Brrr!
## End(Not run)

#### rNOMADS.pdf page 26 example ####
#The archived model list in rNOMADS
archived.model.list <- NOMADSArchiveList("grib")
print(archived.model.list)

archived.model.list <- NOMADSArchiveList("dods")
print(archived.model.list)

#### rNOMADS.pdf page 27 example ####
## Not run:
#Grib filter
model.list <- NOMADSRealTimeList("grib")
print(model.list)
#DODS interface
model.list <- NOMADSRealTimeList("dods")
print(model.list)
## End(Not run)

#### rNOMADS.pdf page 29 example ####
#An example for the Global Forecast System 0.5 degree model
#Get the latest model url
## Not run:
urls.out <- CrawlModels(abbrev = "gfs_0p50", depth = 1)
#Get a list of forecasts, variables and levels
model.parameters <- ParseModelPage(urls.out[1])
## End(Not run)
archive_info <- NOMADSArchiveList(url.type = "grib", abbrev = "namanl")
model.url <- archive_info$url
ParseModelPage(model.url)

#### rNOMADS.pdf page 30-31 example ####
## Not run:
download.file("http://www.unc.edu/~haksaeng/rNOMADS/myTA.RDATA",
              destfile = "myTA.RDATA")
load("myTA.RDATA")
#Find the latest Global Forecast System model run
model.urls <- GetDODSDates("gfs_0p50")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
#Get model nodes
lons <- seq(0, 359.5, by = 0.5)
lats <- seq(-90, 90, by = 0.5)
lon.ind <- which(lons <= (max(myTA$lon + 360) + 1) & lons >= (min(myTA$lon + 360) - 1))
lat.ind <- which(lats <= (max(myTA$lat) + 1) & lats >= (min(myTA$lat) - 1))
levels <- c(0, 46)
time <- c(0, 0)
#Get data
variables <- c("hgtprs", "ugrdprs", "vgrdprs")
model.data <- DODSGrab(latest.model, latest.model.run,
                       variables, time, c(min(lon.ind), max(lon.ind)),
                       c(min(lat.ind), max(lat.ind)), levels)
#Build profiles
profile <- BuildProfile(model.data, myTA$lon + 360, myTA$lat,
                        spatial.average = FALSE)
#Build profiles
zonal.wind <- NULL
meridional.wind <- NULL
height <- NULL
for(k in 1:length(profile)) {
  hgt <- profile[[k]]$profile.data[, which(profile[[k]]$variables == "hgtprs"),]
  ugrd <- profile[[k]]$profile.data[, which(profile[[k]]$variables == "ugrdprs"),]
  vgrd <- profile[[k]]$profile.data[, which(profile[[k]]$variables == "vgrdprs"),]
  synth.hgt <- seq(min(hgt),
                   max(hgt), length.out = 1000)
  ugrd.spline <- splinefun(hgt, ugrd, method = "natural")
  vgrd.spline <- splinefun(hgt, vgrd, method = "natural")
  zonal.wind[[k]] <- ugrd.spline(synth.hgt)
  meridional.wind[[k]] <- vgrd.spline(synth.hgt)
  height[[k]] <- synth.hgt
}
#Plot them all
PlotWindProfile(zonal.wind, meridional.wind, height, lines = TRUE,
                points = FALSE, elev.circles = c(0, 25000, 50000), elev.labels = c(0, 25, 50),
                radial.lines = seq(45, 360, by = 45), colorbar = TRUE, invert = FALSE,
                point.cex = 2, pch = 19, lty = 1, lwd = 1,
                height.range = c(0, 50000), colorbar.label = "Wind Speed (m/s)")
## End(Not run)


