# Learn rNOMADS package - examples
# See https://cran.r-project.org/web/packages/rNOMADS/rNOMADS.pdf

# To Do: see websites on page 1 of rNOMADS.pdf

#### rNOMADS.pdf page 3-4 example ####
#Getting temperature for North Carolina, USA,
#6-12 hours ago depending on when the latest model run was.
#Get values at the ground surface and at the 800 mb level
#Then make a contour plot of the surface temperature.
#We use GrADS-DODS here for compatibility.
#Using the Global Forecast System 0.5x0.5 model
## Not run:
urls.out <- GetDODSDates(abbrev = "gfs_0p50")
model.url <- tail(urls.out$url, 1) #Get most recent model date
#Get most recent model run
model.runs <- GetDODSModelRuns(model.url)
model.run <- tail(model.runs$model.run, 1)
#Get ground temperature for the 6 hour prediction
variable <- "tmp2m" #temp at 2 m
time <- c(2,2) #6 hour prediction
lon.dom <- seq(0, 360, by = 0.5) #domain of longitudes in model
lat.dom <- seq(-90, 90, by = 0.5) #domain of latitudes in model
lon <- which((lon.dom >= 360 - 84) & (lon.dom <= 360 - 74)) - 1 #NOMADS indexes start at 0
lat <- which((lat.dom <= 37) & (lat.dom >= 32)) - 1 #NOMADS indexes start at 0
model.data.surface <- DODSGrab(model.url, model.run, variable, time, c(min(lon), max(lon)),
                               c(min(lat), max(lat)))
lev <- c(8, 8) #800 mb level
variable <- "tmpprs"
model.data.800mb <- DODSGrab(model.url, model.run, variable, time, c(min(lon), max(lon)),
                             c(min(lat), max(lat)), level = lev)
#Make results into arrays
model.array.surface <- ModelGrid(model.data.surface, c(0.5, 0.5))
model.array.800mb <- ModelGrid(model.data.800mb, c(0.5, 0.5))
#Make a contour plot of the temperature around North Carolina, USA:
contour(x = model.array.surface$x - 360, y = model.array.surface$y,
        model.array.surface$z[1,1,,] - 273.15, xlab = "Longitude", ylab = "Latitude",
        main = paste("North Carolina Surface Temperatures for",
                     model.array.surface$fcst.date, "UTC in Celsius"))
dev.new()
contour(x = model.array.800mb$x - 360, y = model.array.800mb$y,
        model.array.800mb$z[1,1,,] - 273.15, xlab = "Longitude", ylab = "Latitude",
        main = paste("North Carolina Temperatures at 800 mb for",
                     model.array.surface$fcst.date, "UTC in Celsius"))
## End(Not run)



#### rNOMADS.pdf page 5-6 example ####
#An example for the Global Forecast System
#Get data for January 1 2014
#Temperature at 2 m above ground
#3 hour prediction
# using GRIB
abbrev <- "gfsanl"
model.date <- 20140101
model.run <- 06
preds <- 3
## Not run:
model.info <- ArchiveGribGrab(abbrev, model.date,
                              model.run, preds, file.type = "grib2")
model.data <- ReadGrib(model.info[[1]]$file.name, c("2 m above ground"), c("TMP"))
#Get surface temperature in Chapel Hill, NC
lat <- 35.907605
lon <- -79.052147
profile <- BuildProfile(model.data, lon, lat, TRUE)
print(paste0("The temperature prediction in Chapel Hill was ",
             sprintf("%.0f", profile[[1]]$profile.data[1,1,1] - 272.15), " degrees Celsius."))
## End(Not run)

#### rNOMADS.pdf page 7-8 example ####
#Get temperature profile in Chapel Hill, NC.
#First, define each location
lon <- -79.052094
lat <- 35.907562
#Get second to latest GFS 0.5 model, use analysis forecast
#(this ensures the data's fully up on NOMADS)
## Not run:
model.url <- CrawlModels(abbrev = "gfs_0p50", depth = 2)[2]
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
grib.info <- GribGrab(model.url, pred, levels, variables,
model.domain = c(-85, -75, 37, 32))
grib.data <- ReadGrib(grib.info[[1]]$file.name, levels, variables)
profile <- BuildProfile(grib.data, lon, lat, TRUE, points = 8)
plot(profile[[1]]$profile.data[,2, 1] - 272.15,
profile[[1]]$profile.data[,1, 1], xlab = "Temperature (C)",
ylab = "Height (m)", main = "Temperature Profile above Chapel Hill, NC")
## End(Not run)

#### rNOMADS.pdf page 9 example ####
#See what is available for January 1 of this year
abbrev <- "gfs4"
model.date <- paste0(format(Sys.time(), "%Y"), "0101")
## Not run:
gfs.available.models <- CheckNOMADSArchive(abbrev, model.date)
## End(Not run)



