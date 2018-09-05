# Try RUC 13 data

#### Call Packages (Library) ####
library(rNOMADS)

# see what the model options are:
NOMADSArchiveList("grib")

rm(list  =  ls())
#See what is available for January 1 of this 2008
#abbrev <- "ruc" # not back to 2008
#abbrev <- "ruc13" # not back to 2008
#abbrev <- "meso-eta-hi" # not back to 2008
#abbrev <- "gfs-avn-hi"  # not back to 2008
#abbrev <- "gfs4" # not back to 2008
#abbrev <- "rap252" # not back to 2008
#abbrev <- "rap130" # not back to 2008
#abbrev <- "gfsanl" # ****has data back to Jan 1, 2008; grib1 files, 55 km resolution****
#abbrev <- "rucanl" # not back to 2008
abbrev <- "namanl" # ****has data back to Jan 1, 2008; grib1 files, 12 km resolution****
#model.date <- paste0(format(Sys.time(), "%Y"), "0101")
#model.date <- 20080101
model.date <- 20090204
#model.date <- 20171131
#model.date <- 20090203
#model.date <- 20141231
## Not run:
list.available.models <- CheckNOMADSArchive(abbrev, model.date)
## End(Not run)


#### Examples - rNOMADS.pdf page 5-6 - modified for ruc13 data ####
#An example for the Rapid Update Cycle 13 km grid
#Get data for January 1 2008
#Temperature at 2 m above ground
#0 hour prediction, i.e., analysis
# using GRIB
abbrev <- "namanl"
#model.date <- 20080101
model.date <- 20090204
model.run <- 00
preds <- 00
model.info <- ArchiveGribGrab(abbrev, model.date,
                              model.run, preds, file.type = "grib1")
thisGribInfo <- GribInfo(model.info[[1]]$file.name,file.type = "grib1")
print(thisGribInfo[["inventory"]])
thisGribInfo[["grid"]]

# GribInfo is equivalent to this in the terminal window:
#wgrib 20080101_0000_000.grb

#this_file_name <- "/home/rstudio/NARR/2008/LI295300_merged_AWIP32.2008070215.grb"

#ReadGrib(file.names, levels, variables,forecasts = NULL, domain = NULL, domain.type = "latlon",
#  file.type = "grib2", missing.data = NULL)

ReadGrib(file.names = model.info[[1]]$file.name, levels = c("2 m above gnd"),
         variables = c("TMP"), forecasts = 00, #domain = NULL, 
         domain.type = "latlon",
          file.type = "grib1")#, missing.data = NULL)

this_model.data <- ReadGrib(model.info[[1]]$file.name, c("2 m above gnd"), c("TMP"), file.type = "grib1") # doesn't work right
#model.data <- ReadGrib(this_file_name, c("sfc"), c("HPBL"), file.type = "grib1")

# Near-ish Boulder
lat <- 40
lon <- -105

#BuildProfile(model.data, lon, lat, spatial.average, points = 4)
profile <- BuildProfile(model.data = this_model.data, lon, lat, spatial.average, points = 4)
profile <- BuildProfile(model.data, lon, lat, TRUE) # doesn't work


#print(paste0("The temperature prediction in Chapel Hill was ",
#             sprintf("%.0f", profile[[1]]$profile.data[1,1,1] - 272.15), " degrees Celsius."))
print(paste0("The temperature prediction in Boulder was ",
             sprintf("%.0f", profile[[1]]$profile.data[1,1,1] - 272.15), " degrees Celsius."))
