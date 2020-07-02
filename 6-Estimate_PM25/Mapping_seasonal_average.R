library(stringr)
library(RColorBrewer)
library(classInt)
library(sp)
library(FNN)
library(scales)
library(dplyr)

library(rgdal)
library(raster)

## Get data from the season of interest:
State<- c("nevada", "colorado", "utah", "new mexico", "arizona",
          "washington", "oregon", "idaho", "montana", "wyoming", "california")

state_fips<- c(32, 8, 49, 35, 4, 53, 41, 16, 30, 56, 6)

year<- 2011 #Change this every time

county_data<- data.frame(State_FIPS=integer(), County_FIPS=integer(),
                         Lon=double(), Lat=double(),
                         Ens_pred=double())

for(s in State){
  print(s)
  load(paste0("~/Predictions/Ensemble_preds_no_CMAQ_", s, "_2.RData"))
  DF$Year<- sapply(DF$Date, function(x) as.numeric(strsplit(as.character(x),"-")[[1]][1]))
  DF$Month<- sapply(DF$Date, function(x) as.numeric(strsplit(as.character(x),"-")[[1]][2]))
  data<- DF[which((DF$Year == year)&(DF$Month %in% c(3,4,5))&is.na(DF$Tract_code)&is.na(DF$ZCTA5_code)),]
  #Change months ^^^ depending on season
  to_add<- data.frame(State_FIPS = state_fips[which(State == s)], data[,c("County_FIPS",
                                                                          "Lon", "Lat", "Ens_pred")])

  county_data<- rbind(county_data, to_add)
}

save(county_data, file = paste0("~/County_data_spring_", year, ".RData"),
     compress = TRUE)

# my_map.fn<- function(shp, data, nclr, plotclr){
#   
#   plotvar <- data$Ens_pred
#   class <- classIntervals(plotvar,
#                           nclr,
#                           style = "quantile", 
#                           dataPrecision = 3)
#   colcode <- findColours(class, plotclr)
#   
#   plot(shp[data$SHP_Index,], col = colcode, main = paste("Spring", year))
#   legend("bottomleft", # position
#          legend = names(attr(colcode, "table")), 
#          title = "Quantiles",
#          fill = attr(colcode, "palette"),
#          cex = 0.75,
#          bty = "n")
# }
# 
# ## Set up
# 
# Countymap<- readOGR(dsn = "/home/rstudio/",layer = "tl_2010_us_county10") # load county map shapefile
# Countymap$State_FIPS <- sapply(Countymap$STATEFP10, function(x) as.numeric(as.character(x)))
# Countymap$County_FIPS <- sapply(Countymap$COUNTYFP10, function(x) as.numeric(as.character(x)))
# Countymap$SHP_Index<- 1:dim(Countymap)[1]
# 
# load(paste0("~/County_data_spring_", year, ".RData"))
# my_avg<- aggregate(Ens_pred ~ County_FIPS + State_FIPS, data=county_data, FUN=mean)
# 
# merged<- inner_join(as.data.frame(Countymap), my_avg, by = c("State_FIPS", "County_FIPS"))
# Merged<- merged[,c("State_FIPS", "County_FIPS", "SHP_Index", "Ens_pred")]
# 
# ##Create map:
# nclr<- 8
# my_map.fn(Countymap, Merged, nclr, brewer.pal(nclr, "YlOrRd"))


################################

##All with the same scale:
my_map.fn2<- function(shp, data, nclr, plotclr, title){
  
  plotvar <- data$Ens_pred
  class <- classIntervals(plotvar,
                          nclr,
                          style = "fixed", 
                          fixedBreaks = c(0,2,4,6,8,12,23,35,60))
  colcode <- findColours(class, plotclr)
  
  plot(shp[data$SHP_Index,], col = colcode, main = title)
  legend("bottomleft", # position
         legend = names(attr(colcode, "table")),
         title = "PM2.5 Levels (ug/m^3)",
         fill = attr(colcode, "palette"),
         cex = 1,
         bty = "n")
}

##Set up:
Countymap<- readOGR(dsn = "/home/rstudio/",layer = "tl_2010_us_county10") # load county map shapefile
Countymap$State_FIPS <- sapply(Countymap$STATEFP10, function(x) as.numeric(as.character(x)))
Countymap$County_FIPS <- sapply(Countymap$COUNTYFP10, function(x) as.numeric(as.character(x)))
Countymap$SHP_Index<- 1:dim(Countymap)[1]

nclr<- 8
plotclr<- brewer.pal(nclr, "YlOrRd")

##Observations:
load("~/ML_inputs.RData")
rm("DF_with_CMAQ")

files<- list.files("~", pattern="County_data*", full.names = TRUE)
for(f in files){
  load(f)
  name<- paste(strsplit(f, "_")[[1]][3], strsplit(strsplit(f, "_")[[1]][4], ".R")[[1]][1])
  Name<- paste(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), sep="")
  
  #Preds:
  my_avg<- aggregate(Ens_pred ~ County_FIPS + State_FIPS, data=county_data, FUN=mean)
  merged<- inner_join(as.data.frame(Countymap), my_avg, by = c("State_FIPS", "County_FIPS"))
  Merged<- merged[,c("State_FIPS", "County_FIPS", "SHP_Index", "Ens_pred")]
  Merged$Ens_pred[which(Merged$Ens_pred < 0)]<- 0
  
  # #Obs:
  # these_obs<- DF_no_CMAQ[which((DF_no_CMAQ$Season == strsplit(f, "_")[[1]][3])&
  #                                DF_no_CMAQ$Year == as.numeric(strsplit(strsplit(f, "_")[[1]][4], ".R")[[1]][1])), 
  #                        c("Lon", "Lat", "PM2.5_Obs")]
  # these_obs$Count<- 1
  # counts<- aggregate(Count ~ Lon + Lat, data = these_obs, sum)
  # avg_obs<- aggregate(PM2.5_Obs ~ Lon + Lat, data = these_obs, mean)
  # obs_to_use<- avg_obs[which(counts$Count > 5),]
  # class <- classIntervals(obs_to_use$PM2.5_Obs,
  #                         nclr,
  #                         style = "fixed", 
  #                         fixedBreaks = c(0,2,4,6,8,12,23,35,300))
  # colcode <- findColours(class, plotclr)
  
  png(filename = paste0("~/Plots/County_map_", Name, ".png"))
  my_map.fn2(Countymap, Merged, nclr, plotclr, Name)
  # points(obs_to_use$Lon, obs_to_use$Lat, pch = 21, col = "blue", bg = colcode)
  dev.off()
  
}
