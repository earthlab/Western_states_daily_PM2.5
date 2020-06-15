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

year<- 2018

county_data<- data.frame(State_FIPS=integer(), County_FIPS=integer(),
                         Lon=double(), Lat=double(),
                         Ens_pred=double())

for(s in State){
  print(s)
  load(paste0("~/Predictions/Ensemble_preds_no_CMAQ_", s, "_2.RData"))
  DF$Year<- sapply(DF$Date, function(x) as.numeric(strsplit(as.character(x),"-")[[1]][1]))
  DF$Month<- sapply(DF$Date, function(x) as.numeric(strsplit(as.character(x),"-")[[1]][2]))
  data<- DF[which((DF$Year == year)&(DF$Month %in% c(6,7,8))&is.na(DF$Tract_code)&is.na(DF$ZCTA5_code)),]

  to_add<- data.frame(State_FIPS = state_fips[which(State == s)], data[,c("County_FIPS",
                                                                          "Lon", "Lat", "Ens_pred")])

  county_data<- rbind(county_data, to_add)
}

save(county_data, file = paste0("~/County_data_summer_", year, ".RData"),
     compress = TRUE)

my_map.fn<- function(shp, data, nclr, plotclr){
  
  plotvar <- data$Ens_pred
  class <- classIntervals(plotvar,
                          nclr,
                          style = "quantile", 
                          dataPrecision = 3)
  colcode <- findColours(class, plotclr)
  
  plot(shp[data$SHP_Index,], col = colcode, main = paste("Summer", year))
  legend("bottomleft", # position
         legend = names(attr(colcode, "table")), 
         title = "Quantiles",
         fill = attr(colcode, "palette"),
         cex = 0.75,
         bty = "n")
}

## Set up

Countymap<- readOGR(dsn = "/home/rstudio/",layer = "tl_2010_us_county10") # load county map shapefile
Countymap$State_FIPS <- sapply(Countymap$STATEFP10, function(x) as.numeric(as.character(x)))
Countymap$County_FIPS <- sapply(Countymap$COUNTYFP10, function(x) as.numeric(as.character(x)))
Countymap$SHP_Index<- 1:dim(Countymap)[1]

load(paste0("~/County_data_summer_", year, ".RData"))
my_avg<- aggregate(Ens_pred ~ County_FIPS + State_FIPS, data=county_data, FUN=mean)

merged<- inner_join(as.data.frame(Countymap), my_avg, by = c("State_FIPS", "County_FIPS"))
Merged<- merged[,c("State_FIPS", "County_FIPS", "SHP_Index", "Ens_pred")]

##Create map:
nclr<- 8
my_map.fn(Countymap, Merged, nclr, brewer.pal(nclr, "YlOrRd"))

