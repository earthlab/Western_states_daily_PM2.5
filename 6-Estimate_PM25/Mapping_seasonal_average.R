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
  load(paste0("~/Predictions/Ensemble_preds_no_CMAQ_", s, ".RData"))
  DF$Year<- sapply(DF$Date, function(x) as.numeric(strsplit(as.character(x),"-")[[1]][1]))
  DF$Month<- sapply(DF$Date, function(x) as.numeric(strsplit(as.character(x),"-")[[1]][2]))
  data<- DF[which((DF$Year == year)&(DF$Month %in% c(3,4,5))&is.na(DF$Tract_code)&is.na(DF$ZCTA5_code)),] ## Change month indices depending on season
  to_add<- data.frame(State_FIPS = state_fips[which(State == s)], data[,c("County_FIPS",
                                                                          "Lon", "Lat", "Ens_pred")])

  county_data<- rbind(county_data, to_add)
}

save(county_data, file = paste0("~/County_data_spring_", year, ".RData"), ## Change name depending on season
     compress = TRUE)
                    

################################

##All with the same scale:
my_map.fn2<- function(shp, data, nclr, plotclr, Title){
  
  plotvar <- data$Ens_pred
  class <- classIntervals(plotvar,
                          nclr,
                          style = "fixed", 
                          fixedBreaks = c(0,2,4,6,8,12,23,35,60))
  colcode <- findColours(class, plotclr)
  
  plot(shp[data$SHP_Index,], col = colcode) #, xlim = c(-150, -95)
  title(Title, line=-1)
  legend(x= "bottomleft", inset = c(-0.2, -0.2), # position
         legend = names(attr(colcode, "table")),
         title = as.expression(bquote(~PM[2.5] ~ "Levels (" * mu * "g/"*m^3*")")),
         fill = attr(colcode, "palette"),
         cex = 1,
         bty = "n")
}

##Set up:
Countymap<- readOGR(dsn = "~", layer = "tl_2010_us_county10") # load county map shapefile
Countymap$State_FIPS <- sapply(Countymap$STATEFP10, function(x) as.numeric(as.character(x)))
Countymap$County_FIPS <- sapply(Countymap$COUNTYFP10, function(x) as.numeric(as.character(x)))
Countymap$SHP_Index<- 1:dim(Countymap)[1]

nclr<- 8
plotclr<- brewer.pal(nclr, "YlOrRd")
                                
## Make plots:                                

files<- list.files("~", pattern="County_data*", full.names = TRUE)

pdf("Figure 4 with legend.pdf", 8, 11)
par(mfrow=c(2,2), xpd = TRUE)

for(f in files){
  load(f)
  name<- paste(strsplit(f, "_")[[1]][3], strsplit(strsplit(f, "_")[[1]][4], ".R")[[1]][1])  
  Name<- paste(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), sep="")
  
  #Preds:
  my_avg<- aggregate(Ens_pred ~ County_FIPS + State_FIPS, data=county_data, FUN=mean)
  merged<- inner_join(as.data.frame(Countymap), my_avg, by = c("State_FIPS", "County_FIPS"))
  Merged<- merged[,c("State_FIPS", "County_FIPS", "SHP_Index", "Ens_pred")]
  Merged$Ens_pred[which(Merged$Ens_pred < 0)]<- 0
  
  my_map.fn2(Countymap, Merged, nclr, plotclr, Name)
}

dev.off()                    

