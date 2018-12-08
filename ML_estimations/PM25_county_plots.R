

library(RColorBrewer)
library(classInt)
library(sp)
library(FNN)
library(scales)

# library(rgdal)
library(raster)

map_KNN.fn<- function(shp, data, K, nclr, plotclr, breaks){
  query<- coordinates(shp)
  coords<- data[, c("Longitude", "Latitude")]
  knn<- get.knnx(coords, query, k = K)
  index<- knn$nn.index
  dist<- knn$nn.dist
  mat<- matrix(data[t(index), "AQ"], nrow = nrow(query), byrow = TRUE)
  invdist<- t(apply(dist, 1, function(x)(x/sum(x))))
  
  aq<- rep(0,nrow(query))
  for (a in 1:length(aq)){
    aq[a]<- sum(invdist[a,]*mat[a,])
  }
  
  #Plotting help: https://www.r-bloggers.com/custom-legend-in-r/ 
  plotvar <- aq
  class <- classIntervals(plotvar,
                          nclr,
                          style = "fixed",
                          fixedBreaks = breaks)
  colcode <- findColours(class, plotclr)
  
  plot(shp, col = colcode)
  legend("bottomleft", # position
         legend = names(attr(colcode, "table")), 
         title = "Quantiles",
         fill = attr(colcode, "palette"),
         cex = 0.75,
         bty = "n")
}

#Create Map

CountyMaps.directory<- "C:/Users/ellen/OneDrive/MyDocs/Earth Lab Internship/Presentations/CountyMap"
Countymap<- readOGR(dsn=file.path(CountyMaps.directory),layer = "cb_2017_us_county_500k") # load county map shapefile
Countymap$STATEFP_NUM <- as.numeric(as.character(Countymap$STATEFP)) # have R recognize state FP's as numerical values (in a new column)
# find the 11 western states included in the study
WestCountymapGeom<- Countymap[Countymap$STATEFP_NUM==4|Countymap$STATEFP_NUM==6|Countymap$STATEFP_NUM==8|Countymap$STATEFP_NUM==16|Countymap$STATEFP_NUM==30|Countymap$STATEFP_NUM==32|Countymap$STATEFP_NUM==35|Countymap$STATEFP_NUM==41|Countymap$STATEFP_NUM==49|Countymap$STATEFP_NUM==53|Countymap$STATEFP_NUM==56,]
proj4string(WestCountymapGeom)<- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"

#Read in and format data 
#Data table for function needs the columns "Longitude", "Latitude", and "AQ" (air quality measurement) 

base<- read.csv("C:/Users/ellen/OneDrive/MyDocs/Earth Lab Internship/Presentations/ML_input_PM25_Step5_part_d_de_duplicated_aves_ML_input.csv") 
preds<- read.csv("C:/Users/ellen/OneDrive/MyDocs/Earth Lab Internship/Presentations/ML_Predictions_CountieswPredictors.csv")

nclr<- 8
base_data<- base[which(base$Date == "2008-07-11"), c("Longitude", "Latitude", "PM2.5_Obs")]
colnames(base_data)<- c("Longitude", "Latitude", "AQ")
base_breaks<- round(quantile(base_data$AQ, seq(0, 1, 1/nclr)), 4)
pred_data<- preds[which(preds$Date == "2008-07-11"), c("Longitude", "Latitude", "PM25_prediction")]
colnames(pred_data)<- c("Longitude", "Latitude", "AQ")
pred_breaks<- round(quantile(pred_data$AQ, seq(0, 1, 1/nclr)), 4)

#Plot actual PM2.5
map_KNN.fn(WestCountymapGeom, base_data, K = 2, nclr, plotclr= brewer.pal(nclr, "YlOrRd"), base_breaks)

#Plot predicted PM2.5
map_KNN.fn(WestCountymapGeom, pred_data, K = 2, nclr, plotclr= brewer.pal(nclr, "YlOrRd"), pred_breaks)

##########################
library(raster)
library(spatialEco)
library(plyr)

points<- SpatialPoints(base_data[,c("Longitude", "Latitude")], CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"), bbox = NULL)
ptdf<- SpatialPointsDataFrame(base_data[,c("Longitude", "Latitude")], data.frame(base_data$AQ), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"), bbox = NULL)

INT<- point.in.poly(ptdf, WestCountymapGeom)
# plot(INT)

# aggregate(d[, 3:4], list(d$Name), mean)
Mean<- aggregate(INT[,1], list(INT$GEOID), mean)
Mean$Group.1<- as.numeric(Mean$Group.1)
WestCountymapGeom$GEOID<- as.numeric(WestCountymapGeom$GEOID)

pos<- which(WestCountymapGeom$GEOID %in% Mean$Group.1)
small<- WestCountymapGeom[pos,]
m<- match(small$GEOID, Mean$Group.1)
aq<- Mean[m,2]

plotclr<- brewer.pal(nclr, "YlOrRd")
plotvar <- aq$base_data.AQ
breaks<- round(quantile(plotvar, seq(0, 1, 1/nclr)), 4)
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = breaks)
colcode <- findColours(class, plotclr)

plot(WestCountymapGeom)
plot(small, col = colcode, add = TRUE)
legend("bottomleft", # position
       legend = names(attr(colcode, "table")), 
       title = "Quantiles",
       fill = attr(colcode, "palette"),
       cex = 0.75,
       bty = "n")


map_avg.fn<- function(shp, data, nclr, plotclr, breaks){
  points<- SpatialPoints(data[,c("Longitude", "Latitude")], CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"), bbox = NULL)
  ptdf<- SpatialPointsDataFrame(data[,c("Longitude", "Latitude")], data.frame(data$AQ), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"), bbox = NULL)
  #take intersection of points and polygons
  INT<- point.in.poly(ptdf, shp)
  #Average all points inside each polygon
  Mean<- aggregate(INT[,"data.AQ"], by = list(INT$GEOID), FUN = mean)
  #Get everything in the right format
  Mean$Group.1<- as.numeric(Mean$Group.1)
  shp$GEOID<- as.numeric(shp$GEOID)
  #match across the data frames
  pos<- which(shp$GEOID %in% Mean$Group.1)
  small<- shp[pos,]
  m<- match(small$GEOID, Mean$Group.1)
  aq<- Mean[m,2]
  #Setting up plotting variables
  plotvar <- aq$data.AQ
  class <- classIntervals(plotvar,
                          nclr,
                          style = "fixed",
                          fixedBreaks = breaks)
  colcode <- findColours(class, plotclr)
  #Plotting
  plot(shp)
  plot(small, col = colcode, add = TRUE)
  # plot(points, add= TRUE)
  legend("bottomleft", # position
         legend = names(attr(colcode, "table")), 
         title = "Quantiles",
         fill = attr(colcode, "palette"),
         cex = 0.75,
         bty = "n")
  
}

map_avg.fn(WestCountymapGeom, base_data, nclr, plotclr= brewer.pal(nclr, "YlOrRd"), base_breaks)

map_avg.fn(WestCountymapGeom, pred_data, nclr, plotclr= brewer.pal(nclr, "YlOrRd"), pred_breaks)
