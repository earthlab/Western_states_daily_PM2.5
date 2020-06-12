### Author: Ellen Considine

## Do this first section one year at a time to avoid crashing...

#Read in data:
# CM2013<- read.csv("./ds.input.cmaq.pm25.2013.csv")
# CM2014<- read.csv("./ds.input.cmaq.pm25.2014.csv")
# CM2015<- read.csv("./ds.input.cmaq.pm25.2015.csv")
CM2016<- read.csv("./ds.input.cmaq.pm25.2016.csv")

names(CM2016) #  Domain Col Row       Lon      Lat       Date    Conc

rm(list=setdiff(ls(), c("CM2016", "DATA")))

#MERGE with other data:
library("FNN") # has get.kknx function

merge_CMAQ<- function(row, X1){

  date<- as.character(row[[1]])
  data<- X1[which(X1$Date == date),]

  Lon<- round(data$Lon, 4)
  Lat<- round(data$Lat, 4)
  # plot(Lon, Lat, cex = 0.01, pch = 16)

  Data<- cbind(Lon, Lat, PM2.5 = as.vector(data$Conc))
  #Get nearest neighbor value:
  NN_val<- Data[get.knnx(Data[,c("Lon", "Lat")],
                         t(c(row[[2]], row[[3]])),
                         k = 1)$nn.index, 3]

  return(as.vector(NN_val))
}

merge_many_CMAQ<- function(pos, N){
  CMAQ<- c()
  for(i in pos){
    CMAQ<- append(CMAQ, merge_CMAQ(late_data[i,c("Date","Lon","Lat")], CM2016)) #Change csv
  }
  write.csv(CMAQ, paste0("CMAQ_", N, ".csv"), row.names = FALSE)
  return(CMAQ)
}

#Read in data:
load("data.RData")

late_data<- DATA[which(DATA$Year %in% 2016),]

#Run through data set, parallelized:
library(future.apply)
library(parallel)

ncores<- detectCores()-1
uniq_seq<- seq(1,dim(late_data)[1], length.out = ncores + 1)
loc_list<- c()
for(j in 1:ncores){
  uniq_locs<- round(uniq_seq[j]):round(uniq_seq[j+1]-1)
  loc_list<- append(loc_list, list(uniq_locs))
}

options(future.globals.maxSize= 2500*1024^2)

plan(multiprocess, workers = ncores)

this_list<- future_lapply(1:length(loc_list), function(j){merge_many_CMAQ(loc_list[[j]],j)})

all_CMAQ<- unlist(this_list)

last<- merge_CMAQ(late_data[dim(late_data)[1],c("Date","Lon","Lat")], CM2016) #Change this each year
all_CMAQ<- append(all_CMAQ, last)

Late_data<- cbind(late_data, all_CMAQ)

Late_data$Mid_Study<- (Late_data$Year > 2011)&(Late_data$Year <= 2015)
Late_data$Late_Study<- Late_data$Year > 2015
Late_data$Year<- as.factor(Late_data$Year)
Late_data$Int_Year_State_Mid<- interaction(Late_data$Mid_Study, Late_data$Region)
Late_data$Int_Year_State_Late<- interaction(Late_data$Late_Study, Late_data$Region)

write.csv(Late_data, "Data_with_CMAQ_2016.csv", row.names = FALSE)

#########################

cm2013<- read.csv("/home/rstudio/Data_with_CMAQ_2013.csv")
cm2014<- read.csv("/home/rstudio/Data_with_CMAQ_2014.csv")
cm2015<- read.csv("/home/rstudio/Data_with_CMAQ_2015.csv")
cm2016<- read.csv("/home/rstudio/Data_with_CMAQ_2016.csv")

all_CM<- rbind(cm2013, cm2014, cm2015, cm2016)

write.csv(all_CM, "Data_with_CMAQ_13-16.csv", row.names = FALSE)

