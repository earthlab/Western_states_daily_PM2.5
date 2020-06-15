##Libraries:
library(dplyr)
library(stringr)
library(caret)

dates<- seq.Date(as.Date(paste0("2008-01-01")), as.Date(paste0("2018-12-31")), by = "day")

# issue_dates<- c(seq.Date(as.Date(paste0("2016-02-17")), as.Date(paste0("2016-02-25")), by = "day"),
          # seq.Date(as.Date(paste0("2011-03-03")), as.Date(paste0("2011-03-05")), by = "day"))
n_days<- length(dates)

Stat<- read.csv("FINAL_Stat.csv") #After getting rid of points from buffer zone (outside the 11 western states)

temporal<- read.csv("Temporal_variables.csv")
temporal$Date<- as.Date(as.character(temporal$Date))

regions<- read.csv("Regions.csv")

## Space-time interactions:
# Region<- rep(unique(regions$Region),3)
# Mid_Study<- c(rep(FALSE,4),rep(TRUE,4),rep(FALSE,4))
# Late_Study<- c(rep(FALSE,4),rep(FALSE,4),rep(TRUE,4))
# stdf<- data.frame(Region, Mid_Study, Late_Study,
#   Region_Mid_Study=interaction(Region, Mid_Study), 
#   Region_Late_Study= interaction(Region, Late_Study))
# write.csv(stdf, "Spacetime_interactions.csv", row.names=FALSE)
stdf<- read.csv("Spacetime_interactions.csv")

#All basic variables:
time_interactions<- inner_join(temporal, stdf, by=c("Mid_Study", "Late_Study"))
ti_state<- inner_join(time_interactions, regions, by="Region")

##Put it all together:
State<- c("nevada", "colorado", "utah", "new mexico", "arizona",
          "washington", "oregon", "idaho", "montana", "wyoming", "california")
#Need to round to four decimals for AF
Lon<- round(Stat$Lon, 4)
Lat<- round(Stat$Lat, 4)
stat<- data.frame(Lon, Lat, Stat[,3:6], State=Stat$State)
af_ref<- cbind(stat, Date = sort(rep(as.Date(dates), dim(stat)[1])))

##Checking for missing locations / days:
Locs<- unique(Stat[,c("Lon", "Lat", "State")])
Ref<- data.frame(Locs, Date = sort(rep(as.Date(dates), dim(Locs)[1])))

for(s in State){
  print(s)
  ref<- Ref[which(Ref$State == s),]
  # locs<- Locs[which(Locs$State == s),]
  
  #Active Fire:
  af<- read.csv(paste0("~/FINAL_state_sets/AF_FINAL_", s, ".csv"))
  ind<- which(names(af) == "X")
  af<- af[which(af$Lon != "Lon"),-ind]
  af$Date<- as.Date(as.character(af$Date))
  af[,which(names(af) != "Date")]<- apply(af[,which(names(af) != "Date")],
                                                        MARGIN = 2, function(x) round(as.numeric(as.character(x)),4) )
  af$Binary_fire<- rowSums(af[,sapply(names(af), FUN = function(x){str_detect(x, "Lag")})]) > 0

  # print("Active Fires:")
  # print(dim(af))
  # print(dim(unique(af[,c("Lon", "Lat")]))[1])
  # print(length(unique(af[,"Date"])))
  # print(dim(unique(af[,c("Lon", "Lat", "Date")]))[1])

  #Be sure to do a left_join with AF, so zeros are taken into account!
  S1<- left_join(af_ref[which(af_ref$State == s),], af, by = c("Lon", "Lat", "Date"))

  S1[which(is.na(S1$Binary_fire)),sapply(0:7, function(x) paste0("Lag",x))]<- 0
  S1[which(is.na(S1$Binary_fire)),"Binary_fire"]<- FALSE

  rm("af")
  print("Done with active fire")

  #MAIAC AOD:
  maiac<- read.csv(paste0("~/FINAL_state_sets/MAIAC_FINAL_", s, ".csv"))[,c(1:6,8,9)]
  maiac<- maiac[which(maiac$Lon != "Lon"),]
  maiac$Date<- as.Date(as.character(maiac$Date))
  maiac[,c("State_FIPS", "County_FIPS", "Tract_code", "ZCTA5_code",
           "Lon", "Lat")]<- apply(maiac[,c("State_FIPS", "County_FIPS", "Tract_code", "ZCTA5_code", "Lon", "Lat")],
                                                                           MARGIN = 2, function(x) as.numeric(as.character(x)) )
  maiac$Lon<- round(maiac$Lon, 5)
  maiac$Lat<- round(maiac$Lat, 5)

  switched<- which(maiac$Lat < 0)
  temp<- maiac[switched, "Lat"]
  maiac[switched, "Lat"]<- maiac[switched, "Lon"]
  maiac[switched, "Lon"]<- temp

  # print("MAIAC:")
  # print(dim(locs)[1] - dim(unique(maiac[,c("Lon", "Lat")]))[1])
  # print(n_days - length(unique(maiac[,"Date"])))
  # print(dim(ref)[1] - dim(unique(maiac[,c("Lon", "Lat", "Date")]))[1])

  S2<- left_join(S1[,c(3:(dim(S1)[2]))], maiac, by = c("State_FIPS","County_FIPS",
                                                        "Tract_code","ZCTA5_code",
                                                        "Date"))
  rm("maiac", "S1")

  # print(dim(locs)[1] - dim(unique(S2[,c("Lon", "Lat")]))[1])
  # print(n_days - length(unique(S2[,"Date"])))
  # print(dim(ref)[1] - dim(unique(S2[,c("Lon", "Lat", "Date")]))[1])
  print("Done with MAIAC AOD")


  #NAM:
  nam<- read.csv(paste0("~/FINAL_state_sets/NAM_FINAL_", s, ".csv"))[,c(1:13)]
  nam<- nam[which(nam$Lon != "Lon"),]
  nam$Date<- as.Date(as.character(nam$Date))
  # nam$Date<- as.Date(format(nam$Date, "%Y-%m-%d"))
  nam[,c("Lon", "Lat")]<- apply(nam[,c("Lon", "Lat")],
                                          MARGIN = 2, function(x) round(as.numeric(as.character(x)),5) )

  NAM<- nam
  # print("NAM:")
  # print(dim(locs)[1] - dim(unique([,c("Lon", "Lat")]))[1])
  # print(n_days - length(unique(NAM[,"Date"])))
  # print(dim(ref)[1] - dim(unique(NAM[,c("Lon", "Lat", "Date")]))[1])

  S3<- left_join(S2, NAM, by = c("Lon", "Lat", "Date"))
  # save(S3, file = paste0("~/FINAL_by_state/AF_MAIAC_NAM-not-unique_", s, ".RData"),
       # compress = TRUE)
  # S3<- distinct(S3)
  rm("nam", "nam08", "NAM", "S2")

  # print(dim(locs)[1] - dim(unique(S3[,c("Lon", "Lat")]))[1])
  # print(n_days - length(unique(S3[,"Date"])))
  # print(dim(ref)[1] - dim(unique(S3[,c("Lon", "Lat", "Date")]))[1])
  print("Done with NAM")

  #NDVI:
  ndvi<- read.csv(paste0("~/FINAL_state_sets/NDVI_FINAL_", s, ".csv"))[,c("Lon", "Lat",
                                                                          "Date", "ndvi")]
  ndvi<- ndvi[which(ndvi$Lon != "Lon"),]
  ndvi$Date<- as.Date(as.character(ndvi$Date))
  ndvi[,c("Lon", "Lat", "ndvi")]<- apply(ndvi[,c("Lon", "Lat", "ndvi")],
                                         MARGIN = 2, function(x) round(as.numeric(as.character(x)),5) )

  # print("NDVI:")
  # print(dim(locs)[1] - dim(distinct(ndvi[,c("Lon", "Lat")]))[1])
  # print(n_days - length(unique(ndvi[,"Date"])))
  # print(dim(ref)[1] - dim(distinct(ndvi[,c("Lon", "Lat", "Date")]))[1])

  S4<- left_join(S3, ndvi, by = c("Lon", "Lat", "Date"))
  rm("ndvi", "S3")

  # print(dim(locs)[1] - dim(unique(S4[,c("Lon", "Lat")]))[1])
  # print(n_days - length(unique(S4[,"Date"])))
  # print(dim(ref)[1] - dim(unique(S4[,c("Lon", "Lat", "Date")]))[1])
  print("Done with NDVI")

  #Add in spacetime variables
  all<- inner_join(S4, regions, by = "State")
  rm("S4")
  all2<- inner_join(all, temporal, by = "Date")
  rm("all")
  all3<- inner_join(all2, stdf, by = c("Region", "Mid_Study", "Late_Study"))
  rm("all2")

  to_save<- left_join(all3, Stat[which(Stat$State == s),])

  To_save<- distinct(to_save)

  save(To_save, file = paste0("~/FINAL_by_state/Pred_withNA_", s, ".RData"),
       compress = TRUE)

  rm("all3", "to_save")
  print("Saved without CMAQ")
  
  #CMAQ:
  cmaq<- read.csv(paste0("~/FINAL_state_sets/CMAQ_FINAL2_", s, ".csv"))
  names(cmaq)[4]<- "all_CMAQ"

  cmaq$Date<- as.Date(as.character(cmaq$Date))
  cmaq[,c("Lon", "Lat")]<- apply(cmaq[,c("Lon", "Lat")],
                                 MARGIN = 2, function(x) as.numeric(as.character(x)) )

  save(cmaq, file = paste0("~/FINAL_by_state/Pred_withNA_CMAQ_", s, ".RData"),
             compress = TRUE)
}




