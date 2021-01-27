library(dplyr)
library(stringr)

## Monitor locations
locs<- read.csv("PM25_Step4_part_f_de_duplicated_aves_ML_input.csv")
Locs<- distinct(locs[,c("Lon", "Lat", "Data_Source_Name_Display")])
Locs$Org<- sapply(Locs[,3], function(x) str_split(x, ",")[[1]][1])
Locs$Org[which(str_detect(Locs$Org, "IMP"))]<- "IMPROVE"

table(Locs$Org)

my_order<- c("EPA PM2.5", "IMPROVE", "CARB", "CARB Mobile Monitor ", 
             "Fire Cache Smoke Monitor (DRI)", "Utah DEQ",
          "PCAPS (Salt Lake Valley)", "Uintah Basin")
Locs$Index<- match(Locs$Org, my_order)
Locs<- Locs[order(Locs$Index),]

my_colors<- c("darkorchid3", "limegreen", "dodgerblue2", "gold2", 
               "coral1",  "red", 
              "midnightblue", "darkred")

## Basemap:
library(maps)
pdf("Monitor_locations.pdf", 8,8)
map("state", region = c("arizona", "california", "colorado", "idaho", 
                        "montana", "nevada", "new mexico", "oregon", 
                        "utah",  "washington:san juan island", 
                        "washington:lopez island", "washington:orcas island",
                        "washington:whidbey island", "washington:main", "wyoming"),
    xlim=c(-150,-95), ylim=c(25, 54))
points(Locs$Lon, Locs$Lat, pch = 17, cex=0.75, col = my_colors[Locs$Index])
legend(x = -145, y = 45, legend = c("EPA PM2.5 - 804", "IMPROVE - 295", 
                                    "CARB - 182", "CARB Mobile Monitor - 3615", 
                                    "Fire Cache Smoke Monitor (DRI) - 6755", 
                                    "Utah DEQ - 17",
                                    "PCAPS (Salt Lake Valley) - 7", "Uintah Basin - 10"), 
       col = my_colors, pch = 17, 
       cex = 0.75)
dev.off()


