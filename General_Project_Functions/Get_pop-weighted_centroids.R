#Source: https://www.census.gov/geographies/reference-files/time-series/geo/centers-population.html

#Read in data
county<- read.csv("C:\\Users\\elco2649\\Documents\\Counties_pop-centroids_2010.txt")
census_tract<- read.csv("C:\\Users\\elco2649\\Documents\\Census_tracts_pop-centroids_2010.txt")

for(df in c("county", "census_tract")){
  X<- get(df)
  X<- X[which(X$STATEFP %in% c(4, 6, 8, 16, 30, 32, 35, 41, 49, 53, 56)), c("LONGITUDE", "LATITUDE")]
  write.csv(X, paste0("C:\\Users\\elco2649\\Documents\\Census-2010_Pop-weighted-centroids_", df, ".csv"), row.names = FALSE)
}

# coords<- as.data.frame(X[,c("LONGITUDE", "LATITUDE")])
# plot(coords)
