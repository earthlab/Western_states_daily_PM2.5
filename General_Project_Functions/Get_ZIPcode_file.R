library(raster)

#Source: https://www2.census.gov/geo/tiger/TIGER2013/ZCTA5/
zips<- shapefile("C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Machine Learning\\tl_2013_us_zcta510.shp")

zips$INTPTLAT10<- as.numeric(zips$INTPTLAT10)
zips$INTPTLON10<- as.numeric(zips$INTPTLON10)

west<- zips[which((zips$INTPTLAT10 < 50) & (zips$INTPTLAT10 > 30)
                  & (zips$INTPTLON10 > -126) & (zips$INTPTLON10 < -100)),]

plot(west) #this still takes a while

shapefile(west, filename = "C:\\Users\\ellen\\OneDrive\\MyDocs\\Earth Lab Internship\\Machine Learning\\Western-US_ZIP_codes.shp")
