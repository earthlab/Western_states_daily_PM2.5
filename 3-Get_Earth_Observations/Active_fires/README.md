# Active Fire Download
First download the MODIS and/or VIIRS active fire product(s) from [https://firms.modaps.eosdis.nasa.gov/download/](Archive Download Tool) and select shp file.

## active_fire.py
This script will spatially join the fire data with the timezone data. This is a necessary step for adjusting the timestamp from UTC to local in the next step.

## buffers_observation_set.py
This script will take in a csv file with lat, lon, and dates (the PM2.5 stations/points of interest) and also take in a shp file with buffers around each of these observation (specified as arguments). The output is a csv file similar to the input csv, but with an additional columns that includes the number of active fires in each buffer. 

## buffers_prediction_set.py
This script will take in a csv file with lat, lon pairs (the prediction locations of interest) and also take in a shp file with buffers around each of these locations (specified as arguments). The output is a csv file similar to the input csv, but with an additional columns that includes the number of active fires in each buffer. 

___Note: the buffer scripts will only include date-location pairs for which the active fires number was greater than zero. Merging scripts down the line add in zeros for missing rows.___ 

