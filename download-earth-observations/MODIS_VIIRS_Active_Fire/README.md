# Active Fire Download
First download the MODIS and/or VIIRS active fire product(s) from [https://firms.modaps.eosdis.nasa.gov/download/](Archive Download Tool) and select shp file.

## active_fire.py
This script will spatially join the fire data with the timezone data. This is a necessary step for adjusting the timestamp from UTC to local in the next step.

## buffers.py
This script will take in a csv file with lat, lon, and dates (the PM2.5 stations/points of interest) and also take in a shp file with buffers around each of these observation (specified as arguments). The output is a csv file similar to the input csv, but with an additional columns that includes the number of active fires in each buffer.

Note: if you don't want to create an intermediate csv file with a list of dates for each spatial coordinate, use buffers2.py
