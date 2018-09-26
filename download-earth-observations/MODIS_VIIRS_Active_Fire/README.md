# Active Fire Download
First download the MODIS and/or VIIRS active fire product(s) from [https://firms.modaps.eosdis.nasa.gov/download/](Archive Download Tool) and select shp file.

## active_fire.py
This script will spatially join the fire data with the timezone data. This is a necessary step for adjusting the timestamp from UTC to local in the next step.

## buffers.py TODO
This script will take in a csv file with lat, lon, and dates (the PM2.5 observations) and create buffers around each observation (specified as arguments). The output is a csv file with the original PM2.5 observations but with a additional columns that includes the number of active fires in each buffer.