
The GASP data was acquired from an FTP link provided by NCEI, after submitting an order at https://www.ncdc.noaa.gov/has/HAS.FileAppRouter?datasetname=3635/3636&subqueryby=STATION&applname=&outdest=FILE 

Official documentation: ftp://satepsanone.nesdis.noaa.gov/GASP/.readerCode/West/ 

For years 2008-2011, each binary data file contains nine 2500*912 arrays, the first of which is the aod, so we simply take the first 2,280,000 out of 20,520,000 values in each file. For years 2012-2014, each binary data file contains ten 2800*962 arrays, the first of which is the aod, so we take the first 2,693,600 our of 26,936,000 values in each file. For all years, the original values are between 0 and 255. The conversion is to divide by 100, then subtract 0.5. We then marked all negative values as missing (-9.99). 

Our latitude and longitude files were obtained from chuanyu.xu@noaa.gov. 
For 2008-2011, use: lat_11.dat and lon_11.dat
For 2012-2014, use: lat_goeswest.dat and lon_goeswest.dat
These files have the same dimensions as the aod arrays, and missing values are coded as -200. 

__Processing:__

(Merge lat.txt and lon.txt files from two time-sections)

Run create_latlon_tzid_lookup_table.py, and update the line in class_approach.py with the directory location of lat_lon_tzid_lookup.csv.

Add "point" to the beginning of the header in lat_lon_tzid_lookup.csv

On EC2/docker: (docker run -it -p 8888:8888 earthlab/spatial-python)

Run class_approach.py, to generate rasters of GASP AOD.

Use extract_points_in_R.R script (in download_earth_observations folder) to get AOD values at monitor locations.


