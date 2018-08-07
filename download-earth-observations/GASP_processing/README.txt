
The GASP data was acquired from an FTP link.  

Official documentation: ftp://satepsanone.nesdis.noaa.gov/GASP/.readerCode/West/ 
Note: the first of 9 variables read in the official IDL script read_GASP.pro is the aod, so we simply take the first 2,280,000 out of 20,520,000 values in each aod file. 
Original values are between 0 and 255. The conversion is to divide by 100, then subtract 0.5. We then marked all negative values as missing (-9.99). 

Our latitude and longitude files were obtained from chuanyu.xu@noaa.gov. These files also have 2500*912 = 2,280,000 values each. Missing values are coded as -200. 

Processing:

On EC2/docker: (docker run -it -p 8888:8888 earthlab/spatial-python)
Note: you must put a copy of lat_11.dat and lon_11.dat in the step0 folder (with the unzipped AOD files)
EC2_main_real.py (calls steps 0-4a)

Then use shp_to_raster.py to convert these average-value shapefiles to rasters. 


