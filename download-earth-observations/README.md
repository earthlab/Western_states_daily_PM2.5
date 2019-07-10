# download-earth-observations

#### Note: code is currently specific to processing data for the ML prediction locations. For code specific to processing data in ML training batches, go back in History to July 7th.

### Also see [Ellen's cheat sheet for downloading and processing various data sets](https://docs.google.com/document/d/1kuMDFJ_wKH3dPIujdieXl7l23f-i6hQHy5dVa0ObtIU/edit?usp=sharing)

## Download scripts to facilitate in downloading and processing various data sets needed for this project.

This folder contains the following scripts:
* **MODIS_FTP_download.py**: This script takes in an available data set on the NASA FTP download site and downloads all data from 2008-2014.

    UPDATE: Recieved this email from NASA LAADS, making script obsolete because FTP site no longer exists. Script will need
    to be refactored to use HTTPS retrievals instead.
    
    "Due to new NASA Policies LAADS FTP servers (both public and private) will be shutting down around Monday, April 20th 2018. 
    Please convert all scripts and other methods of access to using HTTPS. Instructions for accessing data via HTTPS can 
    be found at https://ladsweb.modaps.eosdis.nasa.gov/tools-and-services/data-download-scripts"

* **MODIS_FTP_download.py**:
This versatile script downloads data sets from NASA LAADS HTTPS server: https://ladsweb.modaps.eosdis.nasa.gov/archive/allData.
Options for which dataset, collection number, and date range can be specified in the arguments for this script.

* **modis_aod_create_csv_file.py**: This script converts the raw MODIS AOD .hdf files to corresponding .csv files

* **modis_aod_convert_csv_to_shapefile.py**: This script converts each .csv file to a corresponding .shp file

* **modis_aod_create_daily_averages.py**: This script merges .shp files that occur on the same day into a single .shp file

* **csv_to_avg_shp.py**: This script cycles through .csv files from the same day and creates one .shp file with average values at each location for that day

* **shp_to_raster.py**: This script projects a .shp file (in our case, into ESRI 102003) and then interpolates this information to a raster (.tif) file

* **Compare_interpolations.R**: This script can be used to compare the accuracy of different interpolation types (bilinear, nearest neighbor, inverse distance, etc.)

* **extract_points_in_R.R**: This script extracts the values in a raster at each monitor location


