To get the MAIAC data, run download_from_https.py on EC2. (docker run -it -p 8888:8888 earthlab/spatial-python)

Because the MAIAC is a Level2G (gridded) product, the lat-lon coordinates are not included in each hdf file (these files are sinusoidally-projected). Read more about the MODLAND sinusoidal projection here: https://modis-land.gsfc.nasa.gov/MODLAND_grid.html 
Thus, we must download the relevant tiles from ftp://dataportal.nccs.nasa.gov/DataRelease/MODISTile_lat-lon/ before beginning processing. (Note: this download is not automated because we would need a NASA login to call from the ftp programatically.)

To process the MAIAC data, run maiac_main.py on EC2. (docker run -it -p 8888:8888 earthlab/spatial-python)
This script calls functions to turn the hdf files into csv files (with average values for each day) into projected shape files into rasters, and uploads everything to AWS S3. If creating shape files and rasters is too computationally intensive (as it was for us, even on the cloud), use the k_nearest_neighbors.py script instead of the pair maiac_avg_csv_to_shp.py and maiac_shp_to_raster.py. This extracts a weighted average at each monitor location based on the nearest 8 points listed in the average-value csv file for each day.


