Steps
* Run estimate-pm25 docker
* Download nlcd_reclass2.tif file
* Download locations file
* Create 1km, 5km, 10km buffers around each location of interest
* Run nlcd_process.py

Notes
* Use special Albers projection to create the buffers (can import the projection from existing file on S3 if doing this with ArcGIS or similar software)
