# Western states daily PM2.5
Earth Lab's health team project to estimate air pollution exposures across the western U.S. for an 11-year period (2008-2018). 

To pull and run docker container with Python, execute:

`docker pull earthlab/estimate-pm25`
`docker run -d earthlab/estimate-pm25`
`docker ps` (to get the container name)
`docker exec -it <container name> /bin/bash`

To pull and run docker container with R, execute:

`docker pull earthlab/r-reidgroup`
`docker run -e PASSWORD=yourpassword -d -p 8787:8787 earthlab/r-reidgroup`

This repository contains the following files and directories:

* **PM25-estimation:** R code to execute the machine learning algorithms that will be used to estimate PM2.5 exposures
* **process-earth-observations:** Python code to programatically download and process earth observation data sets necessary for our analysis
