# estimate-pm25
Earth Lab's health team project to estimate air pollution exposures across the western U.S. for a 7-year period (2008-2014). 

To pull and run docker container, execute:

`docker pull earthlab/estimate-pm25`
`docker run -it earthlab/earth-analytics-python-env /bin/bash`

This repository contains the following files and directories:

* **PM25-estimation:** R code to execute the machine learning algorithms that will be used to estimate PM2.5 exposures
* **download-earth-observations:** Python code to programatically download earth observation data sets necessary for our analysis
