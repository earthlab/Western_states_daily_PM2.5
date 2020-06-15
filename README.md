# Western states daily PM2.5
Earth Lab's environmental health team project to estimate daily air pollution exposures across the western U.S. (at the county, ZIP-code, and census tract levels) from 2008-2018. 

To ensure that our work is reproducible, all code is written in open-source languages. Some scripts are in R and others are in Python, both due to specific functionalities in these languages and the different coding backgrounds of our research team members.

To pull and run a docker container with Python, execute:

`docker pull earthlab/estimate-pm25`
`docker run -d earthlab/estimate-pm25`
`docker ps` (to get the container name)
`docker exec -it <container name> /bin/bash`

To pull and run a docker container with R, execute:

`docker pull earthlab/r-reidgroup`
`docker run -e PASSWORD=yourpassword -d -p 8787:8787 earthlab/r-reidgroup`

This repository contains the following files and directories:

* **General_Project_Functions:** Scripts to obtain the prediction set locations as well as tools that are generally useful during the data processing, such as making buffers around points and reprojecting point coordinates.
* **Get_PM25_Observations:** Scripts to process PM2.5 observations from across the western U.S. These observations are used to train our machine learning models.
* **Get_Earth_Observations:** Scripts to download and process observations from data sets that are used both as inputs for our machine learning models during training and as inputs for our models in the prediction stage. The file *Overall_steps* provides all necessary directions. Individual *README* files (in each folder) provide more details, if there are any.
* **Merge_Data:** Scripts to merge all the data together and derive some spatio-temporal variables.
* **Machine_Learning:** Scripts to run and evaluate our machine learning models. The folder **Final_scripts** contains all code used for our final analysis. The code in the **Exploring_models** folder was all preliminary testing.
* **Estimate_PM25:** Scripts to use our machine learning models to make final predictions and to explore the prediction data sets over time and space.

Please contact Ellen Considine (ellen.considine@colorado.edu) with any questions.
