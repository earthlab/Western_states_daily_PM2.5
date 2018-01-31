#!/bin/bash

docker pull earthlab/r-spatial-aws
docker run -d -p 8787:8787 earthlab/r-spatial-aws
name=$(docker ps --format "{{.Names}}")
docker exec -u rstudio $name aws s3 cp s3://earthlab-reid-group/ /home/rstudio/ --recursive
docker exec -it -u rstudio $name /bin/bash
#docker exec -u rstudio $name aws configure
 
 
