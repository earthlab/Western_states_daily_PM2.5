#!/bin/bash

docker pull earthlab/r-spatial-aws
docker run -d -p 8787:8787 earthlab/r-spatial-aws
name=$(docker ps --format "{{.Names}}")
echo $name
docker exec -u rstudio $name aws configure
 
 
