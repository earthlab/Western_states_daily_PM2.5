FROM jupyter/scipy-notebook

MAINTAINER Gina Li <gina.li@colorado.edu>

COPY environment.yml environment.yml

RUN conda env update -n root -f environment.yml

RUN python -c "import rasterio"

RUN python -c "from osgeo import gdal"

RUN python -c "import geopandas"

RUN conda info --envs

RUN rm environment.yml
