#Script1_Install_Pkgs
# https://cran.r-project.org/bin/windows/base/
cat("INSTALL PACKAGES \n")
install.packages(pkgs="maps")
#install.packages(pkgs="mapproj")
install.packages('ggplot2')
install.packages(pkgs="ggmap")
install.packages(pkgs="rgdal")
install.packages(pkgs="rgeos")
install.packages(pkgs="maptools")
install.packages(pkgs="dplyr")
install.packages(pkgs="tidyr")
install.packages(pkgs="tweenr")
install.packages(pkgs = "anim.plots")
install.packages(pkgs = "rNOMADS")
install.packages(pkgs = "measurements") # https://stackoverflow.com/questions/14404596/converting-geo-coordinates-from-degree-to-decimal
install.packages(pkgs = "spatialEco")
install.packages(pkgs = "classInt")
install.packages(pkgs = "GiNA")
install.packages(pkgs = "RColorBrewer")
install.packages(pkgs = "dismo") # used for reprojections
install.packages(pkgs = "stringr") # used for reprojections


install.packages( pkgs= "gbm") # looks like it worked and installed gbm_2.1.3.zip
# install caret
# original: install.packages("C:/R_packages/caret_5.15-023.tar.gz", repos = NULL, type="source")
install.packages( pkgs= "caret") # looks like it worked and installed caret_6.0-77.zip and dependency files
install.packages(pkgs = "caretEnsemble")

# installing packages for which the library command didn't initially work:
#install.packages(pkgs= "randomForest")
install.packages(pkgs= "polspline")
# http://www2.uaem.mx/r-mirror/web/packages/ResearchMethods/ResearchMethods.pdf
# http://www2.uaem.mx/r-mirror/web/packages/ResearchMethods/
# ResearchMethods 'http://www2.uaem.mx/r-mirror/src/contrib/ResearchMethods_1.4.tar.gz'
install.packages(pkgs="irr")
install.packages(pkgs="gplots")
install.packages(pkgs="ellipse")
install.packages(pkgs= "http://www2.uaem.mx/r-mirror/src/contrib/ResearchMethods_1.4.tar.gz", repos = NULL, type="source")
install.packages(pkgs="reshape")
install.packages(pkgs="earth")
install.packages(pkgs="foreach")
install.packages(pkgs="e1071") # needed for treebagFuncs
install.packages(pkgs="glmnet")
install.packages(pkgs="elasticnet") 

install.packages(pkgs = "geosphere")

install.packages(pkgs="lubridate") # for handling dates/times

install.packages(pkgs = "audio")
install.packages(pkgs = "sf") # needed for lutz package
#install.packages(pkgs = "V8") # didn't work
#install.packages(pkgs = "lutz") # didn't work

#install.packages(pkgs = "sqldf")

#install.packages(pkgs = "daff")# didn't work

#install.packages("https://www.rdocumentation.org/packages/GEOmetadb") # not for latest version of R # https://www.rdocumentation.org/packages/GEOmetadb/versions/1.32.2
#install.packages(pkgs = "proj4")
#install.packages(pkgs = "gganimate")
#install.packages(pkgs="tmap")
#install.packages('leaflet')

# 