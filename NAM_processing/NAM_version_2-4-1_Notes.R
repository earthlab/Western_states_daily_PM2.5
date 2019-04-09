#this_model.date <- format(theDate, format = "%Y%m%d") # get date in format YYYYmmdd - needed for rNOMADS functions
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/rNOMADS/rNOMADS_2.3.10.tar.gz" # https://support.rstudio.com/hc/en-us/articles/219949047-Installing-older-versions-of-packages
#install.packages(packageurl, repos=NULL, type="source")

#### Clear variables and sinks; define working directory ####
rm(list  =  ls())
options(warn  =  2) # throw an error when there's a warning and stop the code from running further
if (max(dev.cur())>1) { # make sure it isn't outputting to any figure files
  dev.off(which  =  dev.cur())
} # if (max(dev.cur())>1) {
while (sink.number()>0) {
  sink()
} # while (sink.number()>0) {

library(rNOMADS)

archived.model.list <- NOMADSArchiveList("grib")
print(archived.model.list)

this_model.date <- "20181213"
Model_in_use_abbrev <-  "namanl" # NAM Analysis

list.available.models <- CheckNOMADSArchive(Model_in_use_abbrev, this_model.date) # list all model files available for this model and date
print(list.available.models)



# page 3
