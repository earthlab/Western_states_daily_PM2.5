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

#install.packages("rNOMADS")
library(rNOMADS)
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/rNOMADS/rNOMADS_2.3.10.tar.gz" # https://support.rstudio.com/hc/en-us/articles/219949047-Installing-older-versions-of-packages
#install.packages(packageurl, repos=NULL, type="source")
source(file.path("estimate-pm25","General_Project_Functions","general_project_functions.R"))
source(file.path(define_file_paths.fn("NAM_Code.directory"),"rNOMADS_MMM_edited.R"))

archived.model.list <- NOMADSArchiveList("grib")
print(archived.model.list)

#this_model.date <- "20181213"
this_model.date <- "20040805"
model.date <- "20040805"
Model_in_use_abbrev <-  "namanl" # NAM Analysis
abbrev <- "namanl"

#list.available.models <- CheckNOMADSArchive(Model_in_use_abbrev, this_model.date) # list all model files available for this model and date
list.available.models <- CheckNOMADSArchive_MMM(Model_in_use_abbrev, this_model.date) # list all model files available for this model and date

#CheckNOMADSArchive_MMM
print(list.available.models)


#abbrev <- "namanl"
#model.url <- NOMADSArchiveList("grib", abbrev=abbrev)$url
#model.list <- c()
#model.date_in <- "20040408"
#model.date <- model.date_in
#model.date <- as.numeric(strsplit(as.character(model.date_in), split = "")[[1]])
#check.url <- paste0(model.url, paste(model.date[1:6], collapse = ""), "/", paste(model.date, collapse = ""), "/")
#print(check.url)
#model.list <- append(
#  model.list, 
# grep("grb\\d?$", 
#       LinkExtractor(check.url),
#       value = TRUE)) 

#model.list <- append(
#  model.list, 
#  grep(".grb", 
#       LinkExtractor(check.url),
#       value = TRUE)) 

#model.list <- as.vector(model.list)

#available.models <- list(
#  date = stringr::str_extract(model.list, "\\d{8}"),
#  model.run = stringr::str_replace_all(stringr::str_extract(model.list, "_\\d{4}_"), "_", ""),
#  pred = stringr::str_replace(stringr::str_replace(stringr::str_extract(model.list, "_\\d{3}\\."), "\\.", ""), "_", ""),
#  file.name = model.list)

# page 3
