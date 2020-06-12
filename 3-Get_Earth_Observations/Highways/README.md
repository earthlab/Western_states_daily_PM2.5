We downloaded the National Highway Planning Network (NHPN) shapefile from https://www.fhwa.dot.gov/planning/processes/tools/nhpn/

R_highways.R prepares the highways data from start to finish. 

__Processing Steps:__
* select features from the NHPN shapefile which are classified as either Arterial or Collector roads AND fall into one of the states in our study area
* Project air quality monitor locations and road features to a common projection
* Create 100, 250, 500, and 1000m buffers around each monitor location
* Sum the length of each type of road within each buffer
* Report the lengths of each type separately, and together in various CSVs
