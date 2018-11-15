# NED download
The scripts in this directory have all been used one way or another as tools to download and/or process the NED dataset. However, after much trial and error, the only script that we ended up using for the NED processing was the extract_values_to_points.py script

## extract_values_to_points.py
Given a csv file with the monitoring locations (lat, lon), this script will extract the NED value at the correct location. It does this by first finding the correct tile, and then extracting the elevation values at that lat/lon.

## NED_ulmo.py
Run NED_ulmo.py with a bounding box and an output path. Bounding box should follow format [min lon, min lat, max lon, lax lat] like it currently is in the script. Pass the output file path in as an argument (see --help for arg help)

## mosaic_ned_tiles.py
Run mosaic_ned_tiles.py and pass in a vrt filename, directory, and tif filename. The vrt file gets built as an intermediary step, so the filename in which you give the vrt file does not really matter. The directory is the directory where your downloaded NED tiles are located. The tif filename is your final output seamless tif (see --help for arg help).