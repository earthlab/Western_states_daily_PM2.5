# NED download
The scripts in this directory are used to download the NED image tiles for a given bounding box and for mosaicing those downloaded tiles together to form a seamless raster.

## NED_ulmo.py
Run NED_ulmo.py with a bounding box and an output path. Bounding box should follow format [min lon, min lat, max lon, lax lat] like it currently is in the script. Pass the output file path in as an argument (see --help for arg help)

## mosaic_ned_tiles.py
Run mosaic_ned_tiles.py and pass in a vrt filename, directory, and tif filename. The vrt file gets built as an intermediary step, so the filename in which you give the vrt file does not really matter. The directory is the directory where your downloaded NED tiles are located. The tif filename is your final output seamless tif (see --help for arg help).