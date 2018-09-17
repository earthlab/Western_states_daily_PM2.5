import ulmo
logf = open("ned_download_errors.log", "w")

try:
    ulmo.usgs.ned.get_raster('1/3 arc-second', bbox=[-101.236910, 34.110220, -99.199683, 35.847546], path="C:\\Users\\ginal\\Documents\\EarthLab\\NED_ulmo", mosaic=True)
except Exception as e:
    logf.write(str(e) + "\n")