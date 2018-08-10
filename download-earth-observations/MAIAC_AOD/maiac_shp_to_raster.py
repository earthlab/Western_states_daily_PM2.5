#Author: Ellen Considine
#Date: 8/10/18
#Purpose: convert an average-value shapefile (for each day) to a raster

import subprocess
import os

def rasterize(origpath, outpath, item): #item will be a projected shapefile (in 102003)
    basename = os.path.basename(item)
    print(basename)
    #write raster
    outfile = outpath + basename[:-4] + ".tif"
    subprocess.call(['gdal_grid', '-zfield', 'aod', '-a', 'linear', '-txe', '-2380056.81286844', '-480056.81286844425', '-tye', '-638166.9912686478', '1581833.0087313522', '-outsize', '555', '475', '-of', 'GTiff', '-ot', 'Float64', item, outfile])
    
