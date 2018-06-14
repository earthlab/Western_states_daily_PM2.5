# -----------------------------------------------------------------
# Author: Ellen
# Date: 5/18/18
# Purpose: This is an adaptation of Phil's original GASP script
# (client_data\20141217_gasp_modis_scripts\GASP_scripts\gasp_
# process_step2.py). The script cycles through the renamed files
# in step_1 and grabs the lat, lon and aod file for each day/time
# match. The data is written to an output txt file where each
# line is id, lon, lat and aod (where aod values are not -9.99).
# I added the part about selecting only aod files and matching
# based on the 'slice'. The old script was running through all
# of the files in step_1 which meant it was re-creating the same
# text file 3 times (once for each file - lat, lon and aod).
# -----------------------------------------------------------------

import os, string, glob

origpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_1\\'
outpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_2\\'
for item in sorted(glob.glob(origpath + 'GASP*')):
    item = os.path.basename(item)
    print(item)
    line_num = 1

    file_lat = open('C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_1\\lat.txt', 'r')
    file_lon = open('C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_1\\lon.txt', 'r')

    file_aod = open(origpath + item, 'r')
    file_new = open(outpath + item, 'w')
    file_new.write("Point, Lon, Lat, AOD \n")

    line_lat = iter(file_lat)
    line_lon = iter(file_lon)
    #line_aod = iter(file_aod)

#Note: next method changed from python 2 to python 3 ( .next() ---> next(object) )
    # for lon, lat in zip(file_lon, file_lat):
    #     if next(file_aod) != "-9.99\n":
    #         new_line = str(line_num) + ", " + lon.rstrip("\n") + ", " + lat.rstrip("\n") + ", " + next(file_aod)
    #         file_new.write(new_line)
    #         #print(new_line)
    #         line_num += 1
    #     else:
    #         next(line_aod)
    for line in file_aod:
        lon = next(line_lon)
        lat = next(line_lat)
        #Don't include missing values:
        if (line.rstrip('\n') != "-9.99") & (lon.rstrip('\n') != "-200") & (lat.rstrip('\n') != "-200"):
            #Study area bounding box:
            if(float(lon.rstrip('\n')) >= -126) & (float(lon.rstrip('\n')) <= -101) & (float(lat.rstrip('\n')) >= 25) & (float(lat.rstrip('\n')) <= 50):
                new_line = str(line_num) + ", " + lon.rstrip('\n') + ", " + lat.rstrip('\n') + ", " + line.rstrip('\n')
                file_new.write(new_line + '\n')
                line_num += 1
        # else:
        #     next(line_lon)
        #     next(line_lat)
    file_aod.close()
    #file_new.write("END")
    file_new.close()

    file_lat.close()
    file_lon.close()
    print(line_num)

print('Calculations complete')


# for item in sorted(glob.glob(origpath + 'GOESW*')):
#     item = os.path.basename(item)
#     line_num = 1
#     print item
#
#     if item[5:] == "GOESW":
#         slice = item[:7]
#
#         file_lat = open(origpath + str(slice) + "_lat.dat", "r")
#         file_lon = open(origpath + str(slice) + "_lon.dat", "r")
#         file_aod = open(origpath + str(slice) + "_aod.dat", "r")
#         file_new = open(outpath + str(slice) + ".txt", "w")
#         file_new.write("Point\n")
#
#         line_lat = iter(file_lat)
#         line_lon = iter(file_lon)
#
#         for line in file_aod:
#             if line.rstrip("\n") != "-9.99":
#                 new_line = str(line_num) + ", " + line_lon.next().rstrip("\n") + ", " + line_lat.next().rstrip("\n") + ", " + line.rstrip("\n")
#                 file_new.write(new_line + "\n")
#                 line_num += 1
#             else:
#                 line_lon.next()
#                 line_lat.next()
#         file_lat.close()
#         file_lon.close()
#         file_aod.close()
#         #file_new.write("END")
#         file_new.close()
#
# print 'Calculations complete'
#
#




