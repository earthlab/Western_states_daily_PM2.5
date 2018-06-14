# -----------------------------------------------------------------
# Author: Ellen
# Date: 5/17/18
# Purpose: This is an adaptation of Zev's script, which is an adaptation of Phil's original GASP script
# (client_data\20141217_gasp_modis_scripts\GASP_scripts\gasp_
# process_step1.py). The script cycles through the original
# files (downloaded from FTP) and creates a copy of each
# file (lat, lon and aod) with simplified name.
# -----------------------------------------------------------------

import os
import string
import pandas as pd
import struct


origpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\'
outpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_1\\'

#reading in binary files: http://vislab-ccom.unh.edu/~schwehr/rt/python-binary-files.html

#i = 1
for item in os.listdir(origpath):
    if os.path.isfile(origpath + item):
        print(item)
        #old_file = open(origpath + item, 'rb')
        #j = 0
        #lines = iter(old_file)

        type = item[-3:]
        if type == 'dat':
            with open(origpath + item, 'rb') as datafile:
                f_data = datafile.read()
                new_file = open(outpath + item[:3] + '.txt', 'w')
                # max = 0
                # min = 180
                for i in range(1,2280001): # array is size 2500 * 912
                    d = struct.unpack('f', f_data[((i-1)*4):(i*4)])
                    d = str(d)
                    d = d.replace("(", "")
                    d = d.replace(")", "")
                    d = d.replace(",", "")
                    d = d.replace(" ", "")
                    new_file.write(str(d) + '\n')
            new_file.close()
            # #         if d > max:
            # #             max = d
            # #         if d < min:
            # #             min = d
            # # print ("Min: " + str(min))
            # # print ("Max: " +  str(max))
            print(item + " done")

#Need to read in only the first 9120000 values (aod ones)

        elif item[:5] == 'GOESW': #aod files
            slice = item [-14:-1]
            # with open(origpath + item, 'rb') as datafile:
            #     f_data = datafile.read()
            #     new_file = open(outpath + "GASP_" + slice + '.txt', 'w')
            #     # max = 0
            #     # min = 180
            #     for i in range(1,2280000): # array is size 2500 * 912
            #         d = struct.unpack('i', f_data[((i-1)*4):(i*4)]) # shouldn't this be a long integer?
            #         # https://www.geeksforgeeks.org/struct-module-python/
            #         d = str(d)
            #         d = d.replace("(", "")
            #         d = d.replace(")", "")
            #         d = d.replace(",", "")
            #         d = d.replace(" ", "")
            #         new_file.write(str(d) + '\n')
            # new_file.close()

            with open(origpath + item, 'rb') as datafile:
                new_file = open(outpath + "GASP_" + slice + '.txt', 'w')
                data = datafile.read()
                #data = datafile.read(4)
                max = 0
                min = 300
                j = 0
                for d in data:
                    if j >= 2280000:
                        break
                    else:
                        j += 1
                        #number = int.from_bytes(d, "big") #this needs Python 3
                        #print(d)
                        d = d/100. - 0.5 #convert from 0-255 range as specified by Chuanyu
                        if d < 0:
                            d = -9.99 #Zev's convention
                        new_file.write(str(d) + '\n')
                        #data = datafile.read(4)
                        if d > max:
                                max = d
                        if d < min:
                            min = d
                print ("Min: " + str(min))
                print ("Max: " +  str(max))
                print(j)
                new_file.close()

            print(j)
            #i += 1

print('Finished reading and writing')

# for item in os.listdir(origpath):
#     if item[-3:] == 'dat':
#         print item
#         #slice = item[-26:-19]
#         type = item[0:3]
#         old_file = open(origpath + item, 'r')
#         lines = iter(old_file)
#         new_file = open(outpath + step + '\\' + '_' + type + '.dat', 'w')#+ slice
#
#         # Skip the first 2 lines (includes nrow, ncol)
#         if type == 'lat' or type == 'lon':
#             lines.next()
#             lines.next()
#
#         # Skip the first 6 lines (includes nrow, nodata value, etc)
#         elif item[5:] == 'GOESW':
#             lines.next()
#             lines.next()
#             lines.next()
#             lines.next()
#             lines.next()
#             lines.next()
#
#         for line in lines:
#             linelist = line.split(' ')
#             for value in linelist:
#                 if value != '' and value != '\n':
#                     value = value.rstrip('\n')
#                     value = value.lstrip('\n')
#                     value = value.rstrip()
#                     value = value.lstrip()
#                     new_file.write(value + '\n')
#                     print value
#                     print '\n'
#         old_file.close()
#         new_file.close()
#
# print 'Calculations complete'
#

