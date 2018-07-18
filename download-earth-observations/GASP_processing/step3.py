# -----------------------------------------------------------------
# Author: Ellen
# Date: 6/5/18
# Purpose: Collect text files of same day and average these values at each location
# -----------------------------------------------------------------

import glob, re, os
import shutil
from shutil import copyfile
import multiprocessing


def three(origpath, outpath, day, year):
    vals = dict()
    for file in sorted(glob.glob(origpath + ('*%s.T*' %(day)) )):
        infile = open(file, "r")
        reader = infile.readlines()[1:] #skip the header
        for line in reader:
            line = line.strip('\n')
            sep = re.split(',', line)
            key = sep[1] + ',' + sep[2] #Lon, Lat
            #print("Key = " + key)
            aod = float(sep[3])
            #print("AOD = " + aod)
            if(key in vals):
                vals[key].append(aod)
            else:
                vals[key] = [aod]
        infile.close()
    #print(vals)

    file_new = open(outpath + str(year) + "_" + str(day) +"_avg.txt", 'w')
    file_new.write("Point, Lon, Lat, AOD \n")
    i = 1
    for key, values in vals.items():
        avg_aod = sum(values)/float(len(values))
        #print(avg_aod)
        file_new.write(str(i) + "," + key + ", " + str(avg_aod) + "\n")
        i += 1
    file_new.close()





    # print(item)
    # filename1 = origpath + item
    #
    # year = item[5:9]
    # date_1 = item[9:12]  # For example, 092
    # time_1 = item[14:18]  # For example, 0030
    # #print("date: " + date_1)
    # #print("time: " + time_1)
    #
    # if int(time_1) < 700:
    #     time_2 = str(int(time_1) + 1700)
    #     date_2 = str(int(date_1) - 1)
    # else:
    #     time_2 = str(int(time_1) - 700)
    #     date_2 = date_1
    # print(time_2)
    # if int(time_2) < 1000:
    #     time_2 = "0" + str(int(time_2))
    #
    # filename2 = outpath + year + "_" + date_2 + "_" + time_2 + ".txt"
    # # print("new: " + filename2)
    # copyfile(filename1, filename2)
    # # os.system("copy " + filename1 + " " + filename2)
    # #shutil.move(origpath + item, "D:\\Western_US\\GASP\\step_2\\")
