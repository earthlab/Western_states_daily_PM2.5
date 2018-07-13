# -----------------------------------------------------------------
# Author: Ellen
# Date: 6/5/18
# Purpose: This is an adaptation of Phil's original GASP script
# (client_data\20141217_gasp_modis_scripts\GASP_scripts\gasp_
# process_step3.py). This script cycles through the files in
# step2 and grabs the date and time which are converted from
# UTC to PDT for the purposes of getting daily averages.
# -----------------------------------------------------------------

import glob
import os
import shutil
from shutil import copyfile
import multiprocessing


def three(origpath, outpath, item):
    print(item)
    filename1 = origpath + item

    year = item[5:9]
    date_1 = item[9:12]  # For example, 092
    time_1 = item[14:18]  # For example, 0030
    #print("date: " + date_1)
    #print("time: " + time_1)

    if int(time_1) < 700:
        time_2 = str(int(time_1) + 1700)
        date_2 = str(int(date_1) - 1)
    else:
        time_2 = str(int(time_1) - 700)
        date_2 = date_1
    print(time_2)
    if int(time_2) < 1000:
        time_2 = "0" + str(int(time_2))

    filename2 = outpath + year + "_" + date_2 + "_" + time_2 + ".txt"
    # print("new: " + filename2)
    copyfile(filename1, filename2)
    # os.system("copy " + filename1 + " " + filename2)
    #shutil.move(origpath + item, "D:\\Western_US\\GASP\\step_2\\")
