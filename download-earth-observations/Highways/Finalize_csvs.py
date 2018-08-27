#---------------------------------------------------------------------------------
# Author:   Ellen
# Created:  5/31/2018
# Purpose:  NHPN processing Step 3
# Scale all road lengths correctly; generate final CSVs
#----------------------------------------------------------------------------------

import pandas as pd

buffers = [100, 250, 500, 1000]
for buffer in buffers:

    #Arterial roads
    df = pd.read_csv("Arterial Road Sum " + str(buffer) + ".csv")
    df.loc[:, 'Road Lengt'] *= 10000
    df.to_csv("Final_arterial_" + str(buffer) + ".csv")

    # Collector roads
    df = pd.read_csv("Collector Road Sum " + str(buffer) + ".csv")
    df.loc[:, 'Road Lengt'] *= 10000
    df.to_csv("Final_collector_" + str(buffer) + ".csv")
