# This script takes in a csv with NED download URLs and downloads them one by one to a desired location locally.

'''
Prerequisites:
1) Have a .csv file that lists the download URLs for each NED file that needs to be downloaded for the study area
(i.e. `NED_bulk_download_file_list.csv` located in this folder is the one we use for this project)

Objective:
This script goes through each row of the .csv file and downloads the necessary zipped NED file (.zip) to desired path,
then unzips the NED file.

Results: Original .zip file as well as all NED metadata that is unzipped

'''

import pandas as pd
import sys, os, re
import urllib.request
from zipfile import ZipFile

__credits__ = "Gina Li, Colleen Reid, Melissa Maestas, Ellen Considine"
__email__ = "gina.li@colorado.edu"


csv_file = sys.argv[1]
save_directory = sys.argv[2]

df = pd.read_csv(csv_file)
url_column = df.URL

# Change to directory that you want to save files in
os.chdir(save_directory)
print(os.getcwd())

# Access URLs, which will download the .zip files
for url in url_column:
    print(url)
    #print(url[-2:])
    pattern = re.compile(r'n[0-9]{2}w[0-9]{3}')
    for match in re.findall(pattern, url):
        print(match)
        zipresp = urllib.request.urlopen(url)
        tempzip = open(save_directory + "/tempfile.zip", "wb")
        tempzip.write(zipresp.read())
        tempzip.close()
        zf = ZipFile(save_directory + "/tempfile.zip")
        zf.extractall(path = save_directory + "/" + match + "/")
        zf.close()
        #urllib.urlretrieve(url, "H:/NED/" + match + ".zip")