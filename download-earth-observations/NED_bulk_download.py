import pandas as pd
import sys, os, re
import urllib.request
from zipfile import ZipFile

#####################
# This script takes in a csv with NED download URLs and downloads them one by one to a desired location locally.
#####################

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