from urllib import request
from urllib.error import HTTPError, URLError
import argparse
import csv

def _setup():
            parser = argparse.ArgumentParser(description='arguments for NED download script')
            parser.add_argument('--directory', type=str, required=True,
                                help='directory to save downloaded resources')
            parser.add_argument('--tiles_file', type=str, required=True, 
                                help='new line character-delimited txt file with urls to tiles')
            args = parser.parse_args()
            return args

def dlfile(url):
        # Access URL
        try:
            f = request.urlopen(url)
            filename = url.split("/")[-1]
            print("downloading " + url)

            # Save file
            with open(args.directory + filename, "wb+") as local_file:
                local_file.write(f.read())
        #handle errors
        except HTTPError as e:
            print("HTTP Error:", e.code, url)
        except URLError as e:
            print("URL Error:", e.reason, url)


if __name__ == '__main__':
    args = _setup()
    with open(args.tiles_file, 'r') as csvfile:
        csv_reader = csv.reader(csvfile, delimiter='\n')
        for row in csv_reader:
            dlfile(row[0])