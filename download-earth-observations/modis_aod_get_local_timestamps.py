import geopy as gp
import pytz
import datetime
import sys
import glob
from os import path
import csv

# Pass in the path to the csv files as first argument

csv_files = sys.argv[1]

for csv_file in sorted(glob.glob(csv_files + "\\*.csv")):

    # Get the whole UTC date/time information string from the file name
    date_time_label = path.basename(csv_file).split(".")
    print(date_time_label)

    # Extract the UTC date portion
    date_label = date_time_label[1][1:]
    # Extract the UTC time portion
    time_label = date_time_label[2]

    # Extract the UTC year
    year_str = date_label[:4]
    # Extract the UTC Julian day
    julian_day_str = date_label[4:]
    # Extract the UTC month and day from Julian day

    file_dt = datetime.datetime.strptime(year_str + julian_day_str, '%Y%j').date()

    # Extract the UTC hour
    hour_str = time_label[:2]
    # Extract the UTC minute
    minute_str = time_label[2:]

    g = gp.geocoders.GoogleV3()

    # datetime in UTC
    utc_dt = datetime.datetime(file_dt.year, file_dt.month, file_dt.day, int(hour_str), int(minute_str))

    with open(csv_file, newline='') as csvfile:
        csv_reader = csv.reader(csvfile, delimiter = ",")
        next(csv_reader, None)
        for row in csv_reader:
            long = row[1]
            lat = row[2]

            print(long)
            print(lat)

            coordinate = gp.point.Point(lat, long)
            print(type(coordinate))
            # lat/long of the pixel/observation

            tz = g.timezone(coordinate)

            local_time = pytz.utc.localize(utc_dt, is_dst=None).astimezone(tz)
            print(local_time)
