import pandas as pd
import datetime, glob, os

processed_data = 'H:\MODIS_AOD\processed_data\csv_files\\'
output_data = 'H:\MODIS_AOD\processed_data\csv_files2\\'

for file in sorted(glob.glob(processed_data + "\\*.csv")):
    stamp = os.path.basename(file)[:-4]
    with open(file, 'r') as csv_file:
        print(stamp)

        # Get the whole UTC date/time information string from the file name
        date_time_label = os.path.basename(file).split(".")
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

        year = str(file_dt.year)
        month = str(file_dt.month)
        day = str(file_dt.day)
        hour_min = hour_str + ":" + minute_str

        csv_input = pd.read_csv(csv_file)
        csv_input['year'] = year
        csv_input['month'] = month
        csv_input['day'] = day
        csv_input['hour_min'] = hour_min

        csv_input.to_csv(output_data + stamp + ".csv", index=False)