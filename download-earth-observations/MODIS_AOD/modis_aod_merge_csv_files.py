import pandas as pd
import glob, os, math, time, csv
from multiprocessing import Process

processed_data = 'H:\MODIS_AOD\processed_data\csv_files_utc_merge25\\'
output_data = 'H:\MODIS_AOD\processed_data\csv_files_utc_merge2\\'

num_processes = 2
num_files = num_files = len([f for f in os.listdir(processed_data)if os.path.isfile(os.path.join(processed_data, f))])
chunk_size = math.ceil(num_files / num_processes)
excess = (chunk_size * num_processes) - num_files

def merge_csv(i):
    time.sleep(i)
    prev_csv_input = None
    count = 0

    start = int((chunk_size * i) - chunk_size)
    end = int(chunk_size * i)

    print("start: " + str(start))
    print("end: " + str(end))


    for file in sorted(glob.glob(processed_data + "\\*.csv"))[start:end]:
        with open(file, 'r') as csv_file:
            count += 1
            csv_input = pd.read_csv(csv_file, usecols=["long", "lat", "aod", "year", "month", "day", "hour_min"])

            if count == 1:
                prev_csv_input = csv_input

            if count > 1:
                prev_csv_input = prev_csv_input.append(csv_input)


        print(str(count) + " files processed in process " + str(i))

    if prev_csv_input is not None:
        # add header=None for last iteration
        prev_csv_input.to_csv(output_data + "merged_csv_process_" + str(i) + ".csv", header=None, index=False, quoting=csv.QUOTE_NONNUMERIC)


if __name__ == '__main__':
    for i in range(1, num_processes + 1):
            print("Launched process " + str(i))
            p = Process(target = merge_csv, args=(i,))
            p.start()
            p.join()

    print("Done")