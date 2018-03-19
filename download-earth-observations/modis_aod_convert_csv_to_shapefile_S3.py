from multiprocessing import Process
import time, os, math, glob, csv, shapefile, datetime
import boto
from boto.s3.key import Key


num_threads = 10
processed_data = 'H:\MODIS_AOD\processed_data\csv_files\\'
output_location = 'H:\MODIS_AOD\processed_data\output_temp\\'
num_files = num_files = len([f for f in os.listdir(processed_data)if os.path.isfile(os.path.join(processed_data, f))])
print(num_files)
chunk_size = math.ceil(num_files / num_threads)
print(chunk_size)
excess = (chunk_size * num_threads) - num_files
print(excess)

points = shapefile.Writer(shapefile.POINT)
points.autoBalance = 1

points.field("aod", "F", 10, 5)
points.field("lat", "F", 10, 5)
points.field("long", "F", 10, 5)
points.field("year", "C")
points.field("month", "C")
points.field("day", "C")
points.field("hour_min", "C")


def filecount(dir_name):
    return len([f for f in os.listdir(dir_name) if os.path.isfile(f)])

# function to generate .prj file information using spatialreference.org
def getWKT_PRJ(epsg_code):
    import urllib
    # access projection information
    wkt = urllib.urlopen("http://spatialreference.org/ref/epsg/{0}/prettywkt/".format(epsg_code))
    # remove spaces between charachters
    remove_spaces = wkt.read().replace(" ", "")
    # place all the text on one line
    output = remove_spaces.replace("\n", "")
    return output

def func(i):
        time.sleep(i)
        print(i)

        start = int((chunk_size * i) - chunk_size)
        end = int(chunk_size * i)

        print("start: " + str(start))
        print("end: " + str(end))

        for file in sorted(glob.glob(processed_data + "*.csv"))[start:end]:

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

                        csv_reader = csv.reader(csv_file, delimiter=',')
                        next(csv_reader, None)

                        for row in csv_reader:
                            if len(row) <= 3:
                                raise AttributeError("Number of columns incorrect")
                            long = row[1]
                            lat = row[2]
                            aod = row[3]

                            points.point(float(long), float(lat))
                            points.record(aod, lat, long, year, month, day, hour_min)


                        points.save(output_location + stamp)

                        # Give .prj filename without the acquisition date. For some reason, the output .shp and associated files
                        # do not include the aquisition date (truncates after the last ".". So, we will do the same here in order to
                        # have a correctly associated .prj file

                        stamp2 = os.path.basename(file)[:-18]
                        print("stamp2: " + str(stamp2))
                        prj = open(output_location + stamp2 + ".prj", "w")

                        epsg = getWKT_PRJ("4326")
                        prj.write(epsg)
                        prj.close()

                        keyId = "AKIAJI4EQVEOL3JMBWLA"
                        sKeyId = "j4z5C4p+OUhidT19W8ayTU0h08kXJI33hBLcss3M"

                        # Specify S3 bucket connection
                        bucketName = "earthlab-reid-group"
                        conn = boto.connect_s3(keyId, sKeyId)
                        bucket = conn.get_bucket(bucketName)

                        # Get the Key object of the bucket
                        k = Key(bucket)
                        # Crete a new key with id as the name of the file
                        ext_list = ['.shp', '.dbf', '.prj', '.shx']
                        for ext in ext_list:
                            k.key = "MODIS-AOD/Shapefile/" + stamp2 + ext

                            # Upload the file
                            print(output_location + stamp2 + ext)
                            # result contains the size of the file uploaded
                            result = k.set_contents_from_filename(output_location + stamp2 + ext)

                            # remove the .shp, .dbf, .prj, and .shx from local so not taking up space
                            os.remove(output_location + stamp2 + ext)


if __name__ == '__main__':
    for i in range(1, num_threads + 1):
            p = Process(target = func, args=(i,))
            p.start()
            p.join()
            print("Launched process " + str(i))

    print("Done")