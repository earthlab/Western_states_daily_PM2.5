import ftplib
import sys, os, re

# argv[1] is the name of the data set i.e. MOD14A1
# argv[2] is the local file location to write out to

#Open ftp connection
ftp = ftplib.FTP('ladsweb.nascom.nasa.gov')

ftp.login()

directory = 'allData/6/' + sys.argv[1]
ftp.cwd(directory)

# Each year from 2008-2014
for year in range(2008, 2015):
    year_directory = str(year)
    print(year_directory)
    ftp.cwd(year_directory)
    date_directories = ftp.nlst()
    print(date_directories)
    # each day in given year
    for date_directory in date_directories:
        print(date_directory)
        ftp.cwd(date_directory)
        data_files = ftp.nlst()
        for data_file in data_files:
            # Only download the data files for our study area
            if re.search('(h08v04|h09v04|h10v04|h11v04|h07v05|h08v05|h09v05|h10v05)', data_file) != None:
                print("Now downloading: " + data_file)

                # If OS is windows
                if os.name == 'nt':
                    local_path = sys.argv[2] + "\\" + year_directory + "\\" + date_directory

                # If OS is linux
                if os.name == 'posix':
                    local_path = sys.argv[2] + "/" + year_directory + "/" + date_directory

                if not os.path.exists(local_path):
                    os.makedirs(local_path)
                    os.chdir(local_path)

                # Retrive data
                local_filename = os.path.join(local_path, data_file)
                print(local_filename)
                file = open(local_filename, 'wb')
                ftp.retrbinary('RETR ' + data_file, file.write)

                file.close()

        ftp.cwd('..')

    ftp.cwd('..')

ftp.quit()


