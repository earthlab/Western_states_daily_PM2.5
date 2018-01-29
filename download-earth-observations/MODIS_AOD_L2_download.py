import ftplib
import sys, os

#Open ftp connection
ftp = ftplib.FTP('ladsweb.nascom.nasa.gov')

ftp.login()
directory = 'allData/6/MOD04_L2/'
ftp.cwd(directory)

for year in range(2008, 2015):
    year_directory = str(year)
    print(year_directory)
    ftp.cwd(year_directory)
    date_directories = ftp.nlst()
    print(date_directories)
    for date_directory in date_directories:
        print(date_directory)
        ftp.cwd(date_directory)
        aod_files = ftp.nlst()
        print(aod_files)

        for aod_file in aod_files:
            if os.name == 'nt':
                local_path = sys.argv[1] + "\\" + year_directory + "\\" + date_directory
            if os.name == 'posix':
                local_path = sys.argv[1] + "\\" + year_directory + "\\" + date_directory

            if not os.path.exists(local_path):
                os.makedirs(local_path)
                os.chdir(local_path)

            local_filename = os.path.join(local_path, aod_file)
            file = open(local_filename, 'wb')
            ftp.retrbinary('RETR ' + aod_file, file.write)

            file.close()

        ftp.cwd('..')

    ftp.cwd('..')

ftp.quit()


