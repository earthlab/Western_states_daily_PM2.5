# This is the main file which calls all the GASP AOD processing steps as functions on EC2
# Author: Ellen
# Date: 7/3/18

import glob, os
import multiprocessing
import boto.s3.connection
import argparse
from boto.s3.key import Key

from step0 import zero
from step1 import one
from step2 import two
from step3 import three
from step4a import four

def _setup():
	parser = argparse.ArgumentParser(description='Pass in AWS credentials.')
	parser.add_argument('--access_key',
                    help='aws access key')
	parser.add_argument('--secret_key', type=str,
                    help='secret access key')
	parser.add_argument('--s3_bucket', type=str, help='s3 bucket name')
	parser.add_argument('--data_directory', type=str, help='directory path for processed output')
	args = parser.parse_args()
	print(args.access_key, args.secret_key, args.s3_bucket, args.data_directory)

def upload_to_S3(s3_folder_path, file):
    bucket = conn.get_bucket(args.s3_bucket)
    k = Key(bucket)
    k.key = s3_folder_path + os.path.basename(file)
    k.set_contents_from_filename(file)  # rewind = True if from file

def main():
    # Connect to AWS
    conn = boto.connect_s3(
   	aws_access_key_id = args.access_key,
	aws_secret_access_key = args.secret_key
    )	
    s3_bucket_name = args.s3_bucket	

    # SET YEAR
    year = 2009

    # #Step 0: OR use gunzip on all files in the folder
    # origpath = 'C:\\Users\\elco2649\\Documents\\GASP_DATA\\' #'/home/jovyan/GASP-AOD/'
    # outpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_0\\' #('/home/jovyan/GASP_processed/%d/step0/'  % (year) )
    
    # pool = multiprocessing.Pool()
    # for item in os.listdir(origpath):
    #    pool.apply_async(zero, [origpath, outpath, item])
    # pool.close()
    # pool.join()
    #
    # # for file in sorted(glob.glob(origpath + 'GOESW*')):
    # #     os.remove(file)
    #
    #

    #Step 1
    data_path = args.data_directory + '/%d/step0/'  % (year)
    out_path = args.data_directory + '/%d/step1/'  % (year)
    s3_folder_path = '/%d/step0/'  % (year)
    
    pool = multiprocessing.Pool()
    for item in os.listdir(origpath):
        pool.apply_async(one, [origpath, outpath, item])
    pool.close()
    pool.join()
    
    for file in sorted(glob.glob(origpath + 'GOESW*')):
        upload_to_AWS(subdir, file)
        os.remove(file)

    # # Step 2
    # origpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_1\\' # ('/home/jovyan/GASP_processed/%d/step1/' % (year))
    # outpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_2\\' #('/home/jovyan/GASP_processed/%d/step2/' % (year))
    # subdir = ("GASP_processed/%d/step1/" % (year))
    #
    # pool = multiprocessing.Pool()
    # for item in sorted(glob.glob(origpath + 'GASP*')):
    #     pool.apply_async(two, [origpath, outpath, item])
    # pool.close()
    # pool.join()
    #
    # # for file in sorted(glob.glob(origpath + '*.txt')):
    # #     upload_to_AWS(subdir, file)
    # #     os.remove(file)

    # Step 3
    origpath =  ('/home/jovyan/GASP_processed/%d/step2/' % (year)) # 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_2\\'
    outpath =  ('/home/jovyan/GASP_processed/%d/step3/' % (year)) # 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_3\\'
    subdir = ("GASP_processed/%d/step2/" % (year))

    days = [] #unique julian days
    for item in os.listdir(origpath):
        this_day = item[9:12]
        print(this_day)
        if(this_day in days):
            pass
        else:
            days.append(this_day)
    pool = multiprocessing.Pool()
    for day in days:
        pool.apply_async(three, [origpath, outpath, day, year])
    pool.close()
    pool.join()

    for file in sorted(glob.glob(origpath + '*.txt')):
        #upload_to_AWS(subdir, file)
        os.remove(file)

    # Step4a
    origpath = ('/home/jovyan/GASP_processed/%d/step3/' % (year)) #'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_3\\'
    outpath =  ('/home/jovyan/GASP_processed/%d/step4a/' % (year)) # 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_4a\\'

    epsg = 'GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295],AUTHORITY["EPSG", 4269]]'

    # pool = multiprocessing.Pool()
    subdir = ("GASP_processed/%d/step4a/avg/" % (year))
    for item in os.listdir(origpath):
        # pool.apply_async(four, [origpath, outpath, epsg, item])

        outfile = four(origpath, outpath, epsg, item)
        DBFfile = "%s.dbf" % outfile[:-4]
        upload_to_AWS(subdir, DBFfile)
        os.remove(DBFfile)
    # pool.close()
    # pool.join()

    # Upload step3 files to s3:
    subdir = ("GASP_processed/%d/step3/avg/" % (year))

    for file in sorted(glob.glob(origpath + '*.txt')):
        upload_to_AWS(subdir, file)
        os.remove(file)

    # Upload the rest of the step4a files to s3:
    origpath = ('/home/jovyan/GASP_processed/%d/step4a/' % (year))
    subdir = ("GASP_processed/%d/step4a/avg/" % (year))
    for file in sorted(glob.glob(origpath + '*')):
        upload_to_AWS(subdir, file)
        os.remove(file)
'''

if __name__ == "__main__":
    	_setup()
	#main()
