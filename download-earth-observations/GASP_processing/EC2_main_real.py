# This is the main file which calls all the GASP AOD processing steps as functions on EC2
# Author: Ellen
# Date: 7/3/18

import glob, os
import multiprocessing
import boto.s3.connection
from boto.s3.key import Key

from step0 import zero
from step1 import one
from step2 import two
from step3 import three
from step4a import four

# Setting up AWS S3 Connection
access_key = 'AKIAJQKU7FLSOKSHUQ4A'
secret_key = '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'

conn = boto.connect_s3(
    aws_access_key_id=access_key,
    aws_secret_access_key=secret_key
)

s3Bucketname = "earthlab-reid-group"


def upload_to_AWS(subdir, file):
    bucket = conn.get_bucket(s3Bucketname)
    k = Key(bucket)
    k.key = subdir + os.path.basename(file)
    k.set_contents_from_filename(file)  # rewind = True if from file

def main():

    # SET YEAR
    year = 2008

    # #Step 0: OR use gunzip on all files in the folder
    # origpath = 'C:\\Users\\elco2649\\Documents\\GASP_DATA\\' #'/home/jovyan/GASP-AOD/'
    # outpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_0\\' #('/home/jovyan/GASP_processed/%d/step0/'  % (year) )
    #
    # pool = multiprocessing.Pool()
    # for item in os.listdir(origpath):
    #     pool.apply_async(zero, [origpath, outpath, item])
    # pool.close()
    # pool.join()
    #
    # # for file in sorted(glob.glob(origpath + 'GOESW*')):
    # #     os.remove(file)


#     #Step 1
#     origpath = 'C:\\Users\\elco2649\\Documents\\GASP_DATA\\' #('/home/jovyan/GASP_processed/%d/step0/'  % (year) )
#     outpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_1\\' #('/home/jovyan/GASP_processed/%d/step1/'  % (year) )
#     subdir = ("GASP_processed/%d/step0/"  % (year) )

#     pool = multiprocessing.Pool()
#     for item in os.listdir(origpath):
#         pool.apply_async(one, [origpath, outpath, item])
#     pool.close()
#     pool.join()

#     for file in sorted(glob.glob(origpath + 'GOESW*')):
#         #upload_to_AWS(subdir, file)
#         os.remove(file)

    # Step 2
    origpath = ('/home/jovyan/GASP_processed/%d/step1/' % (year))
    outpath = ('/home/jovyan/GASP_processed/%d/step2/' % (year))
    subdir = ("GASP_processed/%d/step1/" % (year))

    pool = multiprocessing.Pool()
    for item in sorted(glob.glob(origpath + 'GASP*')):
        pool.apply_async(two, [origpath, outpath, item])
    pool.close()
    pool.join()

    for file in sorted(glob.glob(origpath + '*.txt')):
        #upload_to_AWS(subdir, file)
        os.remove(file)

    # Step 3
    origpath = ('/home/jovyan/GASP_processed/%d/step2/' % (year))
    outpath = ('/home/jovyan/GASP_processed/%d/step3/' % (year))
    subdir = ("GASP_processed/%d/step2/" % (year))
    pool = multiprocessing.Pool()
    for item in os.listdir(origpath):
        pool.apply_async(three, [origpath, outpath, item])
    pool.close()
    pool.join()

    for file in sorted(glob.glob(origpath + '*.txt')):
        upload_to_AWS(subdir, file)
        os.remove(file)

    # Step4a
    origpath = ('/home/jovyan/GASP_processed/%d/step3/' % (year))
    outpath = ('/home/jovyan/GASP_processed/%d/step4a/' % (year))

    epsg = 'GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295],AUTHORITY["EPSG", 4269]]'

    # pool = multiprocessing.Pool()
    subdir = ("GASP_processed/%d/step4a/" % (year))
    for item in os.listdir(origpath):
        # pool.apply_async(four, [origpath, outpath, epsg, item])

        outfile = four(origpath, outpath, epsg, item)
        DBFfile = "%s.dbf" % outfile[:-4]
        upload_to_AWS(subdir, DBFfile)
        os.remove(DBFfile)
    # pool.close()
    # pool.join()

    # Upload step3 files to s3:
    subdir = ("GASP_processed/%d/step3/" % (year))

    for file in sorted(glob.glob(origpath + '*.txt')):
        upload_to_AWS(subdir, file)
        os.remove(file)

    # Upload the rest of the step4a files to s3:
    origpath = ('/home/jovyan/GASP_processed/%d/step4a/' % (year))
    subdir = ("GASP_processed/%d/step4a/" % (year))
    for file in sorted(glob.glob(origpath + '*')):
        upload_to_AWS(subdir, file)
        os.remove(file)

if __name__ == "__main__":
    main()