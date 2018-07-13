# This is the main file which calls all the GASP AOD processing steps as functions locally, so we can use the Arcpy package
# Author: Ellen
# Date: 7/3/18

import glob, os, datetime, re
import multiprocessing
from collections import OrderedDict
import arcpy as AP

from step4b import fourB
from step5 import five
from step6 import six
from step7 import seven

import boto.s3.connection
from boto.s3.key import Key

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


#Pulled step4a files down from AWS...

def main():
    #SET YEAR
    year = 2008

    #Step 4b
    origpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_4a\\'
    outpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_4b\\'

    pool = multiprocessing.Pool()
    fcs = sorted(glob.glob(origpath + '*.shp'))
    for index, fc in enumerate(fcs):
        pool.apply_async(fourB, [origpath, outpath, fc])
    pool.close()
    pool.join()


    #Step 5
    origpath = 'C:\Users\elco2649\Documents\GASP_AOD\step_4b\\'
    outpath = 'C:\Users\elco2649\Documents\GASP_AOD\step_5\\'
    subdir = ("GASP_processed/%d/step4b/"  % (year) )

    sa = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\gis\\western_102003.shp'

    start = datetime.datetime.now()
    # print start
    # print '--------------------------'


    desc = AP.Describe(sa)
    extent = desc.extent

    fcs = sorted(glob.glob(origpath + '*.shp'))

    daylist = []

    for fc in fcs:
        fcname = os.path.basename(fc)
        dayslice1 = str(fcname)
        # re.split(r"[\[\]]", "I need to [go out] to lunch")
        dayslice2 = re.split(r"[_]", dayslice1)
        dayslice3 = dayslice2[1]
        # print(dayslice3)
        daylist.append(dayslice3)

    dayFreq = {x: daylist.count(x) for x in daylist}

    x = OrderedDict(sorted(dayFreq.items(), key=lambda t: t[0], reverse=True))
    dayUnique, dayCount = x.keys(), x.values()

    pool = multiprocessing.Pool()  # processes = 4 is a simple test case

    for day, count in zip(dayUnique, dayCount):
        print 'day: ' + day + ', timeslices: ' + str(count)

        if count >= 10: #change this when done with testing
            FCs = sorted(glob.glob(origpath + '*_' + str(day) + '_*.shp'))
            for fc in FCs:
                pool.apply_async(five, [origpath, outpath, fc, day, start, subdir])
                # print(row)
                # spamwriter.writerow(row)

        else:
            print 'Skip, not enough timeslices' + '\n --------------------------'
            # spamwriter.writerow(['-99999', '-99999', -99999, -99999, day, -99999, count])

    pool.close()
    pool.join()

    for file in sorted(os.listdir(origpath)):
        upload_to_AWS(subdir, file)
        os.remove(file)


    #Step6

    origpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\step_5\\'
    outpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\\step_6\\'
    subdir = ("GASP_processed/%d/step5/" % (year))

    pool = multiprocessing.Pool()
    start = datetime.datetime.now()
    folderlist = sorted(os.listdir(origpath))
    for folder in folderlist:
        pool.apply_async(six, [folder, origpath, outpath, subdir, start])

    pool.close()
    pool.join()

    #manually upload step 6 files to AWS (via command line interface)

    # #Step 7
    # origpath = 'C:\\Users\\elco2649\\Documents\\GASP_AOD\step_6\\'
    # subdir = ("GASP_processed/%d/step6/" % (year))
    #
    # for file in sorted(os.listdir(origpath)):
    #     upload_to_AWS(subdir, file)

    #Step 8

if __name__ == "__main__":
    main()