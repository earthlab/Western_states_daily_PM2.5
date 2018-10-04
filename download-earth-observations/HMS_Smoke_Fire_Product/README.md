# HMS Smoke Fire Product

## Run download script
`hms_smoke_fire_product_download.py `. Recommend piping output to a log file

## Extract .gz files
`gunzip *.gz`

## Copy over to S3 bucket
`aws s3 cp <src> s3://<dst>`

## Rearrange folder structure of S3 bucket
Copy over fire files into a subdirectory called "Fire" and smoke files into a subdirectory called "Smoke" using AWS CLI.

## Add a date column for each shp file, and adjsut UTC to local time zone
`process_date.py`

## Combine all daily shp files into one
Use GDAL ogrmerge to merge all shp files into one.


