import psycopg2

# THIS SCRIPT IS NOT DONE YET

from datetime import timedelta, date

def daterange(start_date, end_date):
    for n in range(int ((end_date - start_date).days)):
        yield start_date + timedelta(n)

# Generates one 24hr average grid per day from 2008 to 2014
def generate_24hr_avg_grids(cur):
    start_date = date(2008, 1, 1)
    end_date = date(2008, 1, 2)
    count = 0;
    for single_date in daterange(start_date, end_date):
        single_date_str = single_date.strftime("%Y-%m-%d")
        count+=1

        # Create grid for single date
        # Requires having set up ST_CreateFishnet

        query = """create table public.grid_""" + single_date_str + """ as (SELECT * FROM ST_CreateFishnet(199, 264, 0.1, 0.1, -127.21, 30.23) AS cells)"""


        cur.execute(query)
        #rows = cur.fetchall()
        #for row in rows:
        #    print(row)


    print("num days: " + str(count))




if __name__ == "__main__":
    # Connect to DB
    try:
        conn = psycopg2.connect("dbname='RA2018' user='postgres' password='postgres' host='localhost'")
    except:
        print("I am unable to connect to the database.")

    cur = conn.cursor()

    generate_24hr_avg_grids(cur)






