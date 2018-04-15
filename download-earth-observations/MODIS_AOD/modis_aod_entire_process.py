import psycopg2

def main():
    try:
        conn = psycopg2.connect("host='localhost' dbname='RA2018' user='postgres' password='postgres'")
    except:
        print("I am unable to connect to the database")

    cur = conn.cursor()

    cur.execute("SELECT * FROM public.combined2_geom LIMIT 5")

    rows = cur.fetchall()

    for row in rows:
        # row[8] is the point_geom
        # find out which tz the point intersects with
        print(row)

if __name__ == "__main__":
    main()