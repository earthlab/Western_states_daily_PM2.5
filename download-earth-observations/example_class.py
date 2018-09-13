import argparse
import boto

class Test:
    def __init__(self):
        args = self._setup()
        self.start_year = args.start_year
        self.end_year = args.end_year
        self.access_key = args.access_key
        self.secret_key = args.secret_key
        # connection here
        self.conn = boto.connect_s3(
            aws_access_key = self.access_key,
            aws_secret_access_key = self.secret_key
        )
    
    def _setup(self):
        parser = argparse.ArgumentParser(description='Pass in arguments for MODIS snow cover processing')
        parser.add_argument('--start_year', type=int, required=True, help='starting year for data download (starts with Jan 1 of that year)')
        parser.add_argument('--end_year', type=int, required=True, help='ending year for data download (ends with Dec 31 of that year)')
        parser.add_argument('--secret_key', type=str, required=True)
        parser.add_argument('--access_key', type=str, required=True)
        args = parser.parse_args()
        return args
    
    def one(self):
        print(self.start_year)

    def two(self):
        print(self.end_year)
        
    def main(self):
        self.one()
        self.two()

if __name__ == "__main__":
    Test().main()
    