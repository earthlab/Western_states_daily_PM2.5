import rasterio

def get_NDVI_value_at_point(filename, station_coords):
    with rasterio.open(filename) as src:
        vals = src.sample([station_coords])
        for val in vals:
           return val[0]

if __name__ == '__main__':
    filename = 'C:\\Users\\ginal\\Documents\\EarthLab\\NDVI\\MOD13A3.A2008001.mosaic.tif.tif'
    print(get_NDVI_value_at_point(filename, (-109.539683, 31.3492)))