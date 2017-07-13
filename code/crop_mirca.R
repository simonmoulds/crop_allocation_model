args = commandArgs(trailingOnly = TRUE)

input = args[1]
output = args[2]

xmin = args[3]
xmax = args[4]
ymin = args[5]
ymax = args[6]

fmt = args[7]

ext = raster::extent(67.4, 98, 6, 36)

r = raster::raster(input)
raster::projection(r) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
r = raster::crop(r, ext)
raster::writeRaster(r, output, format=fmt, overwrite=TRUE)
