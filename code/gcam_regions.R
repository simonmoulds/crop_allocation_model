library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(maptools)

## read shapefile
v = readOGR("data", "gcam_32rgn_countries")
vd = as.data.frame(v)

## dissolve polygons
x = unionSpatialPolygons(v, vd$GCAM_ID)

## build data frame
id = as.numeric(row.names(x))
rgn_nm = vd$REGION_NAM[match(id, vd$GCAM_ID)]
xd = data.frame(REGION_NAM=rgn_nm, GCAM_ID=id)
row.names(xd) = row.names(x)

x = SpatialPolygonsDataFrame(x, xd)
writeOGR(x, dsn="data", layer="gcam_32rgn", driver="ESRI Shapefile", overwrite_layer=TRUE)
