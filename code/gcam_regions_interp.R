library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(maptools)
library(splancs)

## latlong
v = readOGR("data", "gcam_32rgn")
template = raster(file.path("data",
                            "iiasa-ifpri-cropland-map",
                            "iiasa_ifpri_cropland_map_5m_ll.tif"))
template = raster(template)
r = rasterize(v, template, field="GCAM_ID")
r[r == 0] = NA

## the following code is to ensure the land area implied by the
## region map exactly matches the land area of MapSPAM maps

## MapSPAM files
fs = list.files(file.path("data", "mapspam_data"), pattern="SPAM2005V3r1_global_A_TA_[A-Z]{4}_A.tif$", full.names=TRUE)

x = raster(fs[1])
x[] = 0
r = crop(r, x) ## crop gcam region map to MapSPAM extent

for (f in fs) {
    x = stackApply(stack(x, raster(f)), indices=c(1,1), fun=sum)
}

## the map resulting from the following commands show where either
## MapSPAM or GCAM region maps have values greater than zero
x[x > 0] = 1  ## set values greater than 0 in x to 1
x[r > 0] = 1  ## set values greater than 0 in r to 1
x[x < 1] = NA ## otherwise, set to NA 

nn = function(x, y, ...) {
    xx = raster(x)
    xx[is.na(x) & !is.na(y)] = 1
    pp = rasterToPoints(xx)[,1:2]

    p = rasterToPoints(x)[,1:2]
    nn = n2dist(p,pp)

    ix = nn$neighs
    vals = rasterToPoints(x)[ix,3]

    pp = as.data.frame(pp)
    coordinates(pp) = ~x+y
    cells = cellFromXY(x, pp)
    x[cells] = vals
    x
}

r = try(nn(r, x))
if ("try-error" %in% class(r)) {
    stop("Function nn did not work - is gcc >4.8 installed?")
}

out = template
xy = as(r, "SpatialPoints")
out[xy] = r[xy]

writeRaster(out, filename="data/gcam_32rgn_rast.tif", format="GTiff", overwrite=TRUE)
