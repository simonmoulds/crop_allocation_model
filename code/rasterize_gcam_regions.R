library(raster)
library(rgdal)
library(sp)
library(splancs)

v = readOGR("data", "gcam_32rgn")
template = raster(nrows=2160, ncols=4320, xmn=-180, xmx=180, ymn=-90, ymx=90, crs="+proj=longlat +datum=WGS84")
r = rasterize(v, template, field="GCAM_ID")
r[r == 0] = NA

fs = list.files(file.path("data", "mapspam_global_phys_area"), pattern="SPAM2005V3r1_global_A_TA_[A-Z]{4}_A.tif$", full.names=TRUE)

x = raster(fs[1])
x[] = 0

for (f in fs) {
    x = stackApply(stack(x, raster(f)), indices=c(1,1), fun=sum)
}

r = crop(r, x)
x[x > 0] = 1
x[r > 0] = 1
x[x < 1] = NA 

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

r = nn(r, x)

out = template
xy = as(r, "SpatialPoints")
out[xy] = r[xy]

writeRaster(out, filename="data/gcam_32rgn_rast_ll.tif", format="GTiff", overwrite=TRUE)
