## Author : Simon Moulds
## Date   : June 2017

library(raster)
library(sp)
library(splancs)
library(magrittr)
library(tidyr)
library(dplyr)
library(rgdal)
options(stringsAsFactors = FALSE)

## ======================================
## global cropland map - using IIASA-IFPRI map
## (http://geo-wiki.org/downloads/)
## ======================================

lulc_path = "data/iiasa-ifpri-cropland-map"
if (!dir.exists(lulc_path)) {
    dir.create(lulc_path)
}

crop_area_2005 =
    raster("data/rawdata/iiasa-ifpri-cropland-map/Hybrid_10042015v9.img") %>%
    raster::aggregate(fact=10, FUN=mean) %>%
    `/`(100)

writeRaster(crop_area_2005,
            file.path(lulc_path, "iiasa_ifpri_cropland_map_5m.tif"),
            format="GTiff",
            overwrite=TRUE)

## ======================================
## MapSPAM
## (http://mapspam.info/bundle-data/)
## ======================================

mapspam_path = "data/mapspam_data"

## physical area, harvested area, production, yield
system(paste0("unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_phys_area.geotiff.zip -d ", mapspam_path))
system(paste0("unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_harv_area.geotiff.zip -d ", mapspam_path))
system(paste0("unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_prod.geotiff.zip -d ", mapspam_path))
system(paste0("unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_yield.geotiff.zip -d ", mapspam_path))

## ======================================
## GAEZ
## (http://www.fao.org/nr/gaez/en/)
## ======================================

if (!dir.exists(file.path("data","gaez_data"))) {
    dir.create(file.path("data","gaez_data"))
}

fs <- list.files(file.path("data", "rawdata", "GAEZ"), pattern="^res03.*\\.zip", full.names=TRUE)
for (i in 1:length(fs)) {

    f <- fs[i]
    d <- file.path("data", "gaez_data", sub("^([^.]*).*", "\\1", basename(f)))
    unzip(f, exdir=d)

    f <- list.files(d, pattern="^[^.]*.tif", full.names=TRUE)
    if (length(f) != 1) {
        stop()
    }
}

## ======================================
## India region
## ======================================

v = readOGR("data", "gcam_32rgn")
template = raster(nrows=2160, ncols=4320, xmn=-180, xmx=180, ymn=-90, ymx=90, crs="+proj=longlat +datum=WGS84")
r = rasterize(v, template, field="GCAM_ID")
r[r == 0] = NA

## the following code is to ensure the land area implied by the
## region map exactly matches the land area of MapSPAM maps

## TODO: update this with the nearest neighbour method shown
## described here -
## http://rspatial.org/analysis/rst/4-interpolation.html

## TODO: uncomment!
## fs = list.files(file.path("data", "mapspam_data"), pattern="SPAM2005V3r1_global_A_TA_[A-Z]{4}_A.tif$", full.names=TRUE)

## x = raster(fs[1])
## x[] = 0

## for (f in fs) {
##     x = stackApply(stack(x, raster(f)), indices=c(1,1), fun=sum)
## }

## r = crop(r, x)
## x[x > 0] = 1
## x[r > 0] = 1
## x[x < 1] = NA 

## nn = function(x, y, ...) {
##     xx = raster(x)
##     xx[is.na(x) & !is.na(y)] = 1
##     pp = rasterToPoints(xx)[,1:2]

##     p = rasterToPoints(x)[,1:2]
##     nn = n2dist(p,pp)

##     ix = nn$neighs
##     vals = rasterToPoints(x)[ix,3]

##     pp = as.data.frame(pp)
##     coordinates(pp) = ~x+y
##     cells = cellFromXY(x, pp)
##     x[cells] = vals
##     x
## }

## r = nn(r, x)

## out = template
## xy = as(r, "SpatialPoints")
## out[xy] = r[xy]

## writeRaster(out, filename="data/gcam_32rgn_rast_ll.tif", format="GTiff", overwrite=TRUE)











## ## india_poly = readOGR(dsn="data", layer="gcam_32rgn")
## india_rgn = raster("data/gcam_32rgn_rast_ll.tif")
## india_rgn[india_rgn != 17] = NA
## india_rgn %<>% trim
## india_rgn[!is.na(india_rgn)] = 1

## india_ext = extent(india_rgn)
## cell_area = area(india_rgn) * 1000 * 1000 / 10000 ## km2 -> Ha

