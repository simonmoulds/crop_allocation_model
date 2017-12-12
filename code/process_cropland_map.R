## Author : Simon Moulds
## Date   : June 2017

library(raster)
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
if (dir.exists(lulc_path)) {
    unlink(lulc_path, recursive=TRUE)
}
dir.create(lulc_path)

crop_area_2005 = raster("data/rawdata/iiasa-ifpri-cropland-map/Hybrid_10042015v9.img")

ext = extent(crop_area_2005)
## template = raster(nrow=nrow(crop_area_2005),
##                   ncol=ncol(crop_area_2005) - 1,
##                   xmn=-180, xmx=180, ymn=ext@ymin, ymx=ext@ymax)
template = raster(nrow=21600, ncol=43200, xmn=-180, xmx=180, ymn=-90, ymx=90)

## warning: this takes a long time (~10min)!
crop_area_2005 =
    resample(crop_area_2005, template, method="bilinear")

crop_area_2005[crop_area_2005 < 0] = 0

## TODO: try and achieve this with GRASS (r.resamp.interp?)

crop_area_2005 =
    crop_area_2005 %>%
    raster::aggregate(fact=10, FUN=mean) %>%
    `/`(100)

writeRaster(crop_area_2005,
            file.path(lulc_path, "iiasa_ifpri_cropland_map_5m_ll.tif"),
            format="GTiff",
            overwrite=TRUE)
