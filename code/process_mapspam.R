## Author : Simon Moulds
## Date   : June 2017

library(raster)
library(magrittr)
library(tidyr)
library(dplyr)
library(rgdal)
library(ggplot2)
options(stringsAsFactors = FALSE)

## ======================================
## india region map
## ======================================

## india_poly = readOGR(dsn="data", layer="gcam_32rgn")
india_rgn = raster("data/gcam_32rgn_rast_ll.tif")
india_rgn[india_rgn != 17] = NA
india_rgn %<>% trim
india_rgn[!is.na(india_rgn)] = 1

india_ext = extent(india_rgn)
cell_area = area(india_rgn) * 1000 * 1000 / 10000 ## km2 -> Ha

## ======================================
## global cropland map - using IIASA-IFPRI map (http://geo-wiki.org/downloads/)
## ======================================

crop_area_2005 =
    raster("data/rawdata/iiasa-ifpri-cropland-map/Hybrid_10042015v9.img") %>%
    crop(india_ext) %>%
    raster::aggregate(fact=10, FUN=mean) %>%
    `/`(100)

## ======================================
## helper functions
## ======================================

get_mapspam_data = function(crop, path, what, ...) {
    ## function to load MapSPAM data

    what = tolower(what)
    vars = c("yield","production","harvested_area","physical_area")
    if (length(what) == 1 && what %in% vars) {
        idx = which(vars %in% what)
        ind = c("Y","P","H","A")[idx]
    } else {
        stop()
    }
    
    out =
        list(total=raster(file.path(path, paste0("SPAM2005V3r1_global_", ind, "_TA_", toupper(crop), "_A.tif"))),
             irri=raster(file.path(path, paste0("SPAM2005V3r1_global_", ind, "_TI_", toupper(crop), "_I.tif"))),
             rain=raster(file.path(path, paste0("SPAM2005V3r1_global_", ind, "_TR_", toupper(crop), "_R.tif"))),
             rain_h=raster(file.path(path, paste0("SPAM2005V3r1_global_", ind, "_TH_", toupper(crop), "_H.tif"))),
             rain_l=raster(file.path(path, paste0("SPAM2005V3r1_global_", ind, "_TL_", toupper(crop), "_L.tif"))),
             rain_s=raster(file.path(path, paste0("SPAM2005V3r1_global_", ind, "_TS_", toupper(crop), "_S.tif"))))
    out
}

## ======================================
## load MapSPAM data
## ======================================
mapspam_path = "data/mapspam_data"

## physical area, harvested area, production, yield
system(paste0("unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_phys_area.geotiff.zip -d ", mapspam_path))
system(paste0("unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_harv_area.geotiff.zip -d ", mapspam_path))
system(paste0("unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_prod.geotiff.zip -d ", mapspam_path))
system(paste0("unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_yield.geotiff.zip -d ", mapspam_path))

## weights matrix for neighbourhood calculation (suitability)
nbw = matrix(data=1, nrow=5, ncol=5)

## kharif crops

## rice
rice_phys =
    get_mapspam_data("rice", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rice_harv =
    get_mapspam_data("rice", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rice_yield =
    get_mapspam_data("rice", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
rice_prod =
    get_mapspam_data("rice", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rice_irri_nb = focal(rice_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
rice_irri_suit = rice_irri_nb / cellStats(rice_irri_nb, stat=max)
rice_rain_nb = focal(rice_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
rice_rain_suit = rice_rain_nb / cellStats(rice_rain_nb, stat=max)

## assume that most rainfed rice occurs during kharif
rice_khar_irri = rice_harv[["irri"]] - rice_phys[["irri"]]
rice_khar_rain = rice_phys[["rain"]] 
rice_khar_rain_h = rice_phys[["rain_h"]]
rice_khar_rain_l = rice_phys[["rain_l"]]
rice_khar_rain_s = rice_phys[["rain_s"]]

## barley
barl_phys =
    get_mapspam_data("barl", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

barl_harv =
    get_mapspam_data("barl", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

barl_yield =
    get_mapspam_data("barl", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
barl_prod =
    get_mapspam_data("barl", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

barl_irri_nb = focal(barl_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
barl_irri_suit = barl_irri_nb / cellStats(barl_irri_nb, stat=max)
barl_rain_nb = focal(barl_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
barl_rain_suit = barl_rain_nb / cellStats(barl_rain_nb, stat=max)

## maize
maiz_phys =
    get_mapspam_data("maiz", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

maiz_harv =
    get_mapspam_data("maiz", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

maiz_yield =
    get_mapspam_data("maiz", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
maiz_prod =
    get_mapspam_data("maiz", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

maiz_irri_nb = focal(maiz_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
maiz_irri_suit = maiz_irri_nb / cellStats(maiz_irri_nb, stat=max)
maiz_rain_nb = focal(maiz_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
maiz_rain_suit = maiz_rain_nb / cellStats(maiz_rain_nb, stat=max)

## millet
pmil_phys =
    get_mapspam_data("pmil", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pmil_harv =
    get_mapspam_data("pmil", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pmil_yield =
    get_mapspam_data("pmil", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
pmil_prod =
    get_mapspam_data("pmil", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pmil_irri_nb = focal(pmil_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
pmil_irri_suit = pmil_irri_nb / cellStats(pmil_irri_nb, stat=max)
pmil_rain_nb = focal(pmil_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
pmil_rain_suit = pmil_rain_nb / cellStats(pmil_rain_nb, stat=max)

smil_phys =
    get_mapspam_data("smil", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

smil_harv =
    get_mapspam_data("smil", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

smil_yield =
    get_mapspam_data("smil", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
smil_prod =
    get_mapspam_data("smil", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

smil_irri_nb = focal(smil_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
smil_irri_suit = smil_irri_nb / cellStats(smil_irri_nb, stat=max)
smil_rain_nb = focal(smil_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
smil_rain_suit = smil_rain_nb / cellStats(smil_rain_nb, stat=max)

## sorghum
sorg_phys =
    get_mapspam_data("sorg", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sorg_harv =
    get_mapspam_data("sorg", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sorg_yield =
    get_mapspam_data("sorg", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
sorg_prod =
    get_mapspam_data("sorg", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sorg_irri_nb = focal(sorg_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
sorg_irri_suit = sorg_irri_nb / cellStats(sorg_irri_nb, stat=max)
sorg_rain_nb = focal(sorg_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
sorg_rain_suit = sorg_rain_nb / cellStats(sorg_rain_nb, stat=max)

## other cereal
ocer_phys =
    get_mapspam_data("ocer", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ocer_harv =
    get_mapspam_data("ocer", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ocer_yield =
    get_mapspam_data("ocer", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
ocer_prod =
    get_mapspam_data("ocer", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ocer_irri_nb = focal(ocer_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
ocer_irri_suit = ocer_irri_nb / cellStats(ocer_irri_nb, stat=max)
ocer_rain_nb = focal(ocer_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
ocer_rain_suit = ocer_rain_nb / cellStats(ocer_rain_nb, stat=max)

## soybean
soyb_phys =
    get_mapspam_data("soyb", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

soyb_harv =
    get_mapspam_data("soyb", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

soyb_yield =
    get_mapspam_data("soyb", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
soyb_prod =
    get_mapspam_data("soyb", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

soyb_irri_nb = focal(soyb_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
soyb_irri_suit = soyb_irri_nb / cellStats(soyb_irri_nb, stat=max)
soyb_rain_nb = focal(soyb_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
soyb_rain_suit = soyb_rain_nb / cellStats(soyb_rain_nb, stat=max)

## groundnut
grou_phys =
    get_mapspam_data("grou", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

grou_harv =
    get_mapspam_data("grou", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

grou_yield =
    get_mapspam_data("grou", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
grou_prod =
    get_mapspam_data("grou", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

grou_irri_nb = focal(grou_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
grou_irri_suit = grou_irri_nb / cellStats(grou_irri_nb, stat=max)
grou_rain_nb = focal(grou_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
grou_rain_suit = grou_rain_nb / cellStats(grou_rain_nb, stat=max)

## sesameseed
sesa_phys =
    get_mapspam_data("sesa", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sesa_harv =
    get_mapspam_data("sesa", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sesa_yield =
    get_mapspam_data("sesa", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
sesa_prod =
    get_mapspam_data("sesa", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sesa_irri_nb = focal(sesa_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
sesa_irri_suit = sesa_irri_nb / cellStats(sesa_irri_nb, stat=max)
sesa_rain_nb = focal(sesa_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
sesa_rain_suit = sesa_rain_nb / cellStats(sesa_rain_nb, stat=max)

## sunflower
sunf_phys =
    get_mapspam_data("sunf", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sunf_harv =
    get_mapspam_data("sunf", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sunf_yield =
    get_mapspam_data("sunf", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
sunf_prod =
    get_mapspam_data("sunf", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sunf_irri_nb = focal(sunf_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
sunf_irri_suit = sunf_irri_nb / cellStats(sunf_irri_nb, stat=max)
sunf_rain_nb = focal(sunf_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
sunf_rain_suit = sunf_rain_nb / cellStats(sunf_rain_nb, stat=max)

## otheroils
ooil_phys =
    get_mapspam_data("ooil", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ooil_harv =
    get_mapspam_data("ooil", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ooil_yield =
    get_mapspam_data("ooil", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
ooil_prod =
    get_mapspam_data("ooil", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ooil_irri_nb = focal(ooil_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
ooil_irri_suit = ooil_irri_nb / cellStats(ooil_irri_nb, stat=max)
ooil_rain_nb = focal(ooil_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
ooil_rain_suit = ooil_rain_nb / cellStats(ooil_rain_nb, stat=max)

## potato
pota_phys =
    get_mapspam_data("pota", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pota_harv =
    get_mapspam_data("pota", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pota_yield =
    get_mapspam_data("pota", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
pota_prod =
    get_mapspam_data("pota", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pota_irri_nb = focal(pota_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
pota_irri_suit = pota_irri_nb / cellStats(pota_irri_nb, stat=max)
pota_rain_nb = focal(pota_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
pota_rain_suit = pota_rain_nb / cellStats(pota_rain_nb, stat=max)

## sweetpotato
swpo_phys =
    get_mapspam_data("swpo", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

swpo_harv =
    get_mapspam_data("swpo", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

swpo_yield =
    get_mapspam_data("swpo", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
swpo_prod =
    get_mapspam_data("swpo", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

swpo_irri_nb = focal(swpo_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
swpo_irri_suit = swpo_irri_nb / cellStats(swpo_irri_nb, stat=max)
swpo_rain_nb = focal(swpo_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
swpo_rain_suit = swpo_rain_nb / cellStats(swpo_rain_nb, stat=max)

## cotton
cott_phys =
    get_mapspam_data("cott", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cott_harv =
    get_mapspam_data("cott", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cott_yield =
    get_mapspam_data("cott", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
cott_prod =
    get_mapspam_data("cott", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cott_irri_nb = focal(cott_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
cott_irri_suit = cott_irri_nb / cellStats(cott_irri_nb, stat=max)
cott_rain_nb = focal(cott_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
cott_rain_suit = cott_rain_nb / cellStats(cott_rain_nb, stat=max)

## other fibre
ofib_phys =
    get_mapspam_data("ofib", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ofib_harv =
    get_mapspam_data("ofib", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ofib_yield =
    get_mapspam_data("ofib", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
ofib_prod =
    get_mapspam_data("ofib", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ofib_irri_nb = focal(ofib_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
ofib_irri_suit = ofib_irri_nb / cellStats(ofib_irri_nb, stat=max)
ofib_rain_nb = focal(ofib_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
ofib_rain_suit = ofib_rain_nb / cellStats(ofib_rain_nb, stat=max)

## tobacco
toba_phys =
    get_mapspam_data("toba", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

toba_harv =
    get_mapspam_data("toba", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

toba_yield =
    get_mapspam_data("toba", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
toba_prod =
    get_mapspam_data("toba", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

toba_irri_nb = focal(toba_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
toba_irri_suit = toba_irri_nb / cellStats(toba_irri_nb, stat=max)
toba_rain_nb = focal(toba_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
toba_rain_suit = toba_rain_nb / cellStats(toba_rain_nb, stat=max)

## rest of crops
rest_phys =
    get_mapspam_data("rest", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rest_harv =
    get_mapspam_data("rest", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rest_yield =
    get_mapspam_data("rest", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
rest_prod =
    get_mapspam_data("rest", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rest_irri_nb = focal(rest_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
rest_irri_suit = rest_irri_nb / cellStats(rest_irri_nb, stat=max)
rest_rain_nb = focal(rest_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
rest_rain_suit = rest_rain_nb / cellStats(rest_rain_nb, stat=max)

## rabi crops

## rice

## assumes that most irrigated rice occurs during rabi and most rainfed rice occurs during kharif
rice_rabi_irri = rice_phys[["irri"]] 
rice_rabi_rain = rice_harv[["rain"]] - rice_phys[["rain"]]
rice_rabi_rain_h = rice_harv[["rain_h"]] - rice_phys[["rain_h"]]
rice_rabi_rain_l = rice_harv[["rain_l"]] - rice_phys[["rain_l"]]
rice_rabi_rain_s = rice_harv[["rain_s"]] - rice_phys[["rain_s"]]

## wheat
whea_phys =
    get_mapspam_data("whea", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

whea_harv =
    get_mapspam_data("whea", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

whea_yield =
    get_mapspam_data("whea", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
whea_prod =
    get_mapspam_data("whea", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

whea_irri_nb = focal(whea_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
whea_irri_suit = whea_irri_nb / cellStats(whea_irri_nb, stat=max)
whea_rain_nb = focal(whea_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
whea_rain_suit = whea_rain_nb / cellStats(whea_rain_nb, stat=max)

## vegetables
vege_phys =
    get_mapspam_data("vege", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

vege_harv =
    get_mapspam_data("vege", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

vege_yield =
    get_mapspam_data("vege", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
vege_prod =
    get_mapspam_data("vege", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

vege_irri_nb = focal(vege_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
vege_irri_suit = vege_irri_nb / cellStats(vege_irri_nb, stat=max)
vege_rain_nb = focal(vege_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
vege_rain_suit = vege_rain_nb / cellStats(vege_rain_nb, stat=max)

## rapeseed
rape_phys =
    get_mapspam_data("rape", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rape_harv =
    get_mapspam_data("rape", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rape_yield =
    get_mapspam_data("rape", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
rape_prod =
    get_mapspam_data("rape", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rape_irri_nb = focal(rape_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
rape_irri_suit = rape_irri_nb / cellStats(rape_irri_nb, stat=max)
rape_rain_nb = focal(rape_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
rape_rain_suit = rape_rain_nb / cellStats(rape_rain_nb, stat=max)

## pulses (NB cowpea not present in India according to MapSPAM)
bean_phys =
    get_mapspam_data("bean", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

bean_harv =
    get_mapspam_data("bean", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

bean_yield =
    get_mapspam_data("bean", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
bean_prod =
    get_mapspam_data("bean", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

bean_irri_nb = focal(bean_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
bean_irri_suit = bean_irri_nb / cellStats(bean_irri_nb, stat=max)
bean_rain_nb = focal(bean_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
bean_rain_suit = bean_rain_nb / cellStats(bean_rain_nb, stat=max)

## chickpea
chic_phys =
    get_mapspam_data("chic", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

chic_harv =
    get_mapspam_data("chic", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

chic_yield =
    get_mapspam_data("chic", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
chic_prod =
    get_mapspam_data("chic", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

chic_irri_nb = focal(chic_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
chic_irri_suit = chic_irri_nb / cellStats(chic_irri_nb, stat=max)
chic_rain_nb = focal(chic_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
chic_rain_suit = chic_rain_nb / cellStats(chic_rain_nb, stat=max)

## pigeon pea
pige_phys =
    get_mapspam_data("pige", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pige_harv =
    get_mapspam_data("pige", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pige_yield =
    get_mapspam_data("pige", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
pige_prod =
    get_mapspam_data("pige", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pige_irri_nb = focal(pige_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
pige_irri_suit = pige_irri_nb / cellStats(pige_irri_nb, stat=max)
pige_rain_nb = focal(pige_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
pige_rain_suit = pige_rain_nb / cellStats(pige_rain_nb, stat=max)

## cowpea
cowp_phys =
    get_mapspam_data("cowp", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cowp_harv =
    get_mapspam_data("cowp", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cowp_yield =
    get_mapspam_data("cowp", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
cowp_prod =
    get_mapspam_data("cowp", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cowp_irri_nb = focal(cowp_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
cowp_irri_suit = cowp_irri_nb / cellStats(cowp_irri_nb, stat=max)
cowp_rain_nb = focal(cowp_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
cowp_rain_suit = cowp_rain_nb / cellStats(cowp_rain_nb, stat=max)

## lentil
lent_phys =
    get_mapspam_data("lent", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

lent_harv =
    get_mapspam_data("lent", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

lent_yield =
    get_mapspam_data("lent", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
lent_prod =
    get_mapspam_data("lent", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

lent_irri_nb = focal(lent_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
lent_irri_suit = lent_irri_nb / cellStats(lent_irri_nb, stat=max)
lent_rain_nb = focal(lent_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
lent_rain_suit = lent_rain_nb / cellStats(lent_rain_nb, stat=max)

## other pulses
opul_phys =
    get_mapspam_data("opul", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

opul_harv =
    get_mapspam_data("opul", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

opul_yield =
    get_mapspam_data("opul", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
opul_prod =
    get_mapspam_data("opul", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

opul_irri_nb = focal(opul_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
opul_irri_suit = opul_irri_nb / cellStats(opul_irri_nb, stat=max)
opul_rain_nb = focal(opul_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
opul_rain_suit = opul_rain_nb / cellStats(opul_rain_nb, stat=max)

## annual crops

## sugarcane (NB sugarbeet not grown in India)
sugc_phys =
    get_mapspam_data("sugc", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sugc_harv =
    get_mapspam_data("sugc", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sugc_yield =
    get_mapspam_data("sugc", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
sugc_prod =
    get_mapspam_data("sugc", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sugc_irri_nb = focal(sugc_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
sugc_irri_suit = sugc_irri_nb / cellStats(sugc_irri_nb, stat=max)
sugc_rain_nb = focal(sugc_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
sugc_rain_suit = sugc_rain_nb / cellStats(sugc_rain_nb, stat=max)

## sugar beet (not grown in India - included for completeness)
sugb_phys =
    get_mapspam_data("sugb", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sugb_harv =
    get_mapspam_data("sugb", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sugb_yield =
    get_mapspam_data("sugb", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
sugb_prod =
    get_mapspam_data("sugb", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sugb_irri_nb = focal(sugb_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
sugb_irri_suit = sugb_irri_nb / cellStats(sugb_irri_nb, stat=max)
sugb_rain_nb = focal(sugb_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
sugb_rain_suit = sugb_rain_nb / cellStats(sugb_rain_nb, stat=max)

## coconut
cnut_phys =
    get_mapspam_data("cnut", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cnut_harv =
    get_mapspam_data("cnut", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cnut_yield =
    get_mapspam_data("cnut", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
cnut_prod =
    get_mapspam_data("cnut", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cnut_irri_nb = focal(cnut_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
cnut_irri_suit = cnut_irri_nb / cellStats(cnut_irri_nb, stat=max)
cnut_rain_nb = focal(cnut_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
cnut_rain_suit = cnut_rain_nb / cellStats(cnut_rain_nb, stat=max)

## oil palm (not grown in India - included for completeness)
oilp_phys =
    get_mapspam_data("oilp", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

oilp_harv =
    get_mapspam_data("oilp", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

oilp_yield =
    get_mapspam_data("oilp", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
oilp_prod =
    get_mapspam_data("oilp", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

oilp_irri_nb = focal(oilp_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
oilp_irri_suit = oilp_irri_nb / cellStats(oilp_irri_nb, stat=max)
oilp_rain_nb = focal(oilp_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
oilp_rain_suit = oilp_rain_nb / cellStats(oilp_rain_nb, stat=max)

## tropical fruit
trof_phys =
    get_mapspam_data("trof", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

trof_harv =
    get_mapspam_data("trof", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

trof_yield =
    get_mapspam_data("trof", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
trof_prod =
    get_mapspam_data("trof", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

trof_irri_nb = focal(trof_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
trof_irri_suit = trof_irri_nb / cellStats(trof_irri_nb, stat=max)
trof_rain_nb = focal(trof_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
trof_rain_suit = trof_rain_nb / cellStats(trof_rain_nb, stat=max)

## temperate fruit
temf_phys =
    get_mapspam_data("temf", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

temf_harv =
    get_mapspam_data("temf", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

temf_yield =
    get_mapspam_data("temf", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
temf_prod =
    get_mapspam_data("temf", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

temf_irri_nb = focal(temf_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
temf_irri_suit = temf_irri_nb / cellStats(temf_irri_nb, stat=max)
temf_rain_nb = focal(temf_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
temf_rain_suit = temf_rain_nb / cellStats(temf_rain_nb, stat=max)

## banana
bana_phys =
    get_mapspam_data("bana", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

bana_harv =
    get_mapspam_data("bana", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

bana_yield =
    get_mapspam_data("bana", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
bana_prod =
    get_mapspam_data("bana", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

bana_irri_nb = focal(bana_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
bana_irri_suit = bana_irri_nb / cellStats(bana_irri_nb, stat=max)
bana_rain_nb = focal(bana_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
bana_rain_suit = bana_rain_nb / cellStats(bana_rain_nb, stat=max)

## plantain
plnt_phys =
    get_mapspam_data("plnt", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

plnt_harv =
    get_mapspam_data("plnt", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

plnt_yield =
    get_mapspam_data("plnt", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
plnt_prod =
    get_mapspam_data("plnt", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

plnt_irri_nb = focal(plnt_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
plnt_irri_suit = plnt_irri_nb / cellStats(plnt_irri_nb, stat=max)
plnt_rain_nb = focal(plnt_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
plnt_rain_suit = plnt_rain_nb / cellStats(plnt_rain_nb, stat=max)

## cassava
cass_phys =
    get_mapspam_data("cass", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cass_harv =
    get_mapspam_data("cass", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cass_yield =
    get_mapspam_data("cass", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
cass_prod =
    get_mapspam_data("cass", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cass_irri_nb = focal(cass_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
cass_irri_suit = cass_irri_nb / cellStats(cass_irri_nb, stat=max)
cass_rain_nb = focal(cass_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
cass_rain_suit = cass_rain_nb / cellStats(cass_rain_nb, stat=max)

## yams
yams_phys =
    get_mapspam_data("yams", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

yams_harv =
    get_mapspam_data("yams", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

yams_yield =
    get_mapspam_data("yams", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
yams_prod =
    get_mapspam_data("yams", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

yams_irri_nb = focal(yams_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
yams_irri_suit = yams_irri_nb / cellStats(yams_irri_nb, stat=max)
yams_rain_nb = focal(yams_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
yams_rain_suit = yams_rain_nb / cellStats(yams_rain_nb, stat=max)

## other roots
orts_phys =
    get_mapspam_data("orts", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

orts_harv =
    get_mapspam_data("orts", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

orts_yield =
    get_mapspam_data("orts", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
orts_prod =
    get_mapspam_data("orts", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

orts_irri_nb = focal(orts_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
orts_irri_suit = orts_irri_nb / cellStats(orts_irri_nb, stat=max)
orts_rain_nb = focal(orts_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
orts_rain_suit = orts_rain_nb / cellStats(orts_rain_nb, stat=max)

## cocoa
coco_phys =
    get_mapspam_data("coco", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

coco_harv =
    get_mapspam_data("coco", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

coco_yield =
    get_mapspam_data("coco", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
coco_prod =
    get_mapspam_data("coco", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

coco_irri_nb = focal(coco_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
coco_irri_suit = coco_irri_nb / cellStats(coco_irri_nb, stat=max)
coco_rain_nb = focal(coco_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
coco_rain_suit = coco_rain_nb / cellStats(coco_rain_nb, stat=max)

## tea
teas_phys =
    get_mapspam_data("teas", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

teas_harv =
    get_mapspam_data("teas", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

teas_yield =
    get_mapspam_data("teas", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
teas_prod =
    get_mapspam_data("teas", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

teas_irri_nb = focal(teas_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
teas_irri_suit = teas_irri_nb / cellStats(teas_irri_nb, stat=max)
teas_rain_nb = focal(teas_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
teas_rain_suit = teas_rain_nb / cellStats(teas_rain_nb, stat=max)

## arabica coffee
acof_phys =
    get_mapspam_data("acof", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

acof_harv =
    get_mapspam_data("acof", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

acof_yield =
    get_mapspam_data("acof", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
acof_prod =
    get_mapspam_data("acof", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

acof_irri_nb = focal(acof_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
acof_irri_suit = acof_irri_nb / cellStats(acof_irri_nb, stat=max)
acof_rain_nb = focal(acof_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
acof_rain_suit = acof_rain_nb / cellStats(acof_rain_nb, stat=max)

## robusta coffee
rcof_phys =
    get_mapspam_data("rcof", mapspam_path, "physical_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rcof_harv =
    get_mapspam_data("rcof", mapspam_path, "harvested_area") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rcof_yield =
    get_mapspam_data("rcof", mapspam_path, "yield") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
rcof_prod =
    get_mapspam_data("rcof", mapspam_path, "production") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rcof_irri_nb = focal(rcof_harv[["irri"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
rcof_irri_suit = rcof_irri_nb / cellStats(rcof_irri_nb, stat=max)
rcof_rain_nb = focal(rcof_harv[["rain"]], w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)
rcof_rain_suit = rcof_rain_nb / cellStats(rcof_rain_nb, stat=max)

## ======================================
## group irrigated and rainfed crops into data frames by season
## ======================================

## index of non-NA cells
cell_ix = which(!is.na(getValues(india_rgn)))
## cell_ix = which(!is.na(getValues(rice_khar_irri)))

## kharif
kharif_irri_area_df = 
    list(rice=rice_khar_irri, barl=barl_harv[["irri"]], maiz=maiz_harv[["irri"]],
         pmil=pmil_harv[["irri"]], smil=smil_harv[["irri"]], sorg=sorg_harv[["irri"]],
         ocer=ocer_harv[["irri"]], soyb=soyb_harv[["irri"]], grou=grou_harv[["irri"]],
         sesa=sesa_harv[["irri"]], sunf=sunf_harv[["irri"]], ooil=ooil_harv[["irri"]],
         pota=pota_harv[["irri"]], swpo=swpo_harv[["irri"]], cott=cott_harv[["irri"]],
         ofib=ofib_harv[["irri"]], toba=toba_harv[["irri"]], rest=rest_harv[["irri"]]) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate(season="kharif", input="irrigated", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

kharif_rain_area_df = 
    list(rice=rice_khar_rain, barl=barl_harv[["rain"]], maiz=maiz_harv[["rain"]],
         pmil=pmil_harv[["rain"]], smil=smil_harv[["rain"]], sorg=sorg_harv[["rain"]],
         ocer=ocer_harv[["rain"]], soyb=soyb_harv[["rain"]], grou=grou_harv[["rain"]],
         sesa=sesa_harv[["rain"]], sunf=sunf_harv[["rain"]], ooil=ooil_harv[["rain"]],
         pota=pota_harv[["rain"]], swpo=swpo_harv[["rain"]], cott=cott_harv[["rain"]],
         ofib=ofib_harv[["rain"]], toba=toba_harv[["rain"]], rest=rest_harv[["rain"]]) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate(season="kharif", input="rainfed", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

## rabi
rabi_irri_area_df =
    list(rice=rice_rabi_irri, whea=whea_harv[["irri"]], vege=vege_harv[["irri"]],
         rape=rape_harv[["irri"]], bean=bean_harv[["irri"]], chic=chic_harv[["irri"]],
         pige=pige_harv[["irri"]], cowp=cowp_harv[["irri"]], lent=lent_harv[["irri"]],
         opul=opul_harv[["irri"]]) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate(season="rabi", input="irrigated", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

rabi_rain_area_df =
    list(rice=rice_rabi_rain, whea=whea_harv[["rain"]], vege=vege_harv[["rain"]],
         rape=rape_harv[["rain"]], bean=bean_harv[["rain"]], chic=chic_harv[["rain"]],
         pige=pige_harv[["rain"]], cowp=cowp_harv[["rain"]], lent=lent_harv[["rain"]],
         opul=opul_harv[["rain"]]) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate(season="rabi", input="rainfed", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

## annual
annual_irri_area_df =
    list(sugc=sugc_harv[["irri"]], sugb=sugb_harv[["irri"]], cnut=cnut_harv[["irri"]],
         oilp=oilp_harv[["irri"]], trof=trof_harv[["irri"]], temf=temf_harv[["irri"]],
         bana=bana_harv[["irri"]], plnt=plnt_harv[["irri"]], coco=coco_harv[["irri"]],
         teas=teas_harv[["irri"]], acof=acof_harv[["irri"]], rcof=rcof_harv[["irri"]],
         cass=cass_harv[["irri"]], yams=yams_harv[["irri"]], orts=orts_harv[["irri"]]) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate(season="annual", input="irrigated", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

annual_rain_area_df =
    list(sugc=sugc_harv[["rain"]], sugb=sugb_harv[["rain"]], cnut=cnut_harv[["rain"]],
         oilp=oilp_harv[["rain"]], trof=trof_harv[["rain"]], temf=temf_harv[["rain"]],
         bana=bana_harv[["rain"]], plnt=plnt_harv[["rain"]], coco=coco_harv[["rain"]],
         teas=teas_harv[["rain"]], acof=acof_harv[["rain"]], rcof=rcof_harv[["rain"]],
         cass=cass_harv[["rain"]], yams=yams_harv[["rain"]], orts=orts_harv[["rain"]]) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate(season="annual", input="rainfed", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

## now, group these together
area_df =
    kharif_irri_area_df %>%
    full_join(kharif_rain_area_df) %>%
    full_join(rabi_irri_area_df) %>%
    full_join(rabi_rain_area_df) %>%
    full_join(annual_irri_area_df) %>%
    full_join(annual_rain_area_df) %>%
    select(cell, season, input, acof, bana, barl, bean, cass, chic, cnut, coco, cott, cowp, grou, lent, maiz, ocer, ofib, oilp, ooil, opul, orts, pige, plnt, pmil, pota, rape, rcof, rest, rice, sesa, smil, sorg, soyb, sugb, sugc, sunf, swpo, teas, temf, toba, trof, vege, whea, yams) %>% 
    arrange(cell, season, input)

## save objects
saveRDS(area_df, "data/mapspam_crop_area_df.rds")

## ## total of each crop in respective seasons
## total_area_df =
##     area_df %>%
##     gather(crop, value, -cell, -season, -input) %>%
##     group_by(season, crop) %>%
##     summarise_each(funs(sum(., na.rm=TRUE)), value) %>%
##     spread(crop, value)
## saveRDS(total_area_df, "data/mapspam_total_crop_area_df.rds")

## ======================================
## yield
## ======================================

## kharif
kharif_irri_yield_df =
    list(rice=rice_yield[["irri"]], barl=barl_yield[["irri"]], maiz=maiz_yield[["irri"]],
         pmil=pmil_yield[["irri"]], smil=smil_yield[["irri"]], sorg=sorg_yield[["irri"]],
         ocer=ocer_yield[["irri"]], soyb=soyb_yield[["irri"]], grou=grou_yield[["irri"]],
         sesa=sesa_yield[["irri"]], sunf=sunf_yield[["irri"]], ooil=ooil_yield[["irri"]],
         pota=pota_yield[["irri"]], swpo=swpo_yield[["irri"]], cott=cott_yield[["irri"]],
         ofib=ofib_yield[["irri"]], toba=toba_yield[["irri"]], rest=rest_yield[["irri"]]) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate_each(funs(./1000)) %>%   ## kg/ha -> tonne/ha
    mutate(season="kharif", input="irrigated", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)
    
kharif_rain_yield_df =
    list(rice=rice_yield[["rain"]], barl=barl_yield[["rain"]], maiz=maiz_yield[["rain"]],
         pmil=pmil_yield[["rain"]], smil=smil_yield[["rain"]], sorg=sorg_yield[["rain"]],
         ocer=ocer_yield[["rain"]], soyb=soyb_yield[["rain"]], grou=grou_yield[["rain"]],
         sesa=sesa_yield[["rain"]], sunf=sunf_yield[["rain"]], ooil=ooil_yield[["rain"]],
         pota=pota_yield[["rain"]], swpo=swpo_yield[["rain"]], cott=cott_yield[["rain"]],
         ofib=ofib_yield[["rain"]], toba=toba_yield[["rain"]], rest=rest_yield[["rain"]]) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate_each(funs(./1000)) %>%   ## kg/ha -> tonne/ha
    mutate(season="kharif", input="rainfed", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

## rabi - yield
rabi_irri_yield_df =
    list(rice=rice_yield[["irri"]], whea=whea_yield[["irri"]], vege=vege_yield[["irri"]],
         rape=rape_yield[["irri"]], bean=bean_yield[["irri"]], chic=chic_yield[["irri"]],
         pige=pige_yield[["irri"]], cowp=cowp_yield[["irri"]], lent=lent_yield[["irri"]],
         opul=opul_yield[["irri"]]) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate_each(funs(./1000)) %>%   ## kg/ha -> tonne/ha
    mutate(season="rabi", input="irrigated", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

rabi_rain_yield_df =
    list(rice=rice_yield[["rain"]], whea=whea_yield[["rain"]], vege=vege_yield[["rain"]],
         rape=rape_yield[["rain"]], bean=bean_yield[["rain"]], chic=chic_yield[["rain"]],
         pige=pige_yield[["rain"]], cowp=cowp_yield[["rain"]], lent=lent_yield[["rain"]],
         opul=opul_yield[["rain"]]) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate_each(funs(./1000)) %>%   ## kg/ha -> tonne/ha
    mutate(season="rabi", input="rainfed", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

## annual - yield
annual_irri_yield_df =
    list(sugc=sugc_yield[["irri"]], sugb=sugb_yield[["irri"]], cnut=cnut_yield[["irri"]],
         oilp=oilp_yield[["irri"]], trof=trof_yield[["irri"]], temf=temf_yield[["irri"]],
         bana=bana_yield[["irri"]], plnt=plnt_yield[["irri"]], coco=coco_yield[["irri"]],
         teas=teas_yield[["irri"]], acof=acof_yield[["irri"]], rcof=rcof_yield[["irri"]],
         cass=cass_yield[["irri"]], yams=yams_yield[["irri"]], orts=orts_yield[["irri"]]) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate_each(funs(./1000)) %>%   ## kg/ha -> tonne/ha
    mutate(season="annual", input="irrigated", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

annual_rain_yield_df =
    list(sugc=sugc_yield[["rain"]], sugb=sugb_yield[["rain"]], cnut=cnut_yield[["rain"]],
         oilp=oilp_yield[["rain"]], trof=trof_yield[["rain"]], temf=temf_yield[["rain"]],
         bana=bana_yield[["rain"]], plnt=plnt_yield[["rain"]], coco=coco_yield[["rain"]],
         teas=teas_yield[["rain"]], acof=acof_yield[["rain"]], rcof=rcof_yield[["rain"]],
         cass=cass_yield[["rain"]], yams=yams_yield[["rain"]], orts=orts_yield[["rain"]]) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate_each(funs(./1000)) %>%   ## kg/ha -> tonne/ha
    mutate(season="annual", input="rainfed", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

yield_df =
    kharif_irri_yield_df %>%
    full_join(kharif_rain_yield_df) %>%
    full_join(rabi_irri_yield_df) %>%
    full_join(rabi_rain_yield_df) %>%
    full_join(annual_irri_yield_df) %>%
    full_join(annual_rain_yield_df) %>%
    select(cell, season, input, acof, bana, barl, bean, cass, chic, cnut, coco, cott, cowp, grou, lent, maiz, ocer, ofib, oilp, ooil, opul, orts, pige, plnt, pmil, pota, rape, rcof, rest, rice, sesa, smil, sorg, soyb, sugb, sugc, sunf, swpo, teas, temf, toba, trof, vege, whea, yams) %>% 
    arrange(cell, season, input)

## save object
saveRDS(yield_df, "data/mapspam_crop_yield_df.rds")

## ======================================
## suitability
## ======================================

## kharif
kharif_irri_suit_df =
    list(rice=rice_irri_suit, barl=barl_irri_suit, maiz=maiz_irri_suit,
         pmil=pmil_irri_suit, smil=smil_irri_suit, sorg=sorg_irri_suit,
         ocer=ocer_irri_suit, soyb=soyb_irri_suit, grou=grou_irri_suit,
         sesa=sesa_irri_suit, sunf=sunf_irri_suit, ooil=ooil_irri_suit,
         pota=pota_irri_suit, swpo=swpo_irri_suit, cott=cott_irri_suit,
         ofib=ofib_irri_suit, toba=toba_irri_suit, rest=rest_irri_suit) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate(season="kharif", input="irrigated", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

kharif_rain_suit_df =
    list(rice=rice_rain_suit, barl=barl_rain_suit, maiz=maiz_rain_suit,
         pmil=pmil_rain_suit, smil=smil_rain_suit, sorg=sorg_rain_suit,
         ocer=ocer_rain_suit, soyb=soyb_rain_suit, grou=grou_rain_suit,
         sesa=sesa_rain_suit, sunf=sunf_rain_suit, ooil=ooil_rain_suit,
         pota=pota_rain_suit, swpo=swpo_rain_suit, cott=cott_rain_suit,
         ofib=ofib_rain_suit, toba=toba_rain_suit, rest=rest_rain_suit) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate(season="kharif", input="rainfed", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

## rabi
rabi_irri_suit_df =
    list(rice=rice_irri_suit, whea=whea_irri_suit, vege=vege_irri_suit,
         rape=rape_irri_suit, bean=bean_irri_suit, chic=chic_irri_suit,
         pige=pige_irri_suit, cowp=cowp_irri_suit, lent=lent_irri_suit,
         opul=opul_irri_suit) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate(season="rabi", input="irrigated", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

rabi_rain_suit_df =
    list(rice=rice_rain_suit, whea=whea_rain_suit, vege=vege_rain_suit,
         rape=rape_rain_suit, bean=bean_rain_suit, chic=chic_rain_suit,
         pige=pige_rain_suit, cowp=cowp_rain_suit, lent=lent_rain_suit,
         opul=opul_rain_suit) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate(season="rabi", input="rainfed", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

## annual
annual_irri_suit_df =
    list(sugc=sugc_irri_suit, sugb=sugb_irri_suit, cnut=cnut_irri_suit,
         oilp=oilp_irri_suit, trof=trof_irri_suit, temf=temf_irri_suit,
         bana=bana_irri_suit, plnt=plnt_irri_suit, coco=coco_irri_suit,
         teas=teas_irri_suit, acof=acof_irri_suit, rcof=rcof_irri_suit,
         cass=cass_irri_suit, yams=yams_irri_suit, orts=orts_irri_suit) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate(season="annual", input="irrigated", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

annual_rain_suit_df =
    list(sugc=sugc_rain_suit, sugb=sugb_rain_suit, cnut=cnut_rain_suit,
         oilp=oilp_rain_suit, trof=trof_rain_suit, temf=temf_rain_suit,
         bana=bana_rain_suit, plnt=plnt_rain_suit, coco=coco_rain_suit,
         teas=teas_rain_suit, acof=acof_rain_suit, rcof=rcof_rain_suit,
         cass=cass_rain_suit, yams=yams_rain_suit, orts=orts_rain_suit) %>%
    stack %>%
    as.data.frame(na.rm=FALSE) %>%
    mutate(season="annual", input="rainfed", cell=seq_len(ncell(india_rgn))) %>%
    `[`(cell_ix,)

suit_df =
    kharif_irri_suit_df %>%
    full_join(kharif_rain_suit_df) %>%
    full_join(rabi_irri_suit_df) %>%
    full_join(rabi_rain_suit_df) %>%
    full_join(annual_irri_suit_df) %>%
    full_join(annual_rain_suit_df) %>%
    select(cell, season, input, acof, bana, barl, bean, cass, chic, cnut, coco, cott, cowp, grou, lent, maiz, ocer, ofib, oilp, ooil, opul, orts, pige, plnt, pmil, pota, rape, rcof, rest, rice, sesa, smil, sorg, soyb, sugb, sugc, sunf, swpo, teas, temf, toba, trof, vege, whea, yams) %>% 
    arrange(cell, season, input)

## save object
saveRDS(suit_df, "data/neighb_crop_suit_df.rds")

## ======================================
## process GCAM output
## ======================================

## load GCAM data
devtools::load_all("../GCAM/pkg/rgcam")

db <- addScenario(dbFile="/scratch/projects/GCAM/v4.3/gcam-core/output/database_basexdb",
                  proj="/scratch/projects/GCAM/data/output/proj_full.dat",
                  queryFile="/scratch/projects/GCAM/sample-queries.xml", ## change this?
                  clobber=TRUE)

proj <- loadProject("/scratch/projects/GCAM/data/output/proj_full.dat")
listScenarios(proj)
listQueries(proj)
    
## get agricultural production from GCAM reference scenario
gcam_prod =
    proj %>%
    extract2("Reference") %>%
    extract2("Ag Production by Crop Type") %>%
    gather(year, production,
           -scenario,
           -region,
           -sector,
           -output,
           -Units) %>%
    dplyr::select(-sector) %>%
    mutate(year = gsub("X", "", year)) %>%
    mutate(year = as.numeric(year)) %>%
    filter(!output %in% c("biomass","UnmanagedLand","Forest","NonFoodDemand_Forest")) %>%
    filter(region %in% "India") 

gcam_prod %>%
    ggplot(aes(x=year, y=production, colour=output)) +
    geom_line() +
    labs(x="", y="Production (Mt)") +
    theme(legend.title=element_blank(), legend.position="bottom")+
    guides(colour=guide_legend(ncol=3))

## spread according to crop type
gcam_prod %<>% spread(output, production)

## calibrate GCAM data so that it matches MapSPAM in 2005

## rice
total_production_fun = function(area, yield, ...) {
    area_v = getValues(area)
    yield_v = getValues(yield)
    prod_v = area_v * yield_v
    sum(prod_v, na.rm=TRUE)
}

rice_sf = total_production_fun(rice_harv[["total"]], rice_yield[["total"]]) / 1e3 / 1e6 / gcam_prod[["Rice"]][2]
## rice_sf  = sum(getValues(rice_prod[["total"]]), na.rm=TRUE) / 1e6 / gcam_prod[["Rice"]][2]

## wheat
wheat_sf = total_production_fun(whea_harv[["total"]], whea_yield[["total"]]) / 1e3 / 1e6 / gcam_prod[["Wheat"]][2]
## wheat_sf = sum(getValues(whea_prod[["total"]]), na.rm=TRUE) / 1e6 / gcam_prod[["Wheat"]][2]

## maize/corn
corn_sf = total_production_fun(maiz_harv[["total"]], maiz_yield[["total"]]) / 1e3 / 1e6 / gcam_prod[["Corn"]][2]
## corn_sf  = sum(getValues(maiz_prod[["total"]]), na.rm=TRUE) / 1e6 / gcam_prod[["Corn"]][2]

## other cereals
ocer_total =
    sum(c(total_production_fun(barl_harv[["total"]], barl_yield[["total"]]),
          total_production_fun(sorg_harv[["total"]], sorg_yield[["total"]]),
          total_production_fun(pmil_harv[["total"]], pmil_yield[["total"]]),
          total_production_fun(smil_harv[["total"]], smil_yield[["total"]]),
          total_production_fun(ocer_harv[["total"]], ocer_yield[["total"]])), na.rm=TRUE)
ocer_sf  = ocer_total / 1e3 / 1e6 / gcam_prod[["OtherGrain"]][2]

ocer_barl_frac = total_production_fun(barl_harv[["total"]], barl_yield[["total"]]) / ocer_total
ocer_sorg_frac = total_production_fun(sorg_harv[["total"]], sorg_yield[["total"]]) / ocer_total
ocer_pmil_frac = total_production_fun(pmil_harv[["total"]], pmil_yield[["total"]]) / ocer_total
ocer_smil_frac = total_production_fun(smil_harv[["total"]], smil_yield[["total"]]) / ocer_total
ocer_ocer_frac = total_production_fun(ocer_harv[["total"]], ocer_yield[["total"]]) / ocer_total

## ocer_total =
##     sum(c(getValues(barl_prod[["total"]]),
##           getValues(sorg_prod[["total"]]),
##           getValues(pmil_prod[["total"]]),
##           getValues(smil_prod[["total"]]),
##           getValues(ocer_prod[["total"]])), na.rm=TRUE)

## ocer_sf  = ocer_total / 1e6 / gcam_prod[["OtherGrain"]][2]

## ocer_barl_frac = sum(getValues(barl_prod[["total"]]), na.rm=TRUE) / ocer_total
## ocer_sorg_frac = sum(getValues(sorg_prod[["total"]]), na.rm=TRUE) / ocer_total
## ocer_pmil_frac = sum(getValues(pmil_prod[["total"]]), na.rm=TRUE) / ocer_total
## ocer_smil_frac = sum(getValues(smil_prod[["total"]]), na.rm=TRUE) / ocer_total
## ocer_ocer_frac = sum(getValues(ocer_prod[["total"]]), na.rm=TRUE) / ocer_total

## oils
oil_total =
    sum(c(total_production_fun(soyb_harv[["total"]], soyb_yield[["total"]]),
          total_production_fun(grou_harv[["total"]], grou_yield[["total"]]),
          total_production_fun(sesa_harv[["total"]], sesa_yield[["total"]]),
          total_production_fun(sunf_harv[["total"]], sunf_yield[["total"]]),
          total_production_fun(rape_harv[["total"]], rape_yield[["total"]]),
          total_production_fun(ooil_harv[["total"]], ooil_yield[["total"]])), na.rm=TRUE)
oil_sf  = oil_total / 1e3 / 1e6 / gcam_prod[["OilCrop"]][2]

oil_soyb_frac = total_production_fun(soyb_harv[["total"]], soyb_yield[["total"]]) / oil_total
oil_grou_frac = total_production_fun(grou_harv[["total"]], grou_yield[["total"]]) / oil_total
oil_sesa_frac = total_production_fun(sesa_harv[["total"]], sesa_yield[["total"]]) / oil_total
oil_sunf_frac = total_production_fun(sunf_harv[["total"]], sunf_yield[["total"]]) / oil_total
oil_rape_frac = total_production_fun(rape_harv[["total"]], rape_yield[["total"]]) / oil_total
oil_ooil_frac = total_production_fun(ooil_harv[["total"]], ooil_yield[["total"]]) / oil_total

## oil_total =
##     sum(c(getValues(soyb_prod[["total"]]),
##           getValues(grou_prod[["total"]]),
##           getValues(sesa_prod[["total"]]),
##           getValues(sunf_prod[["total"]]),
##           getValues(rape_prod[["total"]]),
##           getValues(ooil_prod[["total"]])), na.rm=TRUE)

## oil_sf   = oil_total / 1e6 / gcam_prod[["OilCrop"]][2]

## oil_soyb_frac = sum(getValues(soyb_prod[["total"]]), na.rm=TRUE) / oil_total
## oil_grou_frac = sum(getValues(grou_prod[["total"]]), na.rm=TRUE) / oil_total
## oil_sesa_frac = sum(getValues(sesa_prod[["total"]]), na.rm=TRUE) / oil_total
## oil_sunf_frac = sum(getValues(sunf_prod[["total"]]), na.rm=TRUE) / oil_total
## oil_rape_frac = sum(getValues(rape_prod[["total"]]), na.rm=TRUE) / oil_total
## oil_ooil_frac = sum(getValues(ooil_prod[["total"]]), na.rm=TRUE) / oil_total

## fibre
fibre_total =
    sum(c(total_production_fun(cott_harv[["total"]], cott_yield[["total"]]),
          total_production_fun(ofib_harv[["total"]], ofib_yield[["total"]])), na.rm=TRUE)
fibre_sf  = fibre_total / 1e3 / 1e6 / gcam_prod[["FiberCrop"]][2]

fibre_cott_frac = total_production_fun(cott_harv[["total"]], cott_yield[["total"]]) / fibre_total
fibre_ofib_frac = total_production_fun(ofib_harv[["total"]], ofib_yield[["total"]]) / fibre_total

## fibre_total = sum(c(getValues(cott_prod[["total"]]),
##                     getValues(ofib_prod[["total"]])), na.rm=TRUE)
## fibre_sf = fibre_total / 1e6 / gcam_prod[["FiberCrop"]][2]

## fibre_cott_frac = sum(getValues(cott_prod[["total"]]), na.rm=TRUE) / fibre_total
## fibre_ofib_frac = sum(getValues(ofib_prod[["total"]]), na.rm=TRUE) / fibre_total

## palm fruit
palm_total =
    sum(c(total_production_fun(cnut_harv[["total"]], cnut_yield[["total"]]),
          total_production_fun(oilp_harv[["total"]], oilp_yield[["total"]])), na.rm=TRUE)
palm_sf  = palm_total / 1e3 / 1e6 / gcam_prod[["PalmFruit"]][2]

palm_cnut_frac = total_production_fun(cnut_harv[["total"]], cnut_yield[["total"]]) / palm_total
palm_oilp_frac = total_production_fun(oilp_harv[["total"]], oilp_yield[["total"]]) / palm_total

## palm_total = sum(c(getValues(cnut_prod[["total"]]),
##                    getValues(oilp_prod[["total"]])), na.rm=TRUE)

## palm_sf  = palm_total / 1e6 / gcam_prod[["PalmFruit"]][2]

## palm_cnut_frac = sum(getValues(cnut_prod[["total"]]), na.rm=TRUE) / palm_total
## palm_oilp_frac = sum(getValues(oilp_prod[["total"]]), na.rm=TRUE) / palm_total

## sugar crop
sugar_total =
    sum(c(total_production_fun(sugc_harv[["total"]], sugc_yield[["total"]]),
          total_production_fun(sugb_harv[["total"]], sugb_yield[["total"]])), na.rm=TRUE)
sugar_sf  = sugar_total / 1e3 / 1e6 / gcam_prod[["SugarCrop"]][2]

sugar_sugc_frac = total_production_fun(sugc_harv[["total"]], sugc_yield[["total"]]) / sugar_total
sugar_sugb_frac = total_production_fun(sugb_harv[["total"]], sugb_yield[["total"]]) / sugar_total

## sugar_total = sum(c(getValues(sugc_prod[["total"]]),
##                     getValues(sugb_prod[["total"]])), na.rm=TRUE)
## sugar_sf = sugar_total / 1e6 / gcam_prod[["SugarCrop"]][2]

## sugar_sugc_frac = sum(getValues(sugc_prod[["total"]]), na.rm=TRUE) / sugar_total
## sugar_sugb_frac = sum(getValues(sugb_prod[["total"]]), na.rm=TRUE) / sugar_total

## root crops
root_total =
    sum(c(total_production_fun(pota_harv[["total"]], pota_yield[["total"]]),
          total_production_fun(swpo_harv[["total"]], swpo_yield[["total"]]),
          total_production_fun(yams_harv[["total"]], yams_yield[["total"]]),
          total_production_fun(cass_harv[["total"]], cass_yield[["total"]]),
          total_production_fun(orts_harv[["total"]], orts_yield[["total"]])), na.rm=TRUE)
root_sf  = root_total / 1e3 / 1e6 / gcam_prod[["Root_Tuber"]][2]

root_pota_frac = total_production_fun(pota_harv[["total"]], pota_yield[["total"]]) / root_total
root_swpo_frac = total_production_fun(swpo_harv[["total"]], swpo_yield[["total"]]) / root_total
root_yams_frac = total_production_fun(yams_harv[["total"]], yams_yield[["total"]]) / root_total
root_cass_frac = total_production_fun(cass_harv[["total"]], cass_yield[["total"]]) / root_total
root_orts_frac = total_production_fun(orts_harv[["total"]], orts_yield[["total"]]) / root_total

## root_total = sum(c(getValues(pota_prod[["total"]]),
##                    getValues(swpo_prod[["total"]]),
##                    getValues(yams_prod[["total"]]),
##                    getValues(cass_prod[["total"]]),
##                    getValues(orts_prod[["total"]])), na.rm=TRUE)             
## root_sf = root_total / 1e6 / gcam_prod[["Root_Tuber"]][2]

## root_pota_frac = sum(getValues(pota_prod[["total"]]), na.rm=TRUE) / root_total
## root_swpo_frac = sum(getValues(swpo_prod[["total"]]), na.rm=TRUE) / root_total
## root_yams_frac = sum(getValues(yams_prod[["total"]]), na.rm=TRUE) / root_total
## root_cass_frac = sum(getValues(cass_prod[["total"]]), na.rm=TRUE) / root_total
## root_orts_frac = sum(getValues(orts_prod[["total"]]), na.rm=TRUE) / root_total

## misc crops
misc_total =
    sum(c(total_production_fun(bean_harv[["total"]], bean_yield[["total"]]),
          total_production_fun(chic_harv[["total"]], chic_yield[["total"]]),
          total_production_fun(pige_harv[["total"]], pige_yield[["total"]]),
          total_production_fun(lent_harv[["total"]], lent_yield[["total"]]),
          total_production_fun(cowp_harv[["total"]], cowp_yield[["total"]]),
          total_production_fun(opul_harv[["total"]], opul_yield[["total"]]),
          total_production_fun(trof_harv[["total"]], trof_yield[["total"]]),
          total_production_fun(temf_harv[["total"]], temf_yield[["total"]]),
          total_production_fun(bana_harv[["total"]], bana_yield[["total"]]),
          total_production_fun(plnt_harv[["total"]], plnt_yield[["total"]]),
          total_production_fun(acof_harv[["total"]], acof_yield[["total"]]),
          total_production_fun(rcof_harv[["total"]], rcof_yield[["total"]]),
          total_production_fun(coco_harv[["total"]], coco_yield[["total"]]),
          total_production_fun(teas_harv[["total"]], teas_yield[["total"]]),
          total_production_fun(toba_harv[["total"]], toba_yield[["total"]]),
          total_production_fun(vege_harv[["total"]], vege_yield[["total"]])), na.rm=TRUE)
misc_sf  = misc_total / 1e3 / 1e6 / gcam_prod[["MiscCrop"]][2]

misc_bean_frac = total_production_fun(bean_harv[["total"]], bean_yield[["total"]]) / misc_total
misc_chic_frac = total_production_fun(chic_harv[["total"]], chic_yield[["total"]]) / misc_total
misc_pige_frac = total_production_fun(pige_harv[["total"]], pige_yield[["total"]]) / misc_total
misc_lent_frac = total_production_fun(lent_harv[["total"]], lent_yield[["total"]]) / misc_total
misc_cowp_frac = total_production_fun(cowp_harv[["total"]], cowp_yield[["total"]]) / misc_total
misc_opul_frac = total_production_fun(opul_harv[["total"]], opul_yield[["total"]]) / misc_total
misc_trof_frac = total_production_fun(trof_harv[["total"]], trof_yield[["total"]]) / misc_total
misc_temf_frac = total_production_fun(temf_harv[["total"]], temf_yield[["total"]]) / misc_total
misc_bana_frac = total_production_fun(bana_harv[["total"]], bana_yield[["total"]]) / misc_total
misc_plnt_frac = total_production_fun(plnt_harv[["total"]], plnt_yield[["total"]]) / misc_total
misc_acof_frac = total_production_fun(acof_harv[["total"]], acof_yield[["total"]]) / misc_total
misc_rcof_frac = total_production_fun(rcof_harv[["total"]], rcof_yield[["total"]]) / misc_total
misc_coco_frac = total_production_fun(coco_harv[["total"]], coco_yield[["total"]]) / misc_total
misc_teas_frac = total_production_fun(teas_harv[["total"]], teas_yield[["total"]]) / misc_total
misc_toba_frac = total_production_fun(toba_harv[["total"]], toba_yield[["total"]]) / misc_total
misc_vege_frac = total_production_fun(vege_harv[["total"]], vege_yield[["total"]]) / misc_total

## misc_total = sum(c(getValues(bean_prod[["total"]]),
##                    getValues(chic_prod[["total"]]),
##                    getValues(pige_prod[["total"]]),
##                    getValues(lent_prod[["total"]]),
##                    getValues(cowp_prod[["total"]]),
##                    getValues(opul_prod[["total"]]),
##                    getValues(trof_prod[["total"]]),
##                    getValues(temf_prod[["total"]]),
##                    getValues(bana_prod[["total"]]),
##                    getValues(plnt_prod[["total"]]),
##                    getValues(acof_prod[["total"]]),
##                    getValues(rcof_prod[["total"]]),
##                    getValues(coco_prod[["total"]]),
##                    getValues(teas_prod[["total"]]),
##                    getValues(toba_prod[["total"]]),
##                    getValues(vege_prod[["total"]])), na.rm=TRUE)

## misc_sf  = misc_total / 1e6 / gcam_prod[["MiscCrop"]][2]

## misc_bean_frac = sum(getValues(bean_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_chic_frac = sum(getValues(chic_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_pige_frac = sum(getValues(pige_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_lent_frac = sum(getValues(lent_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_cowp_frac = sum(getValues(cowp_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_opul_frac = sum(getValues(opul_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_trof_frac = sum(getValues(trof_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_temf_frac = sum(getValues(temf_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_bana_frac = sum(getValues(bana_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_plnt_frac = sum(getValues(plnt_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_acof_frac = sum(getValues(acof_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_rcof_frac = sum(getValues(rcof_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_coco_frac = sum(getValues(coco_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_teas_frac = sum(getValues(teas_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_toba_frac = sum(getValues(toba_prod[["total"]]), na.rm=TRUE) / misc_total
## misc_vege_frac = sum(getValues(vege_prod[["total"]]), na.rm=TRUE) / misc_total

## translate GCAM scenario into demand
time = seq(2005, 2100, by=5)

gcam_demand =
    gcam_prod %>%
    filter(year %in% time) %>%
    select(-(scenario:year), -FodderGrass, -Pasture) %>%
    mutate(Corn = Corn * corn_sf,
           FiberCrop = FiberCrop * fibre_sf,
           MiscCrop = MiscCrop * misc_sf,
           OilCrop = OilCrop * oil_sf,
           OtherGrain = OtherGrain * ocer_sf,
           PalmFruit = PalmFruit * palm_sf,
           Rice = Rice * rice_sf,
           Root_Tuber = Root_Tuber * root_sf,
           SugarCrop = SugarCrop * sugar_sf,
           Wheat = Wheat * wheat_sf) %>%
    mutate_each(funs(.*1e6))            ## Mt -> t

dmd =
    data.frame(acof=gcam_demand[["MiscCrop"]] * misc_acof_frac,
               bana=gcam_demand[["MiscCrop"]] * misc_bana_frac,
               barl=gcam_demand[["OtherGrain"]] * ocer_barl_frac,
               bean=gcam_demand[["MiscCrop"]] * misc_bean_frac,
               cass=gcam_demand[["Root_Tuber"]] * root_cass_frac,
               chic=gcam_demand[["MiscCrop"]] * misc_chic_frac,
               cnut=gcam_demand[["PalmFruit"]] * palm_cnut_frac,
               coco=gcam_demand[["MiscCrop"]] * misc_coco_frac,
               cott=gcam_demand[["FiberCrop"]] * fibre_cott_frac,
               cowp=gcam_demand[["MiscCrop"]] * misc_cowp_frac,
               grou=gcam_demand[["OilCrop"]] * oil_grou_frac,
               lent=gcam_demand[["MiscCrop"]] * misc_lent_frac,
               maiz=gcam_demand[["Corn"]],
               ocer=gcam_demand[["OtherGrain"]] * ocer_ocer_frac,
               ofib=gcam_demand[["FiberCrop"]] * fibre_ofib_frac,
               oilp=gcam_demand[["PalmFruit"]] * palm_oilp_frac,
               ooil=gcam_demand[["OilCrop"]] * oil_ooil_frac,
               opul=gcam_demand[["MiscCrop"]] * misc_opul_frac,
               orts=gcam_demand[["Root_Tuber"]] * root_orts_frac,
               pige=gcam_demand[["MiscCrop"]] * misc_pige_frac,
               plnt=gcam_demand[["MiscCrop"]] * misc_plnt_frac,
               pmil=gcam_demand[["OtherGrain"]] * ocer_pmil_frac,
               pota=gcam_demand[["Root_Tuber"]] * root_pota_frac,
               rape=gcam_demand[["OilCrop"]] * oil_rape_frac,
               rcof=gcam_demand[["MiscCrop"]] * misc_rcof_frac,
               rest=gcam_demand[["MiscCrop"]] * misc_toba_frac,
               rice=gcam_demand[["Rice"]],
               sesa=gcam_demand[["OilCrop"]] * oil_sesa_frac,
               smil=gcam_demand[["OtherGrain"]] * ocer_smil_frac,
               sorg=gcam_demand[["OtherGrain"]] * ocer_sorg_frac,
               soyb=gcam_demand[["OilCrop"]] * oil_soyb_frac,
               sugb=gcam_demand[["SugarCrop"]] * sugar_sugb_frac,
               sugc=gcam_demand[["SugarCrop"]] * sugar_sugc_frac, ## annual dmd
               sunf=gcam_demand[["OilCrop"]] * oil_sunf_frac,
               swpo=gcam_demand[["Root_Tuber"]] * root_swpo_frac,
               teas=gcam_demand[["MiscCrop"]] * misc_teas_frac,
               temf=gcam_demand[["MiscCrop"]] * misc_temf_frac,
               toba=gcam_demand[["MiscCrop"]] * misc_toba_frac,
               trof=gcam_demand[["MiscCrop"]] * misc_trof_frac,
               vege=gcam_demand[["MiscCrop"]] * misc_vege_frac, ## NB vegetables also grown in kharif
               whea=gcam_demand[["Wheat"]],
               yams=gcam_demand[["Root_Tuber"]] * root_yams_frac) %>%
    round %>% as.matrix

## save object
saveRDS(dmd, "data/gcam_reference_demand.rds")









## not used:

## library(Rcpp)
## cppFunction(code='int myfun(NumericVector x) {
##   return std::find_if(x.begin(), x.end(), [](int i){ return i > 0;});
## }')

## cppFunction(code='bool myfun(NumericMatrix x, int index) {
##   NumericVector xx = x(_,index);
##   IntegerVector index2 = IntegerVector::create(0,1,2);
##   NumericVector xxx = xx[index2];
##   return is_true(any(xxx > 0));
## }')

## cppFunction(code='IntegerVector seqC(int start, int len) {
##   return seq(start, start + len);
## }')

## cppFunction(code='int start(IntegerVector x) {
##   return *x.begin();
## }')

## cppFunction(code='double sumC(NumericMatrix x, int index) {
##   return sum(x(_,index));
## }')

## cppFunction(code='double sumC(NumericVector x, int index) {
##   NumericVector xx = x[index];
##   return sum(xx);
## }')

## cppFunction(code='NumericVector transformEx2(NumericVector x, NumericVector y) {
##     NumericVector z(x.size());
##     std::transform(x.begin(), x.end(), y.begin(), z.begin(), std::plus<double>());
##     return z;
## }')

## cppFunction(code='double myfun(NumericMatrix x, int index) {
##   return sum(x(_,index)) - sum(x(_,2));
## }')

## cppFunction(code='IntegerVector ifelseC(NumericVector x, NumericVector y) {
##   return ifelse(x < y,1,0);
## }')

## cppFunction(code='int remove_first(IntegerVector x) {
##   IntegerVector xx = x;
##   xx.erase(0);
##   int mx = which_max(xx) + 1;
##   return mx;
## }')

## cppFunction(code='NumericMatrix myfun(NumericMatrix x, int index) {
##   NumericVector v = x(index,_);
##   int n = v.size();
##   for (int i = 0; i < n; i++) {
##     v[i] += 10;
##   }
##   x(index,_) = v;
##   return x;
## }')

## cppFunction(code='NumericVector sum_interval(NumericVector x, int interval) {
##   int n = x.size();
##   if (n % interval != 0) {
##     stop("The number of elements in x must be a multiple of the supplied interval");
##   } 
##   int nn = n / interval;
##   NumericVector res(nn);
##   for (int i = 0; i < nn; i++) {
##     int start = i * interval;
##     int end = start + (interval - 1);
##     IntegerVector index = seq(start, end);
##     NumericVector v = x[index];
##     res[i] = sum(v);
##   }
##   return res;
## }')

## ## cppFunction(code='NumericVector myfun(NumericMatrix x) {
## ##   return rowSums(x);
## ## }')

## cppFunction(code='int rand(int n) {
##   int ix = (sample(n, 1) - 1)[0];
##   return ix;
## }')

## cppFunction(code='int count(LogicalVector y) {
##   int ct = std::count(y.begin(), y.end(), true);
##   return ct;
## }')

## cppFunction(code='int match_stringC(String x, CharacterVector y) {
##   CharacterVector::iterator it = std::find(y.begin(), y.end(), x);
##   return it - y.begin();
## }')

## cppFunction(code='int which_maxC(NumericVector x) {
##   int n = x.size();
##   int idx = 0;
##   double val = x[0];
##   for (int i = 1; i < n; i++) {
##     if (x[i] > val) {
##       idx = i;
##       val = x[i];
##     }
##   }
##   return idx;
## }')

## cppFunction(code='LogicalVector match_stringC(CharacterVector x, CharacterVector y) {
##   int nx = x.size();
##   int ny = y.size();
##   LogicalVector out(nx);

##   for (int i = 0; i < nx; i++ ) {
##     bool res = false;
##     int j = 0;
##     while (res == false && j < ny) {
##       if (x[i] == y[j]) {
##         res = true;
##       }
##       j++;
##     }
##     out[i] = res;
##   }
##   return out;
## }')

## cppFunction('bool gtzero(NumericVector x) {
##   int n = x.size();
##   int i = 0;
##   bool flag = false;
##   while (flag == false && i < n) {
##     if (x[i] > 0) {
##       flag = true;
##     }
##     i++;
##   }
##   return flag;
## }')

## cppFunction('NumericVector myfun(NumericMatrix x, int n_season, int n_input) {
##   int ncol = x.ncol();
##   int row_ix = 0;
##   NumericVector total_season_area(n_season);
##   for (int i = 0; i < n_season; i++) {
##     double total_season = 0;
##     for (int j = 0; j < n_input; j++) {
##       double total_input = 0;
##       int ij = row_ix + (i * n_input) + j;
##       for (int k = 0; k < ncol; k++) {
##         total_input += x(ij,k);
##       }
##       total_season += total_input;
##     }
##     total_season_area[i] += total_season;
##   }
##   return total_season_area;
## }')

## cppFunction('double mat_idx(NumericMatrix x, int i, int j) {
##   double out = x(i,j);
##   return out;
## }')
## cppFunction('IntegerVector sq(NumericVector x) {
##   IntegerVector out = seq_len(x.size()) - 1;
##   return out;
## }')
## cppFunction('NumericVector index_vec(NumericVector x, LogicalVector idx) {
##   return x[idx];
## }')
## cppFunction('NumericVector add_vec(NumericVector x, double y) {
##   int n = x.size();
##   for (int i = 0; i < n; i++) {
##     x[i] = x[i] + y;
##   }
##   return x;
## }')

## calculate_change = function(area, ...) {
##     UseMethod("calculate_change", area)
## }

## calculate_change.numeric = function(area, area1, yield, demand, ...) {
##     calculate_change(matrix(area, nrow=1), matrix(area1, nrow=1), matrix(yield, nrow=1), demand, ...)
## }

## calculate_change.matrix = function(area, area1, yield, demand, ...) {

##     if (!isTRUE(all.equal(dim(area), dim(area1), dim(yield)))) {
##         stop()
##     }

##     area[is.na(area)] = 0
##     area1[is.na(area1)] = 0
##     yield[is.na(yield)] = 0

##     prod = area * yield   ## initial production
##     prod1 = area1 * yield

##     prod_change = colSums(prod1, na.rm=TRUE) - colSums(prod, na.rm=TRUE)

##     demand = demand - prod_change
##     demand
## }

## allocate_crop_conversion = function(area, ...) {
##     UseMethod("allocate_crop_conversion", area)
## }

## allocate_crop_conversion.numeric = function(area, ...) {
##     allocate_crop_conversion(matrix(area, nrow=1), ...)
## }

## allocate_crop_conversion.matrix = function(area, crop_ix, decr_ix, cell_area, fact, ...) {
##     ## conversion of crops with decreasing demand (not currently stochastic)
##     ##
##     ## Args:
##     ##   area      : matrix with columns corresponding to areas of individual
##     ##               crops and rows to various input levels
##     ##   crop_ix   : numeric. Column of 'area' containing data for the crop under consideration
##     ##   decr_ix   : numeric. Columns of crops with decreasing demand overall
##     ##   cell_area : numeric. Area of grid cell under consideration.
##     ##   fact      : numeric. Factor controlling the amount of change.
##     ##   ...       : additional arguments (not used)
##     ##
##     ## Return:
##     ##   matrix with updated crop area

##     area1 = area ## make a working copy

##     ## check if there are any crops with decreasing demand - if not, simply return 'area' unchanged
##     if (any(decr_ix)) {

##         n_level = nrow(area)
##         total_area = colSums(area, na.rm=TRUE)

##         ar = cell_area * fact ## amount by which area will change

##         total_decr_area = total_area[decr_ix]
##         sum_total_decr_area = sum(total_decr_area, na.rm=TRUE)

##         if (sum_total_decr_area > 0) {

##             if (sum_total_decr_area < ar) {
##                 ar = sum_total_decr_area
##             }
            
##             ## first, subtract area from the total area, then disaggregate the change
##             total_decr_area = total_decr_area - (ar * (total_decr_area / sum(total_decr_area)))
##             total_area[decr_ix] = total_decr_area

##             ## split the decrease in crop area between the various input levels (frac is a matrix showing
##             ## the fraction of the total area in each input level)
##             if (n_level > 1) {
##                 frac = area1
##                 frac[,total_area > 0] = t(t(area1[,total_area > 0]) / total_area[total_area > 0]) ## t(t(...)) ensures total_area is recycled for every row
##                 area1 = t(t(frac) * total_area) ## this disaggregates the change amongst crops with decreasing demand
##                 decr = apply(area - area1, 1, sum, na.rm=TRUE) ## total decrease, by input level
##                 incr = abs(decr) ## total increase
##                 area1[,crop_ix] = area1[,crop_ix] + incr
##             } else {
##                 area1 = total_area1 ## simpler, because it is not necessary to disaggregate by input level
##                 decr = sum(area - area1, na.rm=TRUE)
##                 incr = abs(decr)
##                 area1[crop_ix] = area1[crop_ix] + incr
##             }
##         }
##     }
##     area1
## }

## allocate_expansion = function(area, ...) {
##     UseMethod("allocate_expansion", area)
## }

## allocate_expansion.numeric = function(area, suit, ...) {
##     allocate_expansion(matrix(area, nrow=1), matrix(suit, nrow=1), ...)
## }

## allocate_expansion.matrix = function(area, suit, crop_ix, cropland_area, cell_area, fact, ...) {

##     area1 = area
##     alloc_area = sum(as.numeric(area), na.rm=TRUE) ## total amount of land allocated
##     n_level = nrow(area)

##     if (!isTRUE(all.equal(dim(area), dim(suit)))) {
##         stop()
##     }

##     if (alloc_area < cropland_area) {

##         ar = cell_area * fact
##         ## rand = runif(1)
##         rand = 0
        
##         if (n_level == 1) {

##             if (suit[crop_ix] > rand) {
##                 area1[crop_ix] = area[crop_ix] + ar
##             }

##         } else {

##             area_all_levels = area[,crop_ix,drop=TRUE]
##             suit_all_levels = suit[,crop_ix,drop=TRUE]
##             total_area = sum(area_all_levels)
##             incr = rep(0, n_level)

##             ## this accounts for the fact that some input levels may be more or less suitable
##             for (i in 1:n_level) {
##                 if (suit_all_levels[i] > rand) {
##                     incr[i] = incr[i] + ar
##                 }
##             }
##             incr = incr / length(which(incr > 0)) ## adjust so that total change equals ar
##             area_all_levels = area_all_levels + incr 
##             area1[,crop_ix] = area_all_levels
##         }
##     }
##     area1
## }



## allocate_intensification = function(area, ...) {
##     UseMethod("allocate_intensification", area)
## }

## allocate_intensification.numeric = function(area, suit, ...) {
##     warning("cannot perform intensification because 'area' contains one input level")
##     area
## }

## allocate_intensification.matrix = function(area, suit, crop_ix, cell_area, fact, stochastic=TRUE, ...) {
##     ## lower input level -> higher input level

##     area1 = area
##     n_level = nrow(area)

##     if (n_level == 1) {
##         warning("cannot perform intensification because 'area' contains only one input level")
##         area
##     }
    
##     if (!isTRUE(all.equal(dim(area), dim(suit)))) {
##         stop()
##     }

##     area_all_levels = area[,crop_ix]
##     area_all_levels[is.na(area_all_levels)] = 0

##     suit_all_levels = suit[,crop_ix]

##     sum_area_all_levels = sum(area_all_levels, na.rm=TRUE)
    
##     ## rand = runif(1)
##     rand=0

##     if (sum_area_all_levels > 0) {
        
##         for (i in 1:(n_level - 1)) {
##             ix = n_level - (i-1)
##             crop_area1 = area_all_levels[ix-1] ## higher intensity
##             crop_area2 = area_all_levels[ix]   ## lower intensity

##             ar = cell_area * fact

##             if (crop_area2 > 0) {
                
##                 if (crop_area2 < ar) {
##                     ar = crop_area2
##                 }

##                 if (stochastic) {
##                     if (suit_all_levels[ix-1] > rand) {
##                         crop_area1 = crop_area1 + ar
##                         crop_area2 = crop_area2 - ar
##                     }
                    
##                 } else {
##                     crop_area1 = crop_area1 + ar
##                     crop_area2 = crop_area2 - ar
##                 }

##                 area_all_levels[ix-1] = crop_area1
##                 area_all_levels[ix] = crop_area2
##             }
            
##         }
##     }
##     area1[,crop_ix] = area_all_levels
##     area1

## }










## not used:
                
            ## allocate_crop_conversion = function(irri_area, rain_area, crop_ix, decr_ix, cell_area, fact, ...) {
            ##     ## conversion of crops with decreasing demand (not currently stochastic)
                
            ##     if (any(decr_ix)) {
                    
            ##         crop_irri_area = irri_area[crop_ix]
            ##         crop_rain_area = rain_area[crop_ix]

            ##         crop_irri_area1 = crop_irri_area
            ##         crop_rain_area1 = crop_rain_area

            ##         ar = cell_area * fact ## amount by which area will be changed

            ##         decr_irri_area = irri_area[decr_ix]
            ##         decr_rain_area = rain_area[decr_ix]

            ##         decr_irri_area1 = decr_irri_area
            ##         decr_rain_area1 = decr_rain_area
                    
            ##         if (sum(c(decr_irri_area, decr_rain_area), na.rm=TRUE) > ar) {
            ##             crop_irri_area1 =
            ##                 crop_irri_area + ar * (sum(decr_irri_area) / (sum(decr_irri_area) + sum(decr_rain_area)))
            ##             crop_rain_area1 =
            ##                 crop_rain_area + ar * (sum(decr_rain_area) / (sum(decr_irri_area) + sum(decr_rain_area)))
            ##             decr_irri_area1 =
            ##                 decr_irri_area - ((irri_area1 - irri_area) * (decr_irri_area / sum(decr_irri_area)))
            ##             decr_rain_area1 =
            ##                 decr_rain_area - ((rain_area1 - rain_area) * (decr_rain_area / sum(decr_rain_area)))
            ##         }
                    
            ##         irri_area[crop_ix] = crop_irri_area1
            ##         rain_area[crop_ix] = crop_rain_area1

            ##         irri_area[decr_ix] = decr_irri_area1
            ##         rain_area[decr_ix] = decr_rain_area1
            ##     }

            ##     list(irri_area=irri_area, rain_area=rain_area)
                
            ## }
                    
            ##         ## decr_irri_chng = decr_irri1 - decr_irri
            ##         ## decr_rain_chng = decr_rain1 - decr_rain
            ##         ## decr_prod_chng = (decr_irri_chng * as.numeric(kharif_irri_yield_df[cell,decr]) +
            ##         ##                   decr_rain_chng * as.numeric(kharif_irri_yield_df[cell,decr]))
                    
            ##         ## dmd1[decr] = dmd1[decr] - decr_prod_chng

            ##         ## kharif_irri_area_df[cell,decr] = kharif_irri_area_df[cell,decr] + decr_irri_chng
            ##         ## kharif_rain_area_df[cell,decr] = kharif_rain_area_df[cell,decr] + decr_rain_chng
                    
            ##     }
                
            ##     if (rain_area > ar) {  ## 2: intensification (rainfed -> irrigated)
            ##         if (irri_suit > rand) {
            ##             irri_area1 = irri_area + ar
            ##             rain_area1 = rain_area - ar
            ##         }
            ##     }

            ##     ## return value?
                
            ## }
            

            ## total_kharif_area = sum(c(as.numeric(kharif_irri_area_df[cell,]),
            ##                           as.numeric(kharif_rain_area_df[cell,])), na.rm=TRUE)

            ## total_annual_area = sum(c(as.numeric(annual_irri_area_df[cell,]),
            ##                           as.numeric(annual_rain_area_df[cell,])), na.rm=TRUE)

## TODO: recalculate decr
## TODO: formulate break statement

## if (alloc_area < cropland) {  ## 1: expansion

##     if (rain_suit > rand & irri_suit > rand) {
##         ar = xx[["cell_area"]] * fact
##         irri1 = irri0 + (irri0 / (irri0 + rain0)) * ar
##         rain1 = rain0 + (rain0 / (irri0 + rain0)) * ar
##     } else if (rain_suit > rand & irri_suit <= rand) {
##         ar = xx[["cell_area"]] * fact
##         irri1 = irri0
##         rain1 = rain0 + ar
##     } else if (rain_suit <= rand & irri_suit > rand) {
##         ar = xx[["cell_area"]] * fact
##         irri1 = irri0 + ar
##         rain1 = rain0
##     }


## pct =
##     data.frame(crop=c("bean","chic","pige","lent","opul","trof","temf","bana","plnt","acof","rcof","coco","teas","toba"),
##                prod=c(sum(getValues(bean_prod[["total"]]), na.rm=TRUE),
##                       sum(getValues(chic_prod[["total"]]), na.rm=TRUE),
##                       sum(getValues(pige_prod[["total"]]), na.rm=TRUE),
##                       sum(getValues(lent_prod[["total"]]), na.rm=TRUE),
##                       sum(getValues(opul_prod[["total"]]), na.rm=TRUE),
##                       sum(getValues(trof_prod[["total"]]), na.rm=TRUE),
##                       sum(getValues(temf_prod[["total"]]), na.rm=TRUE),
##                       sum(getValues(bana_prod[["total"]]), na.rm=TRUE),
##                       sum(getValues(plnt_prod[["total"]]), na.rm=TRUE),
##                       sum(getValues(acof_prod[["total"]]), na.rm=TRUE),
##                       sum(getValues(rcof_prod[["total"]]), na.rm=TRUE),
##                       sum(getValues(coco_prod[["total"]]), na.rm=TRUE),
##                       sum(getValues(teas_prod[["total"]]), na.rm=TRUE),
##                       sum(getValues(toba_prod[["total"]]), na.rm=TRUE)))

## pct = pct[order(pct$prod),]
## pct$prod = round(pct$prod / sum(pct$prod), digits=2)

## ## to start we want to get the proportion of the various crops that make up the
## ## GCAM aggregate classes in each region

## ## Wheat
## wheat = raster(file.path("data", "mapspam_global_harv_area", "SPAM2005V3r1_global_H_TI_WHEA_I.tif")) %>% crop(india_ext)

## ## Rice
## rice = raster(file.path("data", "mapspam_global_harv_area", "SPAM2005V3r1_global_H_TI_RICE_I.tif")) %>% crop(india_ext)

## ## Maize
## maize = raster(file.path("data", "mapspam_global_harv_area", "SPAM2005V3r1_global_H_TI_MAIZ_I.tif")) %>% crop(india_ext)

## ## OtherGrain
## othergrain =
##     list.files(file.path("data", "mapspam_global_harv_area"), "^SPAM2005V3r1_global_H_TA_(BARL|PMIL|SMIL|SORG|OCER)_A.tif$", full.names=TRUE) %>%
##     stack %>% crop(india_ext)

## ## Root_Tuber
## roottuber =
##     list.files("data/mapspam_global_harv_area", pattern="^SPAM2005V3r1_global_H_TI_(POTA|SWPO|YAMS|CASS|ORTS)_I.tif$", full.names=TRUE) %>%
##     stack %>% crop(india_ext)

## ## MiscCrop
## misccrop =
##     list.files("data/mapspam_global_harv_area", pattern="^SPAM2005V3r1_global_H_TI_(BEAN|CHIC|COWP|PIGE|LENT|OPUL|BANA|PLNT|TROF|TEMF|VEGE|ACOF|RCOF|COCO|TEAS|TOBA|REST)_I.tif$", full.names=TRUE) %>%
##     stack %>% crop(india_ext)

## ## OilCrop
## oilcrop =
##     list.files("data/mapspam_global_harv_area", pattern="^SPAM2005V3r1_global_H_TI_(SOYB|GROU|SUNF|RAPE|SESA|OOIL)_I.tif$", full.names=TRUE) %>%
##     stack %>% crop(india_ext)

## ## palm fruit
## palmfruit =
##     list.files("data/mapspam_global_harv_area", pattern="^SPAM2005V3r1_global_H_TI_(CNUT|OILP)_I.tif$", full.names=TRUE) %>%
##     stack %>% crop(india_ext)

## ## SugarCrop
## sugarcrop =
##     list.files("data/mapspam_global_harv_area", pattern="^SPAM2005V3r1_global_H_TI_(SUGC|SUGB)_I.tif$", full.names=TRUE) %>%
##     stack %>% crop(india_ext)

## ## FiberCrop
## fibercrop =
##     list.files("data/mapspam_global_harv_area", pattern="^SPAM2005V3r1_global_H_TI_(COTT|OFIB)_I.tif$", full.names=TRUE) %>%
##     stack %>% crop(india_ext)

## st = stack(wheat,maize,othergrain,roottuber,misccrop,oilcrop,palmfruit,sugarcrop,fibercrop)
## xx = stackApply(st, indices=rep(1, nlayers(st)), fun=sum) / ar

## ## ======================================
## ## area of each 5 arcminute grid cell
## ## ======================================

## ## regular grid, so in fact all we need to do is calculate area for a single longitude

## template = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TI_WHEA_I.tif")
## r = raster(xmn=0, xmx=0 + res(template)[1], ymn=-90, ymx=90, resolution=res(template)[1])
## poly = as(r, "SpatialPolygons")

## library(geosphere)
## ar = areaPolygon(poly) / 1000 / 1000 * 100 ## m2 -> km2 -> Ha
## ar = raster(matrix(data=rep(ar, 4320), nrow=2160, ncol=4320), xmn=-180, xmx=180, ymn=-90, ymx=90)
## ar = crop(ar, template)

## ## irri = list.files("data/mapspam_global_phys_area", pattern="^SPAM2005V3r1_global_A_TI_[A-Z]{4}_I.tif$", full.names=TRUE)
## ## rain = list.files("data/mapspam_global_phys_area", pattern="^SPAM2005V3r1_global_A_TR_[A-Z]{4}_R.tif$", full.names=TRUE)

## x = list.files("data/mapspam_global_harv_area", pattern="^SPAM2005V3r1_global_H_TA_[A-Z]{4}_A.tif$", full.names=TRUE) %>% stack
## nms = names(x)
## x = unstack(x) %>% setNames(nms)
## x = lapply(x, FUN=function(x) x / ar)

## gcam_region = raster(file.path("data", "gcam_32rgn_rast_ll.tif"))
## india_region = gcam_region
## india_region[india_region != 17] = NA

## india_ext = as(india_region, "SpatialPoints") %>% extent
## xx = stack(x) %>% setNames(nms) %>% crop(india_ext)

## test = stackApply(xx, indices=rep(1, nlayers(xx)), fun=sum)

## ## othergrain
## v = myfun(mapspam=othergrain, region_map=gcam_region, region_id=17)
## tot = sort(colSums(v, na.rm=TRUE))
## round(tot / sum(tot) * 100)

## ## roottuber
## v = myfun(mapspam=roottuber, region_map=gcam_region, region_id=17)
## tot = sort(colSums(v, na.rm=TRUE))
## round(tot / sum(tot) * 100)

## ## misccrop
## v = myfun(mapspam=misccrop, region_map=gcam_region, region_id=17)
## tot = sort(colSums(v, na.rm=TRUE))
## round(tot / sum(tot) * 100)

## ## oilcrop
## v = myfun(mapspam=oilcrop, region_map=gcam_region, region_id=17)
## tot = sort(colSums(v, na.rm=TRUE))
## round(tot / sum(tot) * 100)

## ## palmfruit
## v = myfun(mapspam=palmfruit, region_map=gcam_region, region_id=17)
## tot = sort(colSums(v, na.rm=TRUE))
## round(tot / sum(tot) * 100)

## ## sugarcrop
## v = myfun(mapspam=sugarcrop, region_map=gcam_region, region_id=17)
## tot = sort(colSums(v, na.rm=TRUE))
## round(tot / sum(tot) * 100)

## ## fibercrop
## v = myfun(mapspam=fibercrop, region_map=gcam_region, region_id=17)
## tot = sort(colSums(v, na.rm=TRUE))
## round(tot / sum(tot) * 100)
    
## ## ======================================
## ## wheat
## ## ======================================

## wheat_irri = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TI_WHEA_I.tif")
## wheat_rain = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TR_WHEA_R.tif")
## wheat_frac = (wheat_irri + wheat_rain) / ar

## ## ======================================
## ## rice
## ## ======================================

## rice_irri  = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TI_RICE_I.tif")
## rice_rain  = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TR_RICE_R.tif")
## rice_frac  = (rice_irri + rice_rain) / ar

## ## ======================================
## ## maize
## ## ======================================

## maize_irri = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TI_MAIZ_I.tif")
## maize_rain = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TR_MAIZ_R.tif")
## maize_frac = (maize_irri + maize_rain) / ar

## ## ======================================
## ## other grain
## ## ======================================

## ## 4	   barley		barl      OtherGrain
## ## 5	   pearl millet	        pmil      OtherGrain
## ## 6	   small millet	        smil      OtherGrain
## ## 7	   sorghum		sorg      OtherGrain
## ## 8	   other cereals	ocer      OtherGrain

## othgrain_irri_fs = list.files("data/mapspam_global_phys_area",
##                               pattern="^SPAM2005V3r1_global_A_TI_(BARL|PMIL|SMIL|SORG|OCER)_I.tif$", full.names=TRUE)
## othgrain_irri =
##     stack(othgrain_irri_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## othgrain_rain_fs = list.files("data/mapspam_global_phys_area",
##                               pattern="^SPAM2005V3r1_global_A_TR_(BARL|PMIL|SMIL|SORG|OCER)_R.tif$", full.names=TRUE)
## othgrain_rain =
##     stack(othgrain_rain_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## othgrain_frac = (othgrain_irri + othgrain_rain) / ar

## ## ======================================
## ## root, tuber
## ## ======================================

## ## 9	   potato		pota      RootTuber
## ## 10	   sweet potato	        swpo      RootTuber
## ## 11	   yams		        yams      RootTuber
## ## 12	   cassava		cass      RootTuber
## ## 13	   other roots	        orts      RootTuber 

## rtub_irri_fs = list.files("data/mapspam_global_phys_area",
##                           pattern="^SPAM2005V3r1_global_A_TI_(POTA|SWPO|YAMS|CASS|ORTS)_I.tif$", full.names=TRUE)

## rtub_irri =
##     stack(rtub_irri_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## rtub_rain_fs = list.files("data/mapspam_global_phys_area",
##                           pattern="^SPAM2005V3r1_global_A_TR_(POTA|SWPO|YAMS|CASS|ORTS)_R.tif$", full.names=TRUE)
## rtub_rain =
##     stack(rtub_rain_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## rtub_frac = (rtub_irri + rtub_rain) / ar

## ## ======================================
## ## misc. crops
## ## ======================================

## ## 14	   bean		        bean      MiscCrop
## ## 15	   chickpea		chic      MiscCrop
## ## 16	   cowpea		cowp      MiscCrop
## ## 17	   pigeonpea	        pige      MiscCrop
## ## 18	   lentil		lent      MiscCrop
## ## 19	   other pulses	        opul      MiscCrop
## ## 37	   banana		bana      MiscCrop
## ## 38	   plantain		plnt      MiscCrop
## ## 39	   tropical fruit	trof      MiscCrop
## ## 40	   temperate fruit	temf      MiscCrop
## ## 41	   vegetables	        vege      MiscCrop
## ## 32	   arabica coffee	acof      MiscCrop
## ## 33	   robusta coffee	rcof      MiscCrop
## ## 34	   cocoa		coco      MiscCrop
## ## 35	   tea		        teas      MiscCrop
## ## 36	   tobacco		toba      MiscCrop
## ## 42	   rest of crops	rest      MiscCrop
    
## misc_irri_fs = list.files("data/mapspam_global_phys_area",
##                           pattern="^SPAM2005V3r1_global_A_TI_(BEAN|CHIC|COWP|PIGE|LENT|OPUL|BANA|PLNT|TROF|TEMF|VEGE|ACOF|RCOF|COCO|TEAS|TOBA|REST)_I.tif$", full.names=TRUE)

## misc_irri =
##     stack(misc_irri_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## misc_rain_fs = list.files("data/mapspam_global_phys_area",
##                           pattern="^SPAM2005V3r1_global_A_TR_(BEAN|CHIC|COWP|PIGE|LENT|OPUL|BANA|PLNT|TROF|TEMF|VEGE|ACOF|RCOF|COCO|TEAS|TOBA|REST)_R.tif$", full.names=TRUE)

## misc_rain =
##     stack(misc_rain_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## misc_frac = (misc_irri + misc_rain) / ar

## ## ======================================
## ## oil crop
## ## ======================================

## ## 20	   soybean		soyb      OilCrop
## ## 21	   groundnut	        grou      OilCrop
## ## 24	   sunflower	        sunf      OilCrop
## ## 25	   rapeseed		rape      OilCrop
## ## 26	   sesameseed	        sesa      OilCrop
## ## 27	   other oil crops	ooil      OilCrop

## oil_irri_fs = list.files("data/mapspam_global_phys_area",
##                           pattern="^SPAM2005V3r1_global_A_TI_(SOYB|GROU|SUNF|RAPE|SESA|OOIL)_I.tif$", full.names=TRUE)

## oil_irri =
##     stack(misc_irri_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## oil_rain_fs = list.files("data/mapspam_global_phys_area",
##                           pattern="^SPAM2005V3r1_global_A_TR_(SOYB|GROU|SUNF|RAPE|SESA|OOIL)_R.tif$", full.names=TRUE)

## oil_rain =
##     stack(misc_rain_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## oil_frac = (oil_irri + oil_frac) / ar

## ## ======================================
## ## palm fruit
## ## ======================================

## ## 22	   coconut		cnut      PalmFruit
## ## 23	   oilpalm		oilp      PalmFruit

## palm_irri_fs = list.files("data/mapspam_global_phys_area",
##                           pattern="^SPAM2005V3r1_global_A_TI_(CNUT|OILP)_I.tif$", full.names=TRUE)

## palm_irri =
##     stack(palm_irri_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## palm_rain_fs = list.files("data/mapspam_global_phys_area",
##                           pattern="^SPAM2005V3r1_global_A_TR_(CNUT|OILP)_R.tif$", full.names=TRUE)

## cpalm_rain =
##     stack(palm_rain_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## palm_frac = (palm_irri + palm_frac) / ar

## ## ======================================
## ## sugar crop
## ## ======================================

## ## 28	   sugarcane	        sugc      SugarCrop
## ## 29	   sugarbeet	        sugb      SugarCrop

## sugar_irri_fs = list.files("data/mapspam_global_phys_area",
##                            pattern="^SPAM2005V3r1_global_A_TI_(SUGC|SUGB)_I.tif$", full.names=TRUE)

## sugar_irri =
##     stack(sugar_irri_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## sugar_rain_fs = list.files("data/mapspam_global_phys_area",
##                            pattern="^SPAM2005V3r1_global_A_TR_(SUGC|SUGB)_R.tif$", full.names=TRUE)

## sugar_rain =
##     stack(sugar_rain_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## sugar_frac = (sugar_irri + sugar_frac) / ar

## ## ======================================
## ## fiber crop
## ## ======================================

## ## 30	   cotton		cott      FiberCrop
## ## 31	   other fibre crops	ofib      FiberCrop

## fiber_irri_fs = list.files("data/mapspam_global_phys_area",
##                            pattern="^SPAM2005V3r1_global_A_TI_(COTT|OFIB)_I.tif$", full.names=TRUE)

## fiber_irri =
##     stack(fiber_irri_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## fiber_rain_fs = list.files("data/mapspam_global_phys_area",
##                            pattern="^SPAM2005V3r1_global_A_TR_(COTT|OFIB)_R.tif$", full.names=TRUE)

## fiber_rain =
##     stack(fiber_rain_fs) %>%
##     stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

## fiber_frac = (fiber_irri + fiber_frac) / ar




## not used:

## myfun = function(mapspam, region_map, region_id, ...) {
##     region_map[region_map != region_id] = NA
##     xy = as(region_map, "SpatialPoints")
##     vals = mapspam[xy]
##     vals
## }

## x = gcam_region
## x[x != 28] = NA  ## we could divide larger regions into chunks
## poly = as(x, "SpatialPolygons")
## x = crop(x, extent(poly))
## xx = disaggregate(x, fact=10)
## y = raster::extract(xx, poly) ## y is a list
## ## crop_vals = raster::extract(st, poly)

## ## required data:
## ## - cropland area   : GlobCover (Global)
## ## - crop calendar   : MIRCA2000

## ## this should be done in an equal area projection - Eckert IV?





## Food crops:
## **************
## crop #  name 		SPAM name GCAM name
## 1	   wheat		whea      Wheat
## 2	   rice		        rice      Rice
## 3	   maize		maiz      Corn
## 4	   barley		barl      OtherGrain
## 5	   pearl millet	        pmil      OtherGrain
## 6	   small millet	        smil      OtherGrain
## 7	   sorghum		sorg      OtherGrain
## 8	   other cereals	ocer      OtherGrain
## 9	   potato		pota      RootTuber
## 10	   sweet potato	        swpo      RootTuber
## 11	   yams		        yams      RootTuber
## 12	   cassava		cass      RootTuber
## 13	   other roots	        orts      RootTuber 
## 14	   bean		        bean      MiscCrop
## 15	   chickpea		chic      MiscCrop
## 16	   cowpea		cowp      MiscCrop
## 17	   pigeonpea	        pige      MiscCrop
## 18	   lentil		lent      MiscCrop
## 19	   other pulses	        opul      MiscCrop
## 20	   soybean		soyb      OilCrop
## 21	   groundnut	        grou      OilCrop
## 22	   coconut		cnut      PalmFruit
## 37	   banana		bana      MiscCrop
## 38	   plantain		plnt      MiscCrop
## 39	   tropical fruit	trof      MiscCrop
## 40	   temperate fruit	temf      MiscCrop
## 41	   vegetables	        vege      MiscCrop

## Non-food crops:
## *******************
## crop #  name 		SPAM name GCAM name
## 23	   oilpalm		oilp      PalmFruit
## 24	   sunflower	        sunf      OilCrop
## 25	   rapeseed		rape      OilCrop
## 26	   sesameseed	        sesa      OilCrop
## 27	   other oil crops	ooil      OilCrop
## 28	   sugarcane	        sugc      SugarCrop
## 29	   sugarbeet	        sugb      SugarCrop
## 30	   cotton		cott      FiberCrop
## 31	   other fibre crops	ofib      FiberCrop
## 32	   arabica coffee	acof      MiscCrop
## 33	   robusta coffee	rcof      MiscCrop
## 34	   cocoa		coco      MiscCrop
## 35	   tea		        teas      MiscCrop
## 36	   tobacco		toba      MiscCrop
## 42	   rest of crops	rest      MiscCrop
