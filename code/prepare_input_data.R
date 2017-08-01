## Author : Simon Moulds
## Date   : June 2017

library(raster)
library(magrittr)
library(tidyr)
library(dplyr)
library(rgdal)
library(ggplot2)
options(stringsAsFactors = FALSE)

suffix = "_eck4"

## ======================================
## india region map
## ======================================

india_rgn = raster(file.path("data", paste0("gcam_32rgn_rast", suffix, ".tif")))
india_rgn[india_rgn != 17] = NA
india_rgn %<>% trim
india_rgn[!is.na(india_rgn)] = 1
india_ext = extent(india_rgn)

cell_ix = which(!is.na(getValues(india_rgn)))

if (isLonLat(india_rgn)) {
    cell_area = getValues(area(india_rgn)) * 1000 * 1000 / 10000 %>% `[`(cell_ix)
} else {
    cell_area = rep(res(india_rgn)[1] * res(india_rgn)[2] / 10000, length(cell_ix))
}

saveRDS(india_rgn, "data/india_rgn_raster.rds")
saveRDS(cell_area, "data/india_rgn_cell_area.rds")
saveRDS(cell_ix, "data/india_rgn_cell_ix.rds")

## ======================================
## global cropland map - using IIASA-IFPRI map (http://geo-wiki.org/downloads/)
## ======================================

crop_area_2005 =
    raster(file.path("data", paste0("iiasa-ifpri-cropland-map/iiasa_ifpri_cropland_map_5m", suffix, ".tif"))) %>%
    crop(india_ext) %>%
    getValues(.) %>%
    `[`(cell_ix) %>% `*`(cell_area)

saveRDS(crop_area_2005, "data/iiasa_cropland_area.rds")

## ======================================
## helper functions
## ======================================

get_mapspam_data = function(crop, path, what, suffix, ...) {
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
        list(total=raster(file.path(path, paste0("SPAM2005V3r1_global_", ind, "_TA_", toupper(crop), "_A", suffix, ".tif"))),
             irri=raster(file.path(path, paste0("SPAM2005V3r1_global_", ind, "_TI_", toupper(crop), "_I", suffix, ".tif"))),
             rain=raster(file.path(path, paste0("SPAM2005V3r1_global_", ind, "_TR_", toupper(crop), "_R", suffix, ".tif"))),
             rain_h=raster(file.path(path, paste0("SPAM2005V3r1_global_", ind, "_TH_", toupper(crop), "_H", suffix, ".tif"))),
             rain_l=raster(file.path(path, paste0("SPAM2005V3r1_global_", ind, "_TL_", toupper(crop), "_L", suffix, ".tif"))),
             rain_s=raster(file.path(path, paste0("SPAM2005V3r1_global_", ind, "_TS_", toupper(crop), "_S", suffix, ".tif"))))
    out
}

get_gaez_suit_data = function(crop, path, suffix, ...) {

    input_levels = c("h_suhi", ## high input, irrigated
                     "i_suii", ## intermediate input, irrigated
                     "h_suhr", ## high input, rainfed
                     "i_suir", ## intermediate input, rainfed
                     "l_sulr") ## low input, rainfed
    out = list()
    count = 0
    for (i in 1:length(input_levels)) {
        level = input_levels[i]
        f = paste0("res03_crav6190", level, "_", crop, suffix, ".tif")
        if (file.exists(file.path(path, f))) {
            count = count + 1
            r = raster(file.path(path, f))
            out[[level]] = r
        }
    }
    if (count == 0) {
        stop("no files available for the supplied crop")
    }
    out
}

get_mapspam_neighb = function(x, ...) {
    out = vector(mode="list", length=length(x))
    for (i in 1:length(x)) {
        xx = x[[i]]
        if (isLonLat(xx)) {
            ca = raster::area(xx) * 1000 * 1000 / 10000 ## km2 -> Ha
        } else {
            ca = res(xx)[1] * res(xx)[2] / 10000 ## m2 -> Ha
        }
        nb = focal(xx / ca, ...)
        out[[i]] = nb
    }
    names(out) = names(x)
    out
}

## ======================================
## Crop area/suitability maps
## ======================================

mapspam_path = "data/mapspam_data"
gaez_path = "data/gaez_data"

## weights matrix for neighbourhood calculation (suitability)
nbw = matrix(data=1, nrow=5, ncol=5)

## kharif crops

## rice
rice_phys =
    get_mapspam_data("rice", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rice_harv =
    get_mapspam_data("rice", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rice_yield =
    get_mapspam_data("rice", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
rice_suit_w = get_gaez_suit_data("rcw", gaez_path, suffix=suffix)
rice_suit_d = get_gaez_suit_data("rcd", gaez_path, suffix=suffix)
rice_suit =
    c(rice_suit_w[1:2], rice_suit_d[1:3]) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))

rice_khar_phys = list(irri=rice_harv[["irri"]] - rice_phys[["irri"]],
                      rain=rice_phys[["rain"]],
                      rain_h=rice_phys[["rain_h"]],
                      rain_l=rice_phys[["rain_l"]],
                      rain_s=rice_phys[["rain_s"]])

rice_khar_phys_tot = stackApply(stack(rice_khar_phys),
                                indices=rep(1, length(rice_khar_phys)),
                                fun=sum)

rice_khar_phys = c(list(total=rice_khar_phys_tot), rice_khar_phys)
rice_khar_nb = get_mapspam_neighb(rice_khar_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## barley
barl_phys =
    get_mapspam_data("barl", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

barl_harv =
    get_mapspam_data("barl", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

barl_yield =
    get_mapspam_data("barl", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

barl_suit =
    get_gaez_suit_data("brl", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
barl_nb = get_mapspam_neighb(barl_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## maize
maiz_phys =
    get_mapspam_data("maiz", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

maiz_harv =
    get_mapspam_data("maiz", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

maiz_yield =
    get_mapspam_data("maiz", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

maiz_suit =
    get_gaez_suit_data("mze", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
maiz_nb = get_mapspam_neighb(maiz_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## millet
pmil_phys =
    get_mapspam_data("pmil", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pmil_harv =
    get_mapspam_data("pmil", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pmil_yield =
    get_mapspam_data("pmil", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pmil_suit =
    get_gaez_suit_data("pml", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
pmil_nb = get_mapspam_neighb(pmil_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

smil_phys =
    get_mapspam_data("smil", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

smil_harv =
    get_mapspam_data("smil", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

smil_yield =
    get_mapspam_data("smil", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
smil_suit =
    get_gaez_suit_data("fml", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
smil_nb = get_mapspam_neighb(smil_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## sorghum
sorg_phys =
    get_mapspam_data("sorg", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sorg_harv =
    get_mapspam_data("sorg", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sorg_yield =
    get_mapspam_data("sorg", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sorg_suit =
    get_gaez_suit_data("srg", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
sorg_nb = get_mapspam_neighb(sorg_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## other cereal
ocer_phys =
    get_mapspam_data("ocer", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ocer_harv =
    get_mapspam_data("ocer", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ocer_yield =
    get_mapspam_data("ocer", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

## NB suitability based on oat
ocer_suit =
    get_gaez_suit_data("oat", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
ocer_nb = get_mapspam_neighb(ocer_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## soybean
soyb_phys =
    get_mapspam_data("soyb", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

soyb_harv =
    get_mapspam_data("soyb", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

soyb_yield =
    get_mapspam_data("soyb", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

soyb_suit =
    get_gaez_suit_data("soy", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
soyb_nb = get_mapspam_neighb(soyb_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## groundnut
grou_phys =
    get_mapspam_data("grou", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

grou_harv =
    get_mapspam_data("grou", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

grou_yield =
    get_mapspam_data("grou", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

grou_suit =
    get_gaez_suit_data("grd", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
grou_nb = get_mapspam_neighb(grou_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## sesameseed
sesa_phys =
    get_mapspam_data("sesa", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sesa_harv =
    get_mapspam_data("sesa", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sesa_yield =
    get_mapspam_data("sesa", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

## NB: suitability based on rapeseed
sesa_suit =
    get_gaez_suit_data("rsd", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
sesa_nb = get_mapspam_neighb(sesa_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## sunflower
sunf_phys =
    get_mapspam_data("sunf", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sunf_harv =
    get_mapspam_data("sunf", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sunf_yield =
    get_mapspam_data("sunf", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sunf_suit =
    get_gaez_suit_data("sfl", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
sunf_nb = get_mapspam_neighb(sunf_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## otheroils
ooil_phys =
    get_mapspam_data("ooil", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ooil_harv =
    get_mapspam_data("ooil", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ooil_yield =
    get_mapspam_data("ooil", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

## NB other oil suitability based on olive oil
ooil_suit =
    get_gaez_suit_data("olv", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
ooil_nb = get_mapspam_neighb(ooil_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## potato
pota_phys =
    get_mapspam_data("pota", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pota_harv =
    get_mapspam_data("pota", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pota_yield =
    get_mapspam_data("pota", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pota_suit =
    get_gaez_suit_data("wpo", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
pota_nb = get_mapspam_neighb(pota_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## sweetpotato
swpo_phys =
    get_mapspam_data("swpo", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

swpo_harv =
    get_mapspam_data("swpo", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

swpo_yield =
    get_mapspam_data("swpo", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

swpo_suit =
    get_gaez_suit_data("spo", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
swpo_nb = get_mapspam_neighb(swpo_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## cotton
cott_phys =
    get_mapspam_data("cott", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cott_harv =
    get_mapspam_data("cott", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cott_yield =
    get_mapspam_data("cott", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cott_suit =
    get_gaez_suit_data("cot", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
cott_nb = get_mapspam_neighb(cott_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## other fibre
ofib_phys =
    get_mapspam_data("ofib", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ofib_harv =
    get_mapspam_data("ofib", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ofib_yield =
    get_mapspam_data("ofib", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

ofib_suit =
    get_gaez_suit_data("flx", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
ofib_nb = get_mapspam_neighb(ofib_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## tobacco
toba_phys =
    get_mapspam_data("toba", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

toba_harv =
    get_mapspam_data("toba", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

toba_yield =
    get_mapspam_data("toba", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

toba_suit =
    get_gaez_suit_data("tob", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
toba_nb = get_mapspam_neighb(toba_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## rest of crops
rest_phys =
    get_mapspam_data("rest", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rest_harv =
    get_mapspam_data("rest", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rest_yield =
    get_mapspam_data("rest", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rest_suit =
    get_gaez_suit_data("mze", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
rest_nb = get_mapspam_neighb(rest_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## rabi crops

## rice

## assumes that most irrigated rice occurs during rabi and most rainfed rice occurs during kharif
rice_rabi_phys =
    list(irri=rice_phys[["irri"]],
         rain=rice_harv[["rain"]] - rice_phys[["rain"]],
         rain_h=rice_harv[["rain_h"]] - rice_phys[["rain_h"]],
         rain_l=rice_harv[["rain_l"]] - rice_phys[["rain_l"]],
         rain_s=rice_harv[["rain_s"]] - rice_phys[["rain_s"]])

rice_rabi_phys_tot = stackApply(stack(rice_rabi_phys),
                                indices=rep(1, length(rice_rabi_phys)),
                                fun=sum)

rice_rabi_phys = c(list(total=rice_rabi_phys_tot), rice_rabi_phys)
rice_rabi_nb = get_mapspam_neighb(rice_rabi_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## wheat
whea_phys =
    get_mapspam_data("whea", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

whea_harv =
    get_mapspam_data("whea", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

whea_yield =
    get_mapspam_data("whea", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

whea_suit =
    get_gaez_suit_data("whe", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
whea_nb = get_mapspam_neighb(whea_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## vegetables
vege_phys =
    get_mapspam_data("vege", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

vege_harv =
    get_mapspam_data("vege", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

vege_yield =
    get_mapspam_data("vege", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

vege_suit =
    get_gaez_suit_data("oni", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
vege_nb = get_mapspam_neighb(vege_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## rapeseed
rape_phys =
    get_mapspam_data("rape", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rape_harv =
    get_mapspam_data("rape", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rape_yield =
    get_mapspam_data("rape", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rape_suit =
    get_gaez_suit_data("rsd", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
rape_nb = get_mapspam_neighb(rape_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## pulses (NB cowpea not present in India according to MapSPAM)
bean_phys =
    get_mapspam_data("bean", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

bean_harv =
    get_mapspam_data("bean", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

bean_yield =
    get_mapspam_data("bean", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

bean_suit =
    get_gaez_suit_data("phb", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
bean_nb = get_mapspam_neighb(bean_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## chickpea
chic_phys =
    get_mapspam_data("chic", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

chic_harv =
    get_mapspam_data("chic", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

chic_yield =
    get_mapspam_data("chic", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
chic_suit =
    get_gaez_suit_data("chk", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
chic_nb = get_mapspam_neighb(chic_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## pigeon pea
pige_phys =
    get_mapspam_data("pige", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pige_harv =
    get_mapspam_data("pige", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pige_yield =
    get_mapspam_data("pige", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

pige_suit =
    get_gaez_suit_data("pig", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
pige_nb = get_mapspam_neighb(pige_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## cowpea
cowp_phys =
    get_mapspam_data("cowp", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cowp_harv =
    get_mapspam_data("cowp", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cowp_yield =
    get_mapspam_data("cowp", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cowp_suit =
    get_gaez_suit_data("cow", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
cowp_nb = get_mapspam_neighb(cowp_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## lentil
lent_phys =
    get_mapspam_data("lent", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

lent_harv =
    get_mapspam_data("lent", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

lent_yield =
    get_mapspam_data("lent", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

lent_suit =
    get_gaez_suit_data("chk", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
lent_nb = get_mapspam_neighb(lent_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## other pulses
opul_phys =
    get_mapspam_data("opul", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

opul_harv =
    get_mapspam_data("opul", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

opul_yield =
    get_mapspam_data("opul", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

opul_suit =
    get_gaez_suit_data("chk", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
opul_nb = get_mapspam_neighb(opul_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## annual crops

## sugarcane (NB sugarbeet not grown in India)
sugc_phys =
    get_mapspam_data("sugc", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sugc_harv =
    get_mapspam_data("sugc", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sugc_yield =
    get_mapspam_data("sugc", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sugc_suit =
    get_gaez_suit_data("suc", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
sugc_nb = get_mapspam_neighb(sugc_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## sugar beet (not grown in India - included for completeness)
sugb_phys =
    get_mapspam_data("sugb", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sugb_harv =
    get_mapspam_data("sugb", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sugb_yield =
    get_mapspam_data("sugb", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

sugb_suit =
    get_gaez_suit_data("sub", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
sugb_nb = get_mapspam_neighb(sugb_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## coconut
cnut_phys =
    get_mapspam_data("cnut", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cnut_harv =
    get_mapspam_data("cnut", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cnut_yield =
    get_mapspam_data("cnut", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cnut_suit =
    get_gaez_suit_data("con", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
cnut_nb = get_mapspam_neighb(cnut_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## oil palm (not grown in India - included for completeness)
oilp_phys =
    get_mapspam_data("oilp", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

oilp_harv =
    get_mapspam_data("oilp", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

oilp_yield =
    get_mapspam_data("oilp", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
oilp_suit =
    get_gaez_suit_data("olp", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
oilp_nb = get_mapspam_neighb(oilp_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## tropical fruit
trof_phys =
    get_mapspam_data("trof", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

trof_harv =
    get_mapspam_data("trof", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

trof_yield =
    get_mapspam_data("trof", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

trof_suit =
    get_gaez_suit_data("ban", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
trof_nb = get_mapspam_neighb(trof_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## temperate fruit
temf_phys =
    get_mapspam_data("temf", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

temf_harv =
    get_mapspam_data("temf", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

temf_yield =
    get_mapspam_data("temf", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

## NB temperate fruit suitability based on maize
temf_suit =
    get_gaez_suit_data("mze", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
temf_nb = get_mapspam_neighb(temf_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## banana
bana_phys =
    get_mapspam_data("bana", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

bana_harv =
    get_mapspam_data("bana", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

bana_yield =
    get_mapspam_data("bana", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

bana_suit =
    get_gaez_suit_data("ban", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
bana_nb = get_mapspam_neighb(bana_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## plantain
plnt_phys =
    get_mapspam_data("plnt", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

plnt_harv =
    get_mapspam_data("plnt", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

plnt_yield =
    get_mapspam_data("plnt", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

## NB plantain suitability based on plantain
plnt_suit =
    get_gaez_suit_data("ban", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
plnt_nb = get_mapspam_neighb(plnt_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## cassava
cass_phys =
    get_mapspam_data("cass", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cass_harv =
    get_mapspam_data("cass", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cass_yield =
    get_mapspam_data("cass", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

cass_suit =
    get_gaez_suit_data("csv", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,1,1,3,3)) %>%
    ## `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
cass_nb = get_mapspam_neighb(cass_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## yams
yams_phys =
    get_mapspam_data("yams", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

yams_harv =
    get_mapspam_data("yams", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

yams_yield =
    get_mapspam_data("yams", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

yams_suit =
    get_gaez_suit_data("yam", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,1,1,3,3)) %>%
    ## `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
yams_nb = get_mapspam_neighb(yams_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## other roots
orts_phys =
    get_mapspam_data("orts", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

orts_harv =
    get_mapspam_data("orts", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

orts_yield =
    get_mapspam_data("orts", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

## NB other roots suitability based on yam
orts_suit =
    get_gaez_suit_data("yam", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,1,1,3,3)) %>%
    ## `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
orts_nb = get_mapspam_neighb(orts_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## cocoa
coco_phys =
    get_mapspam_data("coco", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

coco_harv =
    get_mapspam_data("coco", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

coco_yield =
    get_mapspam_data("coco", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

coco_suit =
    get_gaez_suit_data("coc", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
coco_nb = get_mapspam_neighb(coco_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## tea
teas_phys =
    get_mapspam_data("teas", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

teas_harv =
    get_mapspam_data("teas", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

teas_yield =
    get_mapspam_data("teas", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

teas_suit =
    get_gaez_suit_data("tea", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
teas_nb = get_mapspam_neighb(teas_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## arabica coffee
acof_phys =
    get_mapspam_data("acof", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

acof_harv =
    get_mapspam_data("acof", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

acof_yield =
    get_mapspam_data("acof", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

acof_suit =
    get_gaez_suit_data("cof", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
acof_nb = get_mapspam_neighb(acof_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## robusta coffee
rcof_phys =
    get_mapspam_data("rcof", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rcof_harv =
    get_mapspam_data("rcof", mapspam_path, "harvested_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rcof_yield =
    get_mapspam_data("rcof", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

rcof_suit =
    get_gaez_suit_data("cof", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))
rcof_nb = get_mapspam_neighb(rcof_phys, w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)

## ======================================
## group irrigated and rainfed crops into data frames by season
## ======================================

myfun = function(..., na_ix, levels=c("total","irri","rain","rain_h","rain_l","rain_s"), season_nm, df=TRUE) {
    objs = list(...)
    if (length(objs) == 0) stop()
    ## TODO: check list is named
    if (missing(season_nm)) stop()
    
    nlevels = length(levels)
    dfs =
        vector("list", length=nlevels) %>%
        setNames(levels)
    
    for (i in 1:nlevels) {
        level = levels[i]
        dfs[[i]] =
            lapply(objs, FUN=function(x) x[[level]]) %>%
            stack %>%
            as.data.frame(na.rm=FALSE) %>%
            mutate(season=season_nm,
                   input=level,
                   cell=seq_len(ncell(objs[[1]][[1]]))) %>%
            `[`(na_ix,)
    }

    if (isTRUE(df)) {
        df = dfs[[1]]
        nms = names(df)
        for (i in 2:length(dfs)) {
            df = full_join(df, dfs[[i]], by=nms)
        }
        df = arrange(df, cell, input)
        return(df)
    } else {
        return(dfs)
    }
}

kharif_area_df = myfun(rice=rice_khar_phys, barl=barl_harv, maiz=maiz_harv,
                       pmil=pmil_harv, smil=smil_harv, sorg=sorg_harv,
                       ocer=ocer_harv, soyb=soyb_harv, grou=grou_harv,
                       sesa=sesa_harv, sunf=sunf_harv, ooil=ooil_harv,
                       pota=pota_harv, swpo=swpo_harv, cott=cott_harv,
                       ofib=ofib_harv, toba=toba_harv, rest=rest_harv,
                       na_ix=cell_ix, season_nm="kharif")

rabi_area_df   = myfun(rice=rice_rabi_phys, whea=whea_harv, vege=vege_harv,
                       rape=rape_harv, bean=bean_harv, chic=chic_harv,
                       pige=pige_harv, cowp=cowp_harv, lent=lent_harv,
                       opul=opul_harv, na_ix=cell_ix, season_nm="rabi")
            
annual_area_df = myfun(sugc=sugc_harv, sugb=sugb_harv, cnut=cnut_harv,
                       oilp=oilp_harv, trof=trof_harv, temf=temf_harv,
                       bana=bana_harv, plnt=plnt_harv, coco=coco_harv,
                       teas=teas_harv, acof=acof_harv, rcof=rcof_harv,
                       cass=cass_harv, yams=yams_harv, orts=orts_harv,
                       na_ix=cell_ix, season_nm="annual")

## now, group these together
area_df =
    kharif_area_df %>%
    full_join(rabi_area_df) %>%
    full_join(annual_area_df) %>%
    select(cell, season, input, acof, bana, barl, bean, cass, chic, cnut, coco, cott, cowp, grou, lent, maiz, ocer, ofib, oilp, ooil, opul, orts, pige, plnt, pmil, pota, rape, rcof, rest, rice, sesa, smil, sorg, soyb, sugb, sugc, sunf, swpo, teas, temf, toba, trof, vege, whea, yams) %>% 
    arrange(cell, season, input)

## save objects
saveRDS(area_df, "data/mapspam_crop_area_df.rds")

## ======================================
## yield
## ======================================

kharif_yield_df = myfun(rice=rice_yield, barl=barl_yield, maiz=maiz_yield,
                        pmil=pmil_yield, smil=smil_yield, sorg=sorg_yield,
                        ocer=ocer_yield, soyb=soyb_yield, grou=grou_yield,
                        sesa=sesa_yield, sunf=sunf_yield, ooil=ooil_yield,
                        pota=pota_yield, swpo=swpo_yield, cott=cott_yield,
                        ofib=ofib_yield, toba=toba_yield, rest=rest_yield,
                        na_ix=cell_ix, season_nm="kharif")

rabi_yield_df   = myfun(rice=rice_yield, whea=whea_yield, vege=vege_yield,
                        rape=rape_yield, bean=bean_yield, chic=chic_yield,
                        pige=pige_yield, cowp=cowp_yield, lent=lent_yield,
                        opul=opul_yield, na_ix=cell_ix, season_nm="rabi")
            
annual_yield_df = myfun(sugc=sugc_yield, sugb=sugb_yield, cnut=cnut_yield,
                        oilp=oilp_yield, trof=trof_yield, temf=temf_yield,
                        bana=bana_yield, plnt=plnt_yield, coco=coco_yield,
                        teas=teas_yield, acof=acof_yield, rcof=rcof_yield,
                        cass=cass_yield, yams=yams_yield, orts=orts_yield,
                        na_ix=cell_ix, season_nm="annual")

## now, group these together
yield_df =
    kharif_yield_df %>%
    full_join(rabi_yield_df) %>%
    full_join(annual_yield_df) %>%
    select(cell, season, input, acof, bana, barl, bean, cass, chic, cnut, coco, cott, cowp, grou, lent, maiz, ocer, ofib, oilp, ooil, opul, orts, pige, plnt, pmil, pota, rape, rcof, rest, rice, sesa, smil, sorg, soyb, sugb, sugc, sunf, swpo, teas, temf, toba, trof, vege, whea, yams) %>%
    mutate_each(funs(./1e3), -(cell:input)) %>% ## kg -> ton
    arrange(cell, season, input)

## save object
saveRDS(yield_df, "data/mapspam_crop_yield_df.rds")

## ======================================
## suitability
## ======================================

## suitability is a combination of neighbourhood and biophysical
kharif_nb_df = myfun(rice=rice_khar_nb, barl=barl_nb, maiz=maiz_nb,
                     pmil=pmil_nb, smil=smil_nb, sorg=sorg_nb,
                     ocer=ocer_nb, soyb=soyb_nb, grou=grou_nb,
                     sesa=sesa_nb, sunf=sunf_nb, ooil=ooil_nb,
                     pota=pota_nb, swpo=swpo_nb, cott=cott_nb,
                     ofib=ofib_nb, toba=toba_nb, rest=rest_nb,
                     na_ix=cell_ix, season_nm="kharif")

rabi_nb_df   = myfun(rice=rice_rabi_nb, whea=whea_nb, vege=vege_nb,
                     rape=rape_nb, bean=bean_nb, chic=chic_nb,
                     pige=pige_nb, cowp=cowp_nb, lent=lent_nb,
                     opul=opul_nb, na_ix=cell_ix, season_nm="rabi")

annual_nb_df = myfun(sugc=sugc_nb, sugb=sugb_nb, cnut=cnut_nb,
                     oilp=oilp_nb, trof=trof_nb, temf=temf_nb,
                     bana=bana_nb, plnt=plnt_nb, coco=coco_nb,
                     teas=teas_nb, acof=acof_nb, rcof=rcof_nb,
                     cass=cass_nb, yams=yams_nb, orts=orts_nb,
                     na_ix=cell_ix, season_nm="annual")

## suitability is a combination of neighbourhood and biophysical
kharif_suit_df = myfun(rice=rice_suit, barl=barl_suit, maiz=maiz_suit,
                       pmil=pmil_suit, smil=smil_suit, sorg=sorg_suit,
                       ocer=ocer_suit, soyb=soyb_suit, grou=grou_suit,
                       sesa=sesa_suit, sunf=sunf_suit, ooil=ooil_suit,
                       pota=pota_suit, swpo=swpo_suit, cott=cott_suit,
                       ofib=ofib_suit, toba=toba_suit, rest=rest_suit,
                       na_ix=cell_ix, season_nm="kharif")

rabi_suit_df   = myfun(rice=rice_suit, whea=whea_suit, vege=vege_suit,
                       rape=rape_suit, bean=bean_suit, chic=chic_suit,
                       pige=pige_suit, cowp=cowp_suit, lent=lent_suit,
                       opul=opul_suit, na_ix=cell_ix, season_nm="rabi")

annual_suit_df = myfun(sugc=sugc_suit, sugb=sugb_suit, cnut=cnut_suit,
                       oilp=oilp_suit, trof=trof_suit, temf=temf_suit,
                       bana=bana_suit, plnt=plnt_suit, coco=coco_suit,
                       teas=teas_suit, acof=acof_suit, rcof=rcof_suit,
                       cass=cass_suit, yams=yams_suit, orts=orts_suit,
                       na_ix=cell_ix, season_nm="annual")

nb_df =
    kharif_nb_df %>%
    full_join(rabi_nb_df) %>%
    full_join(annual_nb_df) %>%
    select(cell, season, input, acof, bana, barl, bean, cass, chic, cnut, coco, cott, cowp, grou, lent, maiz, ocer, ofib, oilp, ooil, opul, orts, pige, plnt, pmil, pota, rape, rcof, rest, rice, sesa, smil, sorg, soyb, sugb, sugc, sunf, swpo, teas, temf, toba, trof, vege, whea, yams) %>% 
    arrange(cell, season, input)

suit_df =
    kharif_suit_df %>%
    full_join(rabi_suit_df) %>%
    full_join(annual_suit_df) %>%
    select(cell, season, input, acof, bana, barl, bean, cass, chic, cnut, coco, cott, cowp, grou, lent, maiz, ocer, ofib, oilp, ooil, opul, orts, pige, plnt, pmil, pota, rape, rcof, rest, rice, sesa, smil, sorg, soyb, sugb, sugc, sunf, swpo, teas, temf, toba, trof, vege, whea, yams) %>% 
    arrange(cell, season, input)

suit_df =
    suit_df %>%
    mutate_each(funs(replace(., .==-1, 0)), -(cell:input)) %>%
    mutate_each(funs(./1e4), -(cell:input))
    
## save object
saveRDS(nb_df, "data/crop_neighb_df.rds")
saveRDS(suit_df, "data/crop_suit_df.rds")

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

## wheat
wheat_sf = total_production_fun(whea_harv[["total"]], whea_yield[["total"]]) / 1e3 / 1e6 / gcam_prod[["Wheat"]][2]

## maize/corn
corn_sf = total_production_fun(maiz_harv[["total"]], maiz_yield[["total"]]) / 1e3 / 1e6 / gcam_prod[["Corn"]][2]

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

## fibre
fibre_total =
    sum(c(total_production_fun(cott_harv[["total"]], cott_yield[["total"]]),
          total_production_fun(ofib_harv[["total"]], ofib_yield[["total"]])), na.rm=TRUE)
fibre_sf  = fibre_total / 1e3 / 1e6 / gcam_prod[["FiberCrop"]][2]

fibre_cott_frac = total_production_fun(cott_harv[["total"]], cott_yield[["total"]]) / fibre_total
fibre_ofib_frac = total_production_fun(ofib_harv[["total"]], ofib_yield[["total"]]) / fibre_total

## palm fruit
palm_total =
    sum(c(total_production_fun(cnut_harv[["total"]], cnut_yield[["total"]]),
          total_production_fun(oilp_harv[["total"]], oilp_yield[["total"]])), na.rm=TRUE)
palm_sf  = palm_total / 1e3 / 1e6 / gcam_prod[["PalmFruit"]][2]

palm_cnut_frac = total_production_fun(cnut_harv[["total"]], cnut_yield[["total"]]) / palm_total
palm_oilp_frac = total_production_fun(oilp_harv[["total"]], oilp_yield[["total"]]) / palm_total

## sugar crop
sugar_total =
    sum(c(total_production_fun(sugc_harv[["total"]], sugc_yield[["total"]]),
          total_production_fun(sugb_harv[["total"]], sugb_yield[["total"]])), na.rm=TRUE)
sugar_sf  = sugar_total / 1e3 / 1e6 / gcam_prod[["SugarCrop"]][2]

sugar_sugc_frac = total_production_fun(sugc_harv[["total"]], sugc_yield[["total"]]) / sugar_total
sugar_sugb_frac = total_production_fun(sugb_harv[["total"]], sugb_yield[["total"]]) / sugar_total

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
