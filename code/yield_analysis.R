## Author : Simon Moulds
## Date   : June 2017

library(raster)
library(magrittr)
library(tidyr)
library(dplyr)
library(rgdal)
library(ggplot2)
options(stringsAsFactors = FALSE)

source("code/helpers.R")

suffix = "_ll"

## ======================================
## india region map
## ======================================

india_rgn = raster(file.path("data", paste0("gcam_32rgn_rast", suffix, ".tif")))
india_rgn[india_rgn != 17] = NA
india_rgn %<>% trim
india_rgn[!is.na(india_rgn)] = 1
india_ext = extent(india_rgn)

cell_ix = !is.na(getValues(india_rgn))

## covars

## market influence, accessibility
mktinf = raster("data/mkt_influence/mkt_infind_5m/w001001.adf") %>% crop(india_ext)

mktacc = raster("data/mkt_influence/mkt_access_5m/w001001.adf") %>% crop(india_ext)

popdens = raster("data/GPWv4/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals_2005.tif") %>% crop(india_ext) %>% raster::aggregate(fact=10, fun=mean)

## rice

rice_yield =
    get_mapspam_data("rice", mapspam_path, "yield", suffix="") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))
    
rice_suit_w = get_gaez_suit_data("rcw", gaez_path, suffix=suffix)
rice_suit_d = get_gaez_suit_data("rcd", gaez_path, suffix=suffix)
rice_suit =
    c(rice_suit_w[1:2], rice_suit_d[1:3]) %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s"))

trof_yield =
    get_mapspam_data("trof", mapspam_path, "yield", suffix="") %>%
    lapply(FUN=function(x) x %>% crop(india_ext) %>% `*`(india_rgn))

## x = as.data.frame(stack(rice_yield[[2]], mktinf, mktacc, popdens)) %>% setNames(c("yield","mkt_influence","mkt_access","pop_density"))
## x = x[complete.cases(x),]
## x = x[x$yield > 0,]

## form = as.formula("yield ~ mkt_influence + mkt_access + pop_density")

## mod = lm(form, data=x)

## obs = x$yield
## pred = predict(mod, newdata=x)
## rsq = 1 - sum((obs - pred) ^ 2) / sum((obs - mean(obs)) ^ 2)


