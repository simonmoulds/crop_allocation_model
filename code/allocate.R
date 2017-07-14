library(raster)
library(magrittr)
library(tidyr)
library(dplyr)
library(rgdal)
library(ggplot2)
library(Rcpp)
library(RcppArmadillo)
options(stringsAsFactors = FALSE)

## source("code/process_mapspam.R")
sourceCpp("code/allocate.cpp")

## india region
india_rgn = raster("data/gcam_32rgn_rast_ll.tif")
india_rgn[india_rgn != 17] = NA
india_rgn = india_rgn %>% trim
india_rgn[!is.na(india_rgn)] = 1

india_ext = extent(india_rgn)
cell_ix = which(!is.na(getValues(india_rgn)))
n_cell = length(cell_ix)

## global cropland map
crop_area_2005 =
    raster("data/rawdata/iiasa-ifpri-cropland-map/Hybrid_10042015v9.img") %>%
    crop(india_ext) %>%
    raster::aggregate(fact=10, FUN=mean) %>%
    `/`(100) 

## load data
area_df       =
    readRDS("data/mapspam_crop_area_df.rds") %>%
    filter(!input %in% c("rain_h","rain_l","rain_s","total"))

yield_df      =
    readRDS("data/mapspam_crop_yield_df.rds") %>%
    filter(!input %in% c("rain_h","rain_l","rain_s","total"))
    
## suit_df       = readRDS("data/neighb_crop_suit_df_orig.rds")
suit_df       =
    readRDS("data/crop_neighb_df.rds") %>%
    filter(!input %in% c("rain_h","rain_l","rain_s","total"))
    
dmd           = readRDS("data/gcam_reference_demand_orig.rds")

## study region characteristics
n_input = 2
n_season = 3
input_nms = c("irrigated","rainfed")     ## include names for the purpose of writing output files
season_nms = c("annual","kharif","rabi")

## time
time = seq(2005, 2100, by=5)

## initial matrices
init_area_mat =
    area_df %>%
    select(-(cell:input)) %>%
    mutate_each(funs(replace(., is.na(.), 0))) %>%
    as.matrix

init_total_area_mat =
    get_total_areaC(init_area_mat, n_season, n_input) %>%
    `colnames<-`(colnames(init_area_mat))

init_yield_mat =
    yield_df %>%
    select(-(cell:input)) %>%
    mutate_each(funs(replace(., is.na(.), 0))) %>%
    as.matrix

init_suit_mat =
    suit_df %>%
    select(-(cell:input)) %>%
    mutate_each(funs(replace(., is.na(.), 0))) %>%
    as.matrix

## ultimately, all maps will be projected in an equal area projection and hence
## cell_area will be a single value
cell_area = getValues(area(india_rgn) * 100) %>% `[`(cell_ix)

## model parameters
alloc_order = c("rice","whea")
fact = 0.01   ## this value controls how much change is made in each cell

## specify output directory
out_path = "data"

## start routine
area_mat = init_area_mat
total_area_mat = init_total_area_mat
yield_mat = init_yield_mat
suit_mat = init_suit_mat

## for (i in 2:length(time)) {
for (i in 2:3) {    
    ## check dimensions etc.
    if (!isTRUE(all.equal(colnames(dmd), colnames(area_mat), colnames(total_area_mat), colnames(yield_mat)))) {
        stop()
    }

    if (!isTRUE(all.equal(dim(area_mat), dim(yield_mat)))) {
        stop()
    }

    ## TODO: run land use change model here to get cropland area
    cropland_area = getValues(crop_area_2005 * area(crop_area_2005) * 100) %>% `[`(cell_ix)
   
    ## calculate demand (the amount by which production must change, rather than
    ## total production)
    dmd0 = dmd[i-1,]      
    dmd1 = dmd[i,] - dmd0

    ## index and names of crops with increasing/decreasing demand
    decr_ix = dmd1 < 0
    incr_ix = !decr_ix

    crop_nms = colnames(dmd)
    decr_crops = crop_nms[decr_ix]
    incr_crops = crop_nms[incr_ix]

    ## first, allocate crops with increasing demand
    for (j in 1:length(alloc_order)) {
        crop = alloc_order[j]
        if (crop %in% incr_crops) {
            crop_ix = crop_nms %in% crop

            ## perform allocation
            res = allocate(area_mat,       ## crop_area
                           yield_mat,      ## crop_yield
                           suit_mat,       ## crop_suit
                           total_area_mat, ## total_crop_area
                           cropland_area,  ## cropland_area
                           cell_area,      ## cell_area
                           dmd1,           ## demand
                           crop_ix,        ## crop
                           decr_ix,        ## decr
                           n_season,       ## n_season
                           n_input,        ## n_input
                           fact,           ## fact
                           0,
                           0,
                           1000,           ## tol
                           1000000)        ## maxiter

            ## get new values for subsequent crop
            area_mat = res[["area"]]
            total_area_mat = res[["total_area"]]
            dmd1 = res[["demand"]]
        }
    }

    ## then run allocation for crops with decreasing demand
    for (crop in alloc_order) {
        if (crop %in% decr_crops) {
            crop_ix = crop_nms %in% crop

            ## perform allocation
            res = allocate(area_mat,       ## crop_area
                           yield_mat,      ## crop_yield
                           suit_mat,       ## crop_suit
                           total_area_mat, ## total_crop_area
                           cropland_area,  ## cropland_area
                           cell_area,      ## cell_area
                           dmd1,           ## demand
                           crop_ix,        ## crop
                           decr_ix,        ## decr
                           n_season,       ## n_season
                           n_input,        ## n_input
                           fact,           ## fact
                           0,
                           0,
                           1000,           ## tol
                           1000000)        ## maxiter

            ## get new values for subsequent crop
            area_mat = res[["area"]]
            total_area_mat = res[["total_area"]]
            dmd1 = res[["demand"]]
        }
    }

    ## write output to file
    for (j in 1:length(alloc_order)) {
        crop = alloc_order[j]
        crop_ix = which(crop_nms %in% crop)
        
        for (k in 1:n_season) {
            
            if (total_area_mat[k,crop_ix] > 0) { ## i.e. only write file if there are values > 0
                v = area_mat[,crop_ix,drop=TRUE]
                
                for (m in 1:n_input) {
                    r = raster(india_rgn)  ## template
                    row_ix = seq((k - 1) * n_input + m, by=n_season * n_input, length.out = n_cell)
                    r[cell_ix] = v[row_ix] ## assign values

                    ## TODO: run neighbourhood calculation here to update suitability

                    fn = paste0("INDIA_",
                                toupper(crop), "_",
                                toupper(season_nms[k]), "_",
                                toupper(input_nms[m]), "_",
                                time[i], ".tif")
                    
                    writeRaster(r, file.path(out_path, fn), format="GTiff", overwrite=TRUE)
                }
            }
        }
    }
}

## cppFunction(code='NumericMatrix addmat(NumericMatrix x, NumericMatrix y) {
##   return x + y;
## }')

## library(RcppArmadillo)
## cppFunction("arma::mat schur(arma::mat& a, arma::mat& b) {
##   return(a % b);
## }", depends="RcppArmadillo")
