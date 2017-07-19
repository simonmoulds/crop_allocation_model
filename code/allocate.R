library(raster)
library(magrittr)
library(tidyr)
library(dplyr)
library(rgdal)
library(ggplot2)
library(Rcpp)
library(RcppArmadillo)
options(stringsAsFactors = FALSE)

## source("code/prepare_input_data.R")
sourceCpp("code/allocate.cpp")
 
## load required input data (from prepare_input_data.R)
india_rgn = readRDS("data/india_rgn_raster.rds")

cell_area = readRDS("data/india_rgn_cell_area.rds")

cell_ix = readRDS("data/india_rgn_cell_ix.rds")

crop_area_2005 = readRDS("data/iiasa_cropland_area.rds")

area_df =
    readRDS("data/mapspam_crop_area_df.rds") %>%
    filter(!input %in% c("rain_h","rain_l","rain_s","total"))

yield_df =
    readRDS("data/mapspam_crop_yield_df.rds") %>%
    filter(!input %in% c("rain_h","rain_l","rain_s","total"))
    
nb_df =
    readRDS("data/crop_neighb_df.rds") %>%
    filter(!input %in% c("rain_h","rain_l","rain_s","total"))

suit_df =
    readRDS("data/crop_suit_df.rds") %>%
    filter(!input %in% c("rain_h","rain_l","rain_s","total"))
    
dmd = readRDS("data/gcam_reference_demand.rds")

## study region characteristics
n_cell = length(cell_ix)
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

init_nb_mat =
    nb_df %>%
    select(-(cell:input)) %>%
    mutate_each(funs(replace(., is.na(.), 0))) %>%
    as.matrix

init_suit_mat =
    suit_df %>%
    select(-(cell:input)) %>%
    mutate_each(funs(replace(., is.na(.), 0))) %>%
    as.matrix

## total suitability is the maximum of biophysical suitability from
## GAEZ and neighbourhood suitability
init_tsuit_mat =
    pmax(init_nb_mat, init_suit_mat)

## model parameters
alloc_order = c("rice","whea")
fact = 0.01 ## this value controls how much change is made in each cell
rand_min = 0
rand_max = 0.25

## specify output directory
out_path = "data"

## start routine
area_mat = init_area_mat
total_area_mat = init_total_area_mat
yield_mat = init_yield_mat
suit_mat = init_tsuit_mat

for (i in 2:length(time)) {    

    ## check dimensions etc.
    if (!isTRUE(all.equal(colnames(dmd), colnames(area_mat), colnames(total_area_mat), colnames(yield_mat)))) {
        stop()
    }

    if (!isTRUE(all.equal(dim(area_mat), dim(yield_mat)))) {
        stop()
    }

    ## TODO: run land use change model here to get cropland area
    ## cropland_area = getValues(crop_area_2005 * area(crop_area_2005) * 100) %>% `[`(cell_ix)
    cropland_area = crop_area_2005
   
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
                           rand_min,       ## rand_min
                           rand_max,       ## rand_max
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
                           rand_min,       ## rand_min
                           rand_max,       ## rand_max
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

## library(RcppArmadillo)
## cppFunction("arma::mat schur(arma::mat& a, arma::mat& b) {
##   return(a % b);
## }", depends="RcppArmadillo")
