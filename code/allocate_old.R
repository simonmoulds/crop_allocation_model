library(raster)
library(magrittr)
library(tidyr)
library(dplyr)
library(rgdal)
library(ggplot2)
library(Rcpp)
library(RcppArmadillo)
options(stringsAsFactors = FALSE)

source("code/helpers.R")
nbw = matrix(data=1, nrow=5, ncol=5)

## source("code/prepare_input_data.R")
sourceCpp("code/allocate.cpp")

## study region characteristics
input_levels = c("irri","rain_h","rain_l","rain_s")
growing_seasons = c("annual","kharif","rabi")
india_rgn = readRDS("data/india_rgn_raster.rds")
cell_area = readRDS("data/india_rgn_cell_area.rds")
cell_ix = readRDS("data/india_rgn_cell_ix.rds")
crop_area_2005 = readRDS("data/iiasa_cropland_area.rds")
n_cell = length(cell_ix)
n_input = length(input_levels)
n_season = length(growing_seasons)

## load required input data (from prepare_input_data.R)
init_area_mat =
    readRDS("data/mapspam_crop_area_df.rds") %>%
    filter(input %in% input_levels) %>%
    select(-(cell:input)) %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    as.matrix

init_area_mat[init_area_mat < 0] = 0

init_total_area_mat =
    get_total_areaC(init_area_mat, n_season, n_input) %>%
    `colnames<-`(colnames(init_area_mat))

init_yield_mat =
    readRDS("data/mapspam_crop_yield_df.rds") %>%
    filter(input %in% input_levels) %>%
    select(-(cell:input)) %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    as.matrix
    
init_nb_mat =
    readRDS("data/crop_neighb_df.rds") %>%
    filter(input %in% input_levels) %>%
    select(-(cell:input)) %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    as.matrix

init_suit_mat =
    readRDS("data/crop_suit_df.rds") %>%
    filter(input %in% input_levels) %>%
    select(-(cell:input)) %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    as.matrix

init_potyld_mat = 
    readRDS("data/crop_potyld_df.rds") %>%
    filter(input %in% input_levels) %>%
    select(-(cell:input)) %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    as.matrix

popn_df = readRDS("data/popn_df.rds")

## total suitability is the maximum of biophysical suitability from
## GAEZ and neighbourhood suitability
init_tsuit_mat =
    pmax(init_nb_mat, init_suit_mat)

dmd = readRDS("data/gcam_reference_demand.rds")

## model parameters
alloc_order = c("sugc","rice","vege","whea","pota","trof","bana","maiz","cott","pmil","soyb","temf","cnut","sorg","rape","cass","grou","chic","bean","pige","ofib","smil","opul","ooil","sunf","barl","swpo","lent","teas","sesa","rest","toba","rcof","acof","coco","orts","ocer","plnt","cowp","oilp","sugb","yams")

fact = 0.01 ## this value controls how much change is made in each cell
rand_min = 0
rand_max = 0.25

## specify output directory
out_path = "data/output"
if (!dir.exists(out_path)) {
    dir.create(out_path)
}

## start routine
time = seq(2005, 2100, by=5)
area_mat = init_area_mat
total_area_mat = init_total_area_mat
yield_mat = init_yield_mat
nb_mat = init_nb_mat
suit_mat = init_suit_mat
potyld_mat = init_potyld_mat

## time = time[1:3]

## eff_seed = sample(1:2^15, 1)
## print(sprintf("Seed for session: %s", eff_seed))

set.seed(18384)

for (i in 2:length(time)) {    

    popdens = rep(popn_df[,i], each=(n_season * n_input))
    tsuit_mat = (suit_mat >= 1) * popdens * potyld_mat
    for (j in 1:ncol(tsuit_mat)) {
        x = tsuit_mat[,i]
        tsuit_mat[,i] = x / max(x)
    }
        
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
    incr_ix = !decr_ix ##dmd1 > 0

    crop_nms = colnames(dmd)
    decr_crops = crop_nms[decr_ix]
    incr_crops = crop_nms[incr_ix]

    ## first, allocate crops with increasing demand
    for (j in 1:length(alloc_order)) {
        crop = alloc_order[j]; ##print(crop)
        if (crop %in% incr_crops) {
            crop_ix = crop_nms %in% crop

            ## perform allocation
            res = allocate(area_mat,       ## crop_area
                           yield_mat,      ## crop_yield
                           suit_mat,       ## crop_suit
                           nb_mat,
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
                           500000)         ## maxiter

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
                           nb_mat,
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
                           100000)        ## maxiter

            ## get new values for subsequent crop
            area_mat = res[["area"]]
            total_area_mat = res[["total_area"]]
            dmd1 = res[["demand"]]
        }
    }

    dmd[i,] = colSums(area_mat * yield_mat)
    
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

                    nb = get_mapspam_neighb(list(r), w=nbw, fun=mean, na.rm=TRUE, pad=TRUE)[[1]]
                    nb_vals = getValues(nb) %>% `[`(cell_ix)
                    nb_mat[row_ix,crop_ix] = nb_vals ## update nb_mat

                    fn = paste0("INDIA_",
                                toupper(crop), "_",
                                toupper(growing_seasons[k]), "_",
                                toupper(input_levels[m]), "_",
                                time[i], ".tif")
                    
                    writeRaster(r, file.path(out_path, fn), format="GTiff", overwrite=TRUE)
                }
            }
        }
    }    
}

write.table(dmd, "data/output/allocated_area.txt", sep=" ", row.names=FALSE, col.names=TRUE)

