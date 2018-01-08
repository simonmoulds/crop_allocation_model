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

subcrops = list(rice = "rice",
                wheat = "whea",
                corn = "maiz",
                oth_cereal = c("barl","sorg","pmil","smil","ocer"),
                oils = c("soyb","grou","sesa","sunf","rape","ooil"),
                fibre = c("cott","ofib"),
                palm = c("cnut","oilp"),
                sugar = c("sugc","sugb"),
                root = c("pota","swpo","yams","cass","orts"),
                misc = c("bean","chic","pige","lent","cowp","opul","trof","temf","bana","plnt","acof","rcof","coco","teas","toba","vege","rest"))

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
crop_nms = colnames(dmd)
target_intens = 0.25

intensify_R = function(area_mat, tsuit_mat, yield_mat, dmd, target_intens, crop_ix, n_cell, n_season, n_input, cell_area, fact, no_chng_count_max=1, ...) {

    ## function to intensify production

    ## target_dmd is the target increase from this round
    ## of intensification
    target_dmd = dmd[crop_ix] * target_intens

    max_suit = rep(NA, n_cell)

    for (i in 1:n_cell) {
        start_row_ix = (i-1) * (n_season * n_input) + 1
        end_row_ix = i * (n_season * n_input)
        tsuit_v = tsuit_mat[start_row_ix:end_row_ix,crop_ix]
        area_v = area_mat[start_row_ix:end_row_ix,crop_ix]
        if (any(tsuit_v > 0) && any(area_v > 0)) {
            max_suit[i] = tsuit_v[which(tsuit_v > 0)[1]]
        } else {
            max_suit[i] = 0
        }
    }

    ordr = order(max_suit, decreasing=TRUE) ## high -> low

    index = 0
    total_prod_chng = 0
    no_chng_count = 0

    repeat {

        index = index + 1
        ix = ordr[index]

        for (i in 1:n_season) {

            ## get the start and end rows of the particular season
            start_row_ix = (ix-1) * (n_season * n_input) + (i-1) * n_season + 1
            end_row_ix = start_row_ix + (n_input - 1)

            ## loop through input levels
            for (j in 1:(n_input-1)) {
                hi_ix = end_row_ix - j
                lo_ix = hi_ix + 1

                higher_intensity_area = area_mat[hi_ix,crop_ix]
                lower_intensity_area = area_mat[lo_ix,crop_ix]

                if (lower_intensity_area > 0) {

                    area_chng0 = min(lower_intensity_area, cell_area[index] * fact)
                    prod_chng0 = area_chng0 * (yield_mat[hi_ix,crop_ix] - yield_mat[lo_ix,crop_ix])

                    if (prod_chng0 > 0) {
                        
                        if (prod_chng0 < target_dmd) {
                            prod_chng = prod_chng0
                            area_chng = area_chng0
                        } else {
                            prod_chng = target_dmd
                            area_chng = prod_chng / (yield_mat[hi_ix,crop_ix] - yield_mat[lo_ix,crop_ix]) 
                        }
                        
                    } else {
                        prod_chng = 0
                        area_chng = 0
                    }
                    
                } else {
                    prod_chng = 0
                    area_chng = 0
                }

                area_mat[hi_ix,crop_ix] = area_mat[hi_ix,crop_ix] + area_chng
                area_mat[lo_ix,crop_ix] = area_mat[lo_ix,crop_ix] - area_chng

                dmd[crop_ix] = dmd[crop_ix] - prod_chng
                target_dmd = target_dmd - prod_chng
                total_prod_chng = total_prod_chng + prod_chng
                
            }
        }

        if (index == n_cell) {
            index = 0
            if (total_prod_chng == 0) {
                no_chng_count = no_chng_count + 1
            }
            total_prod_chng = 0
        }
        
        ## continue until target is met or we know that
        ## the target cannot be met
        if ((target_dmd == 0) || no_chng_count > no_chng_count_max) {
            if (target_dmd == 0) print("Target demand met")
            if (no_chng_count > no_chng_count_max) print("Cannot meet target demand: returning early")
            break()
        }
    }
    area_mat
}

extensify = function(area_mat, tsuit_mat, yield_mat, cropland_area, dmd, target_extens, crop_ix, n_cell, n_season, n_input, cell_area, fact, no_chng_count_max=5, ...) {

    target_dmd = dmd[crop_ix] * target_extens
    target_dmd_start = target_dmd
    
    tsuit_v = tsuit_mat[,crop_ix]
    ordr = order(tsuit_v, decreasing=TRUE) ## high -> low

    ## TODO:
    ## * jitter suitability
    ## * make sure suitability in seasons apart from allowed growing
    ##   seasons is set to zero

    index = 0
    total_prod_chng = 0
    no_chng_count = 0
    repeat {

        index = index + 1
        ix = ordr[index]
        cell_ix = ceiling(ix / (n_season * n_input))
        season_ix = ceiling((ix - ((cell_ix - 1) * n_season * n_input)) / n_input)
        
        start_row_ix = (cell_ix-1) * (n_season * n_input) + (season_ix-1) * n_season + 1
        end_row_ix = start_row_ix + (n_input - 1)        
        alloc_area = sum(rowSums(area_mat[start_row_ix:end_row_ix,]))

        ## if annual crop and more than one growing season
        if (season_ix == 1 && n_season > 1) {
            a = rep(NA, n_season)
            a[1] = alloc_area
            for (i in 2:n_season) {
                start_row_ix = (cell_ix-1) * (n_season * n_input) + (i-1) * n_season + 1
                end_row_ix = start_row_ix + (n_input - 1)
                a[i] = sum(rowSums(area_mat[start_row_ix:end_row_ix,]))
            }
            alloc_area = sum(a[1], max(a[2:n_season]))
        }
        
        ## initial area change
        unalloc_area = cropland_area[cell_ix] - alloc_area
        if (unalloc_area > 0) {

            area_chng0 = min(unalloc_area, cell_area[cell_ix] * fact)
            prod_chng0 = area_chng0 * yield_mat[ix,crop_ix]

            if (prod_chng0 > 0) {

                if (prod_chng0 < target_dmd) {
                    prod_chng = prod_chng0
                    area_chng = area_chng0
                } else {
                    prod_chng = target_dmd
                    area_chng = prod_chng / yield_mat[ix,crop_ix]
                }

            } else {
                prod_chng = 0
                area_chng = 0
            }

            area_mat[ix,crop_ix] = area_mat[ix,crop_ix] + area_chng
            target_dmd = target_dmd - prod_chng
            total_prod_chng = total_prod_chng + prod_chng
        }

        if (index == n_cell * n_season * n_input) {
            index = 0
            if (total_prod_chng == 0) {
                no_chng_count = no_chng_count + 1
            }
            total_prod_chng = 0
        }

        ## continue until target is met or we know that the target
        ## cannot be met
        if ((target_dmd <= 0) || (no_chng_count > no_chng_count_max)){
            break()
        }                    
    }
    area_mat
}

set.seed(18384)

for (i in 2:length(time)) {

    print(i)

    ## total suitability based on revenue model of You et al. 2014

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
   
    ## calculate demand (the amount by which production must change,
    ## rather than total production)
    dmd0 = dmd[i-1,]      
    dmd1 = dmd[i,] - dmd0

    ## index and names of crops with increasing/decreasing demand
    decr_ix = dmd1 < 0
    incr_ix = !decr_ix

    decr_crops = crop_nms[decr_ix]
    incr_crops = crop_nms[incr_ix]

    ar = cell_area * fact
    
    ## first, allocate crops with decreasing demand, converting to
    ## holding capacity denoting the input level + water source

    ## **this seems to be working**
    
    converted_cropland = rep(0, n_cell * n_season * n_input)

    for (j in 1:length(alloc_order)) {
        crop = alloc_order[j]
        
        if (crop %in% decr_crops) {

            print("decreasing")

            crop_ix = crop_nms %in% crop
            tsuit_v = tsuit_mat[,crop_ix]
            ordr = order(tsuit_v, decreasing=FALSE) ## low -> high
            ## ordr = ordr[area_mat[ordr,crop_ix] > 0]

            index = 0
            repeat {

                index = index + 1
                ix = ordr[index]
                tmp_cell_ix = ceiling(ix / (n_season * n_input))

                if (area_mat[ix,crop_ix] > 0) {

                    ## initial area change
                    area_chng0 = min(area_mat[ix,crop_ix], cell_area[tmp_cell_ix] * fact)
                    prod_chng0 = area_chng0 * yield_mat[ix,crop_ix]

                    if (prod_chng0 < abs(dmd1[crop_ix])) {
                        prod_chng = prod_chng0
                        area_chng = area_chng0
                    } else {
                        prod_chng = abs(dmd1[crop_ix])
                        area_chng = prod_chng / yield_mat[ix,crop_ix]
                    }

                    area_mat[ix,crop_ix] = area_mat[ix,crop_ix] - area_chng
                    dmd1[crop_ix] = dmd1[crop_ix] + prod_chng
                    converted_cropland[ix] = converted_cropland[ix] + area_chng
                }
                
                if (index == n_cell * n_season * n_input) {
                    index = 0
                }
                
                if (abs(dmd1[crop_ix]) <= 0) {
                    break()
                }
                
            }
        }
    }
    ## next, allocate crops with increasing demand

    ## wrap in repeat {...} loop because crops which cannot be allocated
    ## are added to subcrops

    total_prod_chng = 0
    repeat {

        total_prod0 = colSums(area_mat * yield_mat)
        
        for (j in 1:length(alloc_order)) {

            crop = alloc_order[j]

            if (crop %in% incr_crops) {

                print("increasing")
                
                crop_ix = crop_nms %in% crop
                
                ## first round of intensification
                ## ##############################

                stop()
                ar0 = colSums(area_mat * yield_mat)[crop_ix]                

                area_mat = intensify(area_mat, tsuit_mat, yield_mat, dmd1, target_intens, crop_ix, n_cell, n_season, n_input, cell_area, fact, no_chng_count_max=2)

                chng = colSums(area_mat * yield_mat)[crop_ix] - ar0
                dmd1[crop_ix] = dmd1[crop_ix] - chng

                ## extensification
                ## ###############

                ## here we attempt to allocate the outstanding demand through
                ## extensification processes

                ar0 = colSums(area_mat * yield_mat)[crop_ix]
                
                area_mat = extensify(area_mat, tsuit_mat, yield_mat, cropland_area, dmd1, 1, crop_ix, n_cell, n_season, n_input, cell_area, fact)

                chng = colSums(area_mat * yield_mat)[crop_ix] - ar0
                dmd1[crop_ix] = dmd1[crop_ix] - chng
                
                ## second round of intensification
                ## ###############################

                ## if demand is still not met we perform another round of
                ## intensification

                ## set target_intens=1 so that the routine attempts to allocate
                ## all outstanding production

                if (dmd1[crop_ix] > sqrt(.Machine$double.eps)) {                    
                    ar0 = colSums(area_mat * yield_mat)[crop_ix]                    
                    area_mat = intensify(area_mat, tsuit_mat, yield_mat, dmd1, target_intens=1, crop_ix, n_cell, n_season, n_input, cell_area, fact)
                    chng = colSums(area_mat * yield_mat)[crop_ix] - ar0
                    dmd1[crop_ix] = dmd1[crop_ix] - chng
                }

                ## if there is still unallocated crop area, the outstanding
                ## demand should be allocated between other subcrops within
                ## the main crop class

                ## TODO: design table to indicate main crops/subcrops

                if (dmd1[crop_ix] > sqrt(.Machine$double.eps)) {                    
                    print(paste0("Cannot allocate ", dmd1[crop_ix], " Mt: assigning to other subcrops"))
                    outstanding_dmd = dmd1[crop_ix]
                    main_crop_ix = sapply(subcrops, FUN=function(x) crop %in% x)
                    if (length(which(main_crop_ix)) != 1) stop()
                    subcrop_nms = subcrops[[which(main_crop_ix)]]
                    subcrop_nms = subcrop_nms[!subcrop_nms %in% crop] ## i.e. exclude crop for which demand could not be satisfied
                    subcrop_ix = crop_nms %in% subcrop_nms
                    subcrop_dmd = dmd[i,subcrop_ix] ## demand for current time
                    dmd1[subcrop_ix] = dmd1[subcrop_ix] + outstanding_dmd * (subcrop_dmd / sum(subcrop_dmd))
                }
            }
        }

        ## break()
        total_prod1 = colSums(area_mat * yield_mat)
        any_change = !isTRUE(all.equal(total_prod0, total_prod1))

        if (all(dmd1 < sqrt(.Machine$double.eps)) || !any_change) {
            break()
        }
    }

    ## stop()
    
    dmd[i,] = colSums(area_mat * yield_mat)
    
    ## write output to file
    for (j in 1:length(alloc_order)) {
        crop = alloc_order[j]
        crop_ix = which(crop_nms %in% crop)
        
        for (k in 1:n_season) {
            
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

write.table(dmd, "data/output/allocated_area.txt", sep=" ", row.names=FALSE, col.names=TRUE)

