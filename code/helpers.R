## Author : Simon Moulds
## Date   : August 2017

## helper functions

interpolate_yield = function(yield, suit, ...) {
    ## Function to interpolate yield values to cells where MapSPAM
    ## yield data is not available but for which suitability values
    ## exist
    ##
    ## Args:
    ##   x : yield maps (list)
    ##   y : suitability maps (list)
    ##
    ## Return:
    ##   List containing updated yield maps

    input_levels = c("irri","rain_h","rain_l","rain_s")
    for (i in 1:length(input_levels)) {
        level = input_levels[[i]]
        x = yield[[level]]
        y = suit[[level]] ## not used!!!!!

        if (cellStats(x, stat=max) == 0) {
            idw = x
        } else {
            
            x[x == 0] = NA
            spdf = as(x, "SpatialPointsDataFrame") %>% setNames("yield")
            gs = gstat(formula=yield~1, data=spdf)
            idw <- interpolate(x, gs)

            idw[!is.na(x)] = x[!is.na(x)]
            ## idw[y <= 0 | is.na(y)] = 0
        }
        
        yield[[level]] = idw
    }

    ## NB 'total' and 'rain' are the mean yield, weighted according to
    ## harvested area

    yield
}

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

    ## if (suffix == "_ll") suffix = ""
    
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

    input_levels = c("h_schi", ## high input, irrigated
                     "i_scii", ## intermediate input, irrigated
                     "h_schr", ## high input, rainfed
                     "i_scir", ## intermediate input, rainfed
                     "l_sclr") ## low input, rainfed
    ## input_levels = c("h_suhi", ## high input, irrigated
    ##                  "i_suii", ## intermediate input, irrigated
    ##                  "h_suhr", ## high input, rainfed
    ##                  "i_suir", ## intermediate input, rainfed
    ##                  "l_sulr") ## low input, rainfed
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

get_gaez_potyld_data = function(crop, path, suffix, ...) {

    ## high/intermediate irrigated, high/intermediate/low rainfed
    input_levels = paste0(c("h","i","h","i","l"), "_", crop, c("000a","000a","150b","150b","150b"))

    out = list()
    count = 0
    for (i in 1:length(input_levels)) {
        level = input_levels[i]
        f = paste0("res02_crav6190", level, "_yld", suffix, ".tif")
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
        ## if (isLonLat(xx)) {
        ##     ca = raster::area(xx) * 1000 * 1000 / 10000 ## km2 -> Ha
        ## } else {
        ##     ca = res(xx)[1] * res(xx)[2] / 10000 ## m2 -> Ha
        ## }
        ## nb = focal(xx / ca, ...)
        xx[xx > 0] = 1
        nb = focal(xx, ...)
        out[[i]] = nb
    }
    names(out) = names(x)
    out
}
