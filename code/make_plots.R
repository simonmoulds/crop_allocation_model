library(rasterVis)
library(maptools)
library(grid)
library(viridis)
library(magrittr)
library(dplyr)
library(tidyr)

## demand
aggregate_dmd = function(x, ...) {
    x %<>% mutate(Wheat=whea,
                  Rice=rice,
                  Corn=maiz,
                  OtherGrain=barl + sorg + pmil + smil + ocer,
                  OilCrop=soyb + grou + sesa + sunf + rape + ooil,
                  FiberCrop=cott + ofib,
                  PalmFruit=cnut + oilp,
                  SugarCrop=sugc + sugb,
                  Root_Tuber=pota + swpo + yams + cass + orts,
                  MiscCrop=bean + chic + pige + lent + cowp + opul + trof + temf + bana + plnt + acof + rcof + coco + teas + toba + vege + rest)
    x = x[,!colnames(x) %in% c("acof","bana","barl","bean","cass","chic","cnut","coco","cott","cowp","grou","lent","maiz","ocer","ofib","oilp","ooil","opul","orts","pige","plnt","pmil","pota","rape","rcof","rest","rice","sesa","smil","sorg","soyb","sugb","sugc","sunf","swpo","teas","temf","toba","trof","vege","whea","yams")]
    x
}

time = seq(2005, 2100, by=5)
dmd = readRDS("data/gcam_reference_demand.rds") %>% as.data.frame %>% aggregate_dmd
alloc = read.table("data/output/allocated_area.txt", sep=" ", header=TRUE) %>% aggregate_dmd

names(alloc) = paste0(names(alloc), "_")
x = cbind(data.frame(time=time), dmd, alloc) %>%
    gather(crop, production, -time) %>%
    mutate(crop=factor(crop))

## x = cbind(data.frame(time=time), dmd) %>%
##     gather(crop, production, -time) %>%
##     mutate(crop=factor(crop))

## cols=rep(rainbow(10), each=2)
cols=rep(rainbow(10))
p = xyplot(production~time,
           group=crop,
           data=x,
           par.settings=list(background = list(col = "transparent")),
           ## type="l",
           ## lty=1,
           type=rep(c("l","p"), 10),
           lty=rep(c(1,0), 10),
           ## lwd=5,
           pch=rep(c(NA,1), 10),
           col=cols,
           xlab=list("Year", cex=1.5),
           ylab=list("Production (tonnes)", cex=1.5),
           ## scales=list(cex=1.5, tck=1, y=list(rot=0)),
           scales=list(cex=1.5, tck=1, y=list(rot=0, at=c(0,0.5e8, 1e8, 1.5e8, 2e8, 2.5e8), labels=expression(0, 0.5 %*% 10^8, 1.0 %*% 10^8, 1.5 %*% 10^8, 2.0 %*% 10^8, 2.5 %*% 10^8))),
           key=list(space="right",
                    lines=list(col=cols, lwd=5),
                    text=list(levels(x$crop), cex=1.5),
                    columns=1,
                    padding.text=4),
           panel=function(...) {
               grid.rect(gp=gpar(col=NA, fill="white"))
               panel.xyplot(...)
           })
## key=list(space="right",
           ##          text=list(levels(x$crop)[seq(1,19,by=2)], cex=2),
           ##          lines=list(col=cols[seq(1,19,by=2)]),
           ##          columns=1))
p

pname <- file.path(file.path("doc/gcam_production_output.png"))
trellis.device(device="png", width=9, height=6.25, units="in", res=320, file=pname, bg="transparent")
print(p)
dev.off()

sp = getRgshhsMap(fn = system.file("share/gshhs_c.b", package= "maptools"), xlim=c(60,105), ylim=c(5,40))

## ======================================
## model output
## ======================================

## wheat, irrigated
## ################

fs = list.files("data/output", "INDIA_RICE_KHARIF_IRRI_[0-9]{4}.tif", full.names=TRUE)##[c(1,3,5,7,9,11,13,15,17,19)]

st = stack(fs)

p = levelplot(st,
              margin=FALSE,
              xlab=NULL,
              ylab=NULL,
              par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5)), background=list(col="transparent"), layout.widths=list(right.padding=1)),
              par.strip.text=list(cex=1.25),
              names.attr=as.character(seq(2010,2100,5)),
              ## names.attr=c("Irrigated","Rainfed (high input)","Rainfed (low input)","Subsistence"),
              col.regions=viridis(91, direction=-1),
              layout=c(4,5),
              scales=list(draw=TRUE, cex=1.25),
              colorkey=list(space="bottom",
                            width=1,
                            at=seq(0,90000,length.out=91),
                            labels=list(at=seq(0,80000,10000),
                                        labels=seq(0,80000,10000), cex=1.25), tck=0),
              panel=function(...) {
                  grid.rect(gp=gpar(col=NA, fill="white"))
                  panel.levelplot(...)
              })

p <- p + layer(sp.polygons(sp, lwd=1))

pname <- file.path(file.path("doc/wheat_sim.png"))
trellis.device(device="png", width=9, height=13.5, units="in", res=320, file=pname, bg="transparent")
print(p)
pushViewport(viewport(0.52,0.015,0.5,0.07))  ## TODO
grid.text("Area (ha)", gp=gpar(cex=1.25, font=1, col="black"))
dev.off()

## ======================================
## input data
## ======================================

## plots of input data
source("code/helpers.R")

mapspam_path = "data/mapspam_data"
gaez_path = "data/gaez_data"

suffix = "_ll"
fact=3

india_rgn =
    raster(file.path("data", paste0("gcam_32rgn_rast", suffix, ".tif"))) %>%
    { if (fact > 1) raster::aggregate(x=., fact=fact, fun=modal) else .}
india_rgn[india_rgn != 17] = NA
india_rgn %<>% trim
india_rgn[!is.na(india_rgn)] = 1
india_ext = extent(india_rgn)

## wheat (physical area, yield)
## ############################

template = raster(xmn=-180, xmx=180, ymn=-90, ymx=90, nrow=2160, ncol=4320)
whea_phys =
    get_mapspam_data("whea", mapspam_path, "physical_area", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% { if(fact > 1) raster::aggregate(x=., fact=fact, fun=sum) else . } %>% crop(india_ext) %>% `*`(india_rgn)) %>% `[`(c(2,4,5,6)) %>% stack

p = levelplot(whea_phys,
              margin=FALSE,
              xlab=NULL,
              ylab=NULL,
              par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5)), background=list(col="transparent"), layout.widths=list(right.padding=1)),
              par.strip.text=list(cex=1.25),
              names.attr=c("Irrigated","Rainfed (high input)","Rainfed (low input)","Subsistence"),
              col.regions=viridis(91, direction=-1),
              layout=c(4,1),
              scales=list(draw=TRUE, cex=1.25),
              colorkey=list(space="bottom",
                            width=1,
                            at=seq(0,90000,length.out=91),
                            labels=list(at=seq(0,80000,10000),
                                        labels=seq(0,80000,10000), cex=1.25), tck=0),
              panel=function(...) {
                  grid.rect(gp=gpar(col=NA, fill="white"))
                  panel.levelplot(...)
              })

p <- p + layer(sp.polygons(sp, lwd=1))##, fill="lightgrey"))

pname <- file.path(file.path("doc/wheat_mapspam_physical_area.png"))
trellis.device(device="png", width=9, height=4, units="in", res=320, file=pname, bg="transparent")
print(p)
pushViewport(viewport(0.52,0.030,0.5,0.15))  ## TODO
grid.text("Area (ha)", gp=gpar(cex=1.25, font=1, col="black"))
dev.off()

whea_yield =
    get_mapspam_data("whea", mapspam_path, "yield", suffix=suffix) %>%
    lapply(FUN=function(x) x %>% { if(fact > 1) raster::aggregate(x=., fact=fact, fun=mean) else . } %>% crop(india_ext) %>% `*`(india_rgn)) %>% `[`(c(2,4,5,6)) %>% stack %>% `/`(1000)

p = levelplot(whea_yield,
              margin=FALSE,
              xlab=NULL,
              ylab=NULL,
              par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5)), background=list(col="transparent")),
              par.strip.text=list(cex=1.25),
              names.attr=c("Irrigated","Rainfed (high input)","Rainfed (low input)","Subsistence"),
              col.regions=viridis(1201, direction=-1),
              layout=c(4,1),
              scales=list(draw=TRUE, cex=1.25),
              colorkey=list(space="bottom",
                            width=1,
                            at=seq(0,12,length.out=1201),
                            labels=list(at=seq(0,12,2),
                                        labels=seq(0,12,2), cex=1.25), tck=0),
              panel=function(...) {
                  grid.rect(gp=gpar(col=NA, fill="white"))
                  panel.levelplot(...)
              })

p <- p + layer(sp.polygons(sp, lwd=1))

pname <- file.path(file.path("doc/wheat_mapspam_yield.png"))
trellis.device(device="png", width=9, height=4, units="in", res=320, file=pname, bg="transparent")
print(p)
pushViewport(viewport(0.52,0.030,0.5,0.15))  ## TODO
grid.text("Yield (t/ha)", gp=gpar(cex=1.25, font=1, col="black"))
dev.off()

## wheat (potential yield)
## #######################

whea_potyld =
    get_gaez_potyld_data("whea", gaez_path, suffix=suffix) %>%
    lapply(FUN=function(x) x %>% { if(fact > 1) raster::aggregate(x=., fact=fact, fun=mean) else . } %>% crop(india_ext) %>% `*`(india_rgn)) %>%
    `[`(c(1,1,3,3,4,4)) %>%
    setNames(c("total","irri","rain","rain_h","rain_l","rain_s")) %>%
    `[`(c(2,4,5,6)) %>% stack %>% `/`(1000)

p = levelplot(whea_potyld,
              margin=FALSE,
              xlab=NULL,
              ylab=NULL,
              par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5)), background=list(col="transparent")),
              par.strip.text=list(cex=1.25),
              names.attr=c("Irrigated","Rainfed (high input)","Rainfed (low input)","Subsistence"),
              col.regions=viridis(1201, direction=-1),
              layout=c(4,1),
              scales=list(draw=TRUE, cex=1.25),
              colorkey=list(space="bottom",
                            width=1,
                            at=seq(0,12,length.out=1201),
                            labels=list(at=seq(0,12,2),
                                        labels=seq(0,12,2), cex=1.25), tck=0),
              panel=function(...) {
                  grid.rect(gp=gpar(col=NA, fill="white"))
                  panel.levelplot(...)
              })

p <- p + layer(sp.polygons(sp, lwd=1))##, fill="lightgrey"))

pname <- file.path(file.path("doc/whea_gaez_potential_yield.png"))
trellis.device(device="png", width=9, height=4, units="in", res=320, file=pname, bg="transparent")
print(p)
pushViewport(viewport(0.52,0.030,0.5,0.15))  ## TODO
grid.text("Yield (t/ha)", gp=gpar(cex=1.25, font=1, col="black"))
dev.off()

## population density
## ##################

fs = list.files("data/SSP Population Projections", pattern="^ssp[0-9]{1}_[0-9]{4}_dens_5m.tif$", full.names=TRUE)
st =
    stack(fs) %>% unstack %>%
    lapply(FUN=function(x) x %>% { if(fact > 1) raster::aggregate(x=., fact=fact, fun=mean) else . } %>% crop(india_ext) %>% `*`(india_rgn))

## mx = max(cellStats(st, "max"))

## for (i in 1:length(st)) {
##     r = st[[i]]
##     r[is.nan(r)] = 0
##     st[[i]] = r
## }

st = stack(st)
## st[st == 0] = NA
## st[st > 20000] = 20000
mx = max(cellStats(st, "max"))

## p = myfun(st, layout=c(5,2), at=seq(0,mx,length=101))
p = levelplot(st,
              margin=FALSE,
              xlab=NULL,
              ylab=NULL,
              par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5)), background=list(col="transparent")),
              par.strip.text=list(cex=1.25),
              names.attr=paste("SSP2", seq(2010,2100,10)),
              col.regions=viridis(101, direction=-1),
              at = seq(0,mx,length=101),
              layout=c(5,2),
              scales=list(draw=TRUE, cex=1.25),
              colorkey=list(space="bottom",
                            width=1,
                            at=seq(0,mx,length.out=101),
                            labels=list(at=seq(0,20000,4000),
                                        labels=seq(0,20000,4000), cex=1.25), tck=0),
              panel=function(...) {
                  grid.rect(gp=gpar(col=NA, fill="white"))
                  panel.levelplot(...)
              })

p <- p + layer(sp.polygons(sp, lwd=1))

pname <- file.path(file.path("doc/ssp2_popdens.png"))
trellis.device(device="png", width=9, height=5.6, units="in", res=320, file=pname, bg="transparent")
print(p)
pushViewport(viewport(0.52,0.030,0.5,0.15))  ## TODO
grid.text("Population density (people/km2)", gp=gpar(cex=1.25, font=1, col="black"))
dev.off()

## land use (from GCAMLU)
## ######################

## TODO

x = read.csv("/home/simon/projects/GCAMLU/Outputs/GCAMref/Spatial_LU/2010_LU.csv")
coords = x[,c("Loncoord","Latcoord")] %>% setNames(c("x","y"))
coords = SpatialPoints(coords=coords, proj4string=CRS("+proj=longlat +datum=WGS84"))
template = raster(xmn=-180, xmx=180, ymn=-90, ymx=90, nrow=720, ncol=1440)

years = c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100)
data = vector(mode="list", length=length(years))
for (i in 1:length(years)) {
    xx = read.csv(file.path("/home/simon/projects/GCAMLU/Outputs/GCAMref/Spatial_LU", paste0(years[i], "_LU.csv")))
    r = template
    r[coords] = x$crops
    data[[i]] = r
}

data = stack(data) %>% crop(india_ext) %>% `*`(india_rgn) %>% `*`(1000 * 1000 / 10000)

p = levelplot(data,
              margin=FALSE,
              xlab=NULL,
              ylab=NULL,
              par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5)), background=list(col="transparent"), layout.widths=list(right.padding=1)),
              par.strip.text=list(cex=1.25),
              names.attr=as.character(seq(2010,2100,10)),
              col.regions=viridis(91, direction=-1),
              layout=c(5,2),
              scales=list(draw=TRUE, cex=1.25),
              colorkey=list(space="bottom",
                            width=1,
                            at=seq(0,90000,length.out=91),
                            labels=list(at=seq(0,80000,10000),
                                        labels=seq(0,80000,10000), cex=1.25), tck=0),
              panel=function(...) {
                  grid.rect(gp=gpar(col=NA, fill="white"))
                  panel.levelplot(...)
              })

p <- p + layer(sp.polygons(sp, lwd=1))##, fill="lightgrey"))

pname <- file.path(file.path("doc/gcamlu_cropland_area.png"))
trellis.device(device="png", width=9, height=5.6, units="in", res=320, file=pname, bg="transparent")
print(p)
pushViewport(viewport(0.52,0.030,0.5,0.15))  ## TODO
grid.text("Area (Ha)", gp=gpar(cex=1.25, font=1, col="black"))
dev.off()










## not used:

## ## rice, irrigated
## ## ###############
## fs = list.files("data/output", "INDIA_RICE_KHARIF_IRRI_[0-9]{4}.tif", full.names=TRUE)[c(1,3,5,7,9,11,13,15,17,19)]
## p = myfun(stack(fs), layout=c(4,3))
## p <- p + layer(sp.polygons(sp, lwd=1))

## pname <- file.path(file.path("doc/rice_sim.png"))
## trellis.device(device="png", width=12, height=12, units="in", res=320, file=pname)
## print(p)
## pushViewport(viewport(0.52,0.015,0.5,0.07))  ## TODO
## grid.text("Area (Ha)", gp=gpar(cex=0.8, font=1, col="black"))
## dev.off()

## ## rice (physical area, yield)
## ## ###########################

## rice_phys =
##     get_mapspam_data("rice", mapspam_path, "physical_area", suffix=suffix) %>%
##     lapply(FUN=function(x) x %>% { if(fact > 1) raster::aggregate(x=., fact=fact, fun=sum) else . } %>% crop(india_ext) %>% `*`(india_rgn)) %>% `[`(c(2,4,5,6)) %>% stack

## p = myfun(rice_phys, layout=c(4,1))

## pname <- file.path(file.path("doc/rice_mapspam_physical_area.png"))
## trellis.device(device="png", width=12, height=6, units="in", res=320, file=pname)
## print(p)
## pushViewport(viewport(0.52,0.015,0.5,0.07))  ## TODO
## grid.text("Area (Ha)", gp=gpar(cex=0.8, font=1, col="black"))
## dev.off()

## rice_yield =
##     get_mapspam_data("rice", mapspam_path, "yield", suffix=suffix) %>%
##     lapply(FUN=function(x) x %>% { if(fact > 1) raster::aggregate(x=., fact=fact, fun=sum) else . } %>% crop(india_ext) %>% `*`(india_rgn)) %>% `[`(c(2,4,5,6)) %>% stack

## p = myfun(rice_yield, layout=c(4,1))

## pname <- file.path(file.path("doc/rice_mapspam_yield.png"))
## trellis.device(device="png", width=12, height=6, units="in", res=320, file=pname)
## print(p)
## pushViewport(viewport(0.52,0.015,0.5,0.07))  ## TODO
## grid.text("Yield (kg/ha)", gp=gpar(cex=0.8, font=1, col="black"))
## dev.off()

## ## rice (potential yield)
## ## ######################

## rice_potyld_w = get_gaez_potyld_data("ricw", gaez_path, suffix=suffix)
## rice_potyld_d = get_gaez_potyld_data("ricd", gaez_path, suffix=suffix)

## rice_potyld =
##     c(rice_potyld_w[1:2], rice_potyld_d[1:3]) %>%
##     lapply(FUN=function(x) x %>% { if(fact > 1) raster::aggregate(x=., fact=fact, fun=mean) else . } %>% crop(india_ext) %>% `*`(india_rgn)) %>%
##     `[`(c(1,1,3,3,4,4)) %>%
##     setNames(c("total","irri","rain","rain_h","rain_l","rain_s")) %>%
##     `[`(c(2,4,5,6)) %>% stack

## p = myfun(rice_potyld, layout=c(4,1))

## pname <- file.path(file.path("doc/rice_gaez_potential_yield.png"))
## trellis.device(device="png", width=12, height=6, units="in", res=320, file=pname)
## print(p)
## pushViewport(viewport(0.52,0.015,0.5,0.07))  ## TODO
## grid.text("Yield (kg/ha)", gp=gpar(cex=0.8, font=1, col="black"))
## dev.off()

## ## create bounding box for India
## east  <- 100  ## India bounding box
## west  <- 65
## north <- 38
## south <- 6
## nr <- (north - south) / (1/12) ## 1/12 degree resolution
## nc <- (east - west) / (1/12)   

## template <- raster(nrow=nr, ncol=nc, xmn=west, xmx=east, ymn=south, ymx=north)

## world = readShapePoly("data/ne_50m_admin_0_countries")

## ## ## MapSPAM - Harvested area
## ## fs = list.files(pattern="SPAM2005V3r1_global_H_(TI|TH|TL|TS)_WHEA_[A-Z]{1}_ll.tif") %>% `[`(c(2,1,3,4))

## ## st = stack(fs) %>% crop(extent(template)) %>% setNames(toupper(c("Wheat_irrigated_high_input","Wheat_rainfed_high_input","Wheat_rainfed_low_input","Wheat_rainfed_subsistence")))
## ## st[st > 10000] = 10000
## ## st[st == 0] = NA

## ## p = levelplot(st,
## ##               margin=FALSE,
## ##               xlab=NULL,
## ##               ylab=NULL,
## ##               par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
## ##               par.strip.text=list(cex=0.6),
## ##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(11),
## ##               at=seq(0,10000,length=11),
## ##               scales=list(draw=TRUE, cex=0.6),
## ##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.6), tck=0))

## ## p <- p + layer(sp.polygons(world, lwd=1))
## ## ## p

## ## library(grid)
## ## pname <- file.path(file.path("wheat_mapspam.png"))
## ## trellis.device(device="png", width=5, height=5.65, units="in", res=320, file=pname)
## ## print(p)
## ## pushViewport(viewport(0.52,0.025,0.5,0.07))  ## TODO
## ## grid.text("Area (Ha)", gp=gpar(cex=0.6, font=1, col="black"))
## ## dev.off()

## ## ## MapSPAM - Yield?
## ## fs = list.files(pattern="SPAM2005V3r1_global_Y_(TI|TH|TL|TS)_WHEA_[A-Z]{1}.tif") %>% `[`(c(2,1,3,4))

## ## st = stack(fs) %>% crop(extent(template)) %>% setNames(toupper(c("Wheat_irrigated_high_input","Wheat_rainfed_high_input","Wheat_rainfed_low_input","Wheat_rainfed_subsistence")))
## ## st[st == 0] = NA

## ## p = levelplot(st,
## ##               margin=FALSE,
## ##               xlab=NULL,
## ##               ylab=NULL,
## ##               par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
## ##               par.strip.text=list(cex=0.6),
## ##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(11),
## ##               at=seq(0,10000,length=11),
## ##               scales=list(draw=TRUE, cex=0.6),
## ##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.6), tck=0))

## ## p <- p + layer(sp.polygons(world, lwd=1))
## ## ## p

## ## library(grid)
## ## pname <- file.path(file.path("wheat_yield.png"))
## ## trellis.device(device="png", width=5, height=5.65, units="in", res=320, file=pname)
## ## print(p)
## ## pushViewport(viewport(0.52,0.025,0.5,0.07))  ## TODO
## ## grid.text("Yield (kg/hectare)", gp=gpar(cex=0.6, font=1, col="black"))
## ## dev.off()

## ## ## GAEZ - wheat suitability
## ## fs = list.files(pattern="res03_crav6190[a-z]{1}_[a-z]{2}(hi|hr|lr)_whe_ll.tif") %>% `[`(c(1,2,3,3))

## ## st = stack(fs) %>% crop(extent(template)) %>% setNames(toupper(c("Wheat_irrigated_high_input","Wheat_rainfed_high_input","Wheat_rainfed_low_input","Wheat_rainfed_subsistence")))
## ## st[st == 0] = NA
## ## st = st / 10000

## ## p = levelplot(st,
## ##               margin=FALSE,
## ##               xlab=NULL,
## ##               ylab=NULL,
## ##               par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
## ##               par.strip.text=list(cex=0.6),
## ##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(11),
## ##               at=seq(0,1,length=11),
## ##               scales=list(draw=TRUE, cex=0.6),
## ##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.6), tck=0))

## ## p <- p + layer(sp.polygons(world, lwd=1))
## ## ## p

## ## library(grid)
## ## pname <- file.path(file.path("wheat_suit.png"))
## ## trellis.device(device="png", width=5, height=5.65, units="in", res=320, file=pname)
## ## print(p)
## ## pushViewport(viewport(0.52,0.025,0.5,0.07))  ## TODO
## ## grid.text("Biophysical suitability (-)", gp=gpar(cex=0.6, font=1, col="black"))
## ## dev.off()

## ## ======================================
## ## model output
## ## ======================================

## ## wheat
## ## #####

## fs = list.files(path="data", pattern="INDIA_WHEA_RABI_IRRI_(2010|2020|2030|2040)_ll.tif", full.names=TRUE)[c(1,2,3,3)]

## st = stack(fs) %>% setNames(c("WHEAT_RABI_IRRI_2010","WHEAT_RABI_IRRI_2020","WHEAT_RABI_IRRI_2030","WHEAT_RABI_IRRI_2040"))

## ## st = stack(fs) ## %>% setNames(c("WHEAT_RABI_IRRIGATED_2010","WHEAT_RABI_IRRIGATED_2040","WHEAT_RABI_IRRIGATED_2070","WHEAT_RABI_IRRIGATED_2100"))
## ## st[st == 0] = NA

## ## colSums(getValues(st), na.rm=TRUE)
## ## sum(getValues(st), na.rm=TRUE)
## sp = getRgshhsMap(fn = system.file("share/gshhs_c.b", package= "maptools"), xlim=c(60,105), ylim=c(5,40))

## library(rasterVis)
## p = levelplot(st,
##               margin=FALSE,
##               xlab=NULL,
##               ylab=NULL,
##               par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
##               par.strip.text=list(cex=0.8),
##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(11),
##               at=seq(0,10000,length=11),
##               scales=list(draw=TRUE, cex=0.8),
##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.8), tck=0))

## p <- p + layer(sp.polygons(sp, lwd=1))

## library(grid)
## pname <- file.path(file.path("/home/simon/Dropbox/wheat_sim.png"))
## trellis.device(device="png", width=6, height=6.25, units="in", res=320, file=pname)
## print(p)
## pushViewport(viewport(0.52,0.015,0.5,0.07))  ## TODO
## grid.text("Area (Ha)", gp=gpar(cex=0.8, font=1, col="black"))
## dev.off()

## ## rice
## ## ####

## fs = list.files(path="data", pattern="INDIA_RICE_RABI_IRRI_(2010|2020|2030|2040)_ll.tif", full.names=TRUE)[c(1,2,3,3)]

## st = stack(fs) %>% setNames(c("RICE_RABI_IRRI_2010","RICE_RABI_IRRI_2020","RICE_RABI_IRRI_2030","RICE_RABI_IRRI_2040"))
## ## st[st == 0] = NA

## ## sum(getValues(st), na.rm=TRUE)

## library(rasterVis)
## p = levelplot(st,
##               margin=FALSE,
##               xlab=NULL,
##               ylab=NULL,
##               par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
##               par.strip.text=list(cex=0.8),
##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(11),
##               at=seq(0,10000,length=11),
##               scales=list(draw=TRUE, cex=0.8),
##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.8), tck=0))

## p <- p + layer(sp.polygons(sp, lwd=1))

## library(grid)
## pname <- file.path(file.path("/home/simon/Dropbox/rice_sim.png"))
## trellis.device(device="png", width=6, height=6.25, units="in", res=320, file=pname)
## print(p)
## pushViewport(viewport(0.52,0.015,0.5,0.07))  ## TODO
## grid.text("Area (Ha)", gp=gpar(cex=0.8, font=1, col="black"))
## dev.off()


## wheat_prod = raster("~/Downloads/spam2005v2r0_production_wheat_total.tiff") %>% crop(template)
## rice_prod = raster("~/Downloads/spam2005v2r0_production_rice_total.tiff") %>% crop(template)
## ## st = stack(wheat_prod, rice_prod) %>% crop(template)
## p = levelplot(wheat_prod,
##               margin=FALSE,
##               xlab=NULL,
##               ylab=NULL,
##               par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
##               par.strip.text=list(cex=0.8),
##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(101),
##               ## at=seq(0,10000,length=101),
##               scales=list(draw=TRUE, cex=0.8),
##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.8), tck=0))

## p <- p + layer(sp.polygons(sp, lwd=1))

## library(grid)
## pname <- file.path(file.path("/home/simon/Dropbox/wheat_prod_mapspam.png"))
## trellis.device(device="png", width=4, height=4, units="in", res=320, file=pname)
## print(p)
## pushViewport(viewport(0.52,0.015,0.5,0.07))  ## TODO
## grid.text("Production (t)", gp=gpar(cex=0.8, font=1, col="black"))
## dev.off()

## p = levelplot(rice_prod,
##               margin=FALSE,
##               xlab=NULL,
##               ylab=NULL,
##               par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
##               par.strip.text=list(cex=0.8),
##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(101),
##               ## at=seq(0,10000,length=101),
##               scales=list(draw=TRUE, cex=0.8),
##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.8), tck=0))

## p <- p + layer(sp.polygons(sp, lwd=1))

## library(grid)
## pname <- file.path(file.path("/home/simon/Dropbox/rice_prod_mapspam.png"))
## trellis.device(device="png", width=4, height=4, units="in", res=320, file=pname)
## print(p)
## pushViewport(viewport(0.52,0.015,0.5,0.07))  ## TODO
## grid.text("Production (t)", gp=gpar(cex=0.8, font=1, col="black"))
## dev.off()


## wheat_suit = raster("data/gaez_data/res03_crav6190h_suhi_whe_ll.tif") %>% crop(template)
## wheat_suit = wheat_suit / 10000
## p = levelplot(wheat_suit,
##               margin=FALSE,
##               xlab=NULL,
##               ylab=NULL,
##               par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
##               par.strip.text=list(cex=0.8),
##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(101),
##               ## at=seq(0,10000,length=101),
##               scales=list(draw=TRUE, cex=0.8),
##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.8), tck=0))

## p <- p + layer(sp.polygons(sp, lwd=1))

## library(grid)
## pname <- file.path(file.path("/home/simon/Dropbox/wheat_suit.png"))
## trellis.device(device="png", width=4, height=4, units="in", res=320, file=pname)
## print(p)
## pushViewport(viewport(0.52,0.015,0.5,0.07))  ## TODO
## grid.text("Biophysical suitability (-)", gp=gpar(cex=0.8, font=1, col="black"))
## dev.off()

## rice_suit = raster("data/gaez_data/res03_crav6190h_suhi_rcw_ll.tif") %>% crop(template)
## rice_suit = rice_suit / 10000

## p = levelplot(rice_suit,
##               margin=FALSE,
##               xlab=NULL,
##               ylab=NULL,
##               par.settings=list(axis.line=list(lwd=1), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
##               par.strip.text=list(cex=0.8),
##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(101),
##               ## at=seq(0,10000,length=101),
##               scales=list(draw=TRUE, cex=0.8),
##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.8), tck=0))

## p <- p + layer(sp.polygons(sp, lwd=1))

## library(grid)
## pname <- file.path(file.path("/home/simon/Dropbox/rice_suit.png"))
## trellis.device(device="png", width=4, height=4, units="in", res=320, file=pname)
## print(p)
## pushViewport(viewport(0.52,0.015,0.5,0.07))  ## TODO
## grid.text("Biophysical suitability (-)", gp=gpar(cex=0.8, font=1, col="black"))
## dev.off()
