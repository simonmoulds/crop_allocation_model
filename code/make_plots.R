library(rasterVis)
library(maptools)

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
dmd = readRDS("data/gcam_reference_demand.rds") %>% as.data.frame(dmd) %>% aggregate_dmd
alloc = read.table("data/output/allocated_area.txt", sep=" ", header=TRUE) %>% aggregate_dmd

names(alloc) = paste0(names(alloc), "_")
x = cbind(data.frame(time=time), dmd, alloc) %>%
    gather(crop, production, -time) %>%
    mutate(crop=factor(crop))

cols=rep(rainbow(10), each=2)
xyplot(production~time,
       group=crop,
       data=x,
       type=rep(c("l","p"), 10),
       lty=rep(c(1,0), 10),
       pch=rep(c(NA,1), 10),
       col=cols,
       xlab=list("Year", cex=1),
       ylab=list("Production", cex=1),
       scales=list(cex=1, tck=1, y=list(rot=0)),
       key=list(space="bottom",
                text=list(levels(x$crop)[seq(1,19,by=2)], cex=1),
                lines=list(col=cols[seq(1,19,by=2)]),
                columns=2))

## time series of different crops (wheat, rice)

myfun=function(x, ...) {
    p = levelplot(x,
                  margin=FALSE,
                  xlab=NULL,
                  ylab=NULL,
                  par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
                  par.strip.text=list(cex=0.8),
                  col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(101),
                  ## at=seq(0,mx,length=101),
                  scales=list(draw=TRUE, cex=0.8),
                  colorkey=list(space="bottom", width=0.75, labels=list(cex=0.8), tck=0))
    p
}

sp = getRgshhsMap(fn = system.file("share/gshhs_c.b", package= "maptools"), xlim=c(60,105), ylim=c(5,40))

## wheat, irrigated
fs = list.files("data/output", "INDIA_WHEA_RABI_IRRI_[0-9]{4}.tif", full.names=TRUE)[1:18]
p = myfun(stack(fs))
p <- p + layer(sp.polygons(sp, lwd=0.5))
p

## rice, irrigated
fs = list.files("data/output", "INDIA_RICE_KHARIF_IRRI_[0-9]{4}.tif", full.names=TRUE)[1:18]
p = myfun(stack(fs))
p <- p + layer(sp.polygons(sp, lwd=0.5))
p

## library(grid)
## pname <- file.path(file.path("/home/simon/Dropbox/wheat_sim.png"))
## trellis.device(device="png", width=6, height=6.25, units="in", res=320, file=pname)
## print(p)
## pushViewport(viewport(0.52,0.015,0.5,0.07))  ## TODO
## grid.text("Area (Ha)", gp=gpar(cex=0.8, font=1, col="black"))
## dev.off()









## not used:

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
## ##               par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
## ##               par.strip.text=list(cex=0.6),
## ##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(11),
## ##               at=seq(0,10000,length=11),
## ##               scales=list(draw=TRUE, cex=0.6),
## ##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.6), tck=0))

## ## p <- p + layer(sp.polygons(world, lwd=0.5))
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
## ##               par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
## ##               par.strip.text=list(cex=0.6),
## ##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(11),
## ##               at=seq(0,10000,length=11),
## ##               scales=list(draw=TRUE, cex=0.6),
## ##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.6), tck=0))

## ## p <- p + layer(sp.polygons(world, lwd=0.5))
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
## ##               par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
## ##               par.strip.text=list(cex=0.6),
## ##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(11),
## ##               at=seq(0,1,length=11),
## ##               scales=list(draw=TRUE, cex=0.6),
## ##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.6), tck=0))

## ## p <- p + layer(sp.polygons(world, lwd=0.5))
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
##               par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
##               par.strip.text=list(cex=0.8),
##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(11),
##               at=seq(0,10000,length=11),
##               scales=list(draw=TRUE, cex=0.8),
##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.8), tck=0))

## p <- p + layer(sp.polygons(sp, lwd=0.5))

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
##               par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
##               par.strip.text=list(cex=0.8),
##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(11),
##               at=seq(0,10000,length=11),
##               scales=list(draw=TRUE, cex=0.8),
##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.8), tck=0))

## p <- p + layer(sp.polygons(sp, lwd=0.5))

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
##               par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
##               par.strip.text=list(cex=0.8),
##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(101),
##               ## at=seq(0,10000,length=101),
##               scales=list(draw=TRUE, cex=0.8),
##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.8), tck=0))

## p <- p + layer(sp.polygons(sp, lwd=0.5))

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
##               par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
##               par.strip.text=list(cex=0.8),
##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(101),
##               ## at=seq(0,10000,length=101),
##               scales=list(draw=TRUE, cex=0.8),
##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.8), tck=0))

## p <- p + layer(sp.polygons(sp, lwd=0.5))

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
##               par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
##               par.strip.text=list(cex=0.8),
##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(101),
##               ## at=seq(0,10000,length=101),
##               scales=list(draw=TRUE, cex=0.8),
##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.8), tck=0))

## p <- p + layer(sp.polygons(sp, lwd=0.5))

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
##               par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0.5), top=list(tck=0.5), bottom=list(tck=0.5))),
##               par.strip.text=list(cex=0.8),
##               col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(101),
##               ## at=seq(0,10000,length=101),
##               scales=list(draw=TRUE, cex=0.8),
##               colorkey=list(space="bottom", width=0.75, labels=list(cex=0.8), tck=0))

## p <- p + layer(sp.polygons(sp, lwd=0.5))

## library(grid)
## pname <- file.path(file.path("/home/simon/Dropbox/rice_suit.png"))
## trellis.device(device="png", width=4, height=4, units="in", res=320, file=pname)
## print(p)
## pushViewport(viewport(0.52,0.015,0.5,0.07))  ## TODO
## grid.text("Biophysical suitability (-)", gp=gpar(cex=0.8, font=1, col="black"))
## dev.off()
