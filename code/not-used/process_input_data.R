## Author : Simon Moulds
## Date   : June 2017

library(raster)
library(magrittr)
library(tidyr)
library(dplyr)

options(stringsAsFactors = FALSE)

## ======================================
## FAOSTAT
## ======================================

## extract bulk data download
system("unzip -o data/rawdata/FAOSTAT/Production_Crops_E_All_Data.zip -d data/FAOSTAT")

Sys.setlocale('LC_ALL','C') 
iso_lut = read.csv("data/AGLU_ctry.csv", skip=3, header=TRUE)
rgn_lut = read.csv("data/gcam_country_region_mapping.csv", header=TRUE)
rgn_nms = read.csv("data/gcam_32_region_id.csv", header=TRUE)
ag_lut  = read.csv("data/FAO_ag_items_PRODSTAT.csv", skip=3, header=TRUE)

fao =
    read.csv("data/FAOSTAT/Production_Crops_E_All_Data.csv") %>%
    filter(Element %in% "Area harvested") %>%
    mutate_each(funs(replace(., .=="Cabo Verde","Cape Verde"))) %>%
    mutate_each(funs(replace(., .=="C\364te d'Ivoire","Cote dIvoire")), Area) %>%
    mutate_each(funs(replace(., .=="Czechia","Czech Republic")), Area) %>%
    mutate_each(funs(replace(., .=="Democratic People's Republic of Korea","Democratic Peoples Republic of Korea")), Area) %>%
    mutate_each(funs(replace(., .=="Lao People's Democratic Republic","Lao Peoples Democratic Republic")), Area) %>%
    mutate_each(funs(replace(., .=="Micronesia","Micronesia (Federated States of)")), Area) %>%
    mutate_each(funs(replace(., .=="Polynesia","French Polynesia")), Area) %>%
    mutate_each(funs(replace(., .=="R\351union","Reunion")), Area) %>%
    mutate_each(funs(replace(., .=="Saint Helena, Ascension and Tristan da Cunha","Saint Helena")), Area) %>%
    mutate_each(funs(replace(., .=="China, Taiwan Province of","Taiwan")), Area) %>%
    filter(!Area %in% "China") %>%
    mutate_each(funs(replace(., .=="China, mainland","China")), Area) %>%
    filter(Area %in% iso_lut$FAO_country) %>%
    mutate(ISO = iso_lut$iso[match(Area, iso_lut$FAO_country)]) %>%
    mutate(GCAM_ID = rgn_lut$GCAM_region_ID[match(ISO, rgn_lut$iso)]) %>%
    mutate(GCAM_region = rgn_nms$region[match(GCAM_ID, rgn_nms$GCAM_region_ID)])


## "Agave fibres nes"                            "other fibre crops" ofib
## "Almonds, with shell"                         "rest of crops"     rest
## "Anise, badian, fennel, coriander"            "rest of crops"     rest
## "Apples"                                      "temperate fruit"   temf
## "Apricots"                                    "temperate fruit"   temf
## "Areca nuts"                                  "rest of crops"     rest
## "Artichokes"                                  vegetables          vege
## "Asparagus"                                   vegetables          vege
## "Avocados"                                    "tropical fruit"    trof
## "Bambara beans"                               "other pulses"      opul
## "Bananas"                                     banana              bana
## "Barley"                                      barley              barl
## "Bastfibres, other"                           "other fibre crops" ofib
## "Beans, dry"                                  bean                bean
## "Beans, green"                                vegetables          vege
## "Berries nes"                                 "temperate fruit"   temf
## "Blueberries"                                 "temperate fruit"   temf
## "Brazil nuts, with shell"                     "rest of crops"     rest
## "Broad beans, horse beans, dry"               "other pulses"      opul
## "Buckwheat"                                   "other cereals"     ocer
## "Cabbages and other brassicas"                vegetables          vege
## "Canary seed"                                 "other cereals"     ocer
## "Carobs"                                      vegetables          vege
## "Carrots and turnips"                         vegetables          vege
## "Cashew nuts, with shell"                     "rest of crops"     rest
## "Cashewapple"                                 "tropical fruit"    trof
## "Cassava"                                     cassava             cass
## "Cassava leaves"                              vegetables          vege
## "Castor oil seed"                             "other oil crops"   ooil
## "Cauliflowers and broccoli"                   vegetables          vege
## "Cereals (Rice Milled Eqv)"                   NA
## "Cereals, nes"                                "other cereals"     ocer
## "Cereals,Total"                               NA
## "Cherries"                                    "temperate fruit"   temf
## "Cherries, sour"                              "temperate fruit"   temf
## "Chestnut"                                    "rest of crops"     rest
## "Chick peas"                                  chickpea            chic
## "Chicory roots"                               vegetables          vege
## "Chillies and peppers, dry"                   "rest of crops"     rest
## "Chillies and peppers, green"                 vegetables          vege
## "Cinnamon (canella)"                          "rest of crops"     rest
## "Citrus Fruit,Total"                          NA
## "Cloves"                                      "rest of crops"     rest
## "Coarse Grain, Total"                         NA
## "Cocoa, beans"                                cocoa               coco
## "Coconuts"                                    coconut             cnut
## "Coffee, green"                               
## "Coir"                                        "other fibre crops" ofib
## "Cotton lint"                                 "rest of crops"     rest
## "Cottonseed"                                  "other oil crops"   ooil
## "Cow peas, dry"                               cowpea              cowp
## "Cranberries"                                 "temperate fruit"   temf
## "Cucumbers and gherkins"                      vegetables          vege
## "Currants"                                    "temperate fruit"   temf
## "Dates"                                       "tropical fruit"    trof
## "Eggplants (aubergines)"                      vegetables          vege
## "Fibre Crops Primary"                         NA
## "Fibre crops nes"                             "other fibre crops" ofib
## "Figs"                                        "tropical fruit"    trof
## "Flax fibre and tow"                          "other fibre crops" ofib
## "Fonio"                                       "other cereals"     ocer
## "Fruit excl Melons,Total"                     NA
## "Fruit, citrus nes"                           "tropical fruit"    trof
## "Fruit, fresh nes"                            "temperate fruit"   temf
## "Fruit, pome nes"                             "temperate fruit"   temf
## "Fruit, stone nes"                            "temperate fruit"   temf
## "Fruit, tropical fresh nes"                   "tropical fruit"    trof
## "Garlic"                                      vegetables          vege
## "Ginger"                                      "rest of crops"     rest
## "Gooseberries"                                "temperate fruit"   temf
## "Grain, mixed"                                "other cereals"     ocer
## "Grapefruit (inc. pomelos)"                   "tropical fruit"    trof
## "Grapes"                                      "temperate fruit"   temf
## "Groundnuts, with shell"                      groundnut           grou
## "Hazelnuts, with shell"                       "rest of crops"     rest
## "Hemp tow waste"                              "other fibre crops" ofib
## "Hempseed"                                    "other oil crops"   ooil
## "Hops"                                        "rest of crops"     rest
## "Jojoba seed"                                 "other oil crops"   ooil
## "Jute"                                        "other fibre crops" ofib
## "Jute & Jute-like Fibres"                     "other fibre crops" ofib
## "Kapok fruit"                                 "other oil crops"   ooil
## "Karite nuts (sheanuts)"                      "other oil crops"   ooil
## "Kiwi fruit"                                  "temperate fruit"   temf
## "Kola nuts"                                   "rest of crops"     rest
## "Leeks, other alliaceous vegetables"          vegetables          vege
## "Lemons and limes"                            "tropical fruit"    trof
## "Lentils"                                     lentil              lent
## "Lettuce and chicory"                         vegetables          vege
## "Linseed"                                     "other oil crops"   ooil
## "Lupins"                                      "other pulses"      opul
## "Maize"                                       maize               maiz
## "Maize, green"                                vegetables          vege
## "Mangoes, mangosteens, guavas"                "tropical fruit"    trof
## "Manila fibre (abaca)"                        "other fibre crops" ofib
## "Mat\351"                                     "rest of crops"     rest
## "Melons, other (inc.cantaloupes)"             "tropical fruit"    trof
## "Melonseed"                                   "other oil crops"   ooil
## "Millet"                                      
## "Mushrooms and truffles"                      vegetables          vege
## "Mustard seed"                                rapeseed            rape
## "Nutmeg, mace and cardamoms"                  "rest of crops"     rest
## "Nuts, nes"                                   "rest of crops"     rest
## "Oats"                                        "other cereals"     ocer
## "Oil, palm fruit"                             oilpalm             oilp
## "Oilcakes Equivalent"                         
## "Oilcrops Primary"                            
## "Oilseeds nes"                                "other oil crops"   ooil
## "Okra"                                        vegetables          vege
## "Olives"                                      "other oil crops"   ooil
## "Onions, dry"                                 vegetables          vege
## "Onions, shallots, green"                     vegetables          vege
## "Oranges"                                     "tropical fruit"    trof
## "Papayas"                                     "tropical fruit"    trof
## "Peaches and nectarines"                      "temperate fruit"   temf
## "Pears"                                       "temperate fruit"   temf
## "Peas, dry"                                   "other pulses"      opul
## "Peas, green"                                 vegetables          vege
## "Pepper (piper spp.)"                         "rest of crops"     rest
## "Peppermint"                                  "rest of crops"     rest
## "Persimmons"                                  "tropical fruit"    trof
## "Pigeon peas"                                 pigeonpea           pige
## "Pineapples"                                  "tropical fruit"    trof
## "Pistachios"                                  "rest of crops"     rest
## "Plantains and others"                        plantain            plnt
## "Plums and sloes"                             "temperate fruit"   temf
## "Poppy seed"                                  "other oil crops"   ooil
## "Potatoes"                                    potato              pota
## "Pulses, nes"                                 "other pulses"      opul
## "Pulses,Total"                                NA
## "Pumpkins, squash and gourds"                 vegetables          vege
## "Pyrethrum, dried"                            "rest of crops"     rest
## "Quinces"                                     "temperate fruit"   temf
## "Quinoa"                                      "other cereals"     ocer
## "Ramie"                                       "other fibre crops" ofib
## "Rapeseed"                                    rapeseed            rape
## "Raspberries"                                 "temperate fruit"   temf
## "Rice, paddy"                                 rice                rice
## "Roots and Tubers,Total"                      NA
## "Roots and tubers, nes"                       "other roots"       orts
## "Rubber, natural"                             "rest of crops"     rest
## "Rye"                                         "other cereals"     ocer
## "Safflower seed"                              "other oil crops"   ooil
## "Seed cotton"                                 "other oil crops"   ooil
## "Sesame seed"                                 "other oil crops"   ooil
## "Sisal"                                       "other fibre crops" ofib
## "Sorghum"                                     sorghum             sorg
## "Soybeans"                                    soybean             soyb
## "Spices, nes"                                 "rest of crops"     rest
## "Spinach"                                     vegetables          vege
## "Strawberries"                                "temperate fruit"   temf
## "String beans"                                vegetables          vege
## "Sugar beet"                                  sugarbeet           sugb
## "Sugar cane"                                  sugarcane           sugc
## "Sugar crops, nes"                            "rest of crops"     rest
## "Sunflower seed"                              sunflower           sunf
## "Sweet potatoes"                              "sweet potato"      swpo
## "Tallowtree seed"                             "other oil crops"   ooil
## "Tangerines, mandarins, clementines, satsumas""tropical fruit"    trof
## "Taro (cocoyam)"                              "other roots"       orts
## "Tea"                                         tea                 teas
## "Tobacco, unmanufactured"                     "rest of crops"     rest
## "Tomatoes"                                    vegetables          vege
## "Treenuts,Total"                              NA
## "Triticale"                                   "other cereals"     ocer
## "Tung nuts"                                   "other oil crops"   ooil
## "Vanilla"                                     "rest of crops"     rest
## "Vegetables Primary"                          NA
## "Vegetables&Melons, Total"                    NA
## "Vegetables, fresh nes"                       vegetables          vege
## "Vegetables, leguminous nes"                  vegetables          vege
## "Vetches"                                     "other pulses"      opul
## "Walnuts, with shell"                         "rest of crops"     rest
## "Watermelons"                                 "tropical fruit"    trof
## "Wheat"                                       wheat               whea
## "Yams"                                        yams                yams
## "Yautia (cocoyam)"                            "other roots"       orts

## ======================================
## MapSPAM
## ======================================

## physical area
system("unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_phys_area.geotiff.zip -d data/mapspam_global_phys_area")

lut = read.csv("data/FAO_ag_items_PRODSTAT.csv", skip=3, header=TRUE)

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

## ======================================
## area of each 5 arcminute grid cell
## ======================================

## regular grid, so in fact all we need to do is calculate area for a single longitude
template = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TI_WHEA_I.tif")
r = raster(xmn=0, xmx=0 + res(template)[1], ymn=-90, ymx=90, resolution=res(template)[1])
poly = as(r, "SpatialPolygons")

library(geosphere)
ar = areaPolygon(poly) / 1000 / 1000 * 100 ## m2 -> km2 -> Ha
ar = raster(matrix(data=rep(ar, 4320), nrow=2160, ncol=4320), xmn=-180, xmx=180, ymn=-90, ymx=90)
ar = crop(ar, template)

## ======================================
## wheat
## ======================================

wheat_irri = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TI_WHEA_I.tif")
wheat_rain = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TR_WHEA_R.tif")
wheat_frac = (wheat_irri + wheat_rain) / ar

## ======================================
## rice
## ======================================

rice_irri  = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TI_RICE_I.tif")
rice_rain  = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TR_RICE_R.tif")
rice_frac  = (rice_irri + rice_rain) / ar

## ======================================
## maize
## ======================================

maize_irri = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TI_MAIZ_I.tif")
maize_rain = raster("data/mapspam_global_phys_area/SPAM2005V3r1_global_A_TR_MAIZ_R.tif")
maize_frac = (maize_irri + maize_rain) / ar

## ======================================
## other grain
## ======================================

## 4	   barley		barl      OtherGrain
## 5	   pearl millet	        pmil      OtherGrain
## 6	   small millet	        smil      OtherGrain
## 7	   sorghum		sorg      OtherGrain
## 8	   other cereals	ocer      OtherGrain

othgrain_irri_fs = list.files("data/mapspam_global_phys_area",
                              pattern="^SPAM2005V3r1_global_A_TI_(BARL|PMIL|SMIL|SORG|OCER)_I.tif$", full.names=TRUE)
othgrain_irri =
    stack(othgrain_irri_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

othgrain_rain_fs = list.files("data/mapspam_global_phys_area",
                              pattern="^SPAM2005V3r1_global_A_TR_(BARL|PMIL|SMIL|SORG|OCER)_R.tif$", full.names=TRUE)
othgrain_rain =
    stack(othgrain_rain_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

othgrain_frac = (othgrain_irri + othgrain_rain) / ar

## ======================================
## root, tuber
## ======================================

## 9	   potato		pota      RootTuber
## 10	   sweet potato	        swpo      RootTuber
## 11	   yams		        yams      RootTuber
## 12	   cassava		cass      RootTuber
## 13	   other roots	        orts      RootTuber 

rtub_irri_fs = list.files("data/mapspam_global_phys_area",
                          pattern="^SPAM2005V3r1_global_A_TI_(POTA|SWPO|YAMS|CASS|ORTS)_I.tif$", full.names=TRUE)

rtub_irri =
    stack(rtub_irri_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

rtub_rain_fs = list.files("data/mapspam_global_phys_area",
                          pattern="^SPAM2005V3r1_global_A_TR_(POTA|SWPO|YAMS|CASS|ORTS)_R.tif$", full.names=TRUE)
rtub_rain =
    stack(rtub_rain_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

rtub_frac = (rtub_irri + rtub_rain) / ar

## ======================================
## misc. crops
## ======================================

## 14	   bean		        bean      MiscCrop
## 15	   chickpea		chic      MiscCrop
## 16	   cowpea		cowp      MiscCrop
## 17	   pigeonpea	        pige      MiscCrop
## 18	   lentil		lent      MiscCrop
## 19	   other pulses	        opul      MiscCrop
## 37	   banana		bana      MiscCrop
## 38	   plantain		plnt      MiscCrop
## 39	   tropical fruit	trof      MiscCrop
## 40	   temperate fruit	temf      MiscCrop
## 41	   vegetables	        vege      MiscCrop
## 32	   arabica coffee	acof      MiscCrop
## 33	   robusta coffee	rcof      MiscCrop
## 34	   cocoa		coco      MiscCrop
## 35	   tea		        teas      MiscCrop
## 36	   tobacco		toba      MiscCrop
## 42	   rest of crops	rest      MiscCrop
    
misc_irri_fs = list.files("data/mapspam_global_phys_area",
                          pattern="^SPAM2005V3r1_global_A_TI_(BEAN|CHIC|COWP|PIGE|LENT|OPUL|BANA|PLNT|TROF|TEMF|VEGE|ACOF|RCOF|COCO|TEAS|TOBA|REST)_I.tif$", full.names=TRUE)

misc_irri =
    stack(misc_irri_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

misc_rain_fs = list.files("data/mapspam_global_phys_area",
                          pattern="^SPAM2005V3r1_global_A_TR_(BEAN|CHIC|COWP|PIGE|LENT|OPUL|BANA|PLNT|TROF|TEMF|VEGE|ACOF|RCOF|COCO|TEAS|TOBA|REST)_R.tif$", full.names=TRUE)

misc_rain =
    stack(misc_rain_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

misc_frac = (misc_irri + misc_rain) / ar

## ======================================
## oil crop
## ======================================

## 20	   soybean		soyb      OilCrop
## 21	   groundnut	        grou      OilCrop
## 24	   sunflower	        sunf      OilCrop
## 25	   rapeseed		rape      OilCrop
## 26	   sesameseed	        sesa      OilCrop
## 27	   other oil crops	ooil      OilCrop

oil_irri_fs = list.files("data/mapspam_global_phys_area",
                          pattern="^SPAM2005V3r1_global_A_TI_(SOYB|GROU|SUNF|RAPE|SESA|OOIL)_I.tif$", full.names=TRUE)

oil_irri =
    stack(misc_irri_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

oil_rain_fs = list.files("data/mapspam_global_phys_area",
                          pattern="^SPAM2005V3r1_global_A_TR_(SOYB|GROU|SUNF|RAPE|SESA|OOIL)_R.tif$", full.names=TRUE)

oil_rain =
    stack(misc_rain_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

oil_frac = (oil_irri + oil_frac) / ar

## ======================================
## palm fruit
## ======================================

## 22	   coconut		cnut      PalmFruit
## 23	   oilpalm		oilp      PalmFruit

palm_irri_fs = list.files("data/mapspam_global_phys_area",
                          pattern="^SPAM2005V3r1_global_A_TI_(CNUT|OILP)_I.tif$", full.names=TRUE)

palm_irri =
    stack(palm_irri_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

palm_rain_fs = list.files("data/mapspam_global_phys_area",
                          pattern="^SPAM2005V3r1_global_A_TR_(CNUT|OILP)_R.tif$", full.names=TRUE)

cpalm_rain =
    stack(palm_rain_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

palm_frac = (palm_irri + palm_frac) / ar

## ======================================
## sugar crop
## ======================================

## 28	   sugarcane	        sugc      SugarCrop
## 29	   sugarbeet	        sugb      SugarCrop

sugar_irri_fs = list.files("data/mapspam_global_phys_area",
                           pattern="^SPAM2005V3r1_global_A_TI_(SUGC|SUGB)_I.tif$", full.names=TRUE)

sugar_irri =
    stack(sugar_irri_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

sugar_rain_fs = list.files("data/mapspam_global_phys_area",
                           pattern="^SPAM2005V3r1_global_A_TR_(SUGC|SUGB)_R.tif$", full.names=TRUE)

sugar_rain =
    stack(sugar_rain_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

sugar_frac = (sugar_irri + sugar_frac) / ar

## ======================================
## fiber crop
## ======================================

## 30	   cotton		cott      FiberCrop
## 31	   other fibre crops	ofib      FiberCrop

fiber_irri_fs = list.files("data/mapspam_global_phys_area",
                           pattern="^SPAM2005V3r1_global_A_TI_(COTT|OFIB)_I.tif$", full.names=TRUE)

fiber_irri =
    stack(fiber_irri_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

fiber_rain_fs = list.files("data/mapspam_global_phys_area",
                           pattern="^SPAM2005V3r1_global_A_TR_(COTT|OFIB)_R.tif$", full.names=TRUE)

fiber_rain =
    stack(fiber_rain_fs) %>%
    stackApply(indices=rep(1, nlayers(.)), fun=sum, na.rm=TRUE)

fiber_frac = (fiber_irri + fiber_frac) / ar

