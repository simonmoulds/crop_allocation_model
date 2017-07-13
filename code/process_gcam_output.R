## Author : Simon Moulds
## Date   : May 2017

library(raster)
library(readxl)
library(tidyr)
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(zoo)

options(stringsAsFactors=FALSE)

## ## maps countries to 14/32 GCAM regions
## gcam_rgn <- read.csv("data/rawdata/gcam_country_region_mapping.csv",
##                      header=TRUE)

## ## maps region ID to name (32 region)
## gcam_32rgn_id <- read.csv("data/rawdata/gcam_32_region_id.csv",
##                           header=TRUE)

## ## create lookup table for GCAM3 regions -> GCAM4 regions
## gcam_rgn_lut <-
##     (gcam_32rgn_id %>% 
##      mutate(INDEX = match(GCAM_region_ID, gcam_rgn[["GCAM_region_ID"]])) %>%
##      mutate(region_GCAM3 = gcam_rgn[["region_GCAM3"]][INDEX]) %>%
##      mutate(region_GCAM4 = gsub(" ", "_", region),
##             region_GCAM3 = gsub(" ", "_", region_GCAM3)) %>%
##      select(-region,-INDEX))

## ======================================
## Extract the necessary GCAM outputs
## ======================================

devtools::load_all("../GCAM/pkg/rgcam")

db <- addScenario(dbFile="/scratch/projects/GCAM/v4.3/gcam-core/output/database_basexdb",
                  proj="/scratch/projects/GCAM/data/output/proj_full.dat",
                  queryFile="/scratch/projects/GCAM/sample-queries.xml", ## change this?
                  clobber=TRUE)

proj <- loadProject("/scratch/projects/GCAM/data/output/proj_full.dat")
listScenarios(proj)
listQueries(proj)

fancy_scientific <- function(l) {
     # turn in to character string in scientific notation
     l <- format(l, scientific = TRUE)
     # quote the part before the exponent to keep all the digits
     l <- gsub("^(.*)e", "'\\1'e", l)
     # turn the 'e+' into plotmath format
     l <- gsub("e", "%*%10^", l)
     # return this as an expression
     parse(text=l)
}

gcam_gdp <-
    proj %>%
    extract2("Reference") %>%
    extract2("GDP by region") %>%
    gather(year, gdp1990USD,
           -scenario,
           -region,
           -Units) %>%
    mutate(year = gsub("X", "", year)) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(gdp1990USD = gdp1990USD) %>% ## GCAM output data in million 1990 USD
    select(-Units) %>%
    arrange(region, year) %>%
    mutate(region = formatC(region, width=31, flag="-"))

gcam_gdp %>%
    ggplot(aes(x=year, y=gdp1990USD, colour=region)) +
    geom_line() +
    labs(x="", y="GDP (million 1990 USD)") +
    scale_y_continuous(labels=fancy_scientific) + 
    theme(legend.title=element_blank(), legend.position="bottom") +
    guides(colour=guide_legend(ncol=3)) +
    ggsave("gcam_gdp.png", width=8, height=8, dpi=600)

gcam_popn <-
    proj %>%
    extract2("Reference") %>%
    extract2("Population by region") %>%
    gather(year, population,
           -scenario,
           -region,
           -Units) %>%
    mutate(year = gsub("X", "", year)) %>%
    mutate(year = as.numeric(year)) %>%
    select(-Units) %>%
    mutate(population = population * 1000) %>%
    arrange(region, year)

gcam_popn %>%
    ggplot(aes(x=year, y=population, colour=region)) +
    geom_line() +
    labs(x="", y="Population") +
    scale_y_continuous(labels=fancy_scientific) + 
    theme(legend.title=element_blank(), legend.position="bottom") +
    guides(colour=guide_legend(ncol=3)) +
    ggsave("gcam_popn.png", width=8, height=8, dpi=600)

gcam_primary_enrg <- 
    proj %>%
    extract2("Reference") %>%
    extract2("Resource production") %>%
    ## filter(resource %in% c("coal","crude oil","natural gas","unconventional oil","uranium")) %>%
    filter(resource %in% c("coal","crude oil","natural gas","unconventional oil")) %>%
    gather(year, production,
           -scenario,
           -region,
           -resource,
           -Units) %>%
    mutate(year = gsub("X", "", year)) %>%
    mutate(year = as.numeric(year)) %>%
    select(-Units) %>%
    arrange(region, year)

gcam_primary_enrg %>%
    filter(region %in% "India") %>%
    ggplot(aes(x=year, y=production, colour=resource)) +
    geom_line() +
    labs(x="", y="Resource production (EJ)") +
    theme(legend.title=element_blank(), legend.position="bottom")+
    guides(colour=guide_legend(ncol=3)) +
    ggsave("gcam_primary_energy.png", width=8, height=8, dpi=600)

gcam_elec <-
    proj %>%
    extract2("Reference") %>%
    extract2("Electricity generation by aggregate technology") %>%
    gather(year, output,
           -scenario,
           -region,
           -technology,
           -Units) %>%
    mutate(year = gsub("X", "", year)) %>%
    mutate(year = as.numeric(year))

gcam_elec %>%
    filter(region %in% "India") %>%
    ggplot(aes(x=year, y=output, colour=technology)) +
    geom_line() +
    labs(x="", y="Electricity generation (EJ)") +
    theme(legend.title=element_blank(), legend.position="bottom")+
    guides(colour=guide_legend(ncol=3)) +
    ggsave("gcam_electricity_generation.png", width=8, height=8, dpi=600)

gcam_prod <-
    proj %>%
    extract2("Reference") %>%
    extract2("Ag Production by Crop Type") %>%
    gather(year, production,
           -scenario,
           -region,
           -sector,
           -output,
           -Units) %>%
    mutate(year = gsub("X", "", year)) %>%
    mutate(year = as.numeric(year)) %>%
    filter(!output %in% c("biomass","UnmanagedLand","Forest","NonFoodDemand_Forest")) %>%
    select(-Units) 

gcam_prod %>%
    filter(region %in% "India") %>%
    ggplot(aes(x=year, y=production, colour=output)) +
    geom_line() +
    labs(x="", y="Production (Mt)") +
    theme(legend.title=element_blank(), legend.position="bottom")+
    guides(colour=guide_legend(ncol=3)) ## +
    ## ggsave("gcam_ag_production.png", width=8, height=8, dpi=600)

gcam_area <-
    proj %>%
    extract2("Reference") %>%
    extract2("Aggregated Land Allocation") %>%
    gather(year, area,
           -scenario,
           -region,
           -land.allocation,
           -Units) %>%
    mutate(year = gsub("X", "", year)) %>%
    mutate(year = as.numeric(year)) %>%
    select(-Units) %>%
    dplyr::rename(subsector = land.allocation)

gcam_area %>%
    filter(region %in% "India") %>%
    ggplot(aes(x=year, y=area, colour=subsector)) +
    geom_line() +
    labs(x="", y=expression(Area~(thousand~km^2))) +
    theme(legend.title=element_blank(), legend.position="bottom")

## ## trying to reproduce Fig 1 in Chaturvedi et al (2015)
## x <- gcam_area
## x <-
##     (x %>%
##      group_by(year) %>%
##      mutate(sector = gsub("AEZ[0-9]{2}", "", subsector)) %>%
##      select(-subsector) %>%
##      group_by(sector, year) %>%
##      summarise_each(funs(sum(., na.rm=TRUE)), -scenario, -region) %>%
##      filter(sector %in% c("biomass","Wheat","Rice","MiscCrop","OilCrop","Corn","SugarCrop","OtherGrain","Root_Tuber","FiberCrop","PalmFruit")))
     
## x %>%
##     ggplot(aes(x=year, y=area, colour=sector)) +
##     geom_line() +
##     theme(legend.title=element_blank(), legend.position="bottom")

## gcam_meat <-
##     proj %>%
##     extract2("Reference") %>%
##     extract2("Meat Output By Technology") %>%
##     gather(year, production,
##            -scenario,
##            -region,
##            -output,
##            -technology,
##            -Units) %>%
##     mutate(year = gsub("X", "", year)) %>%
##     mutate(year = as.numeric(year)) %>%
##     select(-Units) ##%>%
##     group_by(scenario, region, output, year) %>%
##     summarise_each(funs(sum(., na.rm=TRUE)), -technology) %>%
##     mutate(production = production * 1e6) %>% ## Mt --> t
##     spread(output, production) %>%
##     setNames(c("scenario","region","year",
##                "t_beef",
##                "t_dairy",
##                "t_pork",
##                "t_poultry",
##                "t_sheepgoat"))
