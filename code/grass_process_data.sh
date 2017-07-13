#!/bin/bash

wd=/home/links/sm775/projects/ca_model
datadir="${wd}"/data

# =======================================
# 1 - region boundaries
# =======================================

## clean up files if they already exist
if [ -f "${datadir}"/gcam_32rgn.shp ]
then
    cd "${datadir}"
    ls | grep -P "^gcam_32rgn.(shp|shx|dbf|prj)$" | xargs -d"\n" rm
    cd "${wd}"
j
fi

if [ -f "${datadir}"/gcam_32rgn_countries.shp ]
then
    cd "${datadir}"
    ls | grep -P "^gcam_32rgn_countries.(shp|shx|dbf|prj)$" | xargs -d"\n" rm
    cd "${wd}"
fi
 
if [ -f "${datadir}"/tmp.shp ]
then
    cd "${datadir}"
    ls | grep -P "^tmp.(shp|shx|dbf|prj)$" | xargs -d"\n" rm
    cd "${wd}"
fi

if [ -f "${datadir}"/gcam_32rgn_eck4.shp ]
then
    cd "${datadir}"
    ls | grep -P "^gcam_32rgn_eck4.(shp|shx|dbf|prj)$" | xargs -d"\n" rm
    cd "${wd}"
fi

## project raw data (from gcammaptools R package) to EPSG:4326
ogr2ogr -t_srs EPSG:4326 "${datadir}"/gcam_32rgn_countries.shp "${datadir}"/rawdata/gcammaptools/inst/extdata/rgn32/GCAM_32_wo_Taiwan_clean.geojson -t_srs EPSG:4326

## this R script takes gcam_32rgn_countries.shp and dissolves polygons according to GCAM region ID
Rscript code/gcam_regions.R

## clip to geographic coordinates (otherwise artefacts are created during reprojection)
ogr2ogr -clipsrc -180 -90 180 90 "${datadir}"/tmp.shp "${datadir}"/gcam_32rgn.shp

## this R script converts polygons to raster
Rscript code/rasterize_gcam_regions.R

# # =======================================
# # 2 - process MIRCA2000
# # =======================================

# # NB ultimately, we will use the India map currently in development. However,
# # to speed up the development of the allocation model we have decided to use
# # MIRCA2000 for now.

# exdir=/media/My_Book

# # Rscript code/crop_mirca.R "${exdir}"/Data/monthly_growing_area_grids .

# outdir=data/monthly_growing_area_grids

# if [ -d "${outdir}" ]
# then
#     rm -r "${outdir}"
# fi

# mkdir "${outdir}"

# for infile in "${exdir}"/Data/monthly_growing_area_grids/crop_*_irrigated_*.asc
# do
#     echo $infile
#     nm=${infile##*/}
#     basenm=${nm%.*}
#     outfile="${outdir}"/"${basenm}"_india.tif
#     Rscript code/crop_mirca.R "${infile}" "${outfile}" 67.4 98 6 36 GTiff
# done
