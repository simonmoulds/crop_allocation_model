#!/bin/bash

wd=/home/simon/projects/crop_allocation_model
datadir="${wd}"/data

# # =======================================
# # 1 - create template map
# # =======================================

# if [ ! -d "$wd/data/rawdata/iiasa-ifpri-cropland-map" ]
# then
#     unzip "$wd"/data/rawdata/cropland_hybrid_10042015v9.zip -d "$wd"/data/rawdata/iiasa-ifpri-cropland-map
# fi

# # =======================================
# # 0 - create GRASS location if it doesn't already exist
# # =======================================

# if [ ! -d "/home/simon/grassdata/latlong" ]
# then
#     grass -e -c "$wd"/data/rawdata/iiasa-ifpri-cropland-map/Hybrid_10042015v9.img /home/simon/grassdata/latlong
# fi

# # echo 'export GRASS_MESSAGE_FORMAT=plain
# # wd=/home/simon/projects/crop_allocation_model

# # r.in.gdal -l input="$wd"/data/rawdata/iiasa-ifpri-cropland-map/Hybrid_10042015v9.img output=iiasa_ifpri_cropland_map --overwrite

# # g.region rast=iiasa_ifpri_cropland_map res=0:00:30
# # g.region -p

# # r.resamp.interp input=iiasa_ifpri_cropland_map output=tmp method=bilinear --overwrite
# # r.mapcalc "iiasa_ifpri_cropland_map_resamp = if(tmp < 0, 0, tmp)" --overwrite
# # g.remove -f type=raster name=tmp

# # r.out.gdal input=iiasa_ifpri_cropland_map_resamp output="$wd"/data/iiasa_ifpri_cropland_map_30s.tif --overwrite

# # g.region e=180E w=180W n=90N s=90S res=0:05:00
# # g.region -p

# # r.resamp.stats -w input=iiasa_ifpri_cropland_map_resamp output=tmp method=average --overwrite
# # r.mapcalc "iiasa_ifpri_cropland_map_5m = tmp / 100" --overwrite
# # r.out.gdal input=iiasa_ifpri_cropland_map_5m output="$wd"/data/iiasa_ifpri_cropland_map_5m.tif --overwrite
# # g.remove -f type=raster name=tmp
# # ' > mygrassjob_pt1.sh

# # chmod u+x mygrassjob_pt1.sh
# # export GRASS_BATCH_JOB="${wd}"/mygrassjob_pt1.sh
# # grass /home/simon/grassdata/latlong/PERMANENT
# # unset GRASS_BATCH_JOB
# # rm mygrassjob_pt1.sh

# Rscript code/process_cropland_map.R # this takes a long time!

# # =======================================
# # 2 1/2 SSP population
# # =======================================

# popn_path="${datadir}"

# unzip -o data/rawdata/SSP\ Population\ Projections-20161003T090203Z.zip -d "${popn_path}"
# unzip -o "${popn_path}"/SSP\ Population\ Projections/SSP\ Population\ Projections.zip -d "${popn_path}"

# echo 'export GRASS_MESSAGE_FORMAT=plain
# datadir=/home/simon/projects/crop_allocation_model/data
# g.region e=180E w=180W n=90N s=90S res=0:05:00
# g.region -p

# touch tmp2
# wd=`pwd`
# cd "${datadir}"/SSP\ Population\ Projections

# for scen in SSP2
# do
#     ls "${scen}"/Total/GeoTIFF/*.tif > tmp1
#     while read line
#     do
#         a=${line%%.*}
#         basenm=${a##*/}
#         echo "${basenm}" >> tmp2
#         r.in.gdal input="${line}" output="${basenm}" --overwrite
#     done < tmp1
# done

# g.region rast=ssp2_2010
# g.region -p

# r.cell.area output=cell_area units=km2 --overwrite
# touch tmp3
# while read line
# do
#     newname="${line}"_dens
#     echo "${newname}" >> tmp3
#     echo $line
#     echo $newname
#     r.mapcalc "${newname} = ${line} / cell_area" --overwrite
# done < tmp2

# g.remove -f type=raster name=cell_area

# g.region e=180E w=180W n=85N s=60S res=0:05:00
# g.region -p

# while read line
# do
#     newname="${line}"_5m
#     r.resamp.interp input=${line} output="${newname}" method=bilinear --overwrite
#     r.out.gdal input="${newname}" output="${datadir}"/SSP\ Population\ Projections/"${newname}".tif --overwrite
# done < tmp3
# rm tmp1 tmp2 tmp3
# cd "${wd}"' > mygrassjob_pt2a.sh

# chmod u+x mygrassjob_pt2a.sh
# export GRASS_BATCH_JOB="${wd}"/mygrassjob_pt2a.sh
# grass /home/simon/grassdata/latlong/PERMANENT
# unset GRASS_BATCH_JOB
# rm mygrassjob_pt2a.sh

# =======================================
# 3 - MapSPAM
# =======================================

mapspam_path="${datadir}"/mapspam_data

if [ ! -d "${mapspam_path}" ]
then
    mkdir "${mapspam_path}"
fi

# unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_phys_area.geotiff.zip -d "${mapspam_path}"
# unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_harv_area.geotiff.zip -d "${mapspam_path}"
# unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_yield.geotiff.zip -d "${mapspam_path}"

# a - import MapSPAM files to latlong
echo 'export GRASS_MESSAGE_FORMAT=plain
datadir=/home/simon/projects/crop_allocation_model/data
# g.region e=180E w=180W n=90N s=90S res=0:05:00
g.region e=180E w=180W n=85N s=60S res=0:05:00
g.region -p
wd=`pwd`
cd "${datadir}"/mapspam_data
if [ -f tmp1 ] 
then 
    rm tmp1 
fi

if [ -f tmp2 ] 
then 
    rm tmp2 
fi

touch tmp2
ls *.tif | grep "^SPAM2005V3r1_global_[A-Z]\{1\}_[A-Z]\{2\}_[A-Z]\{4\}_[A-Z]\{1\}.tif" > tmp1
while read line
do
    basenm=${line%%.*}
    echo "${basenm}" >> tmp2
    r.in.gdal input="${line}" output="${basenm}" --overwrite
    r.resamp.interp input="${basenm}" output="${basenm}"_resamp method=bilinear --overwrite
    r.out.gdal input="${basenm}"_resamp output="${basenm}"_ll.tif format=GTiff --overwrite
done < tmp1
rm tmp1
cd "${wd}"' > mygrassjob_pt3.sh

chmod u+x mygrassjob_pt3.sh
export GRASS_BATCH_JOB="${wd}"/mygrassjob_pt3.sh
grass /home/simon/grassdata/latlong/PERMANENT
unset GRASS_BATCH_JOB
rm mygrassjob_pt3.sh

# # =======================================
# # 4 - GAEZ
# # =======================================

# gaez_path="${datadir}"/gaez_data

# if [ ! -d "${gaez_path}" ]
# then
#     mkdir "${gaez_path}"
# fi

# # unzip files to specific directories
# ls "${datadir}"/rawdata/GAEZ | grep '^res0[2|3].*.zip$' > tmp
# while read line
# do
#     dir=${line%%.*}
#     unzip -o "${datadir}"/rawdata/GAEZ/"${line}" -d "${gaez_path}"/"${dir}"
# done < tmp

# # a - import GAEZ files to latlong
# if [ -f tmp ]
# then
#     rm tmp
# fi

# echo 'export GRASS_MESSAGE_FORMAT=plain
# datadir=/home/simon/projects/crop_allocation_model/data
# g.region e=180E w=180W n=90N s=90S res=0:05:00
# g.region -p

# touch tmp
# for dir in "${datadir}"/gaez_data/res03*/
# do
#     basenm=${dir#"${datadir}"/gaez_data/}
#     fn=$(echo "${basenm}" | sed "s/^\(.\{5\}\)\(.\{9\}\)\(.\{4\}\)\(.\{3\}\)\(.\{9\}\)/\1_\2_\3_\4/")
#     echo "${fn}" >> tmp
#     r.in.gdal input="${dir}"/"${fn}".tif output="${fn}" --overwrite
#     r.out.gdal input="${fn}" output="${datadir}"/gaez_data/"${fn}"_ll.tif format=GTiff --overwrite
# done
# rm tmp

# touch tmp
# for dir in "${datadir}"/gaez_data/res02*/
# do
#     basenm=${dir#"${datadir}"/gaez_data/}
#     fn=$(echo "${basenm}" | sed "s/^\(.\{5\}\)\(.\{9\}\)\(.\{4\}\)\(.\{4\}\)\(.\{3\}\)\(.\{9\}\)/\1_\2_\3\4_\5/")
#     echo "${fn}" >> tmp
#     r.in.gdal input="${dir}"/"${fn}".tif output="${fn}" --overwrite
#     r.out.gdal input="${fn}" output="${datadir}"/gaez_data/"${fn}"_ll.tif format=GTiff --overwrite
# done' > mygrassjob_pt4.sh

# chmod u+x mygrassjob_pt4.sh
# export GRASS_BATCH_JOB="${wd}"/mygrassjob_pt4.sh
# grass /home/simon/grassdata/latlong/PERMANENT
# unset GRASS_BATCH_JOB
# rm mygrassjob_pt4.sh

# # =======================================
# # 5 - region boundaries
# # =======================================

# ## clean up files if they already exist
# if [ -f "${datadir}"/gcam_32rgn.shp ]
# then
#     cd "${datadir}"
#     ls | grep -P "^gcam_32rgn.(shp|shx|dbf|prj)$" | xargs -d"\n" rm
#     cd "${wd}"
# fi

# if [ -f "${datadir}"/gcam_32rgn_countries.shp ]
# then
#     cd "${datadir}"
#     ls | grep -P "^gcam_32rgn_countries.(shp|shx|dbf|prj)$" | xargs -d"\n" rm
#     cd "${wd}"
# fi
 
# if [ -f "${datadir}"/tmp.shp ]
# then
#     cd "${datadir}"
#     ls | grep -P "^tmp.(shp|shx|dbf|prj)$" | xargs -d"\n" rm
#     cd "${wd}"
# fi

# if [ -f "${datadir}"/gcam_32rgn_eck4.shp ]
# then
#     cd "${datadir}"
#     ls | grep -P "^gcam_32rgn_eck4.(shp|shx|dbf|prj)$" | xargs -d"\n" rm
#     cd "${wd}"
# fi

# # project raw data (from gcammaptools R package) to EPSG:4326
# ogr2ogr -t_srs EPSG:4326 \
# 	"${datadir}"/gcam_32rgn_countries.shp \
# 	"${datadir}"/rawdata/gcammaptools/inst/extdata/rgn32/GCAM_32_wo_Taiwan_clean.geojson \
# 	-t_srs EPSG:4326

# # this R script takes gcam_32rgn_countries.shp and dissolves
# # polygons according to GCAM region ID
# Rscript code/gcam_regions.R

# # clip to geographic coordinates (otherwise artefacts are created
# # during reprojection)
# ogr2ogr -clipsrc -180 -90 180 90 \
# 	"${datadir}"/tmp.shp \
# 	"${datadir}"/gcam_32rgn.shp

# # reproject
# ogr2ogr -t_srs "${proj}" \
# 	"${datadir}"/gcam_32rgn_eck4.shp \
# 	"${datadir}"/tmp.shp \
# 	-t_srs "${proj}"

# Rscript code/gcam_regions_interp.R
