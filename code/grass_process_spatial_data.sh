#!/bin/bash

wd=/home/links/sm775/projects/crop_allocation_model
datadir="${wd}"/data

# =======================================
# 1 - create template map
# =======================================

Rscript code/process_cropland_map.R

if [ -f "${datadir}"/iiasa-ifpri-cropland-map/iiasa_ifpri_cropland_map_5m_eck4.tif ]
then
    rm "${datadir}"/iiasa-ifpri-cropland-map/iiasa_ifpri_cropland_map_5m_eck4.tif
fi

proj='+proj=eck4 +lon_0=0 +x_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
gdalwarp -t_srs "${proj}"\
	 -tr 9250 9250 \
	 "${datadir}"/iiasa-ifpri-cropland-map/iiasa_ifpri_cropland_map_5m_ll.tif\
	 "${datadir}"/iiasa-ifpri-cropland-map/iiasa_ifpri_cropland_map_5m_eck4_tmp.tif

# =======================================
# 2 - import cropland map to Eckert IV location
# =======================================

# create GRASS location, if it doesn't already exist
if [ ! -d /scratch/grassdata/eckertiv ]
then

    echo 'export GRASS_MESSAGE_FORMAT=plain
datadir=/home/links/sm775/projects/crop_allocation_model/data
r.in.gdal input="${datadir}"/iiasa-ifpri-cropland-map/iiasa_ifpri_cropland_map_5m_eck4_tmp.tif output=iiasa_ifpri_cropland_map location=eckertiv
g.region rast=iiasa_ifpri_cropland_map
g.region -p
r.out.gdal input=iiasa_ifpri_cropland_map output="${datadir}"/iiasa-ifpri-cropland-map/iiasa_ifpri_cropland_map_5m_eck4.tif format=GTiff' > mygrassjob_pt1.sh
    export location=/scratch/grassdata/latlong/global
else

    echo 'export GRASS_MESSAGE_FORMAT=plain
datadir=/home/links/sm775/projects/crop_allocation_model/data
r.in.gdal input="${datadir}"/iiasa-ifpri-cropland-map/iiasa_ifpri_cropland_map_5m_eck4_tmp.tif output=iiasa_ifpri_cropland_map --overwrite
g.region rast=iiasa_ifpri_cropland_map
g.region -p
r.out.gdal input=iiasa_ifpri_cropland_map output="${datadir}"/iiasa-ifpri-cropland-map/iiasa_ifpri_cropland_map_5m_eck4.tif format=GTiff' > mygrassjob_pt1.sh
    export location=/scratch/grassdata/eckertiv/PERMANENT
fi

echo $location
chmod u+x mygrassjob_pt1.sh
export GRASS_BATCH_JOB="${wd}"/mygrassjob_pt1.sh
grass64 "${location}"
unset GRASS_BATCH_JOB
unset location
rm mygrassjob_pt1.sh

# =======================================
# 3 - MapSPAM
# =======================================

# mapspam_path="${datadir}"/mapspam_data

# if [ ! -d "${mapspam_path}" ]
# then
#     mkdir "${mapspam_path}"
#     # rm -r "${mapspam_path}"
# fi

# unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_phys_area.geotiff.zip -d "${mapspam_path}"
# unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_harv_area.geotiff.zip -d "${mapspam_path}"
# unzip -o data/rawdata/MapSPAM/spam2005V3r1_global_yield.geotiff.zip -d "${mapspam_path}"

# # a - import MapSPAM files to latlong
# if [ -f tmp ]
# then
#     rm tmp
# fi
# touch tmp 

# echo 'export GRASS_MESSAGE_FORMAT=plain
# datadir=/home/links/sm775/projects/crop_allocation_model/data
# g.region e=180E w=180W n=90N s=90S res=0:05:00
# g.region -p
# wd=`pwd`
# cd "${datadir}"/mapspam_data
# for file in *.tif
# do
#     basenm=${file%%.*}
#     echo "${basenm}" >> tmp
#     r.in.gdal input="${file}" output="${basenm}" --overwrite
#     # r.out.gdal input="${basenm}" output="${basenm}"_ll.tif format=GTiff
# done
# cd "${wd}"' > mygrassjob_pt2a.sh

# chmod u+x mygrassjob_pt2a.sh
# export GRASS_BATCH_JOB="${wd}"/mygrassjob_pt2a.sh
# grass64 /scratch/grassdata/latlong/global
# unset GRASS_BATCH_JOB
# rm mygrassjob_pt2a.sh

# # b - reproject to Eckert IV
# echo 'export GRASS_MESSAGE_FORMAT=plain
# datadir=/home/links/sm775/projects/crop_allocation_model/data
# g.region rast=iiasa_ifpri_cropland_map
# g.region -p
# wd=`pwd`
# cd "${datadir}"/mapspam_data
# while read line
# do
#     method=bilinear
#     ll="${line}"
#     ea="${line}"_eck4
#     r.proj -n location=latlong mapset=global input="${ll}" output="${ea}" method="${method}" --overwrite
#     r.out.gdal input="${ea}" output="${ea}".tif format=GTiff
# done < tmp
# rm tmp
# cd "${wd}"' > mygrassjob_pt2b.sh

# chmod u+x mygrassjob_pt2b.sh
# export GRASS_BATCH_JOB="${wd}"/mygrassjob_pt2b.sh
# grass64 /scratch/grassdata/eckertiv/PERMANENT
# unset GRASS_BATCH_JOB
# rm mygrassjob_pt2b.sh

# =======================================
# 4 - GAEZ
# =======================================

gaez_path="${datadir}"/gaez_data

if [ ! -d "${gaez_path}" ]
then
    mkdir "${gaez_path}"
    # rm -r "${gaez_path}"
fi

# unzip files to specific directories
ls "${datadir}"/rawdata/GAEZ | grep '^res03.*.zip$' > tmp
while read line
do
    dir=${line%%.*}
    unzip -o "${datadir}"/rawdata/GAEZ/"${line}" -d "${gaez_path}"/"${dir}"
done < tmp

# a - import GAEZ files to latlong
if [ -f tmp ]
then
    rm tmp
fi
touch tmp 

echo 'export GRASS_MESSAGE_FORMAT=plain
datadir=/home/links/sm775/projects/crop_allocation_model/data
g.region e=180E w=180W n=90N s=90S res=0:05:00
g.region -p
for dir in "${datadir}"/gaez_data/*/
do
    basenm=${dir#"${datadir}"/gaez_data/}
    # echo $basenm
    fn=$(echo "${basenm}" | sed "s/^\(.\{5\}\)\(.\{9\}\)\(.\{4\}\)\(.\{3\}\)\(.\{9\}\)/\1_\2_\3_\4/")
    echo $fn
    echo "${fn}" >> tmp
    r.in.gdal input="${dir}"/"${fn}".tif output="${fn}" --overwrite
    r.out.gdal input="${fn}" output="${datadir}"/gaez_data/"${fn}"_ll.tif format=GTiff
done' > mygrassjob_pt3a.sh

chmod u+x mygrassjob_pt3a.sh
export GRASS_BATCH_JOB="${wd}"/mygrassjob_pt3a.sh
grass64 /scratch/grassdata/latlong/global
unset GRASS_BATCH_JOB
rm mygrassjob_pt3a.sh

# b - reproject to Eckert IV
echo 'export GRASS_MESSAGE_FORMAT=plain
datadir=/home/links/sm775/projects/crop_allocation_model/data
g.region rast=iiasa_ifpri_cropland_map
g.region -p
while read line
do
    method="bilinear"
    ll="${line}"
    ea="${line}"_eck4
    r.proj location=latlong mapset=global input="${ll}" output="${ea}" method="${method}" --overwrite
    r.out.gdal input="${ea}" output="${datadir}"/gaez_data/"${ea}".tif format=GTiff
done < tmp
rm tmp' > mygrassjob_pt3b.sh

chmod u+x mygrassjob_pt3b.sh
export GRASS_BATCH_JOB="${wd}"/mygrassjob_pt3b.sh
grass64 /scratch/grassdata/eckertiv/PERMANENT
unset GRASS_BATCH_JOB
rm mygrassjob_pt3b.sh

# =======================================
# 5 - region boundaries
# =======================================

## clean up files if they already exist
if [ -f "${datadir}"/gcam_32rgn.shp ]
then
    cd "${datadir}"
    ls | grep -P "^gcam_32rgn.(shp|shx|dbf|prj)$" | xargs -d"\n" rm
    cd "${wd}"
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

# project raw data (from gcammaptools R package) to EPSG:4326
ogr2ogr -t_srs EPSG:4326 \
	"${datadir}"/gcam_32rgn_countries.shp \
	"${datadir}"/rawdata/gcammaptools/inst/extdata/rgn32/GCAM_32_wo_Taiwan_clean.geojson \
	-t_srs EPSG:4326

# this R script takes gcam_32rgn_countries.shp and dissolves
# polygons according to GCAM region ID
Rscript code/gcam_regions.R

# clip to geographic coordinates (otherwise artefacts are created
# during reprojection)
ogr2ogr -clipsrc -180 -90 180 90 \
	"${datadir}"/tmp.shp \
	"${datadir}"/gcam_32rgn.shp

# reproject
ogr2ogr -t_srs "${proj}" \
	"${datadir}"/gcam_32rgn_eck4.shp \
	"${datadir}"/tmp.shp \
	-t_srs "${proj}"

Rscript code/gcam_regions_interp.R
