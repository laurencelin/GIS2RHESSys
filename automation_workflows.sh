#!/bin/bash
#
# assume new fully bundled GRASS 7.4 is installed from http://grassmac.wikidot.com/downloads
# assume R and rgrass7 package are installed.
# assume ssurgo_extraction.R is downloaded and saved together with this script.
#
# path setup below is for Mac OS
# adding R paths
export PATH=$PATH:/usr/local/bin
export PATH=$PATH:/Library/Frameworks/R.framework/Resources
# adding GRASS bin paths
export PATH=$PATH:/Applications/GRASS-7.4.0.app/Contents/Resources/bin:/Applications/GRASS-7.4.0.app/Contents/Resources/scripts:/etc:/usr/lib
# adding GRASS environment variables: GISBASE_USER and GISBASE_SYSTEM in Grass.sh
export PATH=$PATH:~/Library/GRASS/7.4/Modules/bin:~/Library/GRASS/7.4/Modules/scripts
# adding GDAL and EPSG code lookup paths (Running gdal-config --datadir shows where GDAL searches for gcs.csv)
export GDAL_DATA=/Applications/GRASS-7.4.0.app/Contents/Resources/share/gdal
########################################################################
### Please customizing information below
PROJDIR='proj_folder' # full path to the project location;
mkdir $PROJDIR/obs
mkdir $PROJDIR/rhessys
mkdir $PROJDIR/rhessys/flow
mkdir $PROJDIR/rhessys/worldfiles
mkdir $PROJDIR/rhessys/tecfiles
mkdir $PROJDIR/rhessys/output
mkdir $PROJDIR/rhessys/clim
mkdir $PROJDIR/rhessys/defs
mkdir $PROJDIR/grassdata
CATCHMENTNAME='catchment_name'
#          << boundaries of the DEM, LULC, SSURGO should be sufficient large to cover the catchment area >>
downloadedDEMfile='downloaddem.tiff' # full path to the downloaded <file>
downloadedLULCfile='lulc.tiff' # full path to the downloaded <file>
downloadedSSURGO_directory='' # full path to the downloaded ssurgo <folder>
downloadedSSURGO_soilmu_file='' # the shapefile name, e.g., "soilmu_a_xxx.shp"
#          << user needs to provide outlet location and projection information  >>
gageLat='38.444565' # catchment outlet WSG84 Lat (decimal degree)
gageLong='-78.371133' # catchment outlet WSG84 Long (decimal degree; includes the negative sign if applied)
EPSGCODE='EPSG:26917' # need to (manually) lookup the EPSG code for NAD83 UTM ##N for the catchment
thres='1000' # number of grids (will be updated to use area instead)
#   note that snapping outlet to the cummulated flowline is yet developed
########### create GRASS database and delineate catchment ###############
### no edits in this block
GISDBASE=$PROJDIR/grassdata
LOCATION_NAME=$CATCHMENTNAME
LOCATION=$GISDBASE/$LOCATION_NAME
MAPSET=PERMANENT
grass74 -c $EPSGCODE -e $LOCATION # debug: it yields an error but it still works
grass74 $LOCATION/$MAPSET --exec r.import -o --overwrite input=$downloadedDEMfile output=dem
grass74 $LOCATION/$MAPSET --exec v.import -o --overwrite input=$downloadedSSURGO_directory/spatial/$downloadedSSURGO_soilmu_file output=ssurgo
grass74 $LOCATION/$MAPSET --exec sh grass_setup.sh $PROJDIR $gageLong $gageLat $thres
########### import SSURGO soil vector soilmu_a_xxx.shp ###############
# developing ...
grass74 $LOCATION/$MAPSET --exec v.to.rast --overwrite input=ssurgo use=cat output=soil_ssurgo
grass74 $LOCATION/$MAPSET --exec v.db.select map=ssurgo separator=comma file=$PROJDIR/rhessys/soil_cat_mukey.csv 
Rscript ssurgo_extraction.R $downloadedSSURGO_directory
Rscript ssurgo_soiltexture2gis.R $PROJDIR/rhessys/ssurgo_cat_mukey.csv $downloadedSSURGO_directory/soil_mukey_texture.csv
# command below will set loam as the soil type for the whole catchment (in case SSURGO is unavailable)
# grass74 $LOCATION/$MAPSET --exec r.mapcalc --overwrite expression="soil_texture = 9"
########### import LULC(e.g., NLCD) ###############
### simply use NLCD LULC to setup roads, stratum, landuse, lai, and impervious (see rules)
grass74 $LOCATION/$MAPSET --exec r.import -o --overwrite input=$downloadedLULCfile output=lulc
grass74 $LOCATION/$MAPSET --exec r.reclass --overwrite input=lulc output=roads rules=rules/default_road_reclass.txt
grass74 $LOCATION/$MAPSET --exec r.reclass --overwrite input=lulc output=stratum rules=rules/default_stratum_reclass.txt
grass74 $LOCATION/$MAPSET --exec r.reclass --overwrite input=lulc output=landuse rules=rules/default_landuse_reclass.txt
grass74 $LOCATION/$MAPSET --exec r.recode --overwrite input=lulc output=lai rules=rules/default_lai_recode.txt
grass74 $LOCATION/$MAPSET --exec r.recode --overwrite input=lulc output=impervious rules=rules/default_impervious_recode.txt

########### import optional/customized raster information ###############
# (e.g., lai, stratum, impervious, canopycover)
# grass74 $LOCATION/$MAPSET --exec r.import -o --overwrite input='lai.tiff' output=lai
# grass74 $LOCATION/$MAPSET --exec r.import -o --overwrite input='stratum.tiff' output=stratum
# grass74 $LOCATION/$MAPSET --exec r.import -o --overwrite input='impervious.tiff' output=impervious
# grass74 $LOCATION/$MAPSET --exec r.import -o --overwrite input='canopycover.tiff' output=canopycover
########### G2W and CF calculation (may take some time depends on catchment size and spatial resolution)
grass74 $LOCATION/$MAPSET --exec Rscript g2w_default.R $PROJDIR 101 defs_default/rhessys_veg_default.csv
grass74 $LOCATION/$MAPSET --exec Rscript cf_default.R $PROJDIR
Rscript LIB_RHESSys_writeTable2World.R na $PROJDIR/worldfile.csv $PROJDIR/worldfile



