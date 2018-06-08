#!/bin/bash
export PATH=$PATH:/usr/local/bin
export PATH=$PATH:/Library/Frameworks/R.framework/Resources
PROJDIR='~'
ClimateStationID='101'
DEM='dem'
LandUSE=''
LAI=''
stratum=''
gageUTMx='277826.68'
gageUTMy='3881430.28'
thres='1000'
## need to pre-process the DEM for pits
## r.lake
#########
cd $PROJDIR
g.region raster=$DEM
r.mask -r
declare $(r.info map=$DEM | awk 'NR==13{print "maxcol="$3}')
r.watershed -s --overwrite elevation=$DEM accumulation=uaa drainage=drain tci=wettness_index
r.slope.aspect --overwrite elevation=$DEM slope=slope aspect=aspect
r.water.outlet input=drain output=basin coordinates=$gageUTMx,$gageUTMy
r.horizon -d elevation=$DEM direction=180 output="west" distance=1.0
r.horizon -d elevation=$DEM direction=0 output="east" distance=1.0
g.region raster=$DEM
g.region zoom=basin
r.mask -r
r.mask raster=basin
r.watershed -s --overwrite elevation=$DEM threshold=$thres basin=sub stream=str half_basin=hill
r.mapcalc expression="basin = if(isnull(hill),null(),1)" --overwrite
g.region raster=$DEM
g.region zoom=basin
r.mask -r
r.mask raster=basin
r.mapcalc expression="patch = row()*$maxcol+col()"
r.mapcalc expression="xmap = x()"
r.mapcalc expression="ymap = y()"
r.mapcalc expression="rowmap = row()"
r.mapcalc expression="colmap = col()"
r.mapcalc expression="ZERO = 0"
r.mapcalc expression="ONE = 1"
# soil (from SSURGO)
# roads, landuse, stratum, lai (could be assigned by NLCD)
# continue in R scripts.
Rscript g2w_patch_grid_default.R $PROJDIR $ClimateStationID
Rscript LIB_RHESSys_writeTable2World.R
Rscript






