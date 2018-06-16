#!/bin/bash
PROJDIR=$1
gageLong=$2
gageLat=$3
#m.proj -i coordinates=-83.43599,35.05
#m.proj -i coordinates=-83.43599,35.05 separator=space | awk '!($3="")'
declare $(m.proj -i coordinates=$gageLong,$gageLat separator=space | awk '{print "xyCoord=" $1 "," $2}')
g.region raster=dem
r.mask -r
declare $(r.info map=dem | awk 'NR==13{print "maxcol="$3}')
r.watershed -s --overwrite elevation=dem accumulation=uaa drainage=drain tci=wetness_index
r.slope.aspect --overwrite elevation=dem slope=slope aspect=aspect
r.horizon -d elevation=dem direction=180 output="west" distance=1.0
r.horizon -d elevation=dem direction=0 output="east" distance=1.0
r.water.outlet input=drain output=basin coordinates=$xyCoord
g.region zoom=basin
r.mask raster=basin
r.watershed -s --overwrite elevation=dem threshold=$4 basin=sub stream=str half_basin=hill
r.mapcalc expression="basin = if(isnull(hill),null(),1)" --overwrite
g.region raster=dem
g.region zoom=basin
r.mask -r
r.mask raster=basin
r.mapcalc --overwrite expression="patch = row()*$maxcol+col()"
r.mapcalc --overwrite expression="zone = patch"
r.mapcalc --overwrite expression="xmap = x()"
r.mapcalc --overwrite expression="ymap = y()"
r.mapcalc --overwrite expression="rowmap = row()"
r.mapcalc --overwrite expression="colmap = col()"
r.mapcalc --overwrite expression="ZERO = 0"
r.mapcalc --overwrite expression="ONE = 1"
