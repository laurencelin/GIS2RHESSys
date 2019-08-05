#!/bin/bash
inputThreshold=$1
inputLower=$2
inputUpper=$3
#
# set maxcol
declare $(r.info map=dem | awk 'NR==13{print "maxcol="$3}')
#
g.region zoom=basin
r.mask raster=basin
r.mapcalc --overwrite expression="basin = if(isnull(hill),null(),1)"
g.region raster=dem
g.region zoom=basin
r.mask -r
r.mask raster=basin
# set patch and other variables
r.mapcalc --overwrite expression="patch = row()*$maxcol+col()"
r.mapcalc --overwrite expression="zone = patch"
r.mapcalc --overwrite expression="rowmap = row()"
r.mapcalc --overwrite expression="colmap = col()"
r.mapcalc --overwrite expression="ZERO = 0"
r.mapcalc --overwrite expression="ONE = 1"
r.mapcalc --overwrite expression="slope = if(isnull(slope_),0.143,slope_)"
r.mapcalc --overwrite expression="aspect = if(isnull(aspect_),abs(drain)*45,aspect_)"
