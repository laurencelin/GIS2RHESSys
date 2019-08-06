#!/bin/bash
inputThreshold=$1
inputLower=$2
inputUpper=$3
#
g.region raster=dem
r.mask -r
# set x, y, and physical variables (e.g., slope, aspect ... etc) based on elevation
r.mapcalc --overwrite expression="xmap = x()"
r.mapcalc --overwrite expression="ymap = y()"
r.mapcalc --overwrite expression="rowmap = row()"
r.mapcalc --overwrite expression="colmap = col()"
r.slope.aspect --overwrite elevation=dem slope=slope_ aspect=aspect_
r.horizon --overwrite -d elevation=dem direction=180 output="west" distance=1.0
r.horizon --overwrite -d elevation=dem direction=0 output="east" distance=1.0
#
r.watershed --overwrite elevation=dem accumulation=uaa drainage=drain tci=wetness_index  # -s
r.watershed --overwrite elevation=dem threshold=$inputThreshold basin=sub_ stream=str_ half_basin=hill_  ## -s
#
# finding the nearest stream/uaa to the provided "outlet" point (vector data)
r.mapcalc --overwrite expression="tmp = if(abs(uaa)>=$inputLower&&abs(uaa)<=$inputUpper,1,null())"
r.to.vect input=tmp output=tmp type=line
declare $(v.distance -p from=outlet from_type=point to=tmp to_type=line upload=to_x,to_y separator=space | awk '{print "xyCoord=" $2 "," $3}')
g.remove -f type=vector name=tmp
g.remove -f type=raster name=tmp
# delineate catchment based on the nearest outlet point on the stream/uaa ##--- this step is contrained by D8
r.water.outlet --overwrite input=drain output=basin_ coordinates=$xyCoord
#
