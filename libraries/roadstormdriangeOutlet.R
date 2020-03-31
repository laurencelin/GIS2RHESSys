options(scipen=999)
arg = commandArgs(T)
library(rgrass7)
library(sp)
tryCatch({ use_sp() },error=function(cond){message(cond)},warning=function(cond){message(cond)},finally={message("Please update the rgrass7 package on R")})

#arg=c('basin','roadExitDEM','roadExit_neighbor_id','roadExit_neighbor_usdDEM','dem','xmap','ymap','patch','roadExit_neighbor_usdDEMmed','roadExitOutletPatchID','roadExitOutlet', '120')

rast = readRAST(arg[1:8])
mask = !is.na(rast@data[[1]])
roadExtDEM = rast@data[[2]][mask]
clumpIDMAP = rast@data[[3]][mask]
valueMAP = rast@data[[4]][mask]
dem = rast@data[[5]][mask]
xx = rast@data[[6]][mask]
yy = rast@data[[7]][mask]
patch = rast@data[[8]][mask]
rast$tmp = rep(NA,length(mask))
rast$tmpII = rep(NA,length(mask))


roadInletCond = !is.na(roadExtDEM)
roadInlet_dem = dem[roadInletCond]
roadInlet_x  = xx[roadInletCond]
roadInlet_y  = yy[roadInletCond]
roadInlet_patch  = patch[roadInletCond]
roadInlet_outlet_index = seq_len(length(roadInlet_x))
roadInlet_outlet_patch = rep(NA,length(roadInlet_x))
roadInlet_outlet_dist = rep(1000,length(roadInlet_x))
roadInlet_outlet_dem = rep(roadInlet_dem,length(roadInlet_x))

clumpID = unique(clumpIDMAP)
clumpID = clumpID[!is.na(clumpID)]
for(ii in clumpID){
	# zonal stats
	cond = !is.na(clumpIDMAP) & (clumpIDMAP==ii); # clumpIDMAP[cond]
	targetValue = median(valueMAP[cond],na.rm=T)
	rast$tmp[mask][cond] = targetValue
	
	# find nearest roadInlet that has higher elevation
	condII = !is.na(valueMAP[cond]) & (valueMAP[cond]>=targetValue); # valueMAP[cond][condII]
	zonalpoints_dem = dem[cond][condII]
	zonalpoints_x = xx[cond][condII]
	zonalpoints_y = yy[cond][condII]
	zonalpoints_patch = patch[cond][condII]
	
	# into the many-to-many relationship
    thresDist = as.numeric(arg[12])
	for(jj in seq_along(zonalpoints_x)){
		distList = sqrt( (roadInlet_x-zonalpoints_x[jj])^2 + (roadInlet_y-zonalpoints_y[jj])^2 )
		selectedInlet = distList < thresDist & roadInlet_dem > zonalpoints_dem[jj]
		
		#finalCond = roadInlet_outlet_dem[roadInlet_outlet_index[selectedInlet]] > zonalpoints_dem[jj];
		
		finalCond = (roadInlet_dem[roadInlet_outlet_index[selectedInlet]] - roadInlet_outlet_dem[roadInlet_outlet_index[selectedInlet]])/roadInlet_outlet_dist[roadInlet_outlet_index[selectedInlet]] < (roadInlet_dem[roadInlet_outlet_index[selectedInlet]]-zonalpoints_dem[jj])/distList[selectedInlet]
		
		roadInlet_outlet_dist[roadInlet_outlet_index[selectedInlet]][finalCond] = distList[selectedInlet][finalCond]
		roadInlet_outlet_dem[roadInlet_outlet_index[selectedInlet]][finalCond] = zonalpoints_dem[jj]
		roadInlet_outlet_patch[roadInlet_outlet_index[selectedInlet]][finalCond] = zonalpoints_patch[jj]
	}#jj
	
}#ii
cbind(roadInlet_patch, roadInlet_outlet_patch)
writeRAST(rast,arg[9],zcol='tmp',overwrite=T)

rast$tmpII[mask][roadInletCond] = roadInlet_outlet_patch
writeRAST(rast,arg[10],zcol='tmpII', overwrite=T)

rast$tmpII = rep(NA,length(mask))
cond = !is.na(roadInlet_outlet_patch)
match_index = match(roadInlet_outlet_patch[cond],patch)
rast$tmpII[mask][match_index] = roadInlet_patch[cond]
writeRAST(rast,arg[11],zcol='tmpII', overwrite=T)


