library(rgrass7)
rast=readRAST(c('basin_','sub_','hill_','str_'))
mask = !is.na(rast@data[[2]])
basin=rast@data[[1]][mask]
sub=rast@data[[2]][mask]
hill=rast@data[[3]][mask]
str=rast@data[[4]][mask]

index = tapply(seq_along(sub),sub,function(ii){ ii })
index_partial = tapply(seq_along(sub),sub,function(ii){ ii[ !is.na(basin[ii]) ] })
sub_in_basin = tapply(seq_along(sub),sub,function(ii){ sum(basin[ii],na.rm=T)/length(ii) })
cond = sub_in_basin>0; # sub_in_basin[cond]

rast$tmp = rep(NA,length(mask))
for( ii in seq_along(sub_in_basin)[cond] ){
	if(sub_in_basin[ii]>0.95){
		rast$tmp[mask][ index[[ii]] ] = 1	
	}else{
		rast$tmp[mask][ index_partial[[ii]] ] = 1
	}
}#ii
writeRAST(rast,"basin",zcol='tmp', overwrite=T)

basinCond = !is.na(rast$tmp[mask])
rast$tmp = rep(NA,length(mask))
rast$tmp[mask][basinCond] = sub[basinCond]
writeRAST(rast,"sub",zcol='tmp', overwrite=T)

basinCond = !is.na(rast$tmp[mask])
rast$tmp = rep(NA,length(mask))
rast$tmp[mask][basinCond] = hill[basinCond]
writeRAST(rast,"hill",zcol='tmp', overwrite=T)

basinCond = !is.na(rast$tmp[mask])
rast$tmp = rep(NA,length(mask))
rast$tmp[mask][basinCond] = str[basinCond]
writeRAST(rast,"str",zcol='tmp', overwrite=T)

