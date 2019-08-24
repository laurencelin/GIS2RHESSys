library(rgrass7)

gisADD = function(x,y){
	return <- sapply(seq_along(x), function(ii){
		ifelse( xor(is.na(x[ii]),is.na(y[ii])), sum(x[ii],y[ii],na.rm=T), x[ii]+y[ii])
	})
}#function


rast = readRAST(c('basin','forestFrac','shrubFrac','lawnFrac','cropFrac','roofFrac','drivewayFrac','pavedroadFrac'))
mask = !is.na(rast@data[[1]])

forested = gisADD(rast@data[[2]][mask], rast@data[[3]][mask]) #1 forest
grassed = gisADD(rast@data[[4]][mask], rast@data[[5]][mask]) #2 lawn/pasture/crop
roofed = rast@data[[6]][mask] #3 roof
parked = rast@data[[7]][mask] #4 parking/driveway
roaded = rast@data[[8]][mask] #5 roads
# 6 bared land / water body

mergedClass = apply(cbind(forested, grassed, roofed, parked, roaded),1,function(x){
	lulcclass = which.max(x); return <- ifelse( length(lulcclass)==0, 6, lulcclass);
})#apply

rast$mergedModeLULC = rep(NA,length(mask))
rast$mergedModeLULC[mask] = as.integer(mergedClass)
writeRAST(rast,'mergedModeLULC',zcol='mergedModeLULC', overwrite=T)