arg=commandArgs(T)

library(rgrass7)

rast = readRAST(arg[1])
mask = !is.na(rast@data[[1]])

patchlulcFrac = read.csv(arg[2])
lulcCodeFrac = read.csv(arg[3])
	# may need to customize rules below 
	# 1 = water
	# 2 = wetland
	# 3 = tree canopy
	# 4 = shrub
	# 5 = lawn
	# 6 = barren
	# 7 = impervious structure
	# 8 = impervious surface
	# 9 = impervious road
	# 10 = tree canopy over structure *
	# 11 = tree canopy over impervious surface *
	# 12 = tree canopy over roads *



	forestCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$forestFrac>0],sep='')
	shurbCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$shrubFrac>0],sep='')
	lawCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$lawnFrac>0],sep='')
	impCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$impFrac>0],sep='')
	roofCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$roofFrac>0],sep='')
	drivewayCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$drivewayFrac>0],sep='')
	pavedroadCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$pavedRoadFrac>0],sep='')
	
	gisOrder = match(rast@data[[1]][mask], patchlulcFrac$patchID)
	forestCode = forestCode[forestCode%in%colnames(patchlulcFrac)]
	shurbCode = shurbCode[shurbCode%in%colnames(patchlulcFrac)]
	lawCode = lawCode[lawCode%in%colnames(patchlulcFrac)]
	impCode = impCode[impCode%in%colnames(patchlulcFrac)]
	
	
	rast$forestFrac = rep(0,length(rast@data[[1]]))
	if(length(forestCode)>1) rast$forestFrac[mask] = (rowSums(patchlulcFrac[, forestCode])/patchlulcFrac$total)[gisOrder]
	if(length(forestCode)==1) rast$forestFrac[mask] = (patchlulcFrac[, forestCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'forestFrac',zcol='forestFrac',overwrite=T)
	
	rast$shrubFrac = rep(0,length(rast@data[[1]]))
	if(length(shurbCode)>1) rast$shrubFrac[mask] = (rowSums(patchlulcFrac[, shurbCode])/patchlulcFrac$total)[gisOrder]
	if(length(shurbCode)==1) rast$shrubFrac[mask] = (patchlulcFrac[, shurbCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'shrubFrac',zcol='shrubFrac',overwrite=T)

	rast$lawnFrac = rep(0,length(rast@data[[1]]))
	if(length(lawCode)>1) rast$lawnFrac[mask] = (rowSums(patchlulcFrac[, lawCode])/patchlulcFrac$total)[gisOrder]
	if(length(lawCode)==1) rast$lawnFrac[mask] = (patchlulcFrac[, lawCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'lawnFrac',zcol='lawnFrac',overwrite=T)

	rast$impFrac = rep(0,length(rast@data[[1]]))
	if(length(impCode)>1) rast$impFrac[mask] = (rowSums(patchlulcFrac[, impCode])/patchlulcFrac$total)[gisOrder]
	if(length(impCode)==1) rast$impFrac[mask] = (patchlulcFrac[, impCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'impFrac',zcol='impFrac',overwrite=T)



	rast$roofFrac = rep(0,length(rast@data[[1]]))
	if(length(roofCode)>1) rast$roofFrac[mask] = (rowSums(patchlulcFrac[, roofCode])/patchlulcFrac$total)[gisOrder]
	if(length(roofCode)==1) rast$roofFrac[mask] = (patchlulcFrac[, roofCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'roofFrac',zcol='roofFrac',overwrite=T)

	rast$drivewayFrac = rep(0,length(rast@data[[1]]))
	if(length(drivewayCode)>1) rast$drivewayFrac[mask] = (rowSums(patchlulcFrac[, drivewayCode])/patchlulcFrac$total)[gisOrder]
	if(length(drivewayCode)==1) rast$drivewayFrac[mask] = (patchlulcFrac[, drivewayCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'parkFrac',zcol='parkFrac',overwrite=T)

	rast$pavedroadFrac = rep(0,length(rast@data[[1]]))
	if(length(pavedroadCode)>1) rast$pavedroadFrac[mask] = (rowSums(patchlulcFrac[, pavedroadCode])/patchlulcFrac$total)[gisOrder]
	if(length(pavedroadCode)==1) rast$pavedroadFrac[mask] = (patchlulcFrac[, pavedroadCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'roadFrac',zcol='roadFrac',overwrite=T)
	
	









