arg=commandArgs(T)
arg=c('patch',
'/Users/laurencelin/Downloads/BeaverCkReservoir/wattsbranch_rhessys/lulcFrac10m.csv',
'/Users/laurencelin/Downloads/BeaverCkReservoir/wattsbranch_rhessys/lulc_codeinformation.csv')


library(rgrass7)

rast = readRAST(arg[1])
mask = !is.na(rast@data[[1]])

patchlulcFrac = read.csv(arg[2])
lulcCodeFrac = read.csv(arg[3])
	# Baltimore 1-m LULC legend
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


	forestCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_forest>0],sep='')
	shurbCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_shrub>0],sep='')
	lawCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_lawn>0],sep='')
	impCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_imp>0],sep='')
	roofCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$impBreakdownFrac_roof>0],sep='')
	drivewayCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$impBreakdownFrac_driveway>0],sep='')
	pavedroadCode = paste('lulc',lulcCodeFrac$lulcCode[lulcCodeFrac$impBreakdownFrac_pavedRoad>0],sep='')
	
	gisOrder = match(rast@data[[1]][mask], patchlulcFrac$patchID)
	forestCode = forestCode[forestCode%in%colnames(patchlulcFrac)]
	shurbCode = shurbCode[shurbCode%in%colnames(patchlulcFrac)]
	lawCode = lawCode[lawCode%in%colnames(patchlulcFrac)]
	impCode = impCode[impCode%in%colnames(patchlulcFrac)]
	
	
	rast$lulcComposition_forest = rep(0,length(rast@data[[1]]))
	if(length(forestCode)>1) rast$lulcComposition_forest[mask] = (rowSums(patchlulcFrac[, forestCode])/patchlulcFrac$total)[gisOrder]
	if(length(forestCode)==1) rast$lulcComposition_forest[mask] = (patchlulcFrac[, forestCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'forestFrac',zcol='lulcComposition_forest',overwrite=T)
	
	rast$lulcComposition_shrub = rep(0,length(rast@data[[1]]))
	if(length(shurbCode)>1) rast$lulcComposition_shrub[mask] = (rowSums(patchlulcFrac[, shurbCode])/patchlulcFrac$total)[gisOrder]
	if(length(shurbCode)==1) rast$lulcComposition_shrub[mask] = (patchlulcFrac[, shurbCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'shrubFrac',zcol='lulcComposition_shrub',overwrite=T)

	rast$lulcComposition_lawn = rep(0,length(rast@data[[1]]))
	if(length(lawCode)>1) rast$lulcComposition_lawn[mask] = (rowSums(patchlulcFrac[, lawCode])/patchlulcFrac$total)[gisOrder]
	if(length(lawCode)==1) rast$lulcComposition_lawn[mask] = (patchlulcFrac[, lawCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'lawnFrac',zcol='lulcComposition_lawn',overwrite=T)

	rast$lulcComposition_imp = rep(0,length(rast@data[[1]]))
	if(length(impCode)>1) rast$lulcComposition_imp[mask] = (rowSums(patchlulcFrac[, impCode])/patchlulcFrac$total)[gisOrder]
	if(length(impCode)==1) rast$lulcComposition_imp[mask] = (patchlulcFrac[, impCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'impFrac',zcol='lulcComposition_imp',overwrite=T)



	rast$impBreakdownFrac_roof = rep(0,length(rast@data[[1]]))
	if(length(roofCode)>1) rast$impBreakdownFrac_roof[mask] = (rowSums(patchlulcFrac[, roofCode])/patchlulcFrac$total)[gisOrder]
	if(length(roofCode)==1) rast$impBreakdownFrac_roof[mask] = (patchlulcFrac[, roofCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'roofFrac',zcol='impBreakdownFrac_roof',overwrite=T)

	rast$impBreakdownFrac_driveway = rep(0,length(rast@data[[1]]))
	if(length(drivewayCode)>1) rast$impBreakdownFrac_driveway[mask] = (rowSums(patchlulcFrac[, drivewayCode])/patchlulcFrac$total)[gisOrder]
	if(length(drivewayCode)==1) rast$impBreakdownFrac_driveway[mask] = (patchlulcFrac[, drivewayCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'drivewayFrac',zcol='impBreakdownFrac_driveway',overwrite=T)

	rast$impBreakdownFrac_pavedRoad = rep(0,length(rast@data[[1]]))
	if(length(pavedroadCode)>1) rast$impBreakdownFrac_pavedRoad[mask] = (rowSums(patchlulcFrac[, pavedroadCode])/patchlulcFrac$total)[gisOrder]
	if(length(pavedroadCode)==1) rast$impBreakdownFrac_pavedRoad[mask] = (patchlulcFrac[, pavedroadCode]/patchlulcFrac$total)[gisOrder]
	writeRAST(rast,'pavedroadFrac',zcol='impBreakdownFrac_pavedRoad',overwrite=T)
	
	









