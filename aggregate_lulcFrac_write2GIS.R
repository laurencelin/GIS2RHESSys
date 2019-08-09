arg=commandArgs(T)

arg=c('patch',
'/Users/laurencelin/Downloads/BeaverCkReservoir/wattsbranch_rhessys_30m/lulcFrac30m.csv',
'/Users/laurencelin/Downloads/BeaverCkReservoir/wattsbranch_rhessys_30m/lulc_codeinformation.csv')




library(rgrass7)

rast = readRAST(arg[1])
mask = !is.na(rast@data[[1]])

patchlulcFrac = read.csv(arg[2]); patchlulcFracCODE = colnames(patchlulcFrac)[c(-1,-2)]
lulcCodeFrac = read.csv(arg[3]); lulcCodeFracCODE = paste('lulc',lulcCodeFrac$lulcCode,sep='')
toPatchCond = match(patchlulcFracCODE,lulcCodeFracCODE)
if( sum(is.na(toPatchCond)) ){
    print('ERROR: the LULC composition table does not match with LULC GIS data.')
}else{
    
    lulcCodeFrac = lulcCodeFrac[toPatchCond,]
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_forest>0]; if(length(tmp)>0) forestCode = paste('lulc',tmp,sep='') else forestCode=NULL
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_shrub>0]; if(length(tmp)>0) shrubCode = paste('lulc',tmp,sep='') else shrubCode=NULL
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_crop>0]; if(length(tmp)>0) cropCode = paste('lulc',tmp,sep='') else cropCode=NULL
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_lawn>0]; if(length(tmp)>0) lawCode = paste('lulc',tmp,sep='') else lawCode=NULL
    
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_imp>0]; if(length(tmp)>0) impCode = paste('lulc',tmp,sep='') else impCode=NULL
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$impBreakdownFrac_roof>0]; if(length(tmp)>0) roofCode = paste('lulc',tmp,sep='') else roofCode=NULL
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$impBreakdownFrac_driveway>0]; if(length(tmp)>0) drivewayCode = paste('lulc',tmp,sep='') else drivewayCode=NULL
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$impBreakdownFrac_pavedRoad>0]; if(length(tmp)>0) pavedroadCode = paste('lulc',tmp,sep='') else  pavedroadCode=NULL
    
    forestCode
    shrubCode
    cropCode
    lawCode
    impCode
    roofCode
    drivewayCode
    pavedroadCode
    
    gisOrder = match(rast@data[[1]][mask], patchlulcFrac$patchID)
    # ... constructing GIS maps
    rast$lulcComposition_forest = rep(0,length(rast@data[[1]]))
    if(length(forestCode)>1) rast$lulcComposition_forest[mask] = (rowSums(patchlulcFrac[, forestCode])/patchlulcFrac$total)[gisOrder]
    if(length(forestCode)==1) rast$lulcComposition_forest[mask] = (patchlulcFrac[, forestCode]/patchlulcFrac$total)[gisOrder]
    writeRAST(rast,'forestFrac',zcol='lulcComposition_forest',overwrite=T)
    
    rast$lulcComposition_shrub = rep(0,length(rast@data[[1]]))
    if(length(shrubCode)>1) rast$lulcComposition_shrub[mask] = (rowSums(patchlulcFrac[, shrubCode])/patchlulcFrac$total)[gisOrder]
    if(length(shrubCode)==1) rast$lulcComposition_shrub[mask] = (patchlulcFrac[, shrubCode]/patchlulcFrac$total)[gisOrder]
    writeRAST(rast,'shrubFrac',zcol='lulcComposition_shrub',overwrite=T)
    
    rast$lulcComposition_crop = rep(0,length(rast@data[[1]]))
    if(length(cropCode)>1) rast$lulcComposition_crop[mask] = (rowSums(patchlulcFrac[, cropCode])/patchlulcFrac$total)[gisOrder]
    if(length(cropCode)==1) rast$lulcComposition_crop[mask] = (patchlulcFrac[, cropCode]/patchlulcFrac$total)[gisOrder]
    writeRAST(rast,'cropFrac',zcol='lulcComposition_crop',overwrite=T)
    
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
    
}#







