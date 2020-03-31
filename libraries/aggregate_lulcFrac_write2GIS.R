arg=commandArgs(T)


library(rgrass7)
library(sp)
tryCatch({ use_sp() },error=function(cond){message(cond)},warning=function(cond){message(cond)},finally={message("Please update the rgrass7 package on R")})

rast = readRAST(arg[1]) # rast = readRAST('patch')
mask = !is.na(rast@data[[1]])

patchlulcFrac = read.csv(arg[2]); patchlulcFracCODE = colnames(patchlulcFrac)[c(-1,-2)]; print('read patchlulcFrac')
lulcCodeFrac = read.csv(arg[3]); lulcCodeFracCODE = paste('lulc',lulcCodeFrac$lulcCode,sep=''); print('read lulcCodeFrac')
toPatchCond = match(patchlulcFracCODE,lulcCodeFracCODE)
if( sum(is.na(toPatchCond)) ){
	print(patchlulcFracCODE)
	print(lulcCodeFracCODE)
    print('ERROR: the LULC composition table does not match with LULC GIS data.')
}else{
    
    lulcCodeFrac = lulcCodeFrac[toPatchCond,]
    forestCode = list(); forestCode$title=NULL; forestCode$value=NULL;
    shrubCode = list(); shrubCode$title=NULL; shrubCode$value=NULL;
    cropCode = list(); cropCode$title=NULL; cropCode$value=NULL;
    lawCode = list(); lawCode$title=NULL; lawCode$value=NULL;
    impCode = list(); impCode$title=NULL; impCode$value=NULL;
    roofCode = list(); roofCode$title=NULL; roofCode$value=NULL;
    drivewayCode = list(); drivewayCode$title=NULL; drivewayCode$value=NULL;
    pavedroadCode = list(); pavedroadCode$title=NULL; pavedroadCode$value=NULL;
    noDataCode = list(); noDataCode$title=NULL; noDataCode$value=NULL;
    waterCode = list(); waterCode$title=NULL; waterCode$value=NULL;
    
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_forest>0];
        if(length(tmp)>0){ forestCode$title = paste('lulc',tmp,sep=''); forestCode$value = match(tmp,lulcCodeFrac$lulcCode); }
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_shrub>0];
        if(length(tmp)>0){ shrubCode$title = paste('lulc',tmp,sep=''); shrubCode$value = match(tmp,lulcCodeFrac$lulcCode); }
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_crop>0];
        if(length(tmp)>0){ cropCode$title = paste('lulc',tmp,sep=''); cropCode$value = match(tmp,lulcCodeFrac$lulcCode); }
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_lawn>0];
        if(length(tmp)>0){ lawCode$title = paste('lulc',tmp,sep=''); lawCode$value = match(tmp,lulcCodeFrac$lulcCode); }
    
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$lulcComposition_imp>0];
        if(length(tmp)>0){ impCode$title = paste('lulc',tmp,sep=''); impCode$value = match(tmp,lulcCodeFrac$lulcCode); }
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$impBreakdownFrac_roof>0];
        if(length(tmp)>0){ roofCode$title = paste('lulc',tmp,sep=''); roofCode$value = match(tmp,lulcCodeFrac$lulcCode); }
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$impBreakdownFrac_driveway>0];
        if(length(tmp)>0){ drivewayCode$title = paste('lulc',tmp,sep=''); drivewayCode$value = match(tmp,lulcCodeFrac$lulcCode); }
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$impBreakdownFrac_pavedRoad>0];
        if(length(tmp)>0){ pavedroadCode$title = paste('lulc',tmp,sep=''); pavedroadCode$value = match(tmp,lulcCodeFrac$lulcCode); }
    
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$noData>0];
    if(length(tmp)>0){ noDataCode$title = paste('lulc',tmp,sep=''); noDataCode$value = match(tmp,lulcCodeFrac$lulcCode); }
    
    tmp=lulcCodeFrac$lulcCode[lulcCodeFrac$waterFrac>0];
    if(length(tmp)>0){ waterCode$title = paste('lulc',tmp,sep=''); waterCode$value = match(tmp,lulcCodeFrac$lulcCode); }
   
   
   
    if(length(arg)>3){suffix = arg[4] }else{ suffix = ''}
    
    #modeList = list()
    gisOrder = match(rast@data[[1]][mask], patchlulcFrac$patchID)
    # ... constructing GIS maps
    rast$lulcComposition = rep(0,length(rast@data[[1]]))
    if(length(forestCode$title)>=1){
        rast$lulcComposition[mask] = (rowSums(do.call(cbind,lapply(seq_along(forestCode$title), function(ii){
            patchlulcFrac[, forestCode$title[ii]]* lulcCodeFrac$lulcComposition_forest[forestCode$value[ii]]
        })))/patchlulcFrac$total)[gisOrder]
    }#if
    writeRAST(rast,paste('forestFrac', suffix,sep=''),zcol='lulcComposition',overwrite=T)
    #modeList[[1]] = (patchlulcFrac[, forestCode]/patchlulcFrac$total)[gisOrder]
    
    rast$lulcComposition = rep(0,length(rast@data[[1]]))
    if(length(shrubCode$title)>=1){
        rast$lulcComposition[mask] = (rowSums(do.call(cbind,lapply(seq_along(shrubCode$title), function(ii){
            patchlulcFrac[, shrubCode$title[ii]]* lulcCodeFrac$lulcComposition_shrub[shrubCode$value[ii]]
        })))/patchlulcFrac$total)[gisOrder]
    }#if
    writeRAST(rast,paste('shrubFrac', suffix,sep=''),zcol='lulcComposition',overwrite=T)
    
    rast$lulcComposition = rep(0,length(rast@data[[1]]))
    if(length(cropCode$title)>=1){
        rast$lulcComposition[mask] = (rowSums(do.call(cbind,lapply(seq_along(cropCode$title), function(ii){
            patchlulcFrac[, cropCode$title[ii]]* lulcCodeFrac$lulcComposition_crop[cropCode$value[ii]]
        })))/patchlulcFrac$total)[gisOrder]
    }#if
    writeRAST(rast,paste('cropFrac', suffix,sep=''),zcol='lulcComposition',overwrite=T)
    
    rast$lulcComposition = rep(0,length(rast@data[[1]]))
    if(length(lawCode$title)>=1){
        rast$lulcComposition[mask] = (rowSums(do.call(cbind,lapply(seq_along(lawCode$title), function(ii){
            patchlulcFrac[, lawCode$title[ii]]* lulcCodeFrac$lulcComposition_lawn[lawCode$value[ii]]
        })))/patchlulcFrac$total)[gisOrder]
    }#if
    writeRAST(rast,paste('lawnFrac', suffix,sep=''),zcol='lulcComposition',overwrite=T)
    
    
    
    
    rast$lulcComposition = rep(0,length(rast@data[[1]]))
    if(length(impCode$title)>=1){
        rast$lulcComposition[mask] = (rowSums(do.call(cbind,lapply(seq_along(impCode$title), function(ii){
            patchlulcFrac[, impCode$title[ii]]* lulcCodeFrac$lulcComposition_imp[impCode$value[ii]]
        })))/patchlulcFrac$total)[gisOrder]
    }#if
    writeRAST(rast,paste('impFrac', suffix,sep=''),zcol='lulcComposition',overwrite=T)
    
    rast$lulcComposition = rep(0,length(rast@data[[1]]))
    if(length(roofCode$title)>=1){
        rast$lulcComposition[mask] = (rowSums(do.call(cbind,lapply(seq_along(roofCode$title), function(ii){
            patchlulcFrac[, roofCode$title[ii]]* lulcCodeFrac$impBreakdownFrac_roof[roofCode$value[ii]]
        })))/patchlulcFrac$total)[gisOrder]
    }#if
    writeRAST(rast,paste('roofFrac', suffix,sep=''),zcol='lulcComposition',overwrite=T)
    
    rast$lulcComposition = rep(0,length(rast@data[[1]]))
    if(length(drivewayCode$title)>=1){
        rast$lulcComposition[mask] = (rowSums(do.call(cbind,lapply(seq_along(drivewayCode$title), function(ii){
            patchlulcFrac[, drivewayCode$title[ii]]* lulcCodeFrac$impBreakdownFrac_driveway[drivewayCode$value[ii]]
        })))/patchlulcFrac$total)[gisOrder]
    }#if
    writeRAST(rast,paste('drivewayFrac', suffix,sep=''),zcol='lulcComposition',overwrite=T)
    
    rast$lulcComposition = rep(0,length(rast@data[[1]]))
    if(length(pavedroadCode$title)>=1){
        rast$lulcComposition[mask] = (rowSums(do.call(cbind,lapply(seq_along(pavedroadCode$title), function(ii){
            patchlulcFrac[, pavedroadCode$title[ii]]* lulcCodeFrac$impBreakdownFrac_pavedRoad[pavedroadCode$value[ii]]
        })))/patchlulcFrac$total)[gisOrder]
    }#if
    writeRAST(rast,paste('pavedroadFrac', suffix,sep=''),zcol='lulcComposition',overwrite=T)
    
    
    
    rast$lulcComposition = rep(0,length(rast@data[[1]]))
    if(length(noDataCode$title)>=1){
        rast$lulcComposition[mask] = (rowSums(do.call(cbind,lapply(seq_along(noDataCode$title), function(ii){
            patchlulcFrac[, noDataCode$title[ii]]* lulcCodeFrac$noData[noDataCode$value[ii]]
        })))/patchlulcFrac$total)[gisOrder]
    }#if
    writeRAST(rast,paste('noDataFrac', suffix,sep=''),zcol='lulcComposition',overwrite=T)
    
    rast$lulcComposition = rep(0,length(rast@data[[1]]))
    if(length(waterCode$title)>=1){
        rast$lulcComposition[mask] = (rowSums(do.call(cbind,lapply(seq_along(waterCode$title), function(ii){
            patchlulcFrac[, waterCode$title[ii]]* lulcCodeFrac$waterFrac[waterCode$value[ii]]
        })))/patchlulcFrac$total)[gisOrder]
    }#if
    writeRAST(rast,paste('waterFrac', suffix,sep=''),zcol='lulcComposition',overwrite=T)
    
}# if wrong LULC code table







