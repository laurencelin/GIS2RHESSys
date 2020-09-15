arg=commandArgs(T)


library(rgrass7)
library(sp)
tryCatch({ use_sp() },error=function(cond){message(cond)},warning=function(cond){message(cond)},finally={message("Please update the rgrass7 package on R")})
#arg=c('patch30m','demRAW','lawnfieldIDs','rooftopIDs','rooftopIDlawnIDs','xmap','ymap')
rast = readRAST(arg[1:7])
mask = !is.na(rast@data[[1]])
patch = rast@data[[1]][mask]
dem = rast@data[[2]][mask]
lawn = rast@data[[3]][mask]
roof = rast@data[[4]][mask]
rooflawn = rast@data[[5]][mask]
xx = rast@data[[6]][mask]
yy = rast@data[[7]][mask]

cond = !is.na(roof)
roofID = unique(roof[cond])
roof_drain_table = as.data.frame(do.call(rbind, lapply(roofID,function(id){
    
    roof_patchID = unique(patch[cond][roof[cond]==id])
    roof_dem = mean(dem[cond][roof[cond]==id])
    roof_patch = patch[patch%in%roof_patchID]
    roof_x = mean(xx[cond][roof[cond]==id])
    roof_y = mean(yy[cond][roof[cond]==id])
    # sources
    sourceCond = dem[patch%in%roof_patchID] > roof_dem & is.na(roof[patch%in%roof_patchID])
    if(sum(sourceCond)>0){
        source_patch_count = table(roof_patch[sourceCond])
        source_patch = as.numeric(names(source_patch_count)[which.max(source_patch_count)]);source_patch
    }else{
        source_patch_count = table(roof_patch)
        source_patch = as.numeric(names(source_patch_count)[which.max(source_patch_count)]);source_patch
    }

    # drain field (rasterization shifted the boundaries)
    dfCond = dem[patch%in%roof_patchID] < roof_dem & !is.na(lawn[patch%in%roof_patchID]) & is.na(roof[patch%in%roof_patchID])
    if(sum(dfCond)>0){
        df_patch_count = table(roof_patch[dfCond])
        df_patch = as.numeric(names(df_patch_count)[which.max(df_patch_count)]);df_patch
    }else{
        # look for nearest lawn field
        nearlawnID = unique(rooflawn[cond][roof[cond]==id])
        nearlawnCond = !is.na(lawn) & lawn%in%nearlawnID
        if(sum(nearlawnCond)>0){
            dist = tapply(sqrt((xx[nearlawnCond] - roof_x)^2 + (yy[nearlawnCond] - roof_y)^2), patch[nearlawnCond], mean)
            selectCond = which.min(dist[table(patch[nearlawnCond])>4])
            df_patch = ifelse(sum(selectCond)>0,as.numeric(names(dist)[selectCond]),as.numeric(names(dist)[which.min(dist)]) )
        }else{
            df_patch = (patch[cond][roof[cond]==id])[which.min(dem[cond][roof[cond]==id])] # if no lawn, find the lowest roottopp patch
        }
    }# if

    # irrigration lawn: roof->lawnID->patch->irrigation area
    roof_lawnID = unique(lawn[patch%in%roof_patchID]); roof_lawnID = roof_lawnID[!is.na(roof_lawnID)]
    lawn_patch = patch[lawn%in%roof_lawnID]
    lawn_patchID = unique(lawn_patch)
    lawn_patchID_count = patch[patch%in%lawn_patchID]
    lawn_patchX = xx[lawn%in%roof_lawnID]
    lawn_patchY = yy[lawn%in%roof_lawnID]
    dist = sqrt((lawn_patchX-roof_x)^2 + (lawn_patchY-roof_y)^2)
    irrigration_weight = ifelse(dist<50, 1, exp(-dist*0.25))
    patch_scale_weight = round(tapply(irrigration_weight,lawn_patch,sum) / tapply(rep(1,length(lawn_patchID_count)),lawn_patchID_count,sum),3)
    irrigration_string = paste(paste(names(patch_scale_weight),collapse=';'),paste(patch_scale_weight,collapse=';'),sep='_')
    
    
    #print(paste(id,source_patch,df_patch, irrigration_string))
    #}
    
    return <-c(id,source_patch,df_patch, irrigration_string,0)
})))
roof_drain_table$dailymax = 4 # 4mm/d
colnames(roof_drain_table)=c('rooftopID','inPatch','outPatch','irrigationString','mode','dailymax')
write.csv(roof_drain_table[,c(1:3,5)],arg[8],row.names=F,quote=F)

irrigrationTable = as.data.frame(do.call(rbind,lapply(seq_len(dim(roof_drain_table)[1]),function(ii){
    
    source_patchID = as.numeric(roof_drain_table[ii,2])
    stringVar = unlist(strsplit(roof_drain_table[ii,4],'_',fixed=T))
    if(length(stringVar)>1){
        patchID = as.numeric(unlist(strsplit(stringVar[1],';',fixed=T)))
        patchIrriFrac = as.numeric(unlist(strsplit(stringVar[2],';',fixed=T)))
        selectCond = patchIrriFrac>0
        if(sum(selectCond)>0){
            return <- cbind(source_patchID,patchID[selectCond],patchIrriFrac[selectCond],0)
        }else{
            return <- NULL
        }
    }else{
        return <- NULL
    }
})))#
irrigrationTable$dailymax = 4 # 4mm/d
colnames(irrigrationTable)=c('sourcePatchID','irrigrationPatchID','frac','mode','dailymax')
write.csv(irrigrationTable,arg[9],row.names=F,quote=F)

## need to update this one to write out surface, subsurface, mode


