library(rgrass7)
rast=readRAST(c('basin_','sub_','hill_','str_','drain'))
mask = !is.na(rast@data[[2]]) # use sub as mask
basin=rast@data[[1]][mask]
sub=rast@data[[2]][mask]
hill=rast@data[[3]][mask]
str=rast@data[[4]][mask]
strCOND = !is.na(str)
drain=rast@data[[5]][mask]

rast2=readRAST(c('rowmap','colmap'))
rows = rast2@data[[1]][mask]
cols = rast2@data[[2]][mask]
	strRows = rows[strCOND]
	strCols = cols[strCOND]
maxCol = max(cols,na.rm=T)
maskRC = rows*maxCol+cols 
maskRC_string2Index <- new.env(hash=T)
    list2env(setNames(as.list(sub),maskRC),envir=maskRC_string2Index) #<<---- native R hash 
     
    
# ... which sub contain D8basin
index = tapply(seq_along(sub),sub,function(ii){ ii })
index_partial = tapply(seq_along(sub),sub,function(ii){ ii[ !is.na(basin[ii]) ] })

# ... stream network structure
# 1.  2. 3.  4. 5.  6. 7.  8. (GRASS from current drainTO code order)
# NE, N, NW, W, SW, S, SE, E
colneighbor = c(1,    0,    -1,    -1,    -1,    0,    1,    1)
rowneighbor = c(-1,    -1,    -1,    0,    1,    1,    1,    0)
downstrRC = 
	sapply(seq_along(strRows), FUN=function(x){strRows[x]+rowneighbor[drain[strCOND][x] ] }) * maxCol + 
	sapply(seq_along(strCols), FUN=function(x){strCols[x]+colneighbor[drain[strCOND][x] ] })

    
    currentstrSUB =  sapply(maskRC[strCOND], function(x){
    		hh = maskRC_string2Index[[ toString(x) ]]
    		return <- ifelse(is.null(hh),NA,hh);
    })

   	downstrSUB =  sapply(downstrRC, function(x){
    		hh = maskRC_string2Index[[ toString(x) ]]
    		return <- ifelse(is.null(hh),NA,hh);
    })
	downstrSUB[is.na(downstrSUB)] = -1

	cond_ = currentstrSUB != downstrSUB
	selectedSUBs = currentstrSUB[cond_]
	selectedSUBs_index = match(selectedSUBs, as.numeric(names(index)))
	outletSUB = selectedSUBs[downstrSUB[cond_]<0]
	

# ... SUB to basin
rast$tmp = rep(NA,length(mask))
for( ii in seq_along(selectedSUBs) ){
	if(selectedSUBs[ii] == outletSUB){
		rast$tmp[mask][ index_partial[[selectedSUBs_index[ii]]] ] = 1
	}else{
		rast$tmp[mask][ index[[selectedSUBs_index[ii]]] ] = 1
	}
}#ii
basinCond = !is.na(rast$tmp[mask])
writeRAST(rast,"basin",zcol='tmp', overwrite=T)


rast$tmp = rep(NA,length(mask))
rast$tmp[mask][basinCond] = sub[basinCond]
writeRAST(rast,"sub",zcol='tmp', overwrite=T)

rast$tmp = rep(NA,length(mask))
rast$tmp[mask][basinCond] = hill[basinCond]
writeRAST(rast,"hill",zcol='tmp', overwrite=T)

rast$tmp = rep(NA,length(mask))
rast$tmp[mask][basinCond] = str[basinCond]
writeRAST(rast,"str",zcol='tmp', overwrite=T)

# rast$tmp = rep(NA,length(mask))
# rast$tmp[mask][basinCond] = maskRC[basinCond]
# writeRAST(rast,"patch",zcol='tmp', overwrite=T)




