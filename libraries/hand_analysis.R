options(scipen=999)	
arg=commandArgs(T)

library(rgrass7)
library(rgdal)
library(sp)
tryCatch({ use_sp() },error=function(cond){message(cond)},warning=function(cond){message(cond)},finally={message("Please update the rgrass7 package on R")})
# arg=c('dem','colmap', 'rowmap', 'drain','hill', 'strExt')

rast = readRAST(arg[1:6],NODATA=0)
mask = !is.na(rast@data[[1]])
dem = rast@data[[1]][mask]
cols = rast@data[[2]][mask]
rows = rast@data[[3]][mask]
drain = rast@data[[4]][mask]
hill = rast@data[[5]][mask]
str = rast@data[[6]][mask]

DtoR = pi/180
colneighbor = c(1,	0,	-1,	-1,	-1,	0,	1,	1)	
rowneighbor = c(-1,	-1,	-1,	0,	1,	1,	1,	0)

maxCol = max(cols,na.rm=T) 
maskRC = rows*maxCol+cols #paste(rows, cols,sep=':') ## row*[max col]+col (yes: unique ID)
hashenv <- new.env(hash=T)
list2env(setNames(as.list(seq_along(dem)),maskRC),envir=hashenv) #<<---- native R hash

	
	##############################################################################################
	### ----------
	print('processing hands ...')
	rast$handsDEM = rep(NA,length(mask)); #	rast$processed = rep(0,length(mask))
	maxlen = sum(mask)
	for(ii in seq_along(dem)){
		traceIndex = list()
		traceIndex_ii = 1;
		
		cii = ii; traceIndex[[traceIndex_ii]]=ii; traceIndex_ii=traceIndex_ii+1
		downIndex = hashenv[[ toString((rows[cii] + rowneighbor[drain[cii]])*maxCol + (cols[cii] + colneighbor[drain[cii]])) ]]
		if( is.null(downIndex) | length(downIndex)==0 ){
			rast$handsDEM[mask][ii] = 0
		}else{
			while( traceIndex_ii<=maxlen &(is.na(str[downIndex]) | is.na(rast$handsDEM[mask][downIndex])) ){
				cii = downIndex; traceIndex[[traceIndex_ii]]=downIndex; traceIndex_ii=traceIndex_ii+1
				downIndex = hashenv[[ toString((rows[cii] + rowneighbor[drain[cii]])*maxCol + (cols[cii] + colneighbor[drain[cii]])) ]]
				if(is.null(downIndex) | length(downIndex)==0 ){ downIndex=traceIndex[[traceIndex_ii-1]]; break; } 
			}#while
			if( traceIndex_ii>=maxlen ) print(paste('warning ',ii,sep=''))
			rast$handsDEM[mask][unlist(traceIndex)] = dem[unlist(traceIndex)] - dem[downIndex] + 
				ifelse(is.na(str[downIndex]) & !is.na(rast$handsDEM[mask][downIndex]),rast$handsDEM[mask][downIndex],0)
		}#
	}# for ii
	#rast$handsDEM[!mask]=NA
	writeRAST(rast, arg[7], zcol='handsDEM', overwrite=T)
	
	