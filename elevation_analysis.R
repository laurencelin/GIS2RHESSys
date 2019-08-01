options(scipen=999)	
arg=commandArgs(T)

library(rgrass7)
library(rgdal)
# arg=c('dem','colmap', 'rowmap', 'drain','hill', 'strExt')

rast = readRAST(arg,NODATA=0)
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
	### ---------------- calculate difference in elevation from current cell to its downslope cell
	rast$output = rep(NA,length(mask))
	rast$output[mask] = unlist(lapply(seq_along(dem), function(ii){
		downIndex = hashenv[[ toString((rows[ii] + rowneighbor[drain[ii]])*maxCol + (cols[ii] + colneighbor[drain[ii]])) ]]
		if(length(downIndex)>0){
			return <- dem[ii] - dem[downIndex]
		}else{
			return <- NA
		}	
	}))#sapply
	rast$output[!mask] = NA
	writeRAST(rast, paste('dsd',toupper(arg[1]),sep=''), zcol='output', overwrite=T)
	
	##############################################################################################
	### ----------------- calculate averaged difference in elevation from current cell to its upslope cell
	rast$usdDEM = rep(0,length(mask))
	rast$usdDEMCount = rep(0,length(mask))
	for(ii in seq_along(dem)){
		downIndex = hashenv[[ toString((rows[ii] + rowneighbor[drain[ii]])*maxCol + (cols[ii] + colneighbor[drain[ii]])) ]]
		demDIFF = ifelse(length(downIndex)>0, dem[ii] - dem[downIndex], NA)
		#averagedValue = mean(c(rast$usdDEM[mask][downIndex], demDIFF),na.rm=T)
		#rast$usdDEM[mask][downIndex] = ifelse(is.na(averagedValue),NA, averagedValue)	
		rast$usdDEM[mask][downIndex] = rast$usdDEM[mask][downIndex] + ifelse(is.na(demDIFF), 0, demDIFF)
		rast$usdDEMCount[mask][downIndex] = rast$usdDEMCount[mask][downIndex] + ifelse(is.na(demDIFF), 0, 1)
	}#
	COND = rast$usdDEMCount>0
	rast$usdDEM[COND] = rast$usdDEM[COND]/rast$usdDEMCount[COND]
	rast$usdDEM[!mask] = NA
	writeRAST(rast, paste('usd',toupper(arg[1]),sep=''), zcol='usdDEM', overwrite=T)
	
	##############################################################################################
	### -------------- ?
	rast$handsDEM = rep(0,length(mask)); #	rast$processed = rep(0,length(mask))
	for(ii in seq_along(dem)){
		traceIndex = list()
		traceIndex_ii = 1;
		
		cii = ii; traceIndex[[traceIndex_ii]]=ii; traceIndex_ii=traceIndex_ii+1
		downIndex = hashenv[[ toString((rows[cii] + rowneighbor[drain[cii]])*maxCol + (cols[cii] + colneighbor[drain[cii]])) ]]
		if( is.null(downIndex) ){
			rast$handsDEM[mask][ii] = 0
		}else{
			while( is.na(str[downIndex]) ){
				cii = downIndex; traceIndex[[traceIndex_ii]]=downIndex; traceIndex_ii=traceIndex_ii+1
				downIndex = hashenv[[ toString((rows[cii] + rowneighbor[drain[cii]])*maxCol + (cols[cii] + colneighbor[drain[cii]])) ]]
			}#while
			rast$handsDEM[mask][unlist(traceIndex)] = dem[unlist(traceIndex)] - dem[downIndex]
		}#
	}# for ii
	rast$handsDEM[!mask]=NA
	writeRAST(rast, paste('hands',toupper(arg[1]),sep=''), zcol='handsDEM', overwrite=T)
	
	##############################################################################################
	# MC Cluster Analysis bounded with hillslopes
	# library(mclust)
	
	# mostChangeDEMindex = unlist(tapply(seq_along(hill), hill, function(ii){
		# fit <- Mclust(rast$usdDEM[mask][ii]);
		# resultTable = aggregate(rast$usdDEM[mask][ii],by=list(fit$classification),FUN=mean);
		# selectClass = resultTable[resultTable[,2]>mean(resultTable[,2]),1];
		# return <- ii[fit$classification %in% selectClass]
	# }))#tapply
	
	# riparian = rep(NA,length(mask))
	# riparian[mask][mostChangeDEMindex] = 1
	# rast$riparian = as.integer(riparian);
	# writeRAST(rast, paste('usd',toupper(arg[1]),'riparian',sep=''), zcol="riparian", overwrite=T)
	
	
	############################################################################################## (previous)
	# # K-Means Cluster Analysis
	# fit <- Mclust(data.frame(usd=rast$usdDEM[mask],sub=sub)) #plot(fit) #summary(fit)
	# fit$cluster = fit$classification
	
	# # K-Means Cluster Analysis
	# # fit <- kmeans(rast$usdDEM[mask], 20,iter.max=1000) # 80 cluster solution
	
	# # get cluster means 
	# resultTable = aggregate(rast$usdDEM[mask],by=list(fit$cluster),FUN=mean)
	# selectClass = resultTable[resultTable[,2]>mean(resultTable[,2]),1]
	
	# zoneGIS = rep(NA,length(mask))
	# zoneGIS[mask] = fit$cluster
	# rast$output = as.integer(zoneGIS);
	# writeRAST(rast, paste('usd',toupper(arg[1]),'analysis',sep=''), zcol="output", overwrite=T)
	
	# riparian = rep(NA,length(mask))
	# riparian[mask][fit$cluster %in% selectClass] = 1
	# rast$riparian = as.integer(riparian);
	# writeRAST(rast, paste('usd',toupper(arg[1]),'riparian',sep=''), zcol="riparian", overwrite=T)








