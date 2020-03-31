
options(scipen=999)	
arg=commandArgs(T)

library(rgrass7)
library(sp)
tryCatch({ use_sp() },error=function(cond){message(cond)},warning=function(cond){message(cond)},finally={message("Please update the rgrass7 package on R")})
rast = readRAST(arg[1:3])
xx = rast@data[[1]]
yy = rast@data[[2]]
dem = rast@data[[3]]

meta = gmeta()
xres = meta$ewres
yres = meta$nsres
xres_1 = 1.0/meta$ewres
yres_1 = 1.0/meta$nsres

# linear method (horizontal, vertical, 45-degree line) to edit DEM within channel flowline
fromPoints = list()
fromPoints[[1]] = c(345405.30, 4359785.16) # xx,yy UTM
fromPoints[[2]] = c(345406.30, 4359795.59) # xx,yy UTM

toPoints = list()
toPoints[[1]] = c(345440.75, 4359788.80) # xx,yy UTM
toPoints[[2]] = c(345445.17, 4359794.65) # xx,yy UTM

numOfLINE = length(fromPoints)

fromINDEX = sapply( seq_len(numOfLINE), function(i){ which.min(abs(xx-fromPoints[[i]][1])+abs(yy-fromPoints[[i]][2])) })
toINDEX = sapply( seq_len(numOfLINE), function(i){ which.min(abs(xx-toPoints[[i]][1])+abs(yy-toPoints[[i]][2]))})

## ... filling between two endpoints
lineINDEX = lapply( seq_len(numOfLINE), function(i){
	xnum = floor( (xx[toINDEX[i]] - xx[fromINDEX[i]])*xres_1 );
	ynum = floor( (yy[toINDEX[i]] - yy[fromINDEX[i]])*yres_1 );
	
	if( xnum==0 ){
		linepty = yy[fromINDEX[i]] + (0:ynum)*yres;
		return <- sapply( seq_along(linepty), function(jj){which.min(abs(xx-xx[fromINDEX[i]])+abs(yy-linepty[jj])) })
	}else if( ynum==0 ){
		lineptx = xx[fromINDEX[i]] + (0:xnum)*xres;
		return <- sapply( seq_along(lineptx), function(jj){which.min(abs(xx-lineptx[jj])+abs(yy-yy[fromINDEX[i]])) })
	}else if( xnum==ynum ){
		linepty = yy[fromINDEX[i]] + (0:ynum)*yres;
		lineptx = xx[fromINDEX[i]] + (0:xnum)*xres;
		return <- sapply( seq_along(lineptx), function(jj){which.min(abs(xx-lineptx[jj])+abs(yy-linepty[jj])) })
	}else{
		return <- NA	
	}
})#lapply


## ... calculate elevation slope and value
lineFIX = lapply( seq_len(numOfLINE), function(i){
	lineElevation = dem[lineINDEX[[i]]]
	lineSlope = (lineElevation[length(lineElevation)] - lineElevation[1])/(length(lineElevation)-1)
	return <- lineElevation[1] + lineSlope*(seq_along(lineElevation)-1)
})#lapply

rast$dem = dem
for(i in seq_len(numOfLINE)){
	rast$dem[lineINDEX[[i]]] = lineFIX[[i]]
}#i
#cond = rast$dem != dem; sum(cond)


## ... write back to GRASS
writeRAST(rast,'dem',zcol='dem', overwrite=T)
	
	
	
	