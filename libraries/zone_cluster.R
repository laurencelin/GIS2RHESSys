options(scipen=999)	
arg=commandArgs(T)
# arg=c('dem', 'slope', 'aspect', 'hill', 'zone_cluster')

library(rgrass7)
library(sp)
tryCatch({ use_sp() },error=function(cond){message(cond)},warning=function(cond){message(cond)},finally={message("Please update the rgrass7 package on R")})
rast = readRAST(arg[1:4])
mask = !is.na(rast@data[[1]])
dem = rast@data[[1]][mask]
slope = rast@data[[2]][mask]
aspect = rast@data[[3]][mask]
hill = rast@data[[4]][mask]
DtoR = pi/180


zoneclassindex = tapply(seq_along(mask)[mask], hill, function(ii){ return <- ii })
zoneclasshill = tapply(seq_along(hill), hill, function(ii){ return <- hill[ii][1] })
zoneclassLen = tapply(seq_along(hill), hill, function(ii){ return <- length(ii) })
zoneclass = tapply(seq_along(hill), hill, function(ii){
	# ii = seq_along(hill)[hill==65]
	# In mathematical speak that is 9.8°C per 1,000 meters; Google
	# form dataset for cluster analysis
	print(hill[ii][1])
	
	clusterData = cbind(
		(dem[ii]-mean(dem[ii]))*0.02, # 50 m elevation band
		scale(slope[ii]),
		scale(cos(aspect[ii]*DtoR)*sin(slope[ii]*DtoR)),
		scale(sin(aspect[ii]*DtoR)*sin(slope[ii]*DtoR))
	); colnames(clusterData)=c('dem','slope','aspectx','aspecty')
	
	# numer of cluster
	# elevationAspect = ceiling((max(dem[ii])-min(dem[ii]))*0.1)*8 # 10m by 8 direction
	# maxNumCluster = min( max(floor(length(ii)*0.1), elevationAspect), elevationAspect) 
	# if( length(ii) < maxNumCluster ) maxNumCluster=length(ii)-1
	
	# elevation band first
	numCluster = max(1,ceiling((max(dem[ii])-min(dem[ii]))*0.02))
	fit <- kmeans(clusterData[,'dem'], numCluster,iter.max=1000,algorithm="MacQueen") #
	elevationBand = fit$cluster
	
	
	# slope-aspect second
	maxNumCluster = 100; if( length(ii) < maxNumCluster ) maxNumCluster=length(ii)-2
	if(maxNumCluster > 3){
		wss <- (dim(clusterData)[1]-2)*sum(apply(clusterData[,c('slope','aspectx','aspecty')],2,var))
		for (i in 2: maxNumCluster) wss[i] <- sum(kmeans(clusterData[,c('slope','aspectx','aspecty')], centers=i,iter.max=1000, algorithm="MacQueen")$withinss)
		AccumImprovement = cumsum(diff(wss))
		#plot( AccumImprovement, type='b')
		numCluster = which( AccumImprovement/min(AccumImprovement) > 0.8)[1]; numCluster
		fit <- kmeans(clusterData[,c('slope','aspectx','aspecty')], numCluster, iter.max=1000,algorithm="MacQueen")
		slope_aspect_cluster = fit$cluster	
	}else{
		slope_aspect_cluster = rep(1,dim(clusterData)[1])
	}#if
	
	
	# combine	
	comb = paste(elevationBand, slope_aspect_cluster,sep='-')	
	comb_class = match(comb,unique(comb))
		
	return <- comb_class
})#tapply

max_num_zone = max(sapply(seq_along(zoneclassindex),function(i){ max(zoneclass[[i]]) }))
hill_multipler = 10^ceiling(log(max_num_zone,base=10))

zoneGIS = rep(NA,length(mask))
for(ii in seq_along(zoneclassindex)){
	zoneGIS[ zoneclassindex[[ii]] ] = zoneclasshill[[ii]]* hill_multipler + zoneclass[[ ii ]]
}#ii
	

rast$zone = as.integer(zoneGIS);
writeRAST(rast, arg[5], zcol="zone", overwrite=T)







