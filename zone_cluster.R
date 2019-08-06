options(scipen=999)	
arg=commandArgs(T)
# arg=c('dem', 'slope', 'aspect', 'hill', 'zone_cluster')

library(rgrass7)

rast = readRAST(arg[1:4])
mask = !is.na(rast@data[[1]])
dem = rast@data[[1]][mask]
slope = rast@data[[2]][mask]
aspect = rast@data[[3]][mask]
hill = rast@data[[4]][mask]
DtoR = pi/180

## temperature vs elevation: 6˚C per 1000 m


	zoneclassindex = tapply(seq_along(mask)[mask], hill, function(ii){ return <- ii })
	zoneclasshill = tapply(seq_along(hill), hill, function(ii){ return <- hill[ii][1] })
		
	zoneclass = tapply(seq_along(hill), hill, function(ii){
		# ii = seq_along(hill)[hill==53]
		# In mathematical speak that is 9.8°C per 1,000 meters; Google
		# form dataset for cluster analysis
		clusterData = cbind(
			scale(dem[ii]),
			scale(slope[ii]),
			scale(cos(aspect[ii]*DtoR)*sin(slope[ii]*DtoR)),
			scale(sin(aspect[ii]*DtoR)*sin(slope[ii]*DtoR))
		); colnames(clusterData)=c('dem','slope','aspectx','aspecty')
		
		# numer of cluster
		elevationAspect = ceiling((max(dem[ii])-min(dem[ii]))*0.1)*8 # 10m by 8 direction
		maxNumCluster = min( max(floor(length(ii)*0.1), elevationAspect), elevationAspect) 
		if( length(ii) < maxNumCluster ) maxNumCluster=length(ii)-1
		
		
		wss <- (dim(clusterData)[1]-1)*sum(apply(clusterData,2,var))
		for (i in 2: maxNumCluster) wss[i] <- sum(kmeans(clusterData, centers=i,iter.max=1000,algorithm="MacQueen")$withinss) 
			AccumImprovement = cumsum(diff(wss))
			#plot( AccumImprovement, type='b')
			numCluster = which( AccumImprovement/min(AccumImprovement) > 0.8)[1]; numCluster
		
		#clustering
		fit <- kmeans(clusterData, numCluster,iter.max=1000,algorithm="MacQueen") 
			
		return <- fit$cluster
	})#tapply

	max_num_zone = max(sapply(seq_along(zoneclassindex),function(i){ max(zoneclass[[i]]) }))
	hill_multipler = 10^ceiling(log(max_num_zone,base=10))

	zoneGIS = rep(NA,length(mask))
	for(ii in seq_along(zoneclassindex)){
		zoneGIS[ zoneclassindex[[ii]] ] = zoneclasshill[[ii]]* hill_multipler + zoneclass[[ ii ]]
	}#ii
	

rast$zone = as.integer(zoneGIS);
writeRAST(rast, arg[5], zcol="zone", overwrite=T)







