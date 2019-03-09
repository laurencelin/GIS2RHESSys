options(scipen=999)	
arg=commandArgs(T)
	arg=c('dem', 'slope', 'aspect', 'hill')

library(rgrass7)
library(rgdal)
library(mclust)

rast = readRAST(arg[1:4])
mask = !is.na(rast@data[[1]])
dem = rast@data[[1]][mask]
slope = rast@data[[2]][mask]
aspect = rast@data[[3]][mask]
hill = rast@data[[4]][mask]
DtoR = pi/180

## temperature vs elevation: 6˚C per 1000 m


# clusterData = cbind(
	# scale(dem,scale=F),
	# scale(slope),
	# scale(cos(aspect*DtoR)*sin(slope*DtoR)),
	# scale(sin(aspect*DtoR)*sin(slope*DtoR))
# ); colnames(clusterData)=c('dem','slope','aspectx','aspecty')


clusterData = cbind(
	scale(slope),
	scale(cos(aspect*DtoR)*sin(slope*DtoR)),
	scale(sin(aspect*DtoR)*sin(slope*DtoR))
); colnames(clusterData)=c('slope','aspectx','aspecty')


# M Cluster Analysis
fit <- Mclust(clusterData) 
fit$cluster <- fit$classification
#fit <- kmeans(clusterData, 80,iter.max=1000) # 80 cluster solution

# elevation band, hill, and cluster grp
combIndex = paste(round(dem/10), hill*100, fit$cluster,sep='-')
combClass = match(combIndex, unique(combIndex))



	# get cluster means 
	#resultTable = aggregate(clusterData,by=list(fit$cluster),FUN=mean)
	#write.csv(resultTable,'~/Desktop/zone_cluster_class.csv', row.names=F)



	# append cluster assignment
	#result = cbind(dem,slope,aspect,hill, fit$cluster, hill*100+ fit$cluster)
	#colnames(result)= c('dem','slope','aspect','hill','cluster','zoneID')
	#write.csv(result,arg[5], row.names=F)


zoneGIS = rep(NA,length(mask))
zoneGIS[mask] = combClass #result[,'zoneID']
rast$zone = as.integer(zoneGIS);
writeRAST(rast, "zone", zcol="zone", overwrite=T)

# # result = read.csv('~/Desktop/zone_map.csv')
# dem_zone = simplify2array(tapply(result[,'dem'], INDEX=result[,'zoneID'], FUN=function(x){return<-c(min(x),mean(x),max(x))}))
# slope_zone = simplify2array(tapply(result[,'slope'], INDEX=result[,'zoneID'], FUN=function(x){return<-c(min(x),mean(x),max(x))}))







