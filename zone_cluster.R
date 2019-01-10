options(scipen=999)	
arg=commandArgs(T)

library(rgrass7)
library(rgdal)


rast = readRAST(arg[1:4])
mask = !is.na(rast@data[[1]])
dem = rast@data[[1]][mask]
slope = rast@data[[2]][mask]
aspect = rast@data[[3]][mask]
hill = rast@data[[4]][mask]
DtoR = pi/180

clusterData = cbind(
	scale(dem),
	scale(slope),
	scale(cos(aspect*DtoR)*sin(slope*DtoR)),
	scale(sin(aspect*DtoR)*sin(slope*DtoR))
); colnames(clusterData)=c('dem','slope','aspectx','aspecty')


# Determine number of clusters
# wss <- (dim(clusterData)[1]-1)*sum(apply(clusterData,2,var))
# for (i in 2:100) wss[i] <- sum(kmeans(clusterData, centers=i,iter.max=1000)$withinss)
# plot(1:100, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(clusterData, 80,iter.max=1000) # 80 cluster solution

# get cluster means 
resultTable = aggregate(clusterData,by=list(fit$cluster),FUN=mean)
#write.csv(resultTable,'~/Desktop/zone_cluster_class.csv', row.names=F)

# append cluster assignment
result = cbind(dem,slope,aspect,hill, fit$cluster, hill*100+ fit$cluster)
colnames(result)= c('dem','slope','aspect','hill','cluster','zoneID')
#write.csv(result,arg[5], row.names=F)


zoneGIS = rep(NA,length(mask))
zoneGIS[mask] = result[,'zoneID']
rast$zone = as.integer(zoneGIS);
writeRAST(rast, "zone", zcol="zone", overwrite=T)

# # result = read.csv('~/Desktop/zone_map.csv')
# dem_zone = simplify2array(tapply(result[,'dem'], INDEX=result[,'zoneID'], FUN=function(x){return<-c(min(x),mean(x),max(x))}))
# slope_zone = simplify2array(tapply(result[,'slope'], INDEX=result[,'zoneID'], FUN=function(x){return<-c(min(x),mean(x),max(x))}))







