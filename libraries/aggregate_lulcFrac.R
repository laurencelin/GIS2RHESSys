arg=commandArgs(T)

library(rgrass7)
library(sp)
tryCatch({ use_sp() },error=function(cond){message(cond)},warning=function(cond){message(cond)},finally={message("Please update the rgrass7 package on R")})
# need to set comuptation region. it does change how much memory R uses to hold raster vectors
rast = readRAST(c(arg[1],arg[2]))
mask = !is.na(rast@data[[1]])
lulc = rast@data[[2]][mask]
lulcID = unique(lulc); lulcID=lulcID[order(lulcID)]

buffer = tapply( lulc, INDEX=rast@data[[1]][mask], function(x){
	paste(c(
		length(x),
		sapply(lulcID,function(j){sum(x==j)})
	),collapse=',')	
})#tapply


write(paste(c('patchID','total',paste('lulc',lulcID,sep='')),collapse=','),arg[3],append=F)
write.table(cbind(names(buffer),buffer),arg[3],row.names=F,col.names=F,sep=',', quote=F,append=T)










