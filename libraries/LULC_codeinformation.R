arg=commandArgs(T)

w = read.csv(arg[1])
output = data.frame(lulcCode = as.numeric( gsub('[a-z]','',colnames(w)[-1:-2]) ))
output$lulcComposition_forest = rep(0,dim(output)[1])
output$lulcComposition_shrub = rep(0,dim(output)[1])
output$lulcComposition_crop = rep(0,dim(output)[1])
output$lulcComposition_lawn = rep(0,dim(output)[1])
output$lulcComposition_imp = rep(0,dim(output)[1])
output$impBreakdownFrac_roof = rep(0,dim(output)[1])
output$impBreakdownFrac_driveway = rep(0,dim(output)[1])
output$impBreakdownFrac_pavedRoad = rep(0,dim(output)[1])
write.csv(output,arg[2],row.names=F)

