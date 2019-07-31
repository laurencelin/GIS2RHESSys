arg=commandArgs(T)
#arg=c('/Users/laurencelin/Downloads/test/rhessys/lulcFrac10m.csv', '/Users/laurencelin/Downloads/test/rhessys/lulc_codeinformation.csv')

w = read.csv(arg[1])
output = data.frame(lulcCode = as.numeric( gsub('[a-z]','',colnames(w)[-1:-2]) ))
output$forestFrac = rep(0,dim(output)[1])
output$shrubFrac= rep(0,dim(output)[1])
output$lawnFrac= rep(0,dim(output)[1])
output$impFrac= rep(0,dim(output)[1])
output$roofFrac= rep(0,dim(output)[1])
output$drivewayFrac= rep(0,dim(output)[1])
output$pavedRoadFrac= rep(0,dim(output)[1])
write.csv(output,arg[2],row.names=F)

