arg=commandArgs(T)

w = read.csv(arg[1])
output = data.frame(lulcCode = as.numeric( gsub('[a-z]','',colnames(w)[-1:-2]) ))
output$vegID1 = rep(0,dim(output)[1])
write.csv(output,arg[2],row.names=F)

