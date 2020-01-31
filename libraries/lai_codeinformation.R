arg=commandArgs(T)

w = read.csv(arg[1])
output = data.frame(codeID = as.numeric( gsub('[a-z]','',colnames(w)[-1:-2]) ))
output$vegID1 = rep(0,dim(output)[1])
write("name",arg[2],ncolumns=1,append=F)
write.table(output,arg[2],row.names=F, append=T,sep=',')

