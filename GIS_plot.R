options(scipen=999)	
arg=commandArgs(T)

library(rgrass7)
rast = readRAST(arg[1])
plot(rast@data[[1]])