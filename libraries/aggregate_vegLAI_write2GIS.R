arg=commandArgs(T)


library(rgrass7)
library(sp)
tryCatch({ use_sp() },error=function(cond){message(cond)},warning=function(cond){message(cond)},finally={message("Please update the rgrass7 package on R")})
rast = readRAST(arg[1])
mask = !is.na(rast@data[[1]])

# ... read the GIS extracted file (row=patchID, column=code)
patchCodeFrac = read.csv(arg[2]);
patchCodeFracCODE = colnames(patchCodeFrac)[c(-1,-2)];
print('read patchCodeFrac')

# ... read the code-veg table (row=unique code ID, column=veg name & ID & Ffrac)
lulcCodeFrac_title = unlist(strsplit(readLines(arg[3],n=1),split=','))[-1];
lulcCodeFrac_vegID = as.numeric(unlist(strsplit(readLines(arg[3],n=2)[2],split=','))[-1]);
lulcCodeFrac = read.csv(arg[3],skip=2,header=F); print('read lulcCodeFrac')
lulcCodeFracCODE = paste('lulc',lulcCodeFrac[,1],sep='');

# .... combining information
toPatchCond = match(patchCodeFracCODE,lulcCodeFracCODE)
if( sum(is.na(toPatchCond)) ){
    print('ERROR: the LULC composition table does not match with LULC GIS data.')
}else{
    
    lulcCodeFrac = lulcCodeFrac[toPatchCond,]# reorganized lulcCodeFrac
    gisOrder = match(rast@data[[1]][mask], patchCodeFrac$patchID)
    rast$tmp = rep(NA,length(rast@data[[1]]))
    for(i in seq_along(lulcCodeFrac_title)){
    	
        rast$tmp[mask] = (apply(sapply(which(lulcCodeFrac[,i+1]>0),function(j){patchCodeFrac[,j+2]*lulcCodeFrac[j,i+1]}),1,sum) / apply(sapply(which(lulcCodeFrac[,i+1]>0),function(j){patchCodeFrac[,j+2]}),1,sum))[gisOrder]
        
        	# .. sum(Ffrac * codeFrac_in_patch)
        writeRAST(rast,paste(lulcCodeFrac_title[i], '_lai',sep=''),zcol='tmp',overwrite=T)
        
    }#i
    
}# if wrong LULC code table







