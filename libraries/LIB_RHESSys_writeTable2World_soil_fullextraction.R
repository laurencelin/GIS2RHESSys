options(scipen=999)
options(stringsAsFactors = FALSE)
LIBnrow = function(x){
	return <- ifelse(is.null(dim(x)[1]), 1, dim(x)[1]) 
}#function

arg=commandArgs(T)
#header = 'NA' #arg[1]
#basefile = '/Users/laurencelin/Library/Mobile Documents/com~apple~CloudDocs/Workspace/current_projects/Coweeta/rhessys_ws18_nlcd_local/worldfiles_explicitsoil/worldfile_test.Y2010M12D31H24.csv' #arg[2]
#outputfile = '/Users/laurencelin/Library/Mobile Documents/com~apple~CloudDocs/Workspace/current_projects/Coweeta/rhessys_ws18_nlcd_local/worldfiles_explicitsoil/worldfile_test.Y2010M12D31H24' #arg[3]
header = arg[1]
basefile = arg[2]
outputfile = arg[3]
print( paste('header = ', header,sep='') )
print( paste('basefile = ', basefile,sep='') )
print( paste('outputfile = ', outputfile,sep='') )

dd = as.matrix(read.csv(basefile))
keytable=data.frame(key=unlist(strsplit(readLines(basefile, n=1),',')),index=seq_len(dim(dd)[2])); 
keytable$format = 1; keytable$format[sapply(keytable$key, function(x){grepl('ID',x)})]=0
	# .. special
	keytable$format[which(keytable$key=='epv.wstress_days')]=0
	keytable$format[which(keytable$key=='cs.age')]=0
	keytable$format[which(keytable$key=='canopy_strata_n_basestations')]=0
	keytable$format[which(keytable$key=='hillslope_n_basestations')]=0
	keytable$format[which(keytable$key=='patch_n_basestations')]=0	
	keytable$format[which(keytable$key=='zone_n_basestations')]=0	
	keytable$format[which(keytable$key=='basin_n_basestations')]=0
		
ID_start_index = sapply(c('basin_ID','hillslope_ID','zone_ID','patch_ID','canopy_strata_ID'),function(x){which(keytable$key==x)})
ID_end_index = c(ID_start_index[-1]-1, dim(dd)[2])
LEN = 1:dim(dd)[1]

	## this indexing system here is not efficient! maybe future updates
worldIndex = 1
basin_ID = unique(dd[,'basin_ID']); basinIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'basin_ID'],basin_ID),function(ii){return<-c(dd[ii,'world_ID'][1],dd[ii,'basin_ID'][1], ii[1]); })))
hillslope_ID = unique(dd[,'hillslope_ID']); hillIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'hillslope_ID'],hillslope_ID),function(ii){return<-c(dd[ii,'world_ID'][1],dd[ii,'basin_ID'][1],dd[ii,'hillslope_ID'][1], ii[1]); })))
zone_ID = unique(dd[,'zone_ID']); zoneIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'zone_ID'],zone_ID),function(ii){return<-c(dd[ii,'world_ID'][1],dd[ii,'basin_ID'][1],dd[ii,'hillslope_ID'][1], dd[ii,'zone_ID'][1], ii[1]); })))
patch_ID = unique(dd[,'patch_ID']); patchIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'patch_ID'],patch_ID),function(ii){return<-c(dd[ii,'world_ID'][1],dd[ii,'basin_ID'][1],dd[ii,'hillslope_ID'][1], dd[ii,'zone_ID'][1],dd[ii,'patch_ID'][1], ii[1]); })))
stratumID = unique(dd[,'canopy_strata_ID']); stratumIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'canopy_strata_ID'],stratumID),function(ii){return<-c(dd[ii,'world_ID'][1],dd[ii,'basin_ID'][1],dd[ii,'hillslope_ID'][1], dd[ii,'zone_ID'][1],dd[ii,'patch_ID'][1],dd[ii,'canopy_strata_ID'][1], ii[1]); })))


### for header
goingAppend=T
if(header=="" | header=="NA" | header=='0' | header=='na'){
	# do nothing
	goingAppend=F
}else{
	con=file(header); open(con)
	line = readLines(con,warn=F); close(con)
	write(line,file=outputfile,ncolumn=1,append=F)
}

# world

num_world = length(worldIndex);
for(worldi in 1:num_world){
	
	world_ID = dd[worldIndex[worldi],'world_ID']
	basinIndex_ = basinIndex[basinIndex[,1]==world_ID,]
	num_basin = LIBnrow(basinIndex_) ## special case: only one basin and one world
	if(num_basin ==1){basinIndex_ = t(as.matrix(basinIndex_)) }
	
	line=c(
		paste(world_ID,"world_id"),
		paste(num_basin,"NUM_of_")
	)#line
	write(line,file=outputfile,ncolumn=1,append=goingAppend); goingAppend=T
	
	for(basini in 1:num_basin){
		
		basin_ID = dd[basinIndex_[basini,3],'basin_ID']
		hillIndex_ = hillIndex[hillIndex[,1]==world_ID & hillIndex[,2]==basin_ID,]
		num_hill = LIBnrow(hillIndex_)
		if(num_hill ==1){hillIndex_ = t(as.matrix(hillIndex_)) }
		
		line = c(
			sapply(ID_start_index[1]:ID_end_index[1],function(ii){ xx=dd[basinIndex_[basini,3],ii]; ifelse(keytable$format[ii]>0, 
				paste(formatC(xx,format="e",digits=6), keytable$key[ii]),
				paste(formatC(xx,format="d"),keytable$key[ii])
				) }),#sapply
			paste(formatC(num_hill,format="d"),"NUM_of_"))#c
		write(line,file=outputfile,ncolumn=1,append=T)
		
		
		for(hilli in 1:num_hill){
			
			hillslope_ID = dd[hillIndex_[hilli,4],'hillslope_ID']
			zoneIndex_ = zoneIndex[zoneIndex[,1]==world_ID & zoneIndex[,2]==basin_ID & zoneIndex[,3]==hillslope_ID,]
			num_zone = LIBnrow(zoneIndex_)
			if(num_zone ==1){zoneIndex_ = t(as.matrix(zoneIndex_)) }
			
			line = c(
				sapply(ID_start_index[2]:ID_end_index[2],function(ii){ xx=dd[hillIndex_[hilli,4],ii]; ifelse(keytable$format[ii]>0, 
					paste(formatC(xx,format="e",digits=6), keytable$key[ii]),
					paste(formatC(xx,format="d"),keytable$key[ii])
					) }),#sapply	
				paste(formatC(num_zone,format="d"),"NUM_of_"))#c
			write(line,file=outputfile,ncolumn=1,append=T)
			
			
			for(zonei in 1:num_zone){	#this num_zone need to read from a quick table
				
				zone_ID = dd[zoneIndex_[zonei,5],'zone_ID']
				patchIndex_ = patchIndex[patchIndex[,1]==world_ID & patchIndex[,2]==basin_ID & patchIndex[,3]==hillslope_ID & patchIndex[,4]==zone_ID,]
				num_patch = LIBnrow(patchIndex_)
				if(num_patch==1){patchIndex_ = t(as.matrix(patchIndex_)) }
				
				line = c(
					sapply(ID_start_index[3]:ID_end_index[3],function(ii){ xx=dd[zoneIndex_[zonei,5],ii]; ifelse(keytable$format[ii]>0, 
						paste(formatC(xx,format="e",digits=6), keytable$key[ii]),
						paste(formatC(xx,format="d"),keytable$key[ii])
						) }),#sapply	
					paste(formatC(num_patch,format="d"),"NUM_of_"))#c
				write(line,file=outputfile,ncolumn=1,append=T)
				
				
				
				for(patchi in 1:num_patch){
					
					patch_ID = dd[patchIndex_[patchi,6],'patch_ID']
					stratumIndex_ = stratumIndex[stratumIndex[,1]==world_ID & stratumIndex[,2]==basin_ID & stratumIndex[,3]==hillslope_ID & stratumIndex[,4]==zone_ID & stratumIndex[,5]==patch_ID,]
					num_strate = LIBnrow(stratumIndex_)
					if(num_strate==1){stratumIndex_ = t(as.matrix(stratumIndex_)) }
				
					line = c(
						sapply(ID_start_index[4]:ID_end_index[4],function(ii){ xx=dd[patchIndex_[patchi,6],ii]; ifelse(keytable$format[ii]>0, 
							paste(formatC(xx,format="e",digits=6), keytable$key[ii]),
							paste(formatC(xx,format="d"),keytable$key[ii])
							) }),#sapply	
						paste(formatC(num_strate,format="d"),"NUM_of_"))#c
					write(line,file=outputfile,ncolumn=1,append=T)



					for(stratei in 1:num_strate){
						
						line = sapply(ID_start_index[5]:ID_end_index[5],function(ii){ xx=dd[stratumIndex_[stratei,7],ii]; ifelse(keytable$format[ii]>0, 
							paste(formatC(xx,format="e",digits=6), keytable$key[ii]),
							paste(formatC(xx,format="d"),keytable$key[ii])
							) })#sapply	
						write(line,file=outputfile,ncolumn=1,append=T)
					}#stratei
				}#patchi
			}#zonei
		}#hilli
	}#basini
}#worldi
