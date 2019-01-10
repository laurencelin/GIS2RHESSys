#source("~/Dropbox/LIB_Rscript/LIB_misc.R")
options(scipen=999)

arg=commandArgs(T)
#arg = c('na', '/Users/laurencelin/Downloads/SLB/rhessys_SLBsub10m/worldfiles/worldfile.csv', '/Users/laurencelin/Downloads/SLB/rhessys_SLBsub10m/worldfiles/worldfile')

LIBnrow=function(x){
	# asssume x is a matrix or dataframe or vector
	if(is.null(nrow(x))){return <- 1}
	else{return <- nrow(x)}
}


header = arg[1]
basefile = arg[2]
outputfile = arg[3]
print( paste('header = ', header,sep='') )
print( paste('basefile = ', basefile,sep='') )
print( paste('outputfile = ', outputfile,sep='') )

dd = as.matrix(read.csv(basefile))
keytable=cbind(colnames(dd),1:ncol(dd))
LEN = seq_len(dim(dd)[1])

	## this indexing system here is not efficient! maybe future updates
worldIndex = 1
basinID = unique(dd[,'basinID']); basinIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'basinID'],basinID),function(ii){return<-c(dd[ii,'worldID'][1],dd[ii,'basinID'][1], ii[1]); })))
hillID = unique(dd[,'hillID']); hillIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'hillID'],hillID),function(ii){return<-c(dd[ii,'worldID'][1],dd[ii,'basinID'][1],dd[ii,'hillID'][1], ii[1]); })))
zoneID = unique(dd[,'zoneID']); zoneIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'zoneID'],zoneID),function(ii){return<-c(dd[ii,'worldID'][1],dd[ii,'basinID'][1],dd[ii,'hillID'][1], dd[ii,'zoneID'][1], ii[1]); })))
patchID = unique(dd[,'patchID']); patchIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'patchID'],patchID),function(ii){return<-c(dd[ii,'worldID'][1],dd[ii,'basinID'][1],dd[ii,'hillID'][1], dd[ii,'zoneID'][1],dd[ii,'patchID'][1], ii[1]); })))
stratumID = unique(dd[,'strateID']); stratumIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'strateID'],stratumID),function(ii){return<-c(dd[ii,'worldID'][1],dd[ii,'basinID'][1],dd[ii,'hillID'][1], dd[ii,'zoneID'][1],dd[ii,'patchID'][1],dd[ii,'strateID'][1], ii[1]); })))


worldfile_buff <- file(outputfile,'w') # open a file connection


### for header
if( !(header=="" | header=="NA" | header=='0' | header=='na') ){
	con=file(header); open(con)
	line = readLines(con,warn=F); close(con)
	cat( line, file=worldfile_buff,sep='\n') 
}

# world

num_world = length(worldIndex);
for(worldi in 1:num_world){
	
	worldID = dd[worldIndex[worldi],'worldID']
	basinIndex_ = basinIndex[basinIndex[,1]==worldID,]
	num_basin = LIBnrow(basinIndex_) ## special case: only one basin and one world
	if(num_basin ==1){basinIndex_ = t(as.matrix(basinIndex_)) }
	line=c(
		paste(formatC(worldID,width=31,format="d",digits=0,flag="-"),"world_id",sep=""),
		paste(formatC(num_basin,width=31,format="d",digits=0,flag="-"),"num_basins",sep="")
	)
	cat( line, file=worldfile_buff,sep='\n')
	
	for(basini in 1:num_basin){
		
		basinID = dd[basinIndex_[basini,3],'basinID']
		hillIndex_ = hillIndex[hillIndex[,1]==worldID & hillIndex[,2]==basinID,]
		num_hill = LIBnrow(hillIndex_)
		if(num_hill ==1){hillIndex_ = t(as.matrix(hillIndex_)) }
		line=c(
			paste("",formatC(basinID,width=31,format="d",digits=0,flag="-"),"basin_ID",sep=""),
			paste("",formatC(dd[basinIndex_[basini,3],3],width=31,format="f",digits=16,flag="-"),"x",sep=""),
			paste("",formatC(dd[basinIndex_[basini,3],4],width=31,format="f",digits=16,flag="-"),"y",sep=""),
			paste("",formatC(dd[basinIndex_[basini,3],5],width=31,format="f",digits=16,flag="-"),"z",sep=""),
			paste("",formatC(dd[basinIndex_[basini,3],6],width=31,format="d",digits=0,flag="-"),"defaultID",sep=""),
			paste("",formatC(dd[basinIndex_[basini,3],7],width=31,format="f",digits=16,flag="-"),"latitude",sep=""),
			paste("",formatC(dd[basinIndex_[basini,3],8],width=31,format="d",digits=0,flag="-"),"n_basestations",sep=""),
			paste("",formatC(num_hill,width=31,format="d",digits=0,flag="-"),"num_hillslopes",sep="")
		)
		cat( line, file=worldfile_buff,sep='\n')
		
		for(hilli in 1:num_hill){
			
			hillID = dd[hillIndex_[hilli,4],'hillID']
			zoneIndex_ = zoneIndex[zoneIndex[,1]==worldID & zoneIndex[,2]==basinID & zoneIndex[,3]==hillID,]
			num_zone = LIBnrow(zoneIndex_)
			if(num_zone ==1){zoneIndex_ = t(as.matrix(zoneIndex_)) }
			line=c(
				paste("",formatC(hillID,width=31,format="d",digits=0,flag="-"),"hillslope_ID",sep=""),
				paste("",formatC(dd[hillIndex_[hilli,4],10],width=31,format="f",digits=16,flag="-"),"x",sep=""),
				paste("",formatC(dd[hillIndex_[hilli,4],11],width=31,format="f",digits=16,flag="-"),"y",sep=""),
				paste("",formatC(dd[hillIndex_[hilli,4],12],width=31,format="f",digits=16,flag="-"),"z",sep=""),
				paste("",formatC(dd[hillIndex_[hilli,4],13],width=31,format="d",digits=0,flag="-"),"defaultID",sep=""),
				paste("",formatC(dd[hillIndex_[hilli,4],14],width=31,format="f",digits=16,flag="-"),"gw_storage",sep=""),
				paste("",formatC(dd[hillIndex_[hilli,4],15],width=31,format="f",digits=16,flag="-"),"gw_NO3",sep=""),
				paste("",formatC(dd[hillIndex_[hilli,4],16],width=31,format="d",digits=0,flag="-"),"n_basestations",sep=""),
				paste("",formatC(num_zone,width=31,format="d",digits=0,flag="-"),"num_zones",sep="")
			)
			cat( line, file=worldfile_buff,sep='\n')
			
			
			for(zonei in 1:num_zone){	#this num_zone need to read from a quick table
				
				zoneID = dd[zoneIndex_[zonei,5],'zoneID']
				patchIndex_ = patchIndex[patchIndex[,1]==worldID & patchIndex[,2]==basinID & patchIndex[,3]==hillID & patchIndex[,4]==zoneID,]
				num_patch = LIBnrow(patchIndex_)
				if(num_patch==1){patchIndex_ = t(as.matrix(patchIndex_)) }
				line=c(
					paste("",formatC(zoneID,width=31,format="d",digits=0,flag="-"),"zone ID",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],18],width=31,format="f",digits=16,flag="-"),"x",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],19],width=31,format="f",digits=16,flag="-"),"y",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],20],width=31,format="f",digits=16,flag="-"),"z",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],21],width=31,format="d",digits=0,flag="-"),"defaultID",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],22],width=31,format="f",digits=16,flag="-"),"area",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],23],width=31,format="f",digits=16,flag="-"),"slope",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],24],width=31,format="f",digits=16,flag="-"),"aspect",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],25],width=31,format="f",digits=16,flag="-"),"isohyet",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],26],width=31,format="f",digits=16,flag="-"),"e_horizon",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],27],width=31,format="f",digits=16,flag="-"),"w_horizon",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],28],width=31,format="d",digits=0,flag="-"),"n_basestations",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],29],width=31,format="d",digits=0,flag="-"),"p_base_station_ID",sep=""),
					paste("",formatC(num_patch,width=31,format="d",digits=0,flag="-"),"num_patches",sep="")
				)
				cat( line, file=worldfile_buff,sep='\n')
				
				for(patchi in 1:num_patch){
					
					patchID = dd[patchIndex_[patchi,6],'patchID']
					stratumIndex_ = stratumIndex[stratumIndex[,1]==worldID & stratumIndex[,2]==basinID & stratumIndex[,3]==hillID & stratumIndex[,4]==zoneID & stratumIndex[,5]==patchID,]
					num_strate = LIBnrow(stratumIndex_)
					if(num_strate==1){stratumIndex_ = t(as.matrix(stratumIndex_)) }
				
					#. ... patch
					line=c(
						paste("",formatC(patchID,width=31,format="d",digits=0,flag="-"),"patch_ID",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],31],width=31,format="f",digits=16,flag="-"),"x",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],32],width=31,format="f",digits=16,flag="-"),"y",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],33],width=31,format="f",digits=16,flag="-"),"z",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],34],width=31,format="d",digits=0,flag="-"),"soil_default_ID",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],35],width=31,format="d",digits=0,flag="-"),"landuse_default_ID",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],36],width=31,format="f",digits=16,flag="-"),"area",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],37],width=31,format="f",digits=16,flag="-"),"slope",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],38],width=31,format="f",digits=16,flag="-"),"lna",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],39],width=31,format="f",digits=16,flag="-"),"Ksat_vertical",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],40],width=31,format="f",digits=16,flag="-"),"m_par",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],41],width=31,format="f",digits=16,flag="-"),"rz_storage",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],42],width=31,format="f",digits=16,flag="-"),"unsat_storage",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],43],width=31,format="f",digits=16,flag="-"),"sat_deficit",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],44],width=31,format="f",digits=16,flag="-"),"snowpack.water_equivalent_depth",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],45],width=31,format="f",digits=16,flag="-"),"snowpack_water_depth",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],46],width=31,format="f",digits=16,flag="-"),"snowpack_T",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],47],width=31,format="f",digits=16,flag="-"),"snowpack_surface_age",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],48],width=31,format="f",digits=16,flag="-"),"snowpack_energy_deficit",sep=""),
						
						paste("",formatC(dd[patchIndex_[patchi,6],49],width=31,format="f",digits=16,flag="-"),"litter.cover_fraction",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],50],width=31,format="f",digits=16,flag="-"),"litter.rain_stored",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],51],width=31,format="f",digits=16,flag="-"),"litter_cs.litr1c",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],52],width=31,format="f",digits=16,flag="-"),"litter_ns.litr1n",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],53],width=31,format="f",digits=16,flag="-"),"litter_cs.litr2c",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],54],width=31,format="f",digits=16,flag="-"),"litter_cs.litr3c",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],55],width=31,format="f",digits=16,flag="-"),"litter_cs.litr4c",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],56],width=31,format="f",digits=16,flag="-"),"soil_cs.soil1c",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],57],width=31,format="f",digits=16,flag="-"),"soil_ns.sminn",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],58],width=31,format="f",digits=16,flag="-"),"soil_ns.nitrate",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],59],width=31,format="f",digits=16,flag="-"),"soil_cs.soil2c",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],60],width=31,format="f",digits=16,flag="-"),"soil_cs.soil3c",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],61],width=31,format="f",digits=16,flag="-"),"soil_cs.soil4c",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],62],width=31,format="d",digits=0,flag="-"),"n_basestations",sep=""),
						paste("",formatC(num_strate,width=31,format="d",digits=0,flag="-"),"num_canopy_strata",sep="")
					)
					cat( line, file=worldfile_buff,sep='\n')

					for(stratei in 1:num_strate){
						
						strateID = dd[stratumIndex_[stratei,7],'strateID']
						line=c(
							paste("",formatC(strateID,width=31,format="d",digits=0,flag="-"),"canopy_strata_ID",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],64],width=31,format="d",digits=0,flag="-"),"default_ID",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],65],width=31,format="f",digits=16,flag="-"),"cover_fraction",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],66],width=31,format="f",digits=16,flag="-"),"gap_fraction",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],67],width=31,format="f",digits=16,flag="-"),"root_depth",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],68],width=31,format="f",digits=16,flag="-"),"snow_stored",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],69],width=31,format="f",digits=16,flag="-"),"rain_stored",sep=""),	
							paste("",formatC(dd[stratumIndex_[stratei,7],70],width=31,format="f",digits=16,flag="-"),"cs_cpool",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],71],width=31,format="f",digits=16,flag="-"),"cs_leafc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],72],width=31,format="f",digits=16,flag="-"),"cs_dead_leafc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],73],width=31,format="f",digits=16,flag="-"),"cs_leafc_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],74],width=31,format="f",digits=16,flag="-"),"cs_leafc_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],75],width=31,format="f",digits=16,flag="-"),"cs_live_stemc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],76],width=31,format="f",digits=16,flag="-"),"cs_livestemc_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],77],width=31,format="f",digits=16,flag="-"),"cs_livestemc_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],78],width=31,format="f",digits=16,flag="-"),"cs_dead_stemc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],79],width=31,format="f",digits=16,flag="-"),"cs_deadstemc_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],80],width=31,format="f",digits=16,flag="-"),"cs_deadstemc_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],81],width=31,format="f",digits=16,flag="-"),"cs_live_crootc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],82],width=31,format="f",digits=16,flag="-"),"cs_livecrootc_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],83],width=31,format="f",digits=16,flag="-"),"cs_livecrootc_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],84],width=31,format="f",digits=16,flag="-"),"cs_dead_crootc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],85],width=31,format="f",digits=16,flag="-"),"cs_deadcrootc_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],86],width=31,format="f",digits=16,flag="-"),"cs_deadcrootc_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],87],width=31,format="f",digits=16,flag="-"),"cs_frootc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],88],width=31,format="f",digits=16,flag="-"),"cs_frootc_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],89],width=31,format="f",digits=16,flag="-"),"cs_frootc_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],90],width=31,format="f",digits=16,flag="-"),"cs_cwdc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],91],width=31,format="f",digits=16,flag="-"),"epv.prev_leafcalloc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],92],width=31,format="f",digits=16,flag="-"),"ns_npool",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],93],width=31,format="f",digits=16,flag="-"),"ns_leafn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],94],width=31,format="f",digits=16,flag="-"),"ns_dead_leafn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],95],width=31,format="f",digits=16,flag="-"),"ns_leafn_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],96],width=31,format="f",digits=16,flag="-"),"ns_leafn_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],97],width=31,format="f",digits=16,flag="-"),"ns_live_stemn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],98],width=31,format="f",digits=16,flag="-"),"ns_livestemn_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],99],width=31,format="f",digits=16,flag="-"),"ns_livestemn_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],100],width=31,format="f",digits=16,flag="-"),"ns_dead_stemn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],101],width=31,format="f",digits=16,flag="-"),"ns_deadstemn_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],102],width=31,format="f",digits=16,flag="-"),"ns_deadstemn_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],103],width=31,format="f",digits=16,flag="-"),"ns_live_nrootn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],104],width=31,format="f",digits=16,flag="-"),"ns_livenrootn_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],105],width=31,format="f",digits=16,flag="-"),"ns_livenrootn_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],106],width=31,format="f",digits=16,flag="-"),"ns_dead_nrootn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],107],width=31,format="f",digits=16,flag="-"),"ns_deadnrootn_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],108],width=31,format="f",digits=16,flag="-"),"ns_deadnrootn_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],109],width=31,format="f",digits=16,flag="-"),"ns_frootn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],110],width=31,format="f",digits=16,flag="-"),"ns_frootn_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],111],width=31,format="f",digits=16,flag="-"),"ns_frootn_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],112],width=31,format="f",digits=16,flag="-"),"ns_nwdn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],113],width=31,format="f",digits=16,flag="-"),"ns_retransn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],114],width=31,format="f",digits=16,flag="-"),"epv_wstress_days",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],115],width=31,format="f",digits=16,flag="-"),"epv_max_fparabs",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],116],width=31,format="f",digits=16,flag="-"),"epv_min_vwc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],117],width=31,format="d",digits=0,flag="-"),"n_basestations",sep="")
						)
						cat( line, file=worldfile_buff,sep='\n')
					}#stratei
				}#patchi
			}#zonei
		}#hilli
	}#basini
}#worldi


close(worldfile_buff)

