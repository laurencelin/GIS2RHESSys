options(scipen=999)
LIBnrow = function(x){
	return <- ifelse(is.null(dim(x)[1]), 1, dim(x)[1]) 
}#function

arg=commandArgs(T)

# arg=c(
	# "NA",
	# "/Users/laurencelin/Dropbox/Myself/UNC/WS14_dynamicNdep/setup_rhessys_5_20_bolstad/rhessys/worldfiles/modify_bolstad_add.csv",
	# "/Users/laurencelin/Dropbox/Myself/UNC/WS14_dynamicNdep/setup_rhessys_5_20_bolstad/rhessys/worldfiles/world_vegBolstad_add2")

# arg=c(
	# 'na','~/Desktop/worldfile.csv','~/Desktop/worldfile'
# )

header = arg[1]
basefile = arg[2]
outputfile = arg[3]
print( paste('header = ', header,sep='') )
print( paste('basefile = ', basefile,sep='') )
print( paste('outputfile = ', outputfile,sep='') )

dd = as.matrix(read.csv(basefile))
keytable=cbind(colnames(dd),1:ncol(dd))
LEN = 1:dim(dd)[1]

	## this indexing system here is not efficient! maybe future updates
worldIndex = 1
basinID = unique(dd[,'basinID']); basinIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'basinID'],basinID),function(ii){return<-c(dd[ii,'worldID'][1],dd[ii,'basinID'][1], ii[1]); })))
hillID = unique(dd[,'hillID']); hillIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'hillID'],hillID),function(ii){return<-c(dd[ii,'worldID'][1],dd[ii,'basinID'][1],dd[ii,'hillID'][1], ii[1]); })))
zoneID = unique(dd[,'zoneID']); zoneIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'zoneID'],zoneID),function(ii){return<-c(dd[ii,'worldID'][1],dd[ii,'basinID'][1],dd[ii,'hillID'][1], dd[ii,'zoneID'][1], ii[1]); })))
patchID = unique(dd[,'patchID']); patchIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'patchID'],patchID),function(ii){return<-c(dd[ii,'worldID'][1],dd[ii,'basinID'][1],dd[ii,'hillID'][1], dd[ii,'zoneID'][1],dd[ii,'patchID'][1], ii[1]); })))
stratumID = unique(dd[,'strateID']); stratumIndex = t(simplify2array(tapply(LEN, INDEX=match(dd[,'strateID'],stratumID),function(ii){return<-c(dd[ii,'worldID'][1],dd[ii,'basinID'][1],dd[ii,'hillID'][1], dd[ii,'zoneID'][1],dd[ii,'patchID'][1],dd[ii,'strateID'][1], ii[1]); })))




goingAppend=T

### for header
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
	
	worldID = dd[worldIndex[worldi],'worldID']
	basinIndex_ = basinIndex[basinIndex[,1]==worldID,]
	num_basin = LIBnrow(basinIndex_) ## special case: only one basin and one world
	if(num_basin ==1){basinIndex_ = t(as.matrix(basinIndex_)) }
	line=c(
		paste(formatC(worldID,width=31,format="d",digits=0,flag="-"),"world_id",sep=""),
		paste(formatC(num_basin,width=31,format="d",digits=0,flag="-"),"num_basins",sep="")
	)
	write(line,file=outputfile,ncolumn=1,append=goingAppend); goingAppend=T
	
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
			paste("",formatC(dd[basinIndex_[basini,3],6],width=31,format="d",digits=0,flag="-"),"basin_parm_ID",sep=""),
			paste("",formatC(dd[basinIndex_[basini,3],7],width=31,format="f",digits=16,flag="-"),"latitude",sep=""),
			paste("",formatC(dd[basinIndex_[basini,3],8],width=31,format="d",digits=0,flag="-"),"n_basestations",sep=""),
			paste("",formatC(num_hill,width=31,format="d",digits=0,flag="-"),"num_hillslopes",sep="")
		)
		write(line,file=outputfile,ncolumn=1,append=T)
		
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
				paste("",formatC(dd[hillIndex_[hilli,4],13],width=31,format="d",digits=0,flag="-"),"hill_parm_ID",sep=""),
				paste("",formatC(dd[hillIndex_[hilli,4],14],width=31,format="f",digits=16,flag="-"),"gw.storage",sep=""),
				paste("",formatC(dd[hillIndex_[hilli,4],15],width=31,format="f",digits=16,flag="-"),"gw.NO3",sep=""),
				paste("",formatC(dd[hillIndex_[hilli,4],16],width=31,format="d",digits=0,flag="-"),"n_basestations",sep=""),
				paste("",formatC(num_zone,width=31,format="d",digits=0,flag="-"),"num_zones",sep="")
			)
			write(line,file=outputfile,ncolumn=1,append=T)
			
			
			for(zonei in 1:num_zone){	#this num_zone need to read from a quick table
				
				zoneID = dd[zoneIndex_[zonei,5],'zoneID']
				patchIndex_ = patchIndex[patchIndex[,1]==worldID & patchIndex[,2]==basinID & patchIndex[,3]==hillID & patchIndex[,4]==zoneID,]
				num_patch = LIBnrow(patchIndex_)
				if(num_patch==1){patchIndex_ = t(as.matrix(patchIndex_)) }
				line=c(
					paste("",formatC(zoneID,width=31,format="d",digits=0,flag="-"),"zone_ID",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],18],width=31,format="f",digits=16,flag="-"),"x",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],19],width=31,format="f",digits=16,flag="-"),"y",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],20],width=31,format="f",digits=16,flag="-"),"z",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],21],width=31,format="d",digits=0,flag="-"),"zone_parm_ID",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],22],width=31,format="f",digits=16,flag="-"),"area",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],23],width=31,format="f",digits=16,flag="-"),"slope",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],24],width=31,format="f",digits=16,flag="-"),"aspect",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],25],width=31,format="f",digits=16,flag="-"),"precip_lapse_rate",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],26],width=31,format="f",digits=16,flag="-"),"e_horizon",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],27],width=31,format="f",digits=16,flag="-"),"w_horizon",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],28],width=31,format="d",digits=0,flag="-"),"n_basestations",sep=""),
					paste("",formatC(dd[zoneIndex_[zonei,5],29],width=31,format="d",digits=0,flag="-"),"base_station_ID",sep=""),
					paste("",formatC(num_patch,width=31,format="d",digits=0,flag="-"),"num_patches",sep="")
				)
				write(line,file=outputfile,ncolumn=1,append=T)
				
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
						paste("",formatC(dd[patchIndex_[patchi,6],34],width=31,format="d",digits=0,flag="-"),"soil_parm_ID",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],35],width=31,format="d",digits=0,flag="-"),"landuse_parm_ID",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],36],width=31,format="f",digits=16,flag="-"),"area",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],37],width=31,format="f",digits=16,flag="-"),"slope",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],38],width=31,format="f",digits=16,flag="-"),"lna",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],39],width=31,format="f",digits=16,flag="-"),"Ksat_vertical",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],40],width=31,format="f",digits=16,flag="-"),"mpar",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],41],width=31,format="f",digits=16,flag="-"),"rz_storage",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],42],width=31,format="f",digits=16,flag="-"),"unsat_storage",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],43],width=31,format="f",digits=16,flag="-"),"sat_deficit",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],44],width=31,format="f",digits=16,flag="-"),"snowpack.water_equivalent_depth",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],45],width=31,format="f",digits=16,flag="-"),"snowpack.water_depth",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],46],width=31,format="f",digits=16,flag="-"),"snowpack.T",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],47],width=31,format="f",digits=16,flag="-"),"snowpack.surface_age",sep=""),
						paste("",formatC(dd[patchIndex_[patchi,6],48],width=31,format="f",digits=16,flag="-"),"snowpack.energy_deficit",sep=""),
						
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
						paste("",formatC(num_strate,width=31,format="d",digits=0,flag="-"),"num_stratum",sep="")
					)
					write(line,file=outputfile,ncolumn=1,append=T)

					for(stratei in 1:num_strate){
						
						strateID = dd[stratumIndex_[stratei,7],'strateID']
						line=c(
							paste("",formatC(strateID,width=31,format="d",digits=0,flag="-"),"canopy_strata_ID",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],64],width=31,format="d",digits=0,flag="-"),"veg_parm_ID",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],65],width=31,format="f",digits=16,flag="-"),"cover_fraction",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],66],width=31,format="f",digits=16,flag="-"),"gap_fraction",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],67],width=31,format="f",digits=16,flag="-"),"rootzone.depth",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],68],width=31,format="f",digits=16,flag="-"),"snow_stored",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],69],width=31,format="f",digits=16,flag="-"),"rain_stored",sep=""),	
							paste("",formatC(dd[stratumIndex_[stratei,7],70],width=31,format="f",digits=16,flag="-"),"cs.cpool",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],71],width=31,format="f",digits=16,flag="-"),"cs.leafc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],72],width=31,format="f",digits=16,flag="-"),"cs.dead_leafc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],73],width=31,format="f",digits=16,flag="-"),"cs.leafc_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],74],width=31,format="f",digits=16,flag="-"),"cs.leafc_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],75],width=31,format="f",digits=16,flag="-"),"cs.live_stemc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],76],width=31,format="f",digits=16,flag="-"),"cs.livestemc_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],77],width=31,format="f",digits=16,flag="-"),"cs.livestemc_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],78],width=31,format="f",digits=16,flag="-"),"cs.dead_stemc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],79],width=31,format="f",digits=16,flag="-"),"cs.deadstemc_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],80],width=31,format="f",digits=16,flag="-"),"cs.deadstemc_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],81],width=31,format="f",digits=16,flag="-"),"cs.live_crootc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],82],width=31,format="f",digits=16,flag="-"),"cs.livecrootc_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],83],width=31,format="f",digits=16,flag="-"),"cs.livecrootc_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],84],width=31,format="f",digits=16,flag="-"),"cs.dead_crootc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],85],width=31,format="f",digits=16,flag="-"),"cs.deadcrootc_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],86],width=31,format="f",digits=16,flag="-"),"cs.deadcrootc_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],87],width=31,format="f",digits=16,flag="-"),"cs.frootc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],88],width=31,format="f",digits=16,flag="-"),"cs.frootc_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],89],width=31,format="f",digits=16,flag="-"),"cs.frootc_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],90],width=31,format="f",digits=16,flag="-"),"cs.cwdc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],91],width=31,format="f",digits=16,flag="-"),"epv.prev_leafcalloc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],92],width=31,format="f",digits=16,flag="-"),"ns.npool",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],93],width=31,format="f",digits=16,flag="-"),"ns.leafn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],94],width=31,format="f",digits=16,flag="-"),"ns.dead_leafn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],95],width=31,format="f",digits=16,flag="-"),"ns.leafn_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],96],width=31,format="f",digits=16,flag="-"),"ns.leafn_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],97],width=31,format="f",digits=16,flag="-"),"ns.live_stemn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],98],width=31,format="f",digits=16,flag="-"),"ns.livestemn_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],99],width=31,format="f",digits=16,flag="-"),"ns.livestemn_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],100],width=31,format="f",digits=16,flag="-"),"ns.dead_stemn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],101],width=31,format="f",digits=16,flag="-"),"ns.deadstemn_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],102],width=31,format="f",digits=16,flag="-"),"ns.deadstemn_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],103],width=31,format="f",digits=16,flag="-"),"ns.live_nrootn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],104],width=31,format="f",digits=16,flag="-"),"ns.livenrootn_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],105],width=31,format="f",digits=16,flag="-"),"ns.livenrootn_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],106],width=31,format="f",digits=16,flag="-"),"ns.dead_nrootn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],107],width=31,format="f",digits=16,flag="-"),"ns.deadnrootn_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],108],width=31,format="f",digits=16,flag="-"),"ns.deadnrootn_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],109],width=31,format="f",digits=16,flag="-"),"ns.frootn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],110],width=31,format="f",digits=16,flag="-"),"ns.frootn_store",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],111],width=31,format="f",digits=16,flag="-"),"ns.frootn_transfer",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],112],width=31,format="f",digits=16,flag="-"),"ns.nwdn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],113],width=31,format="f",digits=16,flag="-"),"ns.retransn",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],114],width=31,format="f",digits=16,flag="-"),"epv.wstress_days",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],115],width=31,format="f",digits=16,flag="-"),"epv.max_fparabs",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],116],width=31,format="f",digits=16,flag="-"),"epv.min_vwc",sep=""),
							paste("",formatC(dd[stratumIndex_[stratei,7],117],width=31,format="d",digits=0,flag="-"),"n_basestations",sep="")
						)
						write(line,file=outputfile,ncolumn=1,append=T)
					}#stratei
				}#patchi
			}#zonei
		}#hilli
	}#basini
}#worldi
#,error=function(err){print(paste(basini,hilli,zonei,stratei));})


# arg=c(
	# "",
	# "/Users/laurencelin/Desktop/WSC/morgancreek/rhessys/worldfiles/mix/world_redefthin_pineplantationDown.csv",
	# "test.csv"
# )
