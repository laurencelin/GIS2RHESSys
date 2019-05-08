## single basin
## sub-patch/grid setting
#----------------------------------------------------------------------------------------------
	options(scipen=999)	
	arg=commandArgs(T)
	
	DtoR = pi/180
	RtoD = 1/DtoR
	defaultWorldName = c('worldID')
	defaultBasinName = c('basinID','basinX','basinY','basinZ','basinDefID','basinLatitude','basinBase')
	defaultHillName = c('hillID','hillX','hillY','hillZ','hillDefID','hillgws','hillgwno3','hillBase')
	defaultZoneName = c('zoneID','zoneX','zoneY','zoneZ','zoneDefID','zoneArea','zoneSlope','zoneAspect','zoneIso','zoneEH','zoneWH','zoneBase','zoneBaseID')
	defaultPatchName = c('patchID','patchX','patchY','patchZ','patchsoilID','patchLandID','patchArea','patchSlope','patchLNA','patchKsat','patchMpar','patchRZstorage','patchUnsat','patchSat','patchSnowEZ','patchSnowZ','patchSnowT','patchSnowAge','patchSnowED','patchLittercfrac','patchLitterStorage','patchLitterc1','patchLittern1','patchLitterc2','patchLitterc3','patchLitterc4','patchsoilIDc1','patchsoilIDsminn','patchsoilIDNO3','patchsoilIDc2','patchsoilIDc3','patchsoilIDc4','patchBase')
	defaultStratumName = c('strateID','stratePlantID','stratecfrac','strateGap','strateRZ','strateSnowStored','strateRainStored','stratecpool','strateleafc','stratedleafc','strateleafcstore','strateleafctrans','stratelstemc','stratelstemcstore','stratelstemctrans','stratedstemc','stratedstemcstore','stratedstemctrans','stratelrootc','stratelrootcstore','stratelrootctrans','stratedrootc','stratedrootcstore','stratedrootctrans','stratefrootc','stratefrootcstore','stratefrootctrans','stratecwdc','strateEPVleafcalloc','stratenpool','strateleafn','stratedleafn','strateleafnstore','strateleafntrans','stratelstemn','stratelstemnstore','stratelstemntrans','stratedstemn','stratedstemnstore','stratedstemntrans','stratelrootn','stratelrootnstore','stratelrootntrans','stratedrootn','stratedrootnstore','stratedrootntrans','stratefrootn','stratefrootnstore','stratefrootntrans','stratecwdn','strateRetransn','epv_wstress_days','epv_max_fparabs','epv_min_vwc','strateBase')
	title = c(defaultWorldName, defaultBasinName, defaultHillName, defaultZoneName, defaultPatchName, defaultStratumName)
	
	# arg=c('/Volumes/LaCie/current_work/SEES/SLB', '101', 'clim/SLB.base',
		  # '/Volumes/LaCie/current_work/SEES/SLB/rhessys_SLB10m/GIS2RHESSys-master/vegCollection.csv',
		  # '/Volumes/LaCie/current_work/SEES/SLB/rhessys_SLB10m/GIS2RHESSys-master/soilCollection.csv',
		  # '/Volumes/LaCie/current_work/SEES/SLB/rhessys_SLB10m/GIS2RHESSys-master/lulcCollection.csv',
		  # '/Volumes/LaCie/current_work/SEES/SLB/rhessys_SLB10m/worldfile.csv',
		  # '/Volumes/LaCie/current_work/SEES/SLB/rhessys_SLB10m/worldfiles/worldfile.hdr',
		  # '/Volumes/LaCie/current_work/SEES/SLB/rhessys_SLB10m/defs'
		 # )
#----------------------------------------------------------------------------------------------
	projectFolder = arg[1]
	climateStationID = as.numeric(arg[2])
	climateStationNAME = arg[3]
		
	## user provides a customized vegetation.csv containing all vegetation vegParameters.
	vegParam = read.csv(arg[4],skip=4,header=T,stringsAsFactors=F) #<<------
	vegParamCOL = cbind(as.numeric(unique(vegParam[1,3:ncol(vegParam)])), 3:ncol(vegParam)); 
	colnames(vegParamCOL) = c('vegID','vegDefIndex')
	vegParam_len = ncol(vegParam)
	
	soilParam = read.csv(arg[5],skip=4,header=T,stringsAsFactors=F) #<<------
	soilParamCOL = cbind(as.numeric(unique(soilParam[1,3:ncol(soilParam)])), 3:ncol(soilParam)); 
	colnames(soilParamCOL) = c('soilID','soilDefIndex')
	soilParam_len = ncol(soilParam)
	
	lulcParam = read.csv(arg[6],skip=4,header=T,stringsAsFactors=F) #<<------
	lulcParamCOL = cbind(as.numeric(unique(lulcParam[1,3:ncol(lulcParam)])), 3:ncol(lulcParam)); 
	colnames(lulcParamCOL) = c('lulcID','lulcDefIndex')
	lulcParam_len = ncol(lulcParam)
	
#----------------------------------------------------------------------------------------------
	# read in rast
	library(sp)
	library(XML)
	library(rgrass7)
	library(rgdal)
	gis = gmeta()
	gridarea = round(gis$nsres * gis$ewres)
	
	# bounded by GIS mask
	basinMap = 'basin'
	hillslopeMap = 'hill'
	zoneMAP = 'zone'
	patchMAP = 'patch'
	# extract RHESSys structural IDs 
	rast0 = readRAST(c(basinMap, hillslopeMap, zoneMAP, patchMAP),NODATA=0)
		mask = !is.na(rast0@data[[1]])
		hill = rast0@data[[2]][mask]
		zone = rast0@data[[3]][mask]
		patch = rast0@data[[4]][mask]
	
	# extract patch def IDs
	soilidMAP = 'soil_texture'
	rast1 = readRAST(c(soilidMAP),NODATA=0)
		soil = rast1@data[[1]][mask]


	# extract patch positive numerical values [0 -> +X]
	xMAP = 'xmap' 	##<<---- use raster calculator
	yMAP = 'ymap'	##<<---- use raster calculator
	demMAP = 'dem' ##<<---- from USGS
	slopeMap = 'slope' 
	aspectMAP = 'aspect' ##<<----- 90=north, 360=east, 180=west 270=south
	twiMAP = 'wetness_index' ##<<---- r.topidx
	whorizonMAP = 'west_180' 
	ehorizonMAP = 'east_000' 
	isohyetMAP = 'isohyet' # <------ "precipitation" mm/m (x >= 1.0)
	rast2 = readRAST(c(xMAP, yMAP, demMAP, slopeMap, aspectMAP, twiMAP, whorizonMAP, ehorizonMAP, isohyetMAP), NODATA=-1)
		xx = rast2@data[[1]][mask]
		yy = rast2@data[[2]][mask]
		dem = rast2@data[[3]][mask]
		slope = rast2@data[[4]][mask] ## ... could be a problem at the edge of the DEM
		aspect = rast2@data[[5]][mask] ## ... could be a problem at the edge of the DEM 
		twi = rast2@data[[6]][mask] ## ... could be a problem at the edge of the DEM
		whorizon = rast2@data[[7]][mask]
		ehorizon = rast2@data[[8]][mask]
		isohyet = rast2@data[[9]][mask]
		
		
	lulcMAP = 'landuse' 
	vegMAP = 'vegid' 
	rast3 = readRAST(c(lulcMAP, vegMAP),NODATA=0)
	lulc = rast3@data[[1]][mask]	
	vegid = rast3@data[[2]][mask]	
	
	laiMAP = 'lai'
    	rast4 = readRAST(laiMAP)
	lai = rast4@data[[1]][mask]	

	coverMAP = 'coverFrac'
    	rast5 = readRAST(coverMAP)
	coverFrac = rast5@data[[1]][mask]	

	impMAP = 'impervious'
	rast6 = readRAST(impMAP)
	imp = rast6@data[[1]][mask]			
		
		
		
	## centroid 
	sputm <- SpatialPoints(cbind(mean(xx),mean(yy)), proj4string=CRS(gmeta()$proj4) )  
  	spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
	latitude = spgeo@coords[2]
	
	##-----<< order >>------#
	len = sum(mask)
	RHESSysOrder = order(hill,zone,patch) ### somehow we need to cut deplulated
	tapplyOrder = rep(NA,sum(mask)); tapplyOrder[RHESSysOrder] = match(patch[RHESSysOrder], unique(patch[RHESSysOrder]))
	
	##-----<< list out IDs >>------#
	idMatrix = unique(cbind(patch,zone,hill)[RHESSysOrder,])
	colnames(idMatrix) = c('patchList', 'zoneList', 'hillList')
	
	## hill 
	hillX = tapply(xx,INDEX=hill, FUN=mean)
	hillY = tapply(yy,INDEX=hill, FUN=mean)
	hillZ = tapply(dem,INDEX=hill, FUN=mean)
		hillID = as.numeric(names(hillX))
		numhill = length(hillID)
		hillIDrhessysOrder = match(idMatrix[,'hillList'], hillID) ## organize unique hill into the idMatrix order

	## zone
	zoneEast = tapply(sin(ehorizon*DtoR),INDEX=zone, FUN=mean)
	zoneWest = tapply(sin(whorizon*DtoR),INDEX=zone, FUN=mean)
	
	zoneSlope = tapply(slope,INDEX=zone, FUN=mean) # vector
		aspectcos = tapply(cos(aspect*DtoR)*sin(slope*DtoR),INDEX=zone, FUN=mean)
		aspectsin = tapply(sin(aspect*DtoR)*sin(slope*DtoR),INDEX=zone, FUN=mean)
	zoneAspect = atan(aspectsin/aspectcos)* RtoD ## always between -90 and 90
		zoneAspect[aspectcos<0] = zoneAspect[aspectcos<0] + 180 #yes
		zoneAspect[aspectsin<0 & aspectcos>0] = zoneAspect[aspectsin<0 & aspectcos>0] + 360
		
	zoneX = tapply(xx,INDEX=zone, FUN=mean)
	zoneY = tapply(yy,INDEX=zone, FUN=mean)
	zoneZ = tapply(dem,INDEX=zone, FUN=mean)
	zoneArea = tapply(rep(1,len),INDEX=zone, FUN=sum)*gridarea
	zoneISO = tapply(isohyet,INDEX=zone, FUN=mean)
		zoneID = as.numeric(names(zoneX))
		numzone = length(zoneID)
		zoneIDrhessysOrder = match(idMatrix[,'zoneList'], zoneID) ## organize unique zone into the idMatrix order
	
	# cbind(zoneAspect,aspect,slope)[80:100,]
	# plot(aspect, zoneAspect); abline(a=0,b=1,lty=2)
	
	
	## patch (patch >= grid)
	patchX = tapply(xx,INDEX=patch, FUN=mean)
	patchY = tapply(yy,INDEX=patch, FUN=mean)
	patchZ = tapply(dem,INDEX=patch, FUN=mean)
	patchCount = tapply(rep(1,len),INDEX=patch, FUN=sum)
	patchArea = patchCount* gridarea
	patchSOIL = tapply(seq_along(patch),INDEX=patch, FUN=function(ii){
		hold=table(soil[ii]); 
		return <- as.numeric(names(hold)[which.max(hold)])
	} )
	patchSLOPE = tapply(slope,INDEX=patch, FUN=mean)
	patchTWI = tapply(abs(twi),INDEX=patch, FUN=mean); patchTWI[is.na(patchTWI)]=0;
		patchID = as.numeric(names(patchX))
		numpatch = length(patchID)	
		patchIDrhessysOrder = match(idMatrix[,'patchList'], patchID) ## organize unique patch into the idMatrix order
	#patchSTR = tapply(str,INDEX=patch, FUN=sum, na.rm=T)

	# rep(patchIDrhessysOrder, times=patchVegnum) in below is expending the idMatrix order by Vegnum
	
	
	
	patchVegCover = coverFrac
	patchVegLAI = lai
	patchVegID = vegid
	patchLAND = lulc 	
	patchIMP = imp  #<<----	
	patchRZ = rep(1,length(imp))
	patchVegnum = rep(1,length(imp))
	lai_ = lai ## already ordered
		stratum = patchVegID
		vegDefID = as.numeric(vegParam[1,3:ncol(vegParam)])
		stratumIndex = match(stratum, vegDefID) # return index
		stratumMass=matrix(0,length(lai_),13)
		colnames(stratumMass) = c("leafc","lstemc","dstemc","lrootc","drootc","frootc","leafcalloc","leafn","lstemn","dstemn","lrootn","drootn","frootn")
		
		cond = lai_ >0 
		stratum.SLA = as.numeric(vegParam[48,3:ncol(vegParam)])[stratumIndex[cond]]
		stratum.LS = as.numeric(vegParam[12,3:ncol(vegParam)])[stratumIndex[cond]]
		stratum.LIVED = as.numeric(vegParam[10,3:ncol(vegParam)])[stratumIndex[cond]]
		stratum.DEAD = 1/stratum.LIVED -1; 
		stratum.DEAD[!is.finite(stratum.DEAD)]=0
		stratum.SC = as.numeric(vegParam[8,3:ncol(vegParam)])[stratumIndex[cond]]
		stratum.LR = as.numeric(vegParam[9,3:ncol(vegParam)])[stratumIndex[cond]]
		stratum.CNL = as.numeric(vegParam[33,3:ncol(vegParam)])[stratumIndex[cond]]
		stratum.CNR = as.numeric(vegParam[21,3:ncol(vegParam)])[stratumIndex[cond]]
		stratum.CNLW = as.numeric(vegParam[39,3:ncol(vegParam)])[stratumIndex[cond]]
		#stratum.CNDW = rep(333.33,sum(cond))
		stratum.CNDW = 1/(as.numeric(vegParam[17,3:ncol(vegParam)])[stratumIndex[cond]]/450 + as.numeric(vegParam[18,3:ncol(vegParam)])[stratumIndex[cond]]/600)
	
		stratumMass[cond,1] =  lai_[cond]/stratum.SLA #leafc
		stratumMass[cond,2] = stratumMass[cond,1]* stratum.LS * stratum.LIVED #live stemc
		stratumMass[cond,3] = stratumMass[cond,2]* stratum.DEAD # dead stemc
		stratumMass[cond,4] = (stratumMass[cond,2]+stratumMass[cond,3])* stratum.SC* stratum.LIVED # live crootc
		stratumMass[cond,5] = stratumMass[cond,4]*stratum.DEAD #dead crootc
		stratumMass[cond,6] = stratumMass[cond,1]* stratum.LR #frootc
		stratum.totmass = rowSums(stratumMass)
		stratumMass[cond,7] = stratumMass[cond,1]*0.05 ##strateEPVleafcalloc
		stratumMass[cond,8] = stratumMass[cond,1]/stratum.CNL
		stratumMass[cond,9] = stratumMass[cond,2]/stratum.CNLW
		stratumMass[cond,10] = stratumMass[cond,3]/stratum.CNDW
		stratumMass[cond,11] = stratumMass[cond,4]/stratum.CNLW
		stratumMass[cond,12] = stratumMass[cond,5]/stratum.CNDW
		stratumMass[cond,13] = stratumMass[cond,6]/stratum.CNR	
		

#----------------------------------------------------------------------------------------------		
	outWorldFilePath = arg[7]
	title = c(defaultWorldName, defaultBasinName, defaultHillName, defaultZoneName, defaultPatchName, defaultStratumName)
	write( title, outWorldFilePath, ncolumns=length(title), append=F, sep=',')
		
		
	WorldBasinColumn = rep(1, sum(patchVegnum)) %o% c(1,1, mean(xx), mean(yy), mean(dem), 1, latitude, 0) #<<---- debug
	
	hillColumn = cbind(
		hillID,hillX, hillY, hillZ,
		rep(1, numhill) %o% c(1,0,0,0)
		)[rep(hillIDrhessysOrder, times=patchVegnum),]## hill
		
	zoneColumn = cbind(
		zoneID, zoneX, zoneY, zoneZ, 
		rep(1, numzone), zoneArea, zoneSlope, zoneAspect,
		zoneISO, zoneEast, zoneWest,  
		rep(1, numzone) %o% c(1, climateStationID)
		)[rep(zoneIDrhessysOrder, times=patchVegnum),]## zone
	
	patchColumn1 = cbind(
		patchID, patchX, patchY, patchZ, patchSOIL)[rep(patchIDrhessysOrder, times=patchVegnum),]## patch
	
	patchColumn2 = cbind(
		patchArea, patchSLOPE, patchTWI)[rep(patchIDrhessysOrder, times=patchVegnum),]## patch
		
	patchColumn3 = cbind(rep(1, numpatch) %o% c(0.12, 0,0,0, 0.28, 0, -10, 0, -0.5,1,0, 0.0000001,0.0000001,0.0000002,0.0000003,0.0000004,0.0000001, 0,0, 0.0000002,0.0000003,4e-9,0))[rep(patchIDrhessysOrder, times=patchVegnum),]## patch
						
	stratumColumn = matrix(0, sum(patchVegnum),length(defaultStratumName))	
	stratumColumn[,1] = 1:dim(stratumColumn)[1] #strateID
	stratumColumn[,2] = patchVegID # stratePlantID
	stratumColumn[,3] = patchVegCover # stratecfrac -->affect precipitation intercept 
	stratumColumn[,4] = 0 # strateGap -->affect sunlight intercept 
	stratumColumn[,5] = patchRZ # strateRZ 1m by default
	stratumColumn[,c(9,13,16,19,22,25,29,31,35,38,41,44,47)] = stratumMass
	
	
	write.table(cbind(
		WorldBasinColumn,
		hillColumn,
		zoneColumn,
		patchColumn1,
		patchLAND,
		patchColumn2,
		1-patchIMP,
		patchColumn3,
		stratumColumn
	), outWorldFilePath, row.names=F,col.names=F, append=T, sep=',')
					
#----------------------------------------------------------------------------------------------	
	tmp = unlist(strsplit(arg[9],'/'))
	defsFolder = tmp[length(tmp)]
	rhessysFolder = tmp[length(tmp)-1]
	
	## write out selected veg definition files
	vegHEADER = NULL
	selectedVeg = vegParamCOL[match(unique(patchVegID), vegParamCOL[,'vegID']), 'vegDefIndex']
	for(ii in selectedVeg ){
		filename = paste(defsFolder,"/stratum_",gsub("\\.","_",colnames(vegParam)[ii]),".def",sep="")
		vegHEADER = c(vegHEADER, paste(filename,'stratum_default_filename'))
		filepth = paste(arg[1],'/',rhessysFolder,'/', filename,sep="")
		write.table(cbind(vegParam[, ii], vegParam[,2]), filepth,sep="\t",row.names=F,col.names=F, quote=F)
	}#i

	
	## write out selected soil definition files
	soilHEADER = NULL
	selectedsoil = soilParamCOL[match(unique(patchSOIL), soilParamCOL[,'soilID']), 'soilDefIndex']
	for(ii in selectedsoil ){
		filename = paste(defsFolder,"/soil_",gsub("\\.","_",colnames(soilParam)[ii]),".def",sep="")
		soilHEADER = c(soilHEADER, paste(filename,'patch_default_filename'))
		filepth = paste(arg[1],'/',rhessysFolder,'/', filename,sep="")
		write.table(cbind(soilParam[, ii], soilParam[,2]), filepth,sep="\t",row.names=F,col.names=F, quote=F)
	}#i
	
	
	## write out selected lulc definition files
	lulcHEADER = NULL
	selectedlulc = lulcParamCOL[match(unique(patchLAND), lulcParamCOL[,'lulcID']), 'lulcDefIndex']
	for(ii in selectedlulc ){
		filename = paste(defsFolder,"/landuse_",gsub("\\.","_",colnames(lulcParam)[ii]),".def",sep="")
		lulcHEADER = c(lulcHEADER, paste(filename,'landuse_default_filename'))
		filepth = paste(arg[1],'/',rhessysFolder,'/', filename,sep="")
		write.table(cbind(lulcParam[, ii], lulcParam[,2]), filepth,sep="\t",row.names=F,col.names=F, quote=F)
	}#i
	
	zoneDEF = NULL
	zoneDEF = c(zoneDEF, '1 zone_default_ID')
	zoneDEF = c(zoneDEF, '0.000029 atm_trans_lapse_rate')
	zoneDEF = c(zoneDEF, '0.0015 dewpoint_lapse_rate')
	zoneDEF = c(zoneDEF, '0.0064 lapse_rate')
	zoneDEF = c(zoneDEF, '10 max_effective_lai')  
	zoneDEF = c(zoneDEF, '1 max_snow_temp')  
	zoneDEF = c(zoneDEF, '-1 min_rain_temp')  
	zoneDEF = c(zoneDEF, '0.001 n_deposition')  
	zoneDEF = c(zoneDEF, '0.0254 pptmin')  
	zoneDEF = c(zoneDEF, '0.75 sea_level_clear_sky_trans')  
	zoneDEF = c(zoneDEF, '0.4 temcf')  
	zoneDEF = c(zoneDEF, '0.003 trans_coeff1')  
	zoneDEF = c(zoneDEF, '2.2 trans_coeff2')  
	zoneDEF = c(zoneDEF, '1 wind')  
	write(zoneDEF, paste(arg[9],"/zone_zone.def",sep=""))
	
	basinDEF = NULL
	basinDEF = c(basinDEF, '1 basin_default_ID')
	write(basinDEF, paste(arg[9],"/basin_basin.def",sep=""))
	
	hillDEF = NULL
	hillDEF = c(hillDEF, '1 hillslope_default_ID')
	hillDEF = c(hillDEF, '1.0 gw_loss_coeff')
	hillDEF = c(hillDEF, '1.0 sat_to_gw_coeff')
	write(hillDEF, paste(arg[9],"/hillslope_hillslope.def",sep=""))
	
	worldHEADER = NULL
	worldHEADER = c(worldHEADER, paste(1,'num_basin_files'))
	worldHEADER = c(worldHEADER, paste(projectFolder,'/',rhessysFolder,'/',defsFolder,'/basin_basin.def basin_default_filename',sep=''))	
	worldHEADER = c(worldHEADER, paste(1,'num_hillslope_files'))	
	worldHEADER = c(worldHEADER, paste(projectFolder,'/',rhessysFolder,'/',defsFolder,'/hillslope_hillslope.def hillslope_default_filename',sep=''))	
	worldHEADER = c(worldHEADER, paste(1,'num_zone_files'))	
	worldHEADER = c(worldHEADER, paste(projectFolder,'/',rhessysFolder,'/',defsFolder,'/zone_zone.def zone_default_filename',sep=''))	
	
	worldHEADER = c(worldHEADER, paste(length(soilHEADER),'num_patch_files'))	
	worldHEADER = c(worldHEADER, paste(projectFolder,'/',rhessysFolder,'/',soilHEADER,sep=''))
	worldHEADER = c(worldHEADER, paste(length(lulcHEADER),'num_landuse_files'))	
	worldHEADER = c(worldHEADER, paste(projectFolder,'/',rhessysFolder,'/',lulcHEADER,sep=''))
	worldHEADER = c(worldHEADER, paste(length(vegHEADER),'num_stratum_files'))	
	worldHEADER = c(worldHEADER, paste(projectFolder,'/',rhessysFolder,'/',vegHEADER,sep=''))
	
	worldHEADER = c(worldHEADER, paste(1,'num_base_stations'))
	worldHEADER = c(worldHEADER, paste(climateStationNAME,' base_station_filename',sep=''))
	
	write(worldHEADER,arg[8])
