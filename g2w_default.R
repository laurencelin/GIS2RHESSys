## single basin

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
	
#----------------------------------------------------------------------------------------------
	projectFolder = arg[1]
	outWorldFile = 'worldfile.csv'
	climateStationID = as.numeric(arg[2])

	param = read.csv(paste(projectFolder,'/','rhessys_veg_default.csv',sep=''),skip=4,header=T,stringsAsFactors=F)
	plantcol = cbind(as.numeric(unique(param[1,3:ncol(param)])), 3:ncol(param)); 
	colnames(plantcol) = c('vegID','vegDefIndex')
	param_len = ncol(param)
	
#----------------------------------------------------------------------------------------------
	# read in rast
	library(rgrass7)
	library(rgdal)
	gis = gmeta()
	gridarea = gis$nsres * gis$ewres
	
	# bounded by GIS mask
	basinMap = 'basin'
	hillslopeMap = 'hill'
	zoneMAP = 'patch'
	patchMAP = 'patch'

	# extract RHESSys structural IDs 
	rast0 = readRAST(c(basinMap, hillslopeMap, zoneMAP, patchMAP),NODATA=0)
		mask = !is.na(rast0@data[[1]])
		hill = rast0@data[[2]]
		zone = rast0@data[[3]]
		patch = rast0@data[[4]]
	
	# extract patch def IDs
	soilidMAP = 'soil_texture'
	lulcidMAP = 'landuse'
	VegidMAP = 'stratum'
	
	rast1 = readRAST(c(soilidMAP, lulcidMAP, VegidMAP),NODATA=0)
		soil = rast1@data[[1]]
		lu = rast1@data[[2]]
		veg = rast1@data[[3]]
	
	# extract patch positive numerical values [0 -> +X]
	xMAP = 'xmap' 	##<<---- use raster calculator
	yMAP = 'ymap'	##<<---- use raster calculator
	demMAP = 'dem' ##<<---- from USGS
	slopeMap = 'slope' ##<<---- r.slope.aspect elevation="dem" slope="slope" aspect="aspect" format="degrees" prec="float" zfactor=1.0 min_slp_allowed=0.0 
	aspectMAP = 'aspect' ##<<----- 90=north, 360=east, 180=west 270=south
	twiMAP = 'wetness_index' ##<<---- r.topidx
	whorizonMAP = 'west_180' ##<<----[0 or higher]: r.horizon -d elevin="dem" direction=180 output="west" distance=1.0 --> sin()
	ehorizonMAP = 'east_000' ##<<----[0 or higher]: r.horizon -d elevin="dem" direction=0 output="east" distance=1.0 --> sin()
	isohyetMAP = 'ZERO' ##<<--------- [can it be 0 or negative?] <------ "precipitation" mm/m
	imperviousMAP = 'impervious' ##<<---- from USGS [0-100]
	laiMAP = 'lai' ##<<---- remote sensing or LULC reclass
	VegCoverMAP = 'ONE' ##<<---- default is 1 everywhere [fractioning the patch by vegetations]
	VegGapMAP = 'ZERO' ##<<---- default is 0 everywhere [this variable is for light canopy penetration]
	
	rast2 = readRAST(c(xMAP, yMAP, demMAP, slopeMap, aspectMAP, twiMAP, whorizonMAP, ehorizonMAP, isohyetMAP, imperviousMAP, laiMAP), NODATA=-1)
		xx = rast2@data[[1]]
		yy = rast2@data[[2]]
		dem = rast2@data[[3]]
		slope = rast2@data[[4]]
		aspect = rast2@data[[5]]
		twi = rast2@data[[6]]
		whorizon = rast2@data[[7]]
		ehorizon = rast2@data[[8]]
		isohyet = rast2@data[[9]]
		impervious = rast2@data[[10]]*0.01 # convert unit to [0-1]
		lai = rast2@data[[11]]

	## centroid 
	sputm <- SpatialPoints(cbind(mean(xx[mask]),mean(yy[mask])), proj4string=CRS(gmeta()$proj4) )  
  	spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
	latitude = spgeo@coords[2]
	
	##-----<< order >>------#
	len = sum(mask)
	RHESSysOrder = order(hill[mask],zone[mask],patch[mask]) ### somehow we need to cut deplulated

	##-----<< list out IDs >>------#
		## we assume one vegetation per patch
	idMatrix = unique(cbind(patch[mask],zone[mask],hill[mask])[RHESSysOrder,])
	colnames(idMatrix) = c('patchList', 'zoneList', 'hillList')
	
	
	## hill 
	hillX = tapply(xx[mask],INDEX=hill[mask], FUN=mean)
	hillY = tapply(yy[mask],INDEX=hill[mask], FUN=mean)
	hillZ = tapply(dem[mask],INDEX=hill[mask], FUN=mean)
		hillID = as.numeric(names(hillX))
		numhill = length(hillID)
		hillIDrhessysOrder = match(idMatrix[,'hillList'], hillID) 

	## zone
	zoneEast = tapply(sin(ehorizon[mask]*DtoR),INDEX=zone[mask], FUN=mean)
	zoneWest = tapply(sin(whorizon[mask]*DtoR),INDEX=zone[mask], FUN=mean)
	
	zoneSlope = tapply(slope[mask],INDEX=zone[mask], FUN=mean) # vector
		aspectcos = tapply(cos(aspect[mask]*DtoR)*sin(slope[mask]*DtoR),INDEX=zone[mask], FUN=mean)
		aspectsin = tapply(sin(aspect[mask]*DtoR)*sin(slope[mask]*DtoR),INDEX=zone[mask], FUN=mean)
	zoneAspect = atan(aspectsin/aspectcos)* RtoD ## always between -90 and 90
		zoneAspect[aspectcos<0] = zoneAspect[aspectcos<0] + 180 #yes
		zoneAspect[aspectsin<0 & aspectcos>0] = zoneAspect[aspectsin<0 & aspectcos>0] + 360
		
	zoneX = tapply(xx[mask],INDEX=zone[mask], FUN=mean)
	zoneY = tapply(yy[mask],INDEX=zone[mask], FUN=mean)
	zoneZ = tapply(dem[mask],INDEX=zone[mask], FUN=mean)
	zoneArea = tapply(rep(1,len),INDEX=zone[mask], FUN=mean)*gridarea
		zoneID = as.numeric(names(zoneX))
		numzone = length(zoneID)
		zoneIDrhessysOrder = match(idMatrix[,'zoneList'], zoneID) 
	
	# cbind(zoneAspect,aspect[mask],slope[mask])[80:100,]
	# plot(aspect[mask], zoneAspect); abline(a=0,b=1,lty=2)
	
	
	## patch (patch >= grid)
	patchX = tapply(xx[mask],INDEX=patch[mask], FUN=mean)
	patchY = tapply(yy[mask],INDEX=patch[mask], FUN=mean)
	patchZ = tapply(dem[mask],INDEX=patch[mask], FUN=mean)
	patchArea = tapply(rep(1,len),INDEX=patch[mask], FUN=mean)*gridarea
	patchSOIL = tapply(soil[mask],INDEX=patch[mask], FUN=function(x){hold=table(x); as.numeric(names(hold)[which.max(hold)])} )
	patchLAND = tapply(lu[mask],INDEX=patch[mask], FUN=function(x){hold=table(x); as.numeric(names(hold)[which.max(hold)])} )
	patchIMP = tapply(impervious[mask],INDEX=patch[mask], FUN=mean)
	patchSLOPE = tapply(slope[mask],INDEX=patch[mask], FUN=mean)
	patchTWI = tapply(twi[mask],INDEX=patch[mask], FUN=mean)
		patchID = as.numeric(names(patchX))
		numpatch = length(patchID)	
		patchIDrhessysOrder = match(idMatrix[,'patchList'], patchID)
	
	
	## patch-strata (a patch may contain multiple strata)
	patchVegnum = tapply(veg[mask],INDEX=patch[mask], FUN=function(x){length(unique(x))} )[patchIDrhessysOrder]
	patchVegLAI = unlist(tapply(1:sum(mask),INDEX=patch[mask], FUN=function(x){
			lai_by_patch_strata = tapply(lai[mask][x],veg[mask][x],mean);
			return <- lai_by_patch_strata
		})[patchIDrhessysOrder])
	patchVegID = unlist(tapply(1:sum(mask),INDEX=patch[mask], FUN=function(x){
			ID_by_patch_strata = tapply(veg[mask][x],veg[mask][x],mean);
			return <- ID_by_patch_strata
		})[patchIDrhessysOrder])
	patchVegCover = unlist(tapply(1:sum(mask),INDEX=patch[mask], FUN=function(x){
			area_by_patch_strata = tapply(rep(1,length(x)),veg[mask][x],sum);
			return <- area_by_patch_strata/sum(area_by_patch_strata)
		})[patchIDrhessysOrder])


	lai_ = patchVegLAI ## already ordered
		stratum = patchVegID
		vegDefID = as.numeric(param[1,3:ncol(param)])
		stratumIndex = match(stratum, vegDefID) # return index
		stratumMass=matrix(0,length(lai_),13)
		colnames(stratumMass) = c("leafc","lstemc","dstemc","lrootc","drootc","frootc","leafcalloc","leafn","lstemn","dstemn","lrootn","drootn","frootn")
		
		cond = lai_ >0 
		stratum.SLA = as.numeric(param[48,3:ncol(param)])[stratumIndex[cond]]
		stratum.LS = as.numeric(param[12,3:ncol(param)])[stratumIndex[cond]]
		stratum.LIVED = as.numeric(param[10,3:ncol(param)])[stratumIndex[cond]]
		stratum.DEAD = 1/stratum.LIVED -1; 
		stratum.DEAD[!is.finite(stratum.DEAD)]=0
		stratum.SC = as.numeric(param[8,3:ncol(param)])[stratumIndex[cond]]
		stratum.LR = as.numeric(param[9,3:ncol(param)])[stratumIndex[cond]]
		stratum.CNL = as.numeric(param[33,3:ncol(param)])[stratumIndex[cond]]
		stratum.CNR = as.numeric(param[21,3:ncol(param)])[stratumIndex[cond]]
		stratum.CNLW = as.numeric(param[39,3:ncol(param)])[stratumIndex[cond]]
		stratum.CNDW = rep(333.33,sum(cond))
	
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
	outWorldFilePath = paste(projectFolder,'/', outWorldFile,sep='')
	title = c(defaultWorldName, defaultBasinName, defaultHillName, defaultZoneName, defaultPatchName, defaultStratumName)
	write( title, outWorldFilePath, ncolumns=length(title), append=F, sep=',')
		
		
	WorldBasinColumn = rep(1, sum(patchVegnum)) %o% c(1,1, mean(xx[mask]), mean(yy[mask]), mean(dem[mask]), 1, latitude, 0)
	
	hillColumn = cbind(
		hillID,hillX, hillY, hillZ,
		rep(1, numhill) %o% c(1,0,0,0)
		)[rep(hillIDrhessysOrder, times=patchVegnum),]## hill
		
	zoneColumn = cbind(
		zoneID, zoneX, zoneY, zoneZ, 
		rep(1, numzone), zoneArea, zoneSlope, zoneAspect,
		rep(1, numzone), zoneEast, zoneWest,  
		rep(1, numzone) %o% c(1, climateStationID)
		)[rep(zoneIDrhessysOrder, times=patchVegnum),]## zone
	
	patchColumn = cbind(
		patchID, patchX, patchY, patchZ, patchSOIL, patchLAND, patchArea,
		patchSLOPE, patchTWI, 1-patchIMP, 
		rep(1, numpatch) %o% c(0.12, 0,0,0, 0.28, 0, -10, 0, -0.5,1,0, 0.0000001,0.0000001,0.0000002,0.0000003,0.0000004,0.0000001, 0,0, 0.0000002,0.0000003,4e-9,0)
		)[rep(patchIDrhessysOrder, times=patchVegnum),]## patch
						
	stratumColumn = matrix(0, sum(patchVegnum),length(defaultStratumName))	
	stratumColumn[,1] = 1:dim(stratumColumn)[1] #strateID
	stratumColumn[,2] = patchVegID # stratePlantID
	stratumColumn[,3] = patchVegCover # stratecfrac -->affect precipitation intercept 
	stratumColumn[,4] = 0 # strateGap -->affect sunlight intercept 
	stratumColumn[,5] = 1 # strateRZ 1m by default
	stratumColumn[,c(9,13,16,19,22,25,29,31,35,38,41,44,47)] = stratumMass
	
	
	write.table(cbind(
		WorldBasinColumn,
		hillColumn,
		zoneColumn,
		patchColumn,
		stratumColumn
	), outWorldFilePath, row.names=F,col.names=F, append=T, sep=',')
					

	
	
	
	
	
	
