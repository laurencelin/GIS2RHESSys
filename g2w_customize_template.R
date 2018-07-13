## single basin
## sub-patch/grid setting
#----------------------------------------------------------------------------------------------
	arg=commandArgs(T)
	DtoR = pi/180
	RtoD = 1/DtoR
	defaultWorldName = c('worldID')
	defaultBasinName = c('basinID','basinX','basinY','basinZ','basinDefID','basinLatitude','basinBase')
	defaultHillName = c('hillID','hillX','hillY','hillZ','hillDefID','hillgws','hillgwno3','hillBase')
	defaultZoneName = c('zoneID','zoneX','zoneY','zoneZ','zoneDefID','zoneArea','zoneSlope','zoneAspect','zoneIso','zoneEH','zoneWH','zoneBase','zoneBaseID')
	defaultPatchName = c('patchID','patchX','patchY','patchZ','patchsoilIDID','patchLandID','patchArea','patchSlope','patchLNA','patchKsat','patchMpar','patchRZstorage','patchUnsat','patchSat','patchSnowEZ','patchSnowZ','patchSnowT','patchSnowAge','patchSnowED','patchLittercfrac','patchLitterStorage','patchLitterc1','patchLittern1','patchLitterc2','patchLitterc3','patchLitterc4','patchsoilIDc1','patchsoilIDsminn','patchsoilIDNO3','patchsoilIDc2','patchsoilIDc3','patchsoilIDc4','patchBase')
	defaultStratumName = c('strateID','stratePlantID','stratecfrac','strateGap','strateRZ','strateSnowStored','strateRainStored','stratecpool','strateleafc','stratedleafc','strateleafcstore','strateleafctrans','stratelstemc','stratelstemcstore','stratelstemctrans','stratedstemc','stratedstemcstore','stratedstemctrans','stratelrootc','stratelrootcstore','stratelrootctrans','stratedrootc','stratedrootcstore','stratedrootctrans','stratefrootc','stratefrootcstore','stratefrootctrans','stratecwdc','strateEPVleafcalloc','stratenpool','strateleafn','stratedleafn','strateleafnstore','strateleafntrans','stratelstemn','stratelstemnstore','stratelstemntrans','stratedstemn','stratedstemnstore','stratedstemntrans','stratelrootn','stratelrootnstore','stratelrootntrans','stratedrootn','stratedrootnstore','stratedrootntrans','stratefrootn','stratefrootnstore','stratefrootntrans','stratecwdn','strateRetransn','epv_wstress_days','epv_max_fparabs','epv_min_vwc','strateBase')
	title = c(defaultWorldName, defaultBasinName, defaultHillName, defaultZoneName, defaultPatchName, defaultStratumName)
	
#----------------------------------------------------------------------------------------------
	projectFolder = '.'
	outWorldFile = 'worldfile.csv'
	climateStationID = 101 #as.numeric(arg[2])
		
	## user provides a customized vegetation.csv containing all vegetation parameters.
	param = read.csv(paste(projectFolder,'/','SLB_Veg.csv',sep=''),skip=4,header=T,stringsAsFactors=F) #<<------
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
	basinMap = 'SLBsite'
	hillslopeMap = 'hill62ha2'
	zoneMAP = 'zone'
	patchMAP = 'patch'

	# extract RHESSys structural IDs 
	rast0 = readRAST(c(basinMap, hillslopeMap, zoneMAP, patchMAP),NODATA=0)
		mask = !is.na(rast0@data[[1]])
		hill = rast0@data[[2]][mask]
		zone = rast0@data[[3]][mask]
		patch = rast0@data[[4]][mask]
	
	# extract patch def IDs
	soilidMAP = 'soil1'
	
	rast1 = readRAST(c(soilidMAP),NODATA=0)
		soil = rast1@data[[1]][mask]


	# extract patch positive numerical values [0 -> +X]
	xMAP = 'xmap' 	##<<---- use raster calculator
	yMAP = 'ymap'	##<<---- use raster calculator
	demMAP = 'dem3m2' ##<<---- from USGS
	slopeMap = 'slope2' 
	aspectMAP = 'aspect2' ##<<----- 90=north, 360=east, 180=west 270=south
	twiMAP = 'wetness_index2' ##<<---- r.topidx
	whorizonMAP = 'west_180' 
	ehorizonMAP = 'east_000' 
	isohyetMAP = 'ZERO' ##<<--------- [can it be 0 or negative?] <------ "precipitation" mm/m
		# ------------------------------------------------------------------------------------------------------ #
	imperviousMAP = 'impervious' ##<<---- from custom [0-1]
	forestFracMAP = 'forestfrac'
	lawnFracMAP = 'lawnfrac'
	
	rast2 = readRAST(c(xMAP, yMAP, demMAP, slopeMap, aspectMAP, twiMAP, whorizonMAP, ehorizonMAP, isohyetMAP, imperviousMAP, forestFracMAP, lawnFracMAP), NODATA=-1)
		xx = rast2@data[[1]][mask]
		yy = rast2@data[[2]][mask]
		dem = rast2@data[[3]][mask]
		slope = rast2@data[[4]][mask]
		aspect = rast2@data[[5]][mask]
		twi = rast2@data[[6]][mask]
		whorizon = rast2@data[[7]][mask]
		ehorizon = rast2@data[[8]][mask]
		isohyet = rast2@data[[9]][mask]
			# ------------------------------------------------------------------------------------------------------ #
			## sub-grid information
		impervious = rast2@data[[10]][mask]
		forestFrac = rast2@data[[11]][mask]
		lawnFrac = rast2@data[[12]][mask]

	rast3 = readRAST('str62ha2Buff', NODATA=-1)
		strbuff = rast3@data[[1]][mask] ##<<-------------------------------- use this for patch output 1=at channel 2=[3-6m], 21=[57-60m]
		strbuff[!is.na(strbuff) & strbuff>21] = NA
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
	patchSOIL = tapply(soil,INDEX=patch, FUN=function(x){hold=table(x); as.numeric(names(hold)[which.max(hold)])} )
	patchSLOPE = tapply(slope,INDEX=patch, FUN=mean)
	patchTWI = tapply(abs(twi),INDEX=patch, FUN=mean); patchTWI[is.na(patchTWI)]=0;
		patchID = as.numeric(names(patchX))
		numpatch = length(patchID)	
		patchIDrhessysOrder = match(idMatrix[,'patchList'], patchID) ## organize unique patch into the idMatrix order

	# rep(patchIDrhessysOrder, times=patchVegnum) in below is expending the idMatrix order by Vegnum
	
	
	## sub-patch/grid setting
		
	subGridAssignment = matrix(NA,4,3)
	subGridAssignment[2:4,1] = c(4.5,2,1) # tree [LAI, vegID, rootz]
	subGridAssignment[2:4,2] = c(1.5,3,0.3) # green [LAI, vegID, rootz]
	subGridAssignment[2:4,3] = c(0.0,4,0) # no veg [LAI, vegID, rootz]		
	landuseClass = c(2,1,3) # RHESSys def
	
		## scanning each grid within a patch and forming within patch configuration using the subgrid information
	subGrid_buff = 'patchID frac lai vegID land imp rootz'
	patchVegnum = tapply(1:sum(mask),INDEX=tapplyOrder, FUN=function(x){
			
			subGridAssignment[1,] = c(mean(forestFrac[x]), mean(lawnFrac[x]), mean(impervious[x]) )
				if(is.na(subGridAssignment[1,1]) ) subGridAssignment[1,1]=0
				if(is.na(subGridAssignment[1,2]) ) subGridAssignment[1,2]=0
				if(is.na(subGridAssignment[1,3]) ) subGridAssignment[1,3] = 1 -subGridAssignment[1,1] -subGridAssignment[1,2]
			patchOUT_singal = mean(strbuff[x]+1,na.rm=T);
			land = landuseClass[which.max(subGridAssignment[1,])] + ifelse(is.na(patchOUT_singal), 0,600) ## use strbuff class to select patch output
				# later is fixed by crossline.
			imp = subGridAssignment[1,3]
			subGridAssignment[1,3]=1;
			
			fracQ = c(subGridAssignment[1,1]>0, subGridAssignment[1,2]>0, subGridAssignment[1,1]==0&subGridAssignment[1,2]==0)
			if(fracQ[1]) subGrid_buff <<- c(subGrid_buff, paste(patch[x][1],paste(subGridAssignment[,1],collapse=' '), land,imp, sep=' '))
			if(fracQ[2]) subGrid_buff <<- c(subGrid_buff, paste(patch[x][1],paste(subGridAssignment[,2],collapse=' '), land,imp, sep=' '))
			if(fracQ[3]) subGrid_buff <<- c(subGrid_buff, paste(patch[x][1],paste(subGridAssignment[,3],collapse=' '), land,imp, sep=' '))
			
			
			return <- sum(fracQ)
		}) # cbind(patch_[RHESSysOrder], subGrid_buff[-1])[1:10,]
		
	subGrid_info = read.table(textConnection(subGrid_buff),sep=' ',header=T)	
	patchVegCover = subGrid_info[,'frac']
	patchVegLAI = subGrid_info[,'lai']
	patchVegID = subGrid_info[,'vegID']
	patchLAND = subGrid_info[,'land'] 	
	patchIMP = subGrid_info[,'imp']  #<<----	
	patchRZ = subGrid_info[,'rootz']

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
	outWorldFilePath = paste(projectFolder,'/worldfiles/', outWorldFile,sep='')
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
		rep(1, numzone), zoneEast, zoneWest,  
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
					

	

					

	
	
	
	
	
	
