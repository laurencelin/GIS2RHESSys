## single basin
## sub-patch/grid setting
#----------------------------------------------------------------------------------------------
source('https://raw.githubusercontent.com/laurencelin/Date_analysis/master/LIB_misc.r')
	options(scipen=999)
    arg = commandArgs(T)

    DtoR = pi/180
	RtoD = 1/DtoR
    BASEMENT_DEPTH = 3 #meter
    PAVEDROAD_DEPTH = 0.3 #meter


	defaultWorldName = c('worldID')
	defaultBasinName = c('basinID','basinX','basinY','basinZ','basinDefID','basinLatitude','basinBase')
	defaultHillName = c('hillID','hillX','hillY','hillZ','hillDefID','hillgws','hillgwno3','hillBase')
	defaultZoneName = c('zoneID','zoneX','zoneY','zoneZ','zoneDefID','zoneArea','zoneSlope','zoneAspect','zoneIso','zoneEH','zoneWH','zoneBase','zoneBaseID')
	defaultPatchName = c('patchID','patchX','patchY','patchZ','patchsoilID','patchLandID','patchArea','patchSlope','patchLNA','patchKsat','patchMpar','patchRZstorage','patchUnsat','patchSat','patchSnowEZ','patchSnowZ','patchSnowT','patchSnowAge','patchSnowED','patchLittercfrac','patchLitterStorage','patchLitterc1','patchLittern1','patchLitterc2','patchLitterc3','patchLitterc4','patchsoilIDc1','patchsoilIDsminn','patchsoilIDNO3','patchsoilIDc2','patchsoilIDc3','patchsoilIDc4','patchBase')
	defaultStratumName = c('strateID','stratePlantID','stratecfrac','strateGap','strateRZ','strateSnowStored','strateRainStored','stratecpool','strateleafc','stratedleafc','strateleafcstore','strateleafctrans','stratelstemc','stratelstemcstore','stratelstemctrans','stratedstemc','stratedstemcstore','stratedstemctrans','stratelrootc','stratelrootcstore','stratelrootctrans','stratedrootc','stratedrootcstore','stratedrootctrans','stratefrootc','stratefrootcstore','stratefrootctrans','stratecwdc','strateEPVleafcalloc','stratenpool','strateleafn','stratedleafn','strateleafnstore','strateleafntrans','stratelstemn','stratelstemnstore','stratelstemntrans','stratedstemn','stratedstemnstore','stratedstemntrans','stratelrootn','stratelrootnstore','stratelrootntrans','stratedrootn','stratedrootnstore','stratedrootntrans','stratefrootn','stratefrootnstore','stratefrootntrans','stratecwdn','strateRetransn','epv_wstress_days','epv_max_fparabs','epv_min_vwc','strateBase')
	title = c(defaultWorldName, defaultBasinName, defaultHillName, defaultZoneName, defaultPatchName, defaultStratumName)
	

#----------------------------------------------------------------------------------------------

    library(sp)
    library(XML)
    library(rgrass7)
    library(rgdal)
    gis = gmeta()
    gridarea = round(gis$nsres * gis$ewres)

    #read template
    # (this part can be read in from stationList.csv)
    # https://stackoverflow.com/questions/17288197/reading-a-csv-file-organized-horizontally
    # modified by Lin June 23 2018
    read.tcsv = function(file, header=T, sep=" ", vskip=0, hskip=0, len=-1, ...) {
        rowCountFields = count.fields(file, sep=sep)
        if(vskip>0) rowCountFields = rowCountFields[-1:-vskip]
        n = max( rowCountFields, na.rm=TRUE)
        
        # x = readLines(file)
        # .splitvar = function(x, sep, n) {
        #     var = unlist(strsplit(x, split=sep))
        #     length(var) = n
        #     return(var)
        # }#function
        # x = do.call(cbind, lapply(x[(vskip+1):endline], .splitvar, sep=sep, n=n))
        # x = apply(x[(hskip+1):dim(x)[1],], 1, paste, collapse=sep)
        # out = read.csv(text=x, sep=sep, header=header, skip=0, ...)
        
        x = read.table(file, sep=' ', fill=T, stringsAsFactors=F )
        endline = dim(x)[1]
        if(len>0) endline = vskip+1+len
        transformedX = sapply( (hskip+1):n, function(ii){
        	paste( x[(vskip+1):endline, ii], collapse=',')
        });
        out = read.csv(text=transformedX, sep=',', header=header, skip=0, ...)
        
        return(out)
    }#function

    templateACTION = read.tcsv(arg[5],stringsAsFactors=F, len=7);
    template = read.tcsv(arg[5],stringsAsFactors=F, vskip=7);
    if(is.null(template$streamFullextension)) template$streamFullextension=''
    if(is.null(template$unpavedroadMap)) template$unpavedroadMap=''
    if(is.null(template$riparianMAP)) template$riparianMAP=''
    if(is.null(template$sewercoverMAP)) template$sewercoverMAP=''
    if(is.null(template$pipecoverMAP)) template$pipecoverMAP=''
    if(is.null(template$stormdrainMAP)) template$stormdrainMAP=''
    if(is.null(template$compactedsoilMAP)) template$compactedsoilMAP=''
    if(is.null(template$additionalSurfaceDrainMAP)) template$additionalSurfaceDrainMAP=''

	projectFolder = arg[1]
	climateStationID = as.numeric(templateACTION$stationID[1])
	climateStationNAME = templateACTION$stationFile[1]
		
	## user provides a customized vegetation.csv containing all vegetation vegParameters.
    print(arg)
	vegParam = read.csv(ifelse(arg[2]=='default'|arg[2]=='NA'|arg[2]=='na'|arg[2]=='.','https://raw.githubusercontent.com/laurencelin/GIS2RHESSys/master/vegCollection.csv', arg[2]),skip=4,header=T,stringsAsFactors=F) #<<------
	vegParamCOL = cbind(as.numeric(unique(vegParam[1,3:ncol(vegParam)])), 3:ncol(vegParam)); 
	colnames(vegParamCOL) = c('vegID','vegDefIndex')
	vegParam_len = ncol(vegParam)
	
	soilParam = read.csv(ifelse(arg[3]=='default'|arg[3]=='NA'|arg[3]=='na'|arg[3]=='.','https://raw.githubusercontent.com/laurencelin/GIS2RHESSys/master/soilCollection.csv', arg[3]),skip=4,header=T,stringsAsFactors=F) #<<------
	soilParamCOL = cbind(as.numeric(unique(soilParam[1,3:ncol(soilParam)])), 3:ncol(soilParam)); 
	colnames(soilParamCOL) = c('soilID','soilDefIndex')
	soilParam_len = ncol(soilParam)
	
	lulcParam = read.csv(ifelse(arg[4]=='default'|arg[4]=='NA'|arg[4]=='na'|arg[4]=='.','https://raw.githubusercontent.com/laurencelin/GIS2RHESSys/master/lulcCollectionEC.csv', arg[4]),skip=4,header=T,stringsAsFactors=F) #<<------
	lulcParamCOL = cbind(as.numeric(unique(lulcParam[1,3:ncol(lulcParam)])), 3:ncol(lulcParam)); 
	colnames(lulcParamCOL) = c('lulcID','lulcDefIndex')
	lulcParam_len = ncol(lulcParam)
	
#----------------------------------------------------------------------------------------------

    ## spatial structures (must have)
	rast = readRAST(c(template$basinMap, template$hillslopeMap, template$zoneMAP, template$patchMAP),NODATA=0)
		mask = !is.na(rast@data[[1]])
		hill = rast@data[[2]][mask]
		zone = rast@data[[3]][mask]
		patch = rast@data[[4]][mask]
	print('reading basin, hill, zone, patch ... DONE')
	
	# extract soil def IDs (must have)
	rast = readRAST(template$soilidMAP,NODATA=0)
		soil = rast@data[[1]][mask]
    print('reading soils ... DONE')

	# extract patch positive numerical values [0 -> +X] (must have)
    # aspect ##<<----- 90=north, 360=east, 180=west 270=south
	rast = readRAST(c(template$xMAP, template$yMAP, template$demMAP, template$slopeMap, template$aspectMAP, template$twiMAP, template$whorizonMAP, template$ehorizonMAP, template$isohyetMAP), NODATA=-1)
		xx = rast@data[[1]][mask]
		yy = rast@data[[2]][mask]
		dem = rast@data[[3]][mask]
		slope = rast@data[[4]][mask] ## ... could be a problem at the edge of the DEM
		aspect = rast@data[[5]][mask] ## ... could be a problem at the edge of the DEM 
		twi = rast@data[[6]][mask] ## ... could be a problem at the edge of the DEM
		whorizon = rast@data[[7]][mask]
		ehorizon = rast@data[[8]][mask]
		isohyet = rast@data[[9]][mask]

    rast = readRAST(c(template$rowMap, template$colMap),NODATA=0)
        rows = rast@data[[1]][mask]
        cols = rast@data[[2]][mask]
    rast = readRAST(template$drainMap)
        drain = abs(rast@data[[1]][mask]) # guide the "flat" surface or surface pit water exit
    print('reading xx,yy,.... ... DONE')

    # extract LULC information (must have)
    rast = readRAST(c(
        template$impFracMAP,#1
        template$roofMAP, #2
        template$drivewayMAP,#3
        template$pavedRoadFracMAP,#4
        template$forestFracMAP,#5
        template$shrubFracMAP,#6
        template$cropFracMAP,#7
        template$grassFracMAP #8
        ), NODATA=-1)

        impFrac = rast@data[[1]][mask]
        roofFrac = rast@data[[2]][mask]
        drivewayFrac = rast@data[[3]][mask]
        pavedRoadFrac = rast@data[[4]][mask]
        forestFrac = rast@data[[5]][mask]
        shrubFrac = rast@data[[6]][mask]
        cropFrac = rast@data[[7]][mask]
        lawnFrac = rast@data[[8]][mask]

    print('reading lulc ... DONE')

    # extract flow table related information
    stream=NULL;tryCatch({
        rast = readRAST(template$streamMap);
        stream <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ stream <<- rep(NA,length(mask)) }
        )#tryCatch
    fullstreamExt=NULL; tryCatch({
        rast = readRAST(template$streamFullextension);
        fullstreamExt <- rast@data[[1]][mask];
        print('reading strExtension ... DONE')},
        error = function(e){ fullstreamExt <<- stream }
        )#tryCatch
    unpavedroad=NULL; tryCatch({
        rast = readRAST(template$unpavedroadMap);
        unpavedroad <- rast@data[[1]][mask];
        print('reading unpavedRoad ... DONE')},
        error = function(e){ unpavedroad <<- rep(NA,length(mask)) }
        )#tryCatch
    riparian=NULL; tryCatch({
        rast = readRAST(template$riparianMAP);
        riparian <- rast@data[[1]][mask];
        print('reading riparian ... DONE')},
        error = function(e){ riparian <<- rep(NA,length(mask)) }
        )#tryCatch
    sewercover=NULL; tryCatch({
        rast = readRAST(template$sewercoverMAP);
        sewercover <- rast@data[[1]][mask];
        print('reading sewer ... DONE')},
        error = function(e){ sewercover <<- rep(NA,length(mask)) }
        )#tryCatch
    septic=NULL; tryCatch({
        rast = readRAST(template$septicMAP);
        septic <- rast@data[[1]][mask];
        print('reading septic ... DONE')},
        error = function(e){ septic <<- rep(NA,length(mask)) }
        )#tryCatch
    pipecover=NULL; tryCatch({
        rast = readRAST(template$pipecoverMAP);
        pipecover <- rast@data[[1]][mask];
        print('reading pipe ... DONE')},
        error = function(e){ pipecover <<- rep(NA,length(mask)) }
        )#tryCatch
    stormdrain=NULL; tryCatch({
        rast = readRAST(template$stormdrainMAP);
        stormdrain <- rast@data[[1]][mask];
        print('reading stormdrain ... DONE')},
        error = function(e){ stormdrain <<- rep(NA,length(mask)) }
        )#tryCatch
    compactedsoilQ=NULL; tryCatch({
        rast = readRAST(template$compactedsoilMAP);
        compactedsoilQ <- rast@data[[1]][mask];
        print('reading non-impacted soil ... DONE')},
        error = function(e){ compactedsoilQ <<- rep(NA,length(mask)) }
        )#tryCatch
    additionalSurfaceDrain=NULL; tryCatch({
        rast = readRAST(template$additionalSurfaceDrainMAP);
        additionalSurfaceDrain <- rast@data[[1]][mask];
        print('reading additional surf drinage ... DONE')},
        error = function(e){ additionalSurfaceDrain <<- rep(NA,length(mask)) }
        )#tryCatch


    # extract strata information
	treeLAI = lapply(1:15, function(i){
		tryCatch({
            rast = readRAST( template[[paste('tree',i,'LAI',sep='')]] ) # paste('tree',i,'LAI',sep='')
			return <- as.numeric(rast@data[[1]][mask]) },
			
			error = function(e){ return <- NA}
			)#tryCatch
		})
	treeID = lapply(1:15, function(i){ 
		tryCatch({
            rast = readRAST( template[[paste('tree',i,'StratumID',sep='')]] ) # paste('tree',i,'StratumID',sep='')
			return <- as.numeric(rast@data[[1]][mask]) },
			
			error = function(e){ return <- NA}
			)#tryCatch
		})
	treeFFrac = lapply(1:15, function(i){ 
		tryCatch({
            rast = readRAST( template[[paste('tree',i,'FFrac',sep='')]] ) # paste('tree',i,'FFrac',sep='')
			return <- as.numeric(rast@data[[1]][mask]) },
			
			error = function(e){ return <- NA}
			)#tryCatch
		})

	shrubLAI = lapply(1:15, function(i){ 
		tryCatch({
            rast = readRAST( template[[paste('shrub',i,'LAI',sep='')]] ) # paste('shrub',i,'LAI',sep='')
			return <- as.numeric(rast@data[[1]][mask]) },
			
			error = function(e){ return <- NA}
			)#tryCatch
		})
	shrubID = lapply(1:15, function(i){ 
		tryCatch({
            rast = readRAST( template[[paste('shrub',i,'StratumID',sep='')]] ) # paste('shrub',i,'StratumID',sep='')
			return <- as.numeric(rast@data[[1]][mask]) },
			
			error = function(e){ return <- NA}
			)#tryCatch
		})
	shrubFFrac = lapply(1:15, function(i){ 
		tryCatch({
            rast = readRAST( template[[paste('shrub',i,'FFrac',sep='')]] ) # paste('shrub',i,'FFrac',sep='')
			return <- as.numeric(rast@data[[1]][mask]) },
			
			error = function(e){ return <- NA}
			)#tryCatch
		})
    # is(shrubLAI[[2]]); is(shrubID[[2]]); is(shrubFFrac[[2]])
	
	grassLAI = lapply(1:15, function(i){ 
		tryCatch({
            rast = readRAST( template[[paste('grass',i,'LAI',sep='')]] ) # paste('grass',i,'LAI',sep='')
			return <- as.numeric(rast@data[[1]][mask]) },
			
			error = function(e){ return <- NA}
			)#tryCatch
		})
	grassID = lapply(1:15, function(i){ 
		tryCatch({
            rast = readRAST( template[[paste('grass',i,'StratumID',sep='')]] ) # paste('grass',i,'StratumID',sep='')
			return <- as.numeric(rast@data[[1]][mask]) },
			
			error = function(e){ return <- NA}
			)#tryCatch
		})
	grassFFrac = lapply(1:15, function(i){ 
		tryCatch({
            rast = readRAST( template[[paste('grass',i,'FFrac',sep='')]] ) # paste('grass',i,'FFrac',sep='')
			return <- as.numeric(rast@data[[1]][mask]) },
			
			error = function(e){ return <- NA}
			)#tryCatch
		})
    # is(grassLAI[[2]]); is(grassID[[2]]); is(grassFFrac[[2]])

	cropLAI = lapply(1:15, function(i){ 
		tryCatch({
            rast = readRAST( template[[paste('crop',i,'LAI',sep='')]] ) # paste('crop',i,'LAI',sep='')
			return <- as.numeric(rast@data[[1]][mask]) },
			
			error = function(e){ return <- NA}
			)#tryCatch
		})
	cropID = lapply(1:15, function(i){ 
		tryCatch({
            rast = readRAST( template[[paste('crop',i,'StratumID',sep='')]] ) # paste('crop',i,'StratumID',sep='')
			return <- as.numeric(rast@data[[1]][mask]) },
			
			error = function(e){ return <- NA}
			)#tryCatch
		})
	cropFFrac = lapply(1:15, function(i){ 
		tryCatch({
            rast = readRAST( template[[paste('crop',i,'FFrac',sep='')]] ) # paste('crop',i,'FFrac',sep='')
			return <- as.numeric(rast@data[[1]][mask]) },
			
			error = function(e){ return <- NA}
			)#tryCatch
		})
    # is(cropLAI[[2]]); is(cropID[[2]]); is(cropFFrac[[2]])



	## basin centroid and latitude
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
    zoneISOHYET = tapply(isohyet,INDEX=zone, FUN=mean)
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
	patchSOIL = tapply(seq_along(patch),INDEX=patch, FUN=function(ii){
		hold=table(soil[ii]); 
		return <- as.numeric(names(hold)[which.max(hold)]) + ifelse(sum(compactedsoilQ[ii],na.rm=T)>0,100,0)# plus 100 for compacted soil.
        # here is a problem when no nonimpactedsoil is defined!!
	})
	patchSLOPE = tapply(slope,INDEX=patch, FUN=mean)
	patchTWI = tapply(abs(twi),INDEX=patch, FUN=mean); patchTWI[is.na(patchTWI)]=0;
		patchID = as.numeric(names(patchX))
		numpatch = length(patchID)	
		patchIDrhessysOrder = match(idMatrix[,'patchList'], patchID) ## organize unique patch into the idMatrix order
	# rep(patchIDrhessysOrder, times=patchVegnum) in below is expending the idMatrix order by Vegnum
	
	## sub-patch/grid setting
	subGridAssignment = matrix(NA,4,5)
	subGridAssignment[2:4,1] = c(NA,	NA,	NA) # tree [LAI, vegID, rootz]
	subGridAssignment[2:4,2] = c(NA,	NA,	NA) # shrub [LAI, vegID, rootz]
	subGridAssignment[2:4,3] = c(NA,	NA,	NA) # crop [LAI, vegID, rootz]
	subGridAssignment[2:4,4] = c(NA,	NA,	NA) # lawn [LAI, vegID, rootz]
	subGridAssignment[2:4,5] = c(0.0,	4,	0) # no veg [LAI, vegID, rootz]		
	landuseClass = c(2,2,1,1,3) # RHESSys def (update RHESSys LULC! need these at all? detention size, fertilizer?, what exact does lulc class do? )
	
		## scanning each grid within a patch and forming within patch configuration using the subgrid information
	subGrid_buff = 'patchID frac lai vegID rootz land imp'
	patchVegnum = tapply(1:sum(mask),INDEX=tapplyOrder, FUN=function(x){
            # 7 -> x= 9029
            #patchSTR = sum(fullstreamExt[x], na.rm=T); # patchSTR==0 --> no canopy on all stream extension!
            patchSTR = sum(stream[x], na.rm=T)
			subGridAssignment[1,] = c(
                ifelse(patchSTR==0,mean(forestFrac[x]),0), #1 tree
                ifelse(patchSTR==0,mean(shrubFrac[x]),0),  #2 shrub
                ifelse(patchSTR==0,mean(cropFrac[x]),0),   #3 crop
                ifelse(patchSTR==0,mean(lawnFrac[x]),0),   #4 lawn
                ifelse(patchSTR==0,mean(impFrac[x]),0)     #5 no veg
				)
			if(is.na(subGridAssignment[1,1]) ) subGridAssignment[1,1]=0
			if(is.na(subGridAssignment[1,2]) ) subGridAssignment[1,2]=0
			if(is.na(subGridAssignment[1,3]) ) subGridAssignment[1,3]=0
            if(is.na(subGridAssignment[1,4]) ) subGridAssignment[1,4]=0
            if(is.na(subGridAssignment[1,5]) ) subGridAssignment[1,5]=0
			
            land = ifelse(sum(septic[x],na.rm=T)>0, 4, landuseClass[which.max(subGridAssignment[1,])] )#  ### not very useful
                # management ACTIONcodes: < (subpatch scale variable)
                # 2 = actionIRRIGRATION  {irrigration daily max} < lawnFrac
                # 3 = actionFERTILIZE {fertilizer application schedule and amount} < lawnFrac
                # 5 = actionGLAZING {lawn/crop glazing} < lawnFrac
                # 7 = actionHARVEST {harvest schedule and amount and aftermath} < lawnFrac & forestFrac?
                # 11 = actionSEPTIC {setpic leaks annual Q and N} < lawnFrac
                # 13 = actionDETENTION {surface detention e.g., very small ponds?, bottom leak, top drainage}
                
                # drainage/routing ACTIONcodes: < (subpatch scale variable)
                # 0 = land (default)
                # 1 = class::stream
                # 2 = class::road
                # 3 = actionSTORMDRAIN {drainage surface Q along roads}
                # 5 = actionGWDRAIN {gw1} < impFrac
                # 7 = actionRIPARIAN {gw2riparian}
                # 17 = actionPIPEDRAIN {drainage surface excessive Q within some areas, e.g., lawn, park}
                # 11 = actionSEWER {drainge subsurface Q; loss of Q}
                
                
			imp = subGridAssignment[1,5]
            fracQ = c(	subGridAssignment[1,1]>0,   # tree
                        subGridAssignment[1,2]>0,   # shrub
                        subGridAssignment[1,3]>0,   # crop
                        subGridAssignment[1,4]>0,   # lawn
						F # no veg
                        )#
                        fracQ[5] = ifelse(sum(fracQ)==0, T, F) # no veg
						
			numVeg = 0			
			subGrid_buff_string = NULL			
			if(fracQ[1]){
				## count how many trees
				FFraclist = sapply(1:15,function(i){ mean(treeFFrac[[i]][x]) }) * subGridAssignment[1,1]
				LAIlist = sapply(1:15,function(i){ mean(treeLAI[[i]][x]) })
				vegIDlist = sapply(1:15,function(i){ treeID[[i]][x][1] })
                
                vegCount = !is.na(FFraclist) & FFraclist>0 & !is.na(LAIlist) & LAIlist>0 & !is.na(vegIDlist) & vegIDlist>0
				numVeg = numVeg + sum(vegCount)		
				subGrid_buff_string = c(subGrid_buff_string,
					sapply(seq_len(15)[vegCount], function(i){
						paste(patch[x][1], FFraclist[i], LAIlist[i], vegIDlist[i], 1, land,imp, sep=' ')
						})
					)#c		
			}# fracQ[1] 
			if(fracQ[2]){ 
				## count how many shrub
				FFraclist = sapply(1:15,function(i){ mean(shrubFFrac[[i]][x]) }) * subGridAssignment[1,2]
				LAIlist = sapply(1:15,function(i){ mean(shrubLAI[[i]][x]) })
				vegIDlist = sapply(1:15,function(i){ shrubID[[i]][x][1] })
                vegCount = !is.na(FFraclist) & FFraclist>0 & !is.na(LAIlist) & LAIlist>0 & !is.na(vegIDlist) & vegIDlist>0
				numVeg = numVeg + sum(vegCount)	
				subGrid_buff_string = c(subGrid_buff_string,
					sapply(seq_len(15)[vegCount], function(i){
						paste(patch[x][1], FFraclist[i], LAIlist[i], vegIDlist[i], 1, land,imp, sep=' ')
						})
					)#c	
			}#fracQ[2]
            if(fracQ[3]){
                ## count how many crop
                FFraclist = sapply(1:15,function(i){ mean(cropFFrac[[i]][x]) }) * subGridAssignment[1,3]
                LAIlist = sapply(1:15,function(i){ mean(cropLAI[[i]][x]) })
                vegIDlist = sapply(1:15,function(i){ cropID[[i]][x][1] })
                vegCount = !is.na(FFraclist) & FFraclist>0 & !is.na(LAIlist) & LAIlist>0 & !is.na(vegIDlist) & vegIDlist>0
                numVeg = numVeg + sum(vegCount)
                subGrid_buff_string = c(subGrid_buff_string,
                sapply(seq_len(15)[vegCount], function(i){
                        paste(patch[x][1], FFraclist[i], LAIlist[i], vegIDlist[i], 0.3, land,imp, sep=' ')
                })
                )#c
            }#fracQ[3]
			if(fracQ[4]){
				## count how many lawn
				FFraclist = sapply(1:15,function(i){ mean(grassFFrac[[i]][x]) }) * subGridAssignment[1,4]
				LAIlist = sapply(1:15,function(i){ mean(grassLAI[[i]][x]) })
				vegIDlist = sapply(1:15,function(i){ grassID[[i]][x][1] })
                vegCount = !is.na(FFraclist) & FFraclist>0 & !is.na(LAIlist) & LAIlist>0 & !is.na(vegIDlist) & vegIDlist>0
				numVeg = numVeg + sum(vegCount)			
				subGrid_buff_string = c(subGrid_buff_string,
					sapply(seq_len(15)[vegCount], function(i){
						paste(patch[x][1], FFraclist[i], LAIlist[i], vegIDlist[i], 0.2, land,imp, sep=' ')
						})
					)#c	
            }#fracQ[4];  numVeg; subGrid_buff_string
			if(fracQ[5]){
				numVeg = numVeg + 1
				subGridAssignment[1,5]=1; #coverFrac for no veg
				subGrid_buff_string = c(subGrid_buff_string,
                        paste(patch[x][1],paste(subGridAssignment[,5],collapse=' '), land,imp, sep=' '))
			}#fracQ[5]
			
			subGrid_buff <<- c(subGrid_buff, unlist(subGrid_buff_string))
			
			return <- numVeg
		})# tapply
		
	subGrid_info = read.table(textConnection(subGrid_buff),sep=' ',header=T)	
	patchVegCover = subGrid_info[,'frac']
	patchVegLAI = subGrid_info[,'lai']
	patchVegID = subGrid_info[,'vegID']
	patchLAND = subGrid_info[,'land'] 	
	patchIMP = subGrid_info[,'imp']  #<<----	
	patchRZ = subGrid_info[,'rootz']

    table(patchVegnum)
    dim(subGrid_info)[1] >= length(patchSLOPE)
    length(patchVegnum) == length(patchSLOPE)
    table(patchVegID)

	lai_ = patchVegLAI ## already ordered
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
		stratum.CNDW = rep(333.33,sum(cond))
	
		stratumMass[cond,1] =  lai_[cond]/stratum.SLA #leafc
		stratumMass[cond,2] = stratumMass[cond,1]* stratum.LS * stratum.LIVED #live stemc
		stratumMass[cond,3] = stratumMass[cond,2]* stratum.DEAD # dead stemc
		stratumMass[cond,4] = (stratumMass[cond,2]+stratumMass[cond,3])* stratum.SC* stratum.LIVED # live crootc
		stratumMass[cond,5] = stratumMass[cond,4]*stratum.DEAD #dead crootc
		stratumMass[cond,6] = stratumMass[cond,1]* stratum.LR #frootc
		
			#iniRootmass = stratumMass[cond,4]+stratumMass[cond,5]+stratumMass[cond,6]
			#iniRTZ = 3/8 * (2* iniRootmass)^(0.8)
		
		stratum.totmass = rowSums(stratumMass)
		stratumMass[cond,7] = stratumMass[cond,1]*0.05 ##strateEPVleafcalloc
		stratumMass[cond,8] = stratumMass[cond,1]/stratum.CNL
		stratumMass[cond,9] = stratumMass[cond,2]/stratum.CNLW
		stratumMass[cond,10] = stratumMass[cond,3]/stratum.CNDW
		stratumMass[cond,11] = stratumMass[cond,4]/stratum.CNLW
		stratumMass[cond,12] = stratumMass[cond,5]/stratum.CNDW
		stratumMass[cond,13] = stratumMass[cond,6]/stratum.CNR	
		

#----------------------------------------------------------------------------------------------		
if(as.numeric(templateACTION$outputWorldfile[2])>0 ){
    outWorldFilePath = templateACTION$outputWorldfile[1]
	title = c(defaultWorldName, defaultBasinName, defaultHillName, defaultZoneName, defaultPatchName, defaultStratumName)
	write( title, outWorldFilePath, ncolumns=length(title), append=F, sep=',')
		
		
	WorldBasinColumn = rep(1, sum(patchVegnum)) %o% c(1,1, mean(xx), mean(yy), mean(dem), 1, latitude, 0)
	
	hillColumn = cbind(
		hillID,hillX, hillY, hillZ,
		rep(1, numhill) %o% c(1,0,0,0)
		)[rep(hillIDrhessysOrder, times=patchVegnum),]## hill
		
	zoneColumn = cbind(
		zoneID, zoneX, zoneY, zoneZ, 
		rep(1, numzone), zoneArea, zoneSlope, zoneAspect,
		zoneISOHYET, zoneEast, zoneWest,
		rep(1, numzone) %o% c(1, climateStationID)
		)[rep(zoneIDrhessysOrder, times=patchVegnum),]## zone
	
	patchColumn1 = cbind(
		patchID, patchX, patchY, patchZ, patchSOIL)[rep(patchIDrhessysOrder, times=patchVegnum),]## patch
	
	patchColumn2 = cbind(
		patchArea, patchSLOPE, patchTWI)[rep(patchIDrhessysOrder, times=patchVegnum),]## patch
		
	patchColumn3 = cbind(rep(1, numpatch) %o% c(0.12, 0,0,0, 0.28, 0, -10, 0, -0.5,1,0, 0.0000001,0.0000001,0.0000002,0.0000003,0.0000004,0.0000001, 0,0, 0.0000002,2,6,0))[rep(patchIDrhessysOrder, times=patchVegnum),]## patch
						
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
}# if templateACTION$outputWorldfile[2]

#----------------------------------------------------------------------------------------------

    tmp = unlist(strsplit(templateACTION$outputDefs[1],'/'))
	defsFolder = tmp[length(tmp)]
	rhessysFolder = tmp[length(tmp)-1]
	
	## write out selected veg definition files
	vegHEADER = NULL
	selectedVeg = vegParamCOL[match(unique(patchVegID), vegParamCOL[,'vegID']), 'vegDefIndex']
	for(ii in selectedVeg ){
		filename = paste(defsFolder,"/stratum_",gsub("\\.","_",colnames(vegParam)[ii]),".def",sep="")
		vegHEADER = c(vegHEADER, paste(filename,'stratum_default_filename'))
		filepth = paste(arg[1],'/',rhessysFolder,'/', filename,sep="")
		if(as.numeric(templateACTION$outputDefs[2])>0) write.table(cbind(vegParam[, ii], vegParam[,2]), filepth,sep="\t",row.names=F,col.names=F, quote=F)
	}#i

	
	## write out selected soil definition files
	soilHEADER = NULL
	selectedsoil = soilParamCOL[match(unique(patchSOIL), soilParamCOL[,'soilID']), 'soilDefIndex']
	for(ii in selectedsoil ){
		filename = paste(defsFolder,"/soil_",gsub("\\.","_",colnames(soilParam)[ii]),".def",sep="")
		soilHEADER = c(soilHEADER, paste(filename,'patch_default_filename'))
		filepth = paste(arg[1],'/',rhessysFolder,'/', filename,sep="")
		if(as.numeric(templateACTION$outputDefs[2])>0) write.table(cbind(soilParam[, ii], soilParam[,2]), filepth,sep="\t",row.names=F,col.names=F, quote=F)
	}#i
	
	
	## write out selected lulc definition files
	lulcHEADER = NULL
	selectedlulc = lulcParamCOL[match(unique(patchLAND), lulcParamCOL[,'lulcID']), 'lulcDefIndex']
	for(ii in selectedlulc ){
		filename = paste(defsFolder,"/landuse_",gsub("\\.","_",colnames(lulcParam)[ii]),".def",sep="")
		lulcHEADER = c(lulcHEADER, paste(filename,'landuse_default_filename'))
		filepth = paste(arg[1],'/',rhessysFolder,'/', filename,sep="")
		if(as.numeric(templateACTION$outputDefs[2])>0) write.table(cbind(lulcParam[, ii], lulcParam[,2]), filepth,sep="\t",row.names=F,col.names=F, quote=F)
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
	if(as.numeric(templateACTION$outputDefs[2])>0) write(zoneDEF, paste(templateACTION$outputDefs[1],"/zone_zone.def",sep=""))
	
	basinDEF = NULL
	basinDEF = c(basinDEF, '1 basin_default_ID')
	if(as.numeric(templateACTION$outputDefs[2])>0) write(basinDEF, paste(templateACTION$outputDefs[1],"/basin_basin.def",sep=""))
	
	hillDEF = NULL
	hillDEF = c(hillDEF, '1 hillslope_default_ID')
	hillDEF = c(hillDEF, '1.0 gw_loss_coeff')
	hillDEF = c(hillDEF, '1.0 sat_to_gw_coeff')
	if(as.numeric(templateACTION$outputDefs[2])>0) write(hillDEF, paste(templateACTION$outputDefs[1],"/hillslope_hillslope.def",sep=""))
	
	worldHEADER = NULL
	worldHEADER = c(worldHEADER, paste(1,'num_basin_files'))
	worldHEADER = c(worldHEADER, paste(defsFolder,'/basin_basin.def basin_default_filename',sep=''))	
	worldHEADER = c(worldHEADER, paste(1,'num_hillslope_files'))	
	worldHEADER = c(worldHEADER, paste(defsFolder,'/hillslope_hillslope.def hillslope_default_filename',sep=''))	
	worldHEADER = c(worldHEADER, paste(1,'num_zone_files'))	
	worldHEADER = c(worldHEADER, paste(defsFolder,'/zone_zone.def zone_default_filename',sep=''))	
	
	worldHEADER = c(worldHEADER, paste(length(soilHEADER),'num_patch_files'))	
	worldHEADER = c(worldHEADER, soilHEADER)
	worldHEADER = c(worldHEADER, paste(length(lulcHEADER),'num_landuse_files'))	
	worldHEADER = c(worldHEADER, lulcHEADER)
	worldHEADER = c(worldHEADER, paste(length(vegHEADER),'num_stratum_files'))	
	worldHEADER = c(worldHEADER, vegHEADER)
	
	worldHEADER = c(worldHEADER, paste(1,'num_base_stations'))
	worldHEADER = c(worldHEADER, paste(climateStationNAME,'base_station_filename'))
	
	if(as.numeric(templateACTION$outputWorldfileHDR[2])>0) write(worldHEADER,templateACTION$outputWorldfileHDR[1] )




	##-------------------------------------------------------------------------------------------------------##
	## flow table -- assume square grid
    cellarea = gis$nsres * gis$ewres
    cellsize = sqrt(cellarea)
    flatDEMdrop = tan(DtoR*0.143)*cellsize # only 0.25m drop per 100m.
    roadWidth = 5
    roadWidth = ifelse(cellsize>=9.99,
    	5, # meters (default)
    	ifelse(roadWidth>cellsize, cellsize, roadWidth)
    )# ifelse
    directEdge = cellsize*0.5
    diagonalEdge = cellsize*sqrt(0.5)

    # 1.  2. 3.  4. 5.  6. 7.  8. (GRASS from current drainTO code order)
    # NE, N, NW, W, SW, S, SE, E
    colneighbor = c(1,    0,    -1,    -1,    -1,    0,    1,    1)
    rowneighbor = c(-1,    -1,    -1,    0,    1,    1,    1,    0)
    directEdgeIndex = c(2,4,6,8)
    indirectEdgeIndex = c(1,3,5,7)

    maxCol = max(cols,na.rm=T)
    maskRC = rows*maxCol+cols #paste(rows, cols,sep=':') ## row*[max col]+col (yes: unique ID)
    maskRC_string2Patch_num <- new.env(hash=T)
    list2env(setNames(as.list(patch),maskRC),envir=maskRC_string2Patch_num) #<<---- native R hash
    gridSurroundRC = sapply(rows, FUN=function(x){x+rowneighbor})*maxCol+sapply(cols, FUN=function(x){x+colneighbor})
    gridSurroundRC[!(gridSurroundRC %in% maskRC)] = -1
	
    # part 1: gathering information to temporary files
    fullLength = seq(1,length.out=length(patch))
    patch_info_dem = tapply(dem,INDEX=patch,mean) # use elevation to organize patch order in the file output
    orderedPatch = as.numeric(names(patch_info_dem[order(patch_info_dem,decreasing=T)])) ### patch could be longer than 'orderedPatch'
    outputOrder = match(patch, orderedPatch) # has the same length as 'patch'
    # ... finding basin outlet and hillslope outlets
    strCOND = !is.na(stream)
    subID = 2*ceiling(hill*0.5)
    suboutlet_orderedPatch_index = tapply(fullLength[strCOND], subID[strCOND],function(ii){
        # must be stream channel
        return <- match(patch[ii][which.min(dem[ii])], orderedPatch)
    })#
    suboutlet_orderedPatch_index_hillID = as.numeric(names(suboutlet_orderedPatch_index))
    basinoutlet_orderedPatch_index = match(patch[strCOND][which.min(dem[strCOND])], orderedPatch)


    maskRC_string2outputOrder_num <- new.env(hash=T)
    list2env(setNames(as.list(outputOrder),maskRC),envir=maskRC_string2outputOrder_num) #<<---- native R hash
    maskRC_string2maskRC_num <- new.env(hash=T)
    list2env(setNames(as.list(maskRC),maskRC),envir= maskRC_string2maskRC_num) #<<---- native R hash
	
    print('starting step I')
    patchInfo = tapply(fullLength, INDEX=outputOrder, FUN=function(ii){
        return <- c(
            subIDindex = match(mean(subID[ii]),suboutlet_orderedPatch_index_hillID),
            patchID = mean(patch[ii]),             #1 patchID
            elevation = mean(dem[ii]),             #2 elevation
            xx = mean(xx[ii]),                #3 x coordinate
            yy = mean(yy[ii]),                #4 y coordinate
            hillID = mean(hill[ii]),                #6 hillID
            zoneID = mean(zone[ii]),                #7 zoneID
            rr = mean(rows[ii]),                #8 row index (from left to right)
            cc = mean(cols[ii]),                #9 col index (from top to bottom)
            len = length(ii),                   #10 num of cells
            aveSlope = tan(mean(slope[ii])*DtoR),   #13 average slope
            maxSlope = tan(max(slope[ii])*DtoR),    #14 max slope
            
            ## ... 
            irrigateQfra = mean(lawnFrac[ii],na.rm=T), # 23 lawnFrac for irrigate
            ## ...
            riparianQ = sum(!is.na(riparian[ii])),	#16 (checking whether patch contains riparian grids)
            sewerdrainQ = sum(!is.na(sewercover[ii])), #17 (checking whether patch contains sewercover grids)
            subsurfpipedrainQ = sum(!is.na(pipecover[ii])), # other non-sewer pipes
            compactedsoilQfrac = mean(compactedsoilQ[ii],na.rm=T), # 24 compactedsoilQ
            ## ... surface routing
            strQ = sum(!is.na(stream[ii])),         #11 modeled stream grids
            nonmodelstrgridQ = sum(!is.na(fullstreamExt[ii])),	#21 non-modeled stream grid (treat as land grids)
            nonstrsurfdrainQ = sum(!is.na(additionalSurfaceDrain[ii])), # 22 (non-stream) land grids need surface water drain
			stormdrainQ = sum(!is.na(stormdrain[ii])), #18 drainage points on paved road network
			# note: impFrac = roofFrac + drivewayFrac + pavedroadFrac
			roofQfrac = mean(roofFrac[ii],na.rm=T), 
			drivewayQfrac = mean(drivewayFrac[ii],na.rm=T), 
			pavedRoadQfrac = mean(pavedRoadFrac[ii],na.rm=T), 
			
            ## ... roads
            pavedRoadQ = sum(!is.na(pavedRoadFrac[ii]) & pavedRoadFrac[ii]>0), # assume does not cut watertable
            unpavedRoadQ = sum(!is.na(unpavedroad[ii])) # assume it cuts watertable
        );
    })#tapply <--- this output is a list of c() in outputOrder
    patch_info_lowest = patchInfo[[ length(patchInfo) ]] ## assume basin outlet
    patch_info_basinoutlet = patchInfo[[ basinoutlet_orderedPatch_index ]]
    patch_info_suboulet = lapply(suboutlet_orderedPatch_index, function(ii){ patchInfo[[ii]] })
    #cbind(suboutlet_orderedPatch_index_hillID, suboutlet_orderedPatch_index)


    ## part 2: sort by 'elevation' & finding neighbor
    print('starting step II')
    ## .......... Neighbour
    # ii=which(orderedPatch==24110) #500
    patchNeighbourRC_edge = tapply(fullLength, INDEX=outputOrder, FUN=function(jj){
        withinPatchGridRC = rows[jj]*maxCol+cols[jj]; # within
        
        hold = as.vector(gridSurroundRC[directEdgeIndex,jj]);
        hold[hold%in% withinPatchGridRC] = -1
        
        hold2 = as.vector(gridSurroundRC[indirectEdgeIndex,jj]);
        hold2[hold2%in% withinPatchGridRC] = -1
        
        #return <- c(table(hold[hold>0])* directEdge, table(hold2[hold2>0])* diagonalEdge)
        return <- c( tapply(hold[hold>0],hold[hold>0],length)*directEdge, tapply(hold2[hold2>0],hold2[hold2>0],length)*diagonalEdge)
        
    })

    patchNeighbourRC_LEN = seq_along(patchNeighbourRC_edge) ## <<--------------------------- ordered patch aggregated neighbours
    patchNeighbourRC = sapply(patchNeighbourRC_LEN, function(ii){
        sapply(names(patchNeighbourRC_edge[[ii]]), function(x){maskRC_string2maskRC_num[[ x ]]})
    })
    patchNeighbourPatch = sapply(patchNeighbourRC_LEN, function(ii){
        sapply(names(patchNeighbourRC_edge[[ii]]), function(x){maskRC_string2Patch_num[[ x ]]})
    })
    patchNeighbourPatchIndex = sapply(patchNeighbourRC_LEN, function(ii){
        sapply(names(patchNeighbourRC_edge[[ii]]), function(x){maskRC_string2outputOrder_num[[ x ]]})
    })


    ## .......... prefer Neighbour
    patchPreferNeighbourRC = tapply(fullLength, INDEX=outputOrder, FUN=function(ii){
        withinPatchGridRC = rows[ii]*maxCol+cols[ii]; # within
        drainTO_index = cbind(drain[ii],ii)
        hold3 = as.vector(gridSurroundRC[ drainTO_index ])
        hold3[hold3%in% withinPatchGridRC] = -1
        
        return <- sapply(names(tapply(hold3[hold3>0], hold3[hold3>0],length)),function(x){maskRC_string2maskRC_num[[ x ]]})
    })#

	
	
	
	
	
	
    ## .......... writing out flow table
    if(as.numeric(templateACTION$outputSubFlow[2])>0 ){
        subsurfaceflow_table_buff <- file(templateACTION$outputSubFlow[1],'w') # open a file connection
        cat( length(patchInfo), '\n', file=subsurfaceflow_table_buff) #,sep='\n'
    }
    if(as.numeric(templateACTION$outputSurfFlow[2])>0 ){
        surfaceflow_table_buff <- file(templateACTION$outputSurfFlow[1],'w') # open a file connection
        cat( length(patchInfo), '\n', file=surfaceflow_table_buff) #,sep='\n'
    }

    for(ii in patchNeighbourRC_LEN){
        
        withinNeighbourRC_edge = patchNeighbourRC_edge[[ii]]
        withinNeighbourRC = patchNeighbourRC[[ii]]
        withinNeighbourRC_prefer = rep(0,length(withinNeighbourRC))            
        withinNeighbourRC_prefer[withinNeighbourRC%in%patchPreferNeighbourRC[[ii]] ] = 1
        index4neighbour = patchNeighbourPatchIndex[[ii]]
        
        current_patch_info = patchInfo[[ii]]
        # 0 = land (default)
        # 1 = class::stream
        # 2 = class::road
        # 3 = actionSTORMDRAIN
        # 5 = actionGWDRAIN
        # 7 = actionRIPARIAN
        # 11 = actionSEWER
        # 13 = actionIRRIGRATION
        # 17 = actionPIPEDRAIN
        
        ## actionCode is mostly for subsurface processes; surface storm drain see below.
        # impFrac = roofFrac + drivewayFrac + pavedpavedRoadFrac
        # gw_drainage is bounded by imp
        actionCode = 	ifelse(	current_patch_info['pavedRoadQfrac']+
        						current_patch_info['roofQfrac']+
        						current_patch_info['drivewayQfrac']>=1,1,5) * # GW drain except road/roof/parkinglot
        				ifelse(current_patch_info['sewerdrainQ']>0,11,1) * # sewer drain (top 3-m)
						ifelse(current_patch_info['subsurfpipedrainQ']>0,17,1) * # subsurface pipe drain (top 1-m) along the ROAD	
        				## ...		
        				ifelse(current_patch_info['riparianQ']>0,7,1) * # riparian
        				## ...
        				ifelse(current_patch_info['irrigateQfra']>0,13,1) * # lawn irrigration & fertilizer
        				## ...
						ifelse(current_patch_info['stormdrainQ']>0,ifelse(current_patch_info['nonmodelstrgridQ']>0,1,3), 1)  # count for stream under road bridge
						
		drainage_type = ifelse(current_patch_info['strQ']>0, 1, # class::stream
						ifelse(actionCode>1, actionCode,0)
						)	
        
        
        
        neighbourLength = 1:length(withinNeighbourRC)
        neighbourOrder = match(withinNeighbourRC,unique(withinNeighbourRC))
        
        allNeighbourInfo = simplify2array(tapply(neighbourLength, INDEX=neighbourOrder, function(jj){
            ## exploring information between "current" and neighbour(jj)
            # index4neighbour[jj][1] # index of neighbour(jj) in "patchInfo" list
            # withinNeighbourRC_edge[jj] # all edges between current and neighbour(jj)
            # withinNeighbourRC_prefer[jj] # all prefers between current and neighbour(jj)
            
            neighbor_patch_info = patchInfo[[ index4neighbour[jj][1] ]];
            idiffDEM = current_patch_info['elevation']-neighbor_patch_info['elevation']
            idiffDEM = ifelse(idiffDEM<0,0, idiffDEM)
            
            return <- c(
            patchID = as.numeric(neighbor_patch_info['patchID']), #patchID, zone, hill [1,2,3]
            zoneID = as.numeric(neighbor_patch_info['zoneID']),
            hillID = as.numeric(neighbor_patch_info['hillID']),
            ## ... distance
            dist = as.numeric(sqrt((neighbor_patch_info['xx']-current_patch_info['xx'])^2 +
            (neighbor_patch_info['yy']-current_patch_info['yy'])^2)), # distance [4]
            ## ... rise
            rise = as.numeric(idiffDEM), #rise (local prefer) [5] # zero correct
            riseRegion = ifelse( mean(withinNeighbourRC_prefer[jj])>0, flatDEMdrop, 0), #rise (regional prefer) [6] # zero correct
            ## ... shared edge
            sharedEdge = sum(withinNeighbourRC_edge[jj]), #edge [7]
            ## info
            neighbor_patch_info['roofQfrac'], # roof 8
            neighbor_patch_info['pavedRoadQfrac'], # road 9
            neighbor_patch_info['drivewayQfrac'], # parking 10
            neighbor_patch_info['irrigateQfra'] # lawn 11
            )
        }))#tapply <<--- not in a right order
        
        ## local prefer
        slope_jj_l = allNeighbourInfo['rise',]/allNeighbourInfo['dist',] # rise / distance
        gamma_jj_l = slope_jj_l*allNeighbourInfo['sharedEdge',] # edge (width)
        
        ## regional prefer
        slope_jj_r = allNeighbourInfo['riseRegion',]/allNeighbourInfo['dist',] # rise / distance
        gamma_jj_r = slope_jj_r*allNeighbourInfo['sharedEdge',]
        
        cc1 = sum(gamma_jj_l)==0 # use gamma_jj_r
        cc2 = sum(gamma_jj_l < gamma_jj_r)==0     ## T: use gamma_jj_l
        cc3 = sum(gamma_jj_l) > sum(gamma_jj_r)
        if(cc1){
            gamma_jj = gamma_jj_r
            selectedFlow2neigbour = slope_jj_r>0
        }else if(cc2){
            gamma_jj = gamma_jj_l
            selectedFlow2neigbour = slope_jj_l>0
        }else if(cc3){
            gamma_jj = gamma_jj_l/sum(gamma_jj_l)*0.3 + (gamma_jj_r>0)*0.7
            selectedFlow2neigbour = slope_jj_r>0 | slope_jj_l>0
        }else{
            gamma_jj = gamma_jj_l + gamma_jj_r
            selectedFlow2neigbour = slope_jj_r>0 | slope_jj_l>0
        }
        
        ## ... neighbour gamma fraction
        neighbor_frac_gamma = gamma_jj/ifelse(sum(gamma_jj)>0,sum(gamma_jj),1)
        
        ## ... total_gamma
        total_perimeter = sum( allNeighbourInfo['sharedEdge', selectedFlow2neigbour] )
        total_gamma = sum(gamma_jj)/total_perimeter*current_patch_info['len']*cellarea; # currrent CF calculation
        if(drainage_type==1) total_gamma = current_patch_info['aveSlope']*current_patch_info['len']*cellarea; # special for stream
        
    #-------------- subsurface -------------------# within a for loop
        if(as.numeric(templateACTION$outputSubFlow[2])>0 ){
            cat(
            paste(current_patch_info[c('patchID','zoneID','hillID')], collapse=' '),
            paste(sprintf('%.1f',current_patch_info[c('rr','cc','elevation')]), collapse=' '),
            sprintf('%.2f',1.0),
            sprintf('%.2f', ifelse(!is.na(current_patch_info['roofQfrac']),current_patch_info['roofQfrac'],0)*BASEMENT_DEPTH + ifelse(!is.na(current_patch_info['pavedRoadQfrac']),current_patch_info['pavedRoadQfrac'],0)*PAVEDROAD_DEPTH + ifelse(!is.na(current_patch_info['drivewayQfrac']),current_patch_info['drivewayQfrac'],0)*PAVEDROAD_DEPTH),
            drainage_type,
            total_gamma, length(withinNeighbourRC),'\n', file=subsurfaceflow_table_buff,sep=' ')
            
            cat( paste(
            allNeighbourInfo['patchID',],
            allNeighbourInfo['zoneID',],
            allNeighbourInfo['hillID',],
            sprintf('%.5f',neighbor_frac_gamma),
            sprintf('%.2f',allNeighbourInfo['sharedEdge',]/allNeighbourInfo['dist',]),
            sprintf('%.2f',allNeighbourInfo['sharedEdge',]),sep=' '), file=subsurfaceflow_table_buff,sep='\n')
            
            # ... traditional road grid, which cannot be stream or outlet grid
            if(drainage_type==2) cat (
            patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID'],
            patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['zoneID'],
            patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['hillID'],
            #patch_info_lowest['patchID'],
            #patch_info_lowest['zoneID'],
            #patch_info_lowest['hillID'],
            roadWidth,'\n', file=subsurfaceflow_table_buff,sep=' ')  #cellsize*current_patch_info[15]
        }
        

	#-------------- surface -------------------# within a for loop
        if( (current_patch_info['roofQfrac']>0 | current_patch_info['drivewayQfrac']>0 | current_patch_info['pavedRoadQfrac']>0 | current_patch_info['nonstrsurfdrainQ']>0) & current_patch_info['strQ']==0 ){
            # debug: surface water is the "detention" in the model; not calculated by gamma/total_gamma
            # debug: road grids (by frac) has only one drain direction; storm drain will overwrite every direction;
            # debug: parking/roof has partial direction by their frac of the grid
            # this scheme (roof/parking to road and then storm) does not work in RHESSys because surface water only move by one grid per day!
            # correction: we need to teleport surface water from root/parking/road to stream (storm drain targets)
            
            # surface
            # what needs to be surface:
            # roof / parking / road on current patch
            stormsurfacedrainFrac = c(
                ifelse(current_patch_info['roofQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0,current_patch_info['roofQfrac'],0),  # roof Frac
                ifelse(current_patch_info['drivewayQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0,current_patch_info['drivewayQfrac'],0), # parking Frac
                ifelse(current_patch_info['pavedRoadQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0,current_patch_info['pavedRoadQfrac'],0),  # road Frac
                ifelse(current_patch_info['nonstrsurfdrainQ']>0 &
                    current_patch_info['nonmodelstrgridQ']==0,0.9,0) # e.g., surface drain around the roof/road/parkinglot
            ); names(stormsurfacedrainFrac) = c('roof','parking','road','extenddrain')
            if(sum(stormsurfacedrainFrac[1:3])>=1){
                stormsurfacedrainFrac[1:3] = stormsurfacedrainFrac[1:3]/sum(stormsurfacedrainFrac[1:3]);
                stormsurfacedrainFrac[4]=0;
            }else if(stormsurfacedrainFrac[4]>0 & sum(stormsurfacedrainFrac[1:4])>=1){ stormsurfacedrainFrac[4] = 1 - sum(stormsurfacedrainFrac[1:3]); }
            
            normal_neighborNum = length(neighbor_frac_gamma)
            normalFrac = 1 - sum(stormsurfacedrainFrac) - ifelse(current_patch_info['nonmodelstrgridQ']>0, 0.4,0.0);
            # current_patch_info[22] = surfaceDrain <= streamExtension
            # current_patch_info[21] = streamExtension
            if(normalFrac<0){ print(paste(ii, normalFrac)); normalFrac = 0;} #<<---- problem to check
            normal_neighbor_frac_gamma = (neighbor_frac_gamma * normalFrac)
            
            
            ## stop routing from roof to roof on surface
            normal_neighbor_frac_gammaSUM = sum(normal_neighbor_frac_gamma)
            normal_neighbor_frac_gamma = normal_neighbor_frac_gamma * (1.0 - allNeighbourInfo['roofQfrac',]) # roofFrac of neighbour patch
            if(sum(normal_neighbor_frac_gamma)>0){
                normal_neighbor_frac_gamma = normal_neighbor_frac_gamma/sum(normal_neighbor_frac_gamma)*normal_neighbor_frac_gammaSUM;
            }else{
                normal_neighbor_frac_gamma = rep(0,length(normal_neighbor_frac_gamma))
            }
            
            
            ## counting number of neighbours
            if(current_patch_info['roofQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0) normal_neighborNum = normal_neighborNum + 1; # roof
            if(current_patch_info['drivewayQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0) normal_neighborNum = normal_neighborNum + 1; # parking
            if(current_patch_info['pavedRoadQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0) normal_neighborNum = normal_neighborNum + 1; # road
            if( stormsurfacedrainFrac['extenddrain']>0 & current_patch_info['nonmodelstrgridQ']==0) normal_neighborNum = normal_neighborNum + 1; # surface drain around the roof/road/parkinglot
            if( current_patch_info['nonmodelstrgridQ']>0) normal_neighborNum = normal_neighborNum + 1; # stream ext.
            
            if(as.numeric(templateACTION$outputSurfFlow[2])>0 ){
                cat(
                paste(current_patch_info[c('patchID','zoneID','hillID')], collapse=' '),
                paste(sprintf('%.1f',current_patch_info[c('rr','cc','elevation')]), collapse=' '),
                sprintf('%.2f',1.0),
                sprintf('%.2f', ifelse(!is.na(current_patch_info['roofQfrac']),current_patch_info['roofQfrac'],0)*BASEMENT_DEPTH + ifelse(!is.na(current_patch_info['pavedRoadQfrac']),current_patch_info['pavedRoadQfrac'],0)*PAVEDROAD_DEPTH + ifelse(!is.na(current_patch_info['drivewayQfrac']),current_patch_info['drivewayQfrac'],0)*PAVEDROAD_DEPTH),
                drainage_type,
                total_gamma, normal_neighborNum, '\n', file=surfaceflow_table_buff,sep=' ')
                
                cat( paste(
                allNeighbourInfo['patchID',],
                allNeighbourInfo['zoneID',],
                allNeighbourInfo['hillID',],
                sprintf('%.5f',normal_neighbor_frac_gamma),
                sprintf('%.2f',allNeighbourInfo[7,]/allNeighbourInfo[4,]),
                sprintf('%.2f',allNeighbourInfo[7,]),sep=' '), file=surfaceflow_table_buff,sep='\n')
                
                # current_patch_info[15]>0    road / storm drain & NOT str grid & NOT strExt
                if(current_patch_info['pavedRoadQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0) cat(
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID'],
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['zoneID'],
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['hillID'],
                #patch_info_lowest['patchID'],
                #patch_info_lowest['zoneID'],
                #patch_info_lowest['hillID'],
                sprintf('%.5f', stormsurfacedrainFrac['road']),
                sprintf('%.2f',1.0),
                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                
                # current_patch_info[19]>0 roof & NOT str grid & NOT strExt
                if(current_patch_info['roofQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0) cat(
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID'],
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['zoneID'],
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['hillID'],
                #patch_info_lowest['patchID'],
                #patch_info_lowest['zoneID'],
                #patch_info_lowest['hillID'],
                sprintf('%.5f', stormsurfacedrainFrac['roof']),
                sprintf('%.2f',1.0),
                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                
                # current_patch_info[20]>0 parking & NOT str grid & NOT strExt
                if(current_patch_info['drivewayQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0) cat(
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID'],
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['zoneID'],
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['hillID'],
                #patch_info_lowest['patchID'],
                #patch_info_lowest['zoneID'],
                #patch_info_lowest['hillID'],
                sprintf('%.5f', stormsurfacedrainFrac['parking']),
                sprintf('%.2f',1.0),
                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                
                # current_patch_info[17]>0 extenddrain & NOT str grid & NOT strExt
                if(stormsurfacedrainFrac['extenddrain']>0 & current_patch_info['nonmodelstrgridQ']==0) cat(
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID'],
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['zoneID'],
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['hillID'],
                #patch_info_lowest['patchID'],
                #patch_info_lowest['zoneID'],
                #patch_info_lowest['hillID'],
                sprintf('%.5f', stormsurfacedrainFrac['extenddrain']),
                sprintf('%.2f',1.0),
                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                
                # strExt (surface) & NOT str grid @ outlet
                if(current_patch_info['nonmodelstrgridQ']>0 & current_patch_info['patchID']!=patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID'] ) cat(
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID'],
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['zoneID'],
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['hillID'],
                #patch_info_lowest['patchID'],
                #patch_info_lowest['zoneID'],
                #patch_info_lowest['hillID'],
                sprintf('%.5f', 0.4),
                sprintf('%.2f',1.0),
                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                
                # strExt (surface) & str grid @ outlet
                if(current_patch_info['nonmodelstrgridQ']>0 & current_patch_info['patchID']==patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID']) cat(
                patch_info_basinoutlet['patchID'],
                patch_info_basinoutlet['zoneID'],
                patch_info_basinoutlet['hillID'],
                sprintf('%.5f', 0.4),
                sprintf('%.2f',1.0),
                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                
            }# if
        }else{
            # same as subsurface flow
            
            normal_neighbor_frac_gamma = neighbor_frac_gamma
            
            ## stop routing from roof to roof on surface
            normal_neighbor_frac_gammaSUM = sum(normal_neighbor_frac_gamma)
            normal_neighbor_frac_gamma = normal_neighbor_frac_gamma * (1.0 - allNeighbourInfo[8,]) # roofFrac
            if(sum(normal_neighbor_frac_gamma)>0){normal_neighbor_frac_gamma = normal_neighbor_frac_gamma/sum(normal_neighbor_frac_gamma)*normal_neighbor_frac_gammaSUM;}else{ normal_neighbor_frac_gamma = rep(0,length(normal_neighbor_frac_gamma)) }
            
            if(as.numeric(templateACTION$outputSurfFlow[2])>0 ){
                cat(
                paste(current_patch_info[c('patchID','zoneID','hillID')], collapse=' '),
                paste(sprintf('%.1f',current_patch_info[c('rr','cc','elevation')]), collapse=' '),
                sprintf('%.2f',1.0),
                sprintf('%.2f', ifelse(!is.na(current_patch_info['roofQfrac']),current_patch_info['roofQfrac'],0)*BASEMENT_DEPTH + ifelse(!is.na(current_patch_info['pavedRoadQfrac']),current_patch_info['pavedRoadQfrac'],0)*PAVEDROAD_DEPTH + ifelse(!is.na(current_patch_info['drivewayQfrac']),current_patch_info['drivewayQfrac'],0)*PAVEDROAD_DEPTH),
                drainage_type,
                total_gamma,length(withinNeighbourRC),'\n', file=surfaceflow_table_buff,sep=' ')
                
                cat( paste(
                allNeighbourInfo['patchID',],
                allNeighbourInfo['zoneID',],
                allNeighbourInfo['hillID',],
                sprintf('%.5f', normal_neighbor_frac_gamma),
                sprintf('%.2f',allNeighbourInfo[7,]/allNeighbourInfo[4,]),
                sprintf('%.2f',allNeighbourInfo[7,]),sep=' '), file=surfaceflow_table_buff,sep='\n')
            }#if
        }# else
    }# for loop ii

    if(as.numeric(templateACTION$outputSubFlow[2])>0 ){
       close(subsurfaceflow_table_buff)
    }
    if(as.numeric(templateACTION$outputSurfFlow[2])>0 ){
        close(surfaceflow_table_buff)
    }
	
	
	
	
	
	
	
	
	
	
	
	
	
	
