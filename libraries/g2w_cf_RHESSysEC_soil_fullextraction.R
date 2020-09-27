## single basin
## sub-patch/grid setting
#----------------------------------------------------------------------------------------------
source('https://raw.githubusercontent.com/laurencelin/Date_analysis/master/LIB_misc.r')
	options(scipen=999)
    arg = commandArgs(T)

    DtoR = pi/180
	RtoD = 1/DtoR
    BASEMENT_DEPTH = 3 #meter
    PAVEDROAD_DEPTH = 0 #0.3 #meter

    # initial worldfile write out
    defaultWorldName = c('world_ID')
	defaultBasinName = c('basin_ID','x','y','z','latitude')
	defaultHillName = c('hillslope_ID','x','y','z')
	defaultZoneName = c('zone_ID','x','y','z','area','slope','aspect','precip_lapse_rate','e_horizon','w_horizon','base_station_ID')
	defaultPatchName = c('patch_ID','x','y','z','soil_parm_ID','landuse_parm_ID','area','slope','lna','Ksat_vertical')

	defaultStratumName = c(
        'canopy_strata_ID',
        'veg_parm_ID',
        'cover_fraction',
        'gap_fraction',
        'rootzone.depth',
        'snow_stored',
        'rain_stored',
        'cs.cpool',
        'cs.leafc','cs.dead_leafc','cs.leafc_store','cs.leafc_transfer',
        'cs.live_stemc','cs.livestemc_store','cs.livestemc_transfer',
        'cs.dead_stemc','cs.deadstemc_store','cs.deadstemc_transfer',
        'cs.live_crootc','cs.livecrootc_store','cs.livecrootc_transfer',
        'cs.dead_crootc','cs.deadcrootc_store','cs.deadcrootc_transfer',
        'cs.frootc','cs.frootc_store','cs.frootc_transfer',
        'cs.cwdc',
        'epv.prev_leafcalloc',
        'ns.npool',
        'ns.leafn','ns.dead_leafn','ns.leafn_store','ns.leafn_transfer',
        'ns.live_stemn','ns.livestemn_store','ns.livestemn_transfer',
        'ns.dead_stemn','ns.deadstemn_store','ns.deadstemn_transfer',
        'ns.live_crootn','ns.livecrootn_store','ns.livecrootn_transfer',
        'ns.dead_crootn','ns.deadcrootn_store','ns.deadcrootn_transfer',
        'ns.frootn','ns.frootn_store','ns.frootn_transfer',
        'ns.cwdn',
        'ns.retransn',
        'epv.wstress_days',
        'epv.max_fparabs',
        'epv.min_vwc')
	#title = c(defaultWorldName, defaultBasinName, defaultHillName, defaultZoneName, defaultPatchName, defaultStratumName)
	

#----------------------------------------------------------------------------------------------

    library(sp)
    library(XML)
    library(rgrass7)
    library(rgdal)
    tryCatch({ use_sp() },error=function(cond){message(cond)},warning=function(cond){message(cond)},finally={message("Please update the rgrass7 package on R")})
    gis = gmeta()
    gridarea = round(gis$nsres * gis$ewres)

    #read template
    # (this part can be read in from stationList.csv)
    # https://stackoverflow.com/questions/17288197/reading-a-csv-file-organized-horizontally
    # modified by Lin June 23 2018
    read.tcsv = function(file, header=T, sep=" ", vskip=0, hskip=0, len=-1, ncolReadin=0, ncolReadout=0, ...) {
        # x = readLines(file)
        # .splitvar = function(x, sep, n) {
        #     var = unlist(strsplit(x, split=sep))
        #     length(var) = n
        #     return(var)
        # }#function
        # x = do.call(cbind, lapply(x[(vskip+1):endline], .splitvar, sep=sep, n=n))
        # x = apply(x[(hskip+1):dim(x)[1],], 1, paste, collapse=sep)
        # out = read.csv(text=x, sep=sep, header=header, skip=0, ...)
        if(ncolReadin>0){
            x = read.table(file, sep=sep, fill=T, stringsAsFactors=F, col.names=seq_len(ncolReadin));
            n = ncolReadin;
        }else{
            x = read.table(file, sep=sep, fill=T, stringsAsFactors=F);
            rowCountFields = count.fields(file, sep=sep);
            if(vskip>0) rowCountFields = rowCountFields[-1:-vskip];
            n = max( rowCountFields, na.rm=TRUE);
        }
        if(ncolReadout>0){
            n = min(hskip+1+ncolReadout,n)
        }
        endline = dim(x)[1]
        if(len>0) endline = vskip+len
        transformedX = sapply( (hskip+1):n, function(ii){
        	paste( x[(vskip+1):endline, ii], collapse=',')
        });
        out = read.csv(text=transformedX, sep=',', header=header, skip=0, ...)
        
        return(out)
    }#function
    print(arg)
    
    headerEndLine = max(grep('projdir|output|Collection',readLines(arg[1]))); print(headerEndLine)
    templateACTION = read.tcsv(ifelse(length(arg)==5,arg[5],arg[1]),stringsAsFactors=F, len=headerEndLine,ncolReadin=3);
    template = read.tcsv(ifelse(length(arg)==5,arg[5],arg[1]),stringsAsFactors=F, vskip=headerEndLine, ncolReadin=3, ncolReadout=1);

    projectFolder = ifelse(length(arg)==5, arg[1], templateACTION$projdir[1])
    climateStationID = as.numeric(template$stationID[1])
	climateStationNAME = template$stationFile[1] # prefix-like (single station)
	
		
	## user provides a customized vegetation.csv containing all vegetation vegParameters.
    if(length(arg)==5){
        ParamFileName= ifelse(arg[2]=='default'|arg[2]=='NA'|arg[2]=='na'|arg[2]=='.','https://raw.githubusercontent.com/laurencelin/GIS2RHESSys/master/vegCollection.csv', arg[2])
    }else{
        ParamFileName = templateACTION$vegCollection[1];
    }
	vegParam = read.csv(ParamFileName,skip=4,header=T,stringsAsFactors=F) #<<------
	vegParamCOL = cbind(as.numeric(unique(vegParam[1,3:ncol(vegParam)])), 3:ncol(vegParam)); 
	colnames(vegParamCOL) = c('vegID','vegDefIndex')
	vegParam_len = ncol(vegParam)
	

    if(length(arg)==5){
        ParamFileName= ifelse(arg[3]=='default'|arg[3]=='NA'|arg[3]=='na'|arg[3]=='.','https://raw.githubusercontent.com/laurencelin/GIS2RHESSys/master/soilCollection.csv', arg[3])
    }else{
        ParamFileName = templateACTION$soilCollection[1];
    }
	soilParam = read.csv(ParamFileName,skip=4,header=T,stringsAsFactors=F) #<<------
	soilParamCOL = cbind(as.numeric(unique(soilParam[1,3:ncol(soilParam)])), 3:ncol(soilParam)); 
	colnames(soilParamCOL) = c('soilID','soilDefIndex')
	soilParam_len = ncol(soilParam)
	
    if(length(arg)==5){
        ParamFileName= ifelse(arg[4]=='default'|arg[4]=='NA'|arg[4]=='na'|arg[4]=='.','https://raw.githubusercontent.com/laurencelin/GIS2RHESSys/master/lulcCollectionEC.csv', arg[4])
    }else{
        ParamFileName = templateACTION$lulcCollection[1];
    }
	lulcParam = read.csv(ParamFileName,skip=4,header=T,stringsAsFactors=F) #<<------
	lulcParamCOL = cbind(as.numeric(unique(lulcParam[1,3:ncol(lulcParam)])), 3:ncol(lulcParam)); 
	colnames(lulcParamCOL) = c('lulcID','lulcDefIndex')
	lulcParam_len = ncol(lulcParam)


    ParamFileName = ifelse(is.null(templateACTION$hillCollection[1]),'https://raw.githubusercontent.com/laurencelin/GIS2RHESSys/master/hillCollection.csv',templateACTION$hillCollection[1])
    hillParam = read.csv(ParamFileName,skip=4,header=T,stringsAsFactors=F) #<<------
    hillParamCOL = cbind(as.numeric(unique(hillParam[1,3:ncol(hillParam)])), 3:ncol(hillParam));
    colnames(hillParamCOL) = c('hillID','hillDefIndex')
    hillParam_len = ncol(hillParam)

    ParamFileName = ifelse(is.null(templateACTION$zoneCollection[1]),'https://raw.githubusercontent.com/laurencelin/GIS2RHESSys/master/zoneCollection.csv',templateACTION$zoneCollection[1])
    zoneParam = read.csv(ParamFileName,skip=4,header=T,stringsAsFactors=F) #<<------
    zoneParamCOL = cbind(as.numeric(unique(zoneParam[1,3:ncol(zoneParam)])), 3:ncol(zoneParam));
    colnames(zoneParamCOL) = c('zoneID','zoneDefIndex')
    zoneParam_len = ncol(zoneParam)
	
#----------------------------------------------------------------------------------------------

    ## spatial structures (must have)
	rast = readRAST(c(template$basinMap, template$hillslopeMap, template$zoneMAP, template$patchMAP),NODATA=0)
		mask = !is.na(rast@data[[1]])
		hill = rast@data[[2]][mask]
		zone = rast@data[[3]][mask]
		patch = rast@data[[4]][mask]
        emptyMAP = rep(NA,sum(mask))
	print('reading basin, hill, zone, patch ... DONE')
	
	##--------- climate station (assume one zone for one station)
	if(is.na(climateStationID)){
        # climateStationID = as.numeric(template$stationID[1]) above; if it's not a single integer, then it's NA
        rast = readRAST(template$stationID[1])
		climateStationID = rast@data[[1]][mask]
        print(paste('multiple base stations',template$stationID[1],'...DONE'))
	}else{
		climateStationID = rep(as.numeric(template$stationID[1]), sum(mask))
	}#if else
	
    ## spatial aggregation for outputs
    spatialAGG=NULL;tryCatch({
        tmpnum = as.numeric(template$spatialAGG)
        if(is.na(tmpnum)){
            rast = readRAST(template$spatialAGG);
            spatialAGG <- rast@data[[1]][mask]
            print("it is on spatialAGG")
        }else{
            spatialAGG <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ spatialAGG <<- emptyMAP }
    )#tryCatch

    treeRootzScaler=NULL;tryCatch({
        tmpnum = as.numeric(template$treeRootzScaler)
        if(is.na(tmpnum)){
            rast = readRAST(template$treeRootzScaler);
            treeRootzScaler <- rast@data[[1]][mask]
        }else{
            treeRootzScaler <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ treeRootzScaler <<- emptyMAP }
    )#tryCatch

    ## GW
    hillDefID=NULL;tryCatch({
           tmpnum = as.numeric(template$hillDefID)
           if(is.na(tmpnum)){
               rast = readRAST(template$hillDefID);
               hillDefID <- tapply(rast@data[[1]][mask],INDEX=hill, FUN=most)
           }else{
               hillDefID <- rep(tmpnum, length(unique(hill)))
           }},
           error = function(e){ }
           )#tryCatch
    hillGWiniQ=NULL;tryCatch({
        tmpnum = as.numeric(template$GWiniStorage)
        if(is.na(tmpnum)){
            rast = readRAST(template$GWiniStorage);
            hillGWiniQ <- tapply(rast@data[[1]][mask],INDEX=hill, FUN=mean)
        }else{
            hillGWiniQ <- rep(tmpnum, length(unique(hill)))
        }},
        error = function(e){ }
        )#tryCatch
    hillGWiniNO3=NULL;tryCatch({
        tmpnum = as.numeric(template$GWiniNO3)
        if(is.na(tmpnum)){
            rast = readRAST(template$GWiniStorage);
            hillGWiniNO3 <- tapply(rast@data[[1]][mask],INDEX=hill, FUN=mean)
        }else{
            hillGWiniNO3 <- rep(tmpnum, length(unique(hill)))
        }},
        error = function(e){ }
        )#tryCatch
    hillGWiniNH4=NULL;tryCatch({
        tmpnum = as.numeric(template$GWiniNH4)
        if(is.na(tmpnum)){
            rast = readRAST(template$GWiniStorage);
            hillGWiniNH4 <- tapply(rast@data[[1]][mask],INDEX=hill, FUN=mean)
        }else{
            hillGWiniNH4 <- rep(tmpnum, length(unique(hill)))
        }},
        error = function(e){ }
        )#tryCatch
    hillGWiniDOC=NULL;tryCatch({
        tmpnum = as.numeric(template$GWiniDOC)
        if(is.na(tmpnum)){
            rast = readRAST(template$GWiniStorage);
            hillGWiniDOC <- tapply(rast@data[[1]][mask],INDEX=hill, FUN=mean)
        }else{
            hillGWiniDOC <- rep(tmpnum, length(unique(hill)))
        }},
        error = function(e){ }
        )#tryCatch
    hillGWiniDON=NULL;tryCatch({
        tmpnum = as.numeric(template$GWiniDON)
        if(is.na(tmpnum)){
            rast = readRAST(template$GWiniStorage);
            hillGWiniDON <- tapply(rast@data[[1]][mask],INDEX=hill, FUN=mean)
        }else{
            hillGWiniDON <- rep(tmpnum, length(unique(hill)))
        }},
        error = function(e){ }
        )#tryCatch
    print('reading hillGWiniQ, hillGWiniNO3, hillGWiniNH4, hillGWiniDOC, hillGWiniDON ... DONE')
	


    zoneDefID=NULL;tryCatch({
        tmpnum = as.numeric(template$zoneDefID)
        if(is.na(tmpnum)){
            rast = readRAST(template$zoneDefID);
            zoneDefID <- tapply(rast@data[[1]][mask],INDEX=zone, FUN=most)
        }else{
            zoneDefID <- rep(tmpnum, length(unique(zone)))
        }},
        error = function(e){ }
        )#tryCatch

    
	# extract soil def IDs; how to handle catchments that have no data?
    rast = readRAST(template$soiltexture,NODATA=0)
        soiltexture = rast@data[[1]][mask]
        
    soil_extID=NULL;tryCatch({
        tmpnum = as.numeric(template$soilidMAP)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilidMAP);
            soil_extID <- rast@data[[1]][mask]
        }else{
            soil_extID <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soil_extID <<- soiltexture }
        )#tryCatch
    soilksat0=NULL;tryCatch({
        tmpnum = as.numeric(template$soilksat0)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilksat0);
            soilksat0 <- rast@data[[1]][mask]
        }else{
            soilksat0 <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilksat0 <<- emptyMAP }
        )#tryCatch
    soilksatdecay=NULL;tryCatch({
        tmpnum = as.numeric(template$soilksatdecay)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilksatdecay);
            soilksatdecay <- rast@data[[1]][mask]
        }else{
            soilksatdecay <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilksatdecay <<- emptyMAP }
        )#tryCatch
    soilpor0=NULL;tryCatch({
        tmpnum = as.numeric(template$soilpor0)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilpor0);
            soilpor0 <- rast@data[[1]][mask]
        }else{
            soilpor0 <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilpor0 <<- emptyMAP }
        )#tryCatch
    soilpordecay=NULL;tryCatch({
        tmpnum = as.numeric(template$soilpordecay)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilpordecay);
            soilpordecay <- rast@data[[1]][mask]
        }else{
            soilpordecay <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilpordecay <<- emptyMAP }
        )#tryCatch
    soilsand=NULL;tryCatch({
        tmpnum = as.numeric(template$soilsand)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilsand);
            soilsand <- rast@data[[1]][mask]
        }else{
            soilsand <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilsand <<- emptyMAP }
        )#tryCatch
    soilsilt=NULL;tryCatch({
        tmpnum = as.numeric(template$soilsilt)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilsilt);
            soilsilt <- rast@data[[1]][mask]
        }else{
            soilsilt <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilsilt <<- emptyMAP }
        )#tryCatch
    soilclay=NULL;tryCatch({
        tmpnum = as.numeric(template$soilclay)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilclay);
            soilclay <- rast@data[[1]][mask]
        }else{
            soilclay <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilclay <<- emptyMAP }
        )#tryCatch
    soilbulkdensity=NULL;tryCatch({
        tmpnum = as.numeric(template$soilbulkdensity)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilbulkdensity);
            soilbulkdensity <- rast@data[[1]][mask]
        }else{
            soilbulkdensity <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilbulkdensity <<- emptyMAP }
        )#tryCatch
    soilparticledensity=NULL;tryCatch({
        tmpnum = as.numeric(template$soilparticledensity)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilparticledensity);
            soilparticledensity <- rast@data[[1]][mask]
        }else{
            soilparticledensity <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilparticledensity <<- emptyMAP }
        )#tryCatch
    soilsoildepth=NULL;tryCatch({
        tmpnum = as.numeric(template$soilsoildepth)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilsoildepth);
            soilsoildepth <- rast@data[[1]][mask]
        }else{
            soilsoildepth <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilsoildepth <<- emptyMAP }
        )#tryCatch
    soilactivedepth=NULL;tryCatch({
        tmpnum = as.numeric(template$soilactivedepth)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilactivedepth);
            soilactivedepth <- rast@data[[1]][mask]
        }else{
            soilactivedepth <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilactivedepth <<- emptyMAP }
        )#tryCatch
    soilmaxrootdepth=NULL;tryCatch({
        tmpnum = as.numeric(template$soilmaxrootdepth)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilmaxrootdepth);
            soilmaxrootdepth <- rast@data[[1]][mask]
        }else{
            soilmaxrootdepth <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilmaxrootdepth <<- emptyMAP }
        )#tryCatch
    soilalbedo=NULL;tryCatch({
        tmpnum = as.numeric(template$soilalbedo)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilalbedo);
            soilalbedo <- rast@data[[1]][mask]
        }else{
            soilalbedo <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilalbedo <<- emptyMAP }
        )#tryCatch
    soilpor_size_index=NULL;tryCatch({
        tmpnum = as.numeric(template$soilpor_size_index)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilpor_size_index);
            soilpor_size_index <- rast@data[[1]][mask]
        }else{
            soilpor_size_index <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilpor_size_index <<- emptyMAP }
        )#tryCatch
    soilpsi_air_entry=NULL;tryCatch({
        tmpnum = as.numeric(template$soilpsi_air_entry)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilpsi_air_entry);
            soilpsi_air_entry <- rast@data[[1]][mask]
        }else{
            soilpsi_air_entry <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilpsi_air_entry <<- emptyMAP }
        )#tryCatch
    soilsoilc=NULL;tryCatch({
        tmpnum = as.numeric(template$soilsoilc)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilsoilc);
            soilsoilc <- rast@data[[1]][mask]
        }else{
            soilsoilc <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilsoilc <<- emptyMAP }
        )#tryCatch
    soilomdecay=NULL;tryCatch({
        tmpnum = as.numeric(template$soilomdecay)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilomdecay);
            soilomdecay <- rast@data[[1]][mask]
        }else{
            soilomdecay <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilomdecay <<- emptyMAP }
        )#tryCatch

    soilpH=NULL;tryCatch({
        tmpnum = as.numeric(template$soilpH)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilpH);
            soilpH <- rast@data[[1]][mask]
        }else{
            soilpH <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soilpH <<- emptyMAP }
        )#tryCatch
    soil_theta_mean_std_p1=NULL;tryCatch({
        tmpnum = as.numeric(template$soil_theta_mean_std_p1)
        if(is.na(tmpnum)){
            rast = readRAST(template$soil_theta_mean_std_p1);
            soil_theta_mean_std_p1 <- rast@data[[1]][mask]
        }else{
            soil_theta_mean_std_p1 <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soil_theta_mean_std_p1 <<- emptyMAP }
        )#tryCatch
    soil_theta_mean_std_p2=NULL;tryCatch({
        tmpnum = as.numeric(template$soil_theta_mean_std_p2)
        if(is.na(tmpnum)){
            rast = readRAST(template$soil_theta_mean_std_p2);
            soil_theta_mean_std_p2 <- rast@data[[1]][mask]
        }else{
            soil_theta_mean_std_p2 <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ soil_theta_mean_std_p2 <<- emptyMAP }
        )#tryCatch

    soilLegecyCNScaler=NULL;tryCatch({
        tmpnum = as.numeric(template$soilLegecyCNScaler)
        if(is.na(tmpnum)){
            rast = readRAST(template$soilLegecyCNScaler);
            soilLegecyCNScaler <- tapply(rast@data[[1]][mask],INDEX=patch, FUN=mean)
        }else{
            soilLegecyCNScaler <- rep(tmpnum, length(unique(patch)))
        }},
        error = function(e){ }
        )#tryCatch
    print('reading soils ... DONE')

    # extract RHESSys LULC IDs
    LULCID=NULL;tryCatch({
        tmpnum = as.numeric(template$LULCidMAP)
        if(is.na(tmpnum)){
            rast = readRAST(template$LULCidMAP);
            LULCID <- rast@data[[1]][mask]
        }else{
            LULCID <- rep(tmpnum, sum(mask))
        }},
        error = function(e){ LULCID <<- emptyMAP }
        )#tryCatch


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
    impFrac=NULL;tryCatch({
        rast = readRAST(template$impFracMAP);
        impFrac <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ impFrac <<- emptyMAP }
        )#tryCatch
    forestFrac=NULL;tryCatch({
        rast = readRAST(template$forestFracMAP);
        forestFrac <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ forestFrac <<- emptyMAP }
        )#tryCatch
    shrubFrac=NULL;tryCatch({
        rast = readRAST(template$shrubFracMAP);
        shrubFrac <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ shrubFrac <<- emptyMAP }
        )#tryCatch
    cropFrac=NULL;tryCatch({
        rast = readRAST(template$cropFracMAP);
        cropFrac <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ cropFrac <<- emptyMAP }
        )#tryCatch
    lawnFrac=NULL;tryCatch({
        rast = readRAST(template$grassFracMAP);
        lawnFrac <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ lawnFrac <<- emptyMAP }
        )#tryCatch

    # extract other LULC information -- surface routing -- stream extension
    stream=NULL;tryCatch({
        rast = readRAST(template$streamMap);
        stream <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ stream <<- emptyMAP }
        )#tryCatch
    fullstreamExt=NULL; tryCatch({
        rast = readRAST(template$streamFullextension);
        fullstreamExt <- rast@data[[1]][mask];
        print('reading strExtension ... DONE')},
        error = function(e){ fullstreamExt <<- stream }
        )#tryCatch
    waterFrac=NULL;tryCatch({
        rast = readRAST(template$waterFracMAP);
        waterFrac <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ waterFrac <<- emptyMAP }
        )#tryCatch

    # extract other LULC information -- surface routing -- road storm drainage
    otherImpFrac=NULL;tryCatch({
        rast = readRAST(template$otherImpMAP);
        otherImpFrac <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ otherImpFrac <<- emptyMAP }
        )#tryCatch
    pavedRoadFrac=NULL;tryCatch({
        rast = readRAST(template$pavedRoadFracMAP);
        pavedRoadFrac <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ pavedRoadFrac <<- emptyMAP }
        )#tryCatch
    onRoadDraingeDir=NULL;tryCatch({
        rast = readRAST(template$onRoadDraingeDir);
        onRoadDraingeDir <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ onRoadDraingeDir <<- emptyMAP }
        )#tryCatch
    roadStormDrainInlet=NULL; tryCatch({
        rast = readRAST(template$roadStormDrainInlet);
        roadStormDrainInlet <- rast@data[[1]][mask];
        print('reading roadStormDrainInlet ... DONE')},
        error = function(e){ roadStormDrainInlet <<- emptyMAP }
        )#tryCatch
    roadStormDrainOutlet=NULL; tryCatch({
        rast = readRAST(template$roadStormDrainOutlet);
        roadStormDrainOutlet <- rast@data[[1]][mask];
        print('reading roadStormDrainInlet ... DONE')},
        error = function(e){ roadStormDrainOutlet <<- emptyMAP }
        )#tryCatch

    # extract other LULC information -- surface routing -- surface drainage via pipes
    additionalSurfaceDrainInlet=NULL; tryCatch({
        rast = readRAST(template$additionalSurfaceDrainInletMAP);
        additionalSurfaceDrainInlet <- rast@data[[1]][mask];
        print('reading additional surf drinage ... DONE')},
        error = function(e){ additionalSurfaceDrainInlet <<- emptyMAP }
        )#tryCatch
    additionalSurfaceDrainOutlet=NULL; tryCatch({
        rast = readRAST(template$additionalSurfaceDrainOutlet);
        additionalSurfaceDrainOutlet <- rast@data[[1]][mask];
        print('reading additional surf drinage ... DONE')},
        error = function(e){ additionalSurfaceDrainOutlet <<- emptyMAP }
        )#tryCatch

    # extract other LULC information -- surface routing -- housing & drive way
    roofFrac=NULL;tryCatch({
        rast = readRAST(template$roofMAP);
        roofFrac <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ roofFrac <<- emptyMAP }
        )#tryCatch
    roofDrainDir=NULL;tryCatch({
        rast = readRAST(template$roofDrainMAP);
        roofDrainDir <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ roofDrainDir <<- emptyMAP }
        )#tryCatch
    roofDrainInlet=NULL;tryCatch({
        rast = readRAST(template$roofRedirectedInlet);
        roofDrainInlet <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ roofDrainInlet <<- emptyMAP }
        )#tryCatch
    roofDrainOutlet=NULL;tryCatch({
        rast = readRAST(template$roofDrainOutlet);
        roofDrainOutlet <- rast@data[[1]][mask];
        print('reading str ... DONE')},
        error = function(e){ roofDrainOutlet <<- emptyMAP }
        )#tryCatch

    # extract other LULC information -- subsurface drainages -- pipelines and sewers
    sewercover=NULL; tryCatch({
        rast = readRAST(template$sewercoverMAP);
        sewercover <- rast@data[[1]][mask];
        print('reading sewer ... DONE')},
        error = function(e){ sewercover <<- emptyMAP }
        )#tryCatch
    pipecover=NULL; tryCatch({
        rast = readRAST(template$pipecoverMAP);
        pipecover <- rast@data[[1]][mask];
        print('reading pipe ... DONE')},
        error = function(e){ pipecover <<- emptyMAP }
        )#tryCatch

    # extract other LULC information -- subsurface drainages -- interrcepts
    unpavedroad=NULL; tryCatch({
        rast = readRAST(template$unpavedRoadMap);
        unpavedroad <- rast@data[[1]][mask];
        print('reading unpavedRoad ... DONE')},
        error = function(e){ unpavedroad <<- emptyMAP }
        )#tryCatch
    riparian=NULL; tryCatch({
        rast = readRAST(template$riparianMAP);
        riparian <- rast@data[[1]][mask];
        print('reading riparian ... DONE')},
        error = function(e){ riparian <<- emptyMAP }
        )#tryCatch
    basement=NULL; tryCatch({
        rast = readRAST(template$basementFracMAP);
        basement <- rast@data[[1]][mask];
        print('reading riparian ... DONE')},
        error = function(e){ basement <<- emptyMAP }
        )#tryCatch


    septic=NULL; tryCatch({
        rast = readRAST(template$septicMAP);
        septic <- rast@data[[1]][mask];
        print('reading septic ... DONE')},
        error = function(e){ septic <<- emptyMAP }
        )#tryCatch 
    irrigationArea=NULL;tryCatch({
        rast = readRAST(template$irrigationMAP);
        irrigationArea <- rast@data[[1]][mask];
        print('reading irrigationArea ... DONE')},
        error = function(e){ irrigationArea <<- emptyMAP }
        )#tryCatch 
    
    print('reading lulc ... DONE')

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


    PRINT_patch=NULL; tryCatch({
        rast = readRAST(template$PRINT_patchMAP);
        PRINT_patch <- rast@data[[1]][mask];
        print('reading PRINT_patch ... DONE')},
        error = function(e){ PRINT_patch <<- emptyMAP }
        )#tryCatch


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
	zoneStationID = tapply(climateStationID,INDEX=zone, FUN=mean)

	# cbind(zoneAspect,aspect,slope)[80:100,]
	# plot(aspect, zoneAspect); abline(a=0,b=1,lty=2)
	
	
	## patch (patch >= grid)
	patchX = tapply(xx,INDEX=patch, FUN=mean)
	patchY = tapply(yy,INDEX=patch, FUN=mean)
	patchZ = tapply(dem,INDEX=patch, FUN=mean)
	patchCount = tapply(rep(1,len),INDEX=patch, FUN=sum)
	patchArea = patchCount* gridarea

    # for every patch, find the dominated soil class / soil_cat
    patchSOIL_info = do.call(rbind,tapply(seq_along(patch),INDEX=patch, FUN=function(ii){
        hold=table(soiltexture[ii]);
        holdII=table(soil_extID[ii])
        return <- c(
        as.numeric(names(hold)[which.max(hold)]), # soil texture,
        as.numeric(names(holdII)[which.max(holdII)]) # soilPextID
        )
    }))
    patchSOIL = patchSOIL_info[,2] # soil_extID
    # pull the dominated soil_extID information together
    ssurgo_info = do.call(rbind,tapply(seq_along(patch), INDEX=soil_extID, FUN=function(ii){
        return <- c(
        soil_extID = soil_extID[ii][1], # soil_cat
        soilksat0 = mean(soilksat0[ii]),
        soilksatdecay = mean(soilksatdecay[ii]),
        soilpor0 = mean(soilpor0[ii]),
        soilpordecay = mean(soilpordecay[ii]),
        soilsand = mean(soilsand[ii]),
        soilsilt = mean(soilsilt[ii]),
        soilclay = mean(soilclay[ii]),
        soilbulkdensity = mean(soilbulkdensity[ii]),
        soilparticledensity = mean(soilparticledensity[ii]),
        soilsoildepth = mean(soilsoildepth[ii]),
        soilactivedepth = mean(soilactivedepth[ii]),
        soilmaxrootdepth = mean(soilmaxrootdepth[ii]),
        soilalbedo = mean(soilalbedo[ii]),
        soilpor_size_index = mean(soilpor_size_index[ii]),
        soilpsi_air_entry = mean(soilpsi_air_entry[ii]),
        soilsoilc = mean(soilsoilc[ii]),
        soilomdecay = mean(soilomdecay[ii]),
        soilph = mean(soilpH[ii]),
        soilthetap1 = most(soil_theta_mean_std_p1[ii]),
        soilthetap2 = most(soil_theta_mean_std_p2[ii])
        )
    }));


    
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
	#landuseClass = c(2,2,1,1,3) # RHESSys def (update RHESSys LULC! need these at all? detention size, fertilizer?, what exact does lulc class do? )
	
		## scanning each grid within a patch and forming within patch configuration using the subgrid information
	subGrid_buff = 'patchID frac lai vegID rootz land imp'
	patchVegnum = tapply(1:sum(mask),INDEX=tapplyOrder, FUN=function(x){
            # 7 -> x= 9029
            #patchSTR = sum(fullstreamExt[x], na.rm=T); # patchSTR==0 --> no canopy on all stream extension!
            patchSTR = sum(stream[x], na.rm=T)/length(x)
            patchWATER = sum(waterFrac[x], na.rm=T)/length(x) # include any water surface and potential non-vegetated channel
            patchTreeRootzScaler = sum(treeRootzScaler[x],na.rm=T)/length(x); if(patchTreeRootzScaler<=0) patchTreeRootzScaler = 1.0;
			subGridAssignment[1,] = c(
                ifelse(patchSTR==0,1,1-patchWATER)*sum(forestFrac[x], na.rm=T)/length(x), #1 tree
                ifelse(patchSTR==0,1,1-patchWATER)*sum(shrubFrac[x], na.rm=T)/length(x),  #2 shrub
                ifelse(patchSTR==0,1,1-patchWATER)*sum(cropFrac[x], na.rm=T)/length(x),   #3 crop
                ifelse(patchSTR==0,1,1-patchWATER)*sum(lawnFrac[x], na.rm=T)/length(x),   #4 lawn
                ifelse(patchSTR==0,1,1-patchWATER)*sum(impFrac[x], na.rm=T)/length(x)     #5 no veg
				)
			if(is.na(subGridAssignment[1,1]) ) subGridAssignment[1,1]=0
			if(is.na(subGridAssignment[1,2]) ) subGridAssignment[1,2]=0
			if(is.na(subGridAssignment[1,3]) ) subGridAssignment[1,3]=0
            if(is.na(subGridAssignment[1,4]) ) subGridAssignment[1,4]=0
            if(is.na(subGridAssignment[1,5]) ) subGridAssignment[1,5]=0
			
            land_ = table(LULCID[x]); if(length(land_)==0){ land_ = 11; names(land_)=11; }
            land = as.numeric(names(land_)[which.max(land_)])
            if(sum(PRINT_patch[x],na.rm=T)>0){ land = land + 1000} ## here
                
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
						paste(patch[x][1], FFraclist[i], LAIlist[i], vegIDlist[i], patchTreeRootzScaler, land,imp, sep=' ')
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
                        paste(patch[x][1], FFraclist[i], LAIlist[i], vegIDlist[i], patchTreeRootzScaler, land,imp, sep=' ')
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
	title = c(defaultWorldName, defaultBasinName, defaultHillName,
        switch( is.null(hillDefID)+1,'hill_parm_ID',NULL),
        switch( is.null(hillGWiniQ)+1,'gw.storage',NULL),
        switch( is.null(hillGWiniNO3)+1,'gw.NO3',NULL),
        switch( is.null(hillGWiniNH4)+1,'gw.NH4',NULL),
        switch( is.null(hillGWiniDOC)+1,'gw.DOC',NULL),
        switch( is.null(hillGWiniDON)+1,'gw.DON',NULL),
        defaultZoneName,
        switch( is.null(zoneDefID)+1,'zone_parm_ID',NULL),
        defaultPatchName,
        switch( is.null(soilLegecyCNScaler)+1,'legacySoilCNScaler',NULL),
        defaultStratumName)
	write( title, outWorldFilePath, ncolumns=length(title), append=F, sep=',')
		
		
	WorldBasinColumn = rep(1, sum(patchVegnum)) %o% c(1,1, mean(xx), mean(yy), mean(dem), latitude)
	
	hillColumn = cbind(
        hillID,hillX, hillY, hillZ,
        switch( is.null(hillDefID)+1,hillDefID,NULL),
        switch( is.null(hillGWiniQ)+1,hillGWiniQ,NULL),
        switch( is.null(hillGWiniNO3)+1,hillGWiniNO3,NULL),
        switch( is.null(hillGWiniNH4)+1,hillGWiniNH4,NULL),
        switch( is.null(hillGWiniDOC)+1,hillGWiniDOC,NULL),
        switch( is.null(hillGWiniDON)+1,hillGWiniDON,NULL)
        #rep(1, numhill) %o% c(1,0,0,0)
		)[rep(hillIDrhessysOrder, times=patchVegnum),]## hill
   
        
	zoneColumn = cbind(
		zoneID, zoneX, zoneY, zoneZ, 
        #rep(1, numzone), ##<<---- zone def ID
		zoneArea, 
		zoneSlope, 
		zoneAspect,
		zoneISOHYET, 
		zoneEast, 
		zoneWest,
        #rep(1, numzone), #<<---- number of base station
		zoneStationID,  #<<---- base station ID -> id of .base file
        switch( is.null(zoneDefID)+1,zoneDefID,NULL)
		#rep(1, numzone) %o% c(1, climateStationID) 
		)[rep(zoneIDrhessysOrder, times=patchVegnum),]## zone
	
	patchColumn1 = cbind(
		patchID, patchX, patchY, patchZ, patchSOIL)[rep(patchIDrhessysOrder, times=patchVegnum),]## patch
	
	patchColumn2 = cbind(
		patchArea,
        patchSLOPE,
        patchTWI,
        switch( is.null(soilLegecyCNScaler)+1,soilLegecyCNScaler,NULL))[rep(patchIDrhessysOrder, times=patchVegnum),]## patch
		
        #patchColumn3 = cbind(rep(1, numpatch) %o% c(0.12, 0,0,0, 0.28, 0, -10, 0, -0.5,1,0, 0.0000001,0.0,0.0,0.0,0.0,0.0000001, 0,0, 0.0000002,2,6,0))[rep(patchIDrhessysOrder, times=patchVegnum),]## patch
						
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
        #patchColumn3,
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
        filepth = paste(projectFolder,'/',rhessysFolder,'/', filename,sep="")
		if(as.numeric(templateACTION$outputDefs[2])>0) write.table(cbind(vegParam[, ii], vegParam[,2]), filepth,sep="\t",row.names=F,col.names=F, quote=F)
	}#i

	
	## write out selected soil definition files
    soilHEADER = NULL
    selectedsoil = soilParamCOL[match(unique(patchSOIL_info[,1]), soilParamCOL[,'soilID']), 'soilDefIndex']
    what = tapply(seq_len(dim(patchSOIL_info)[1]), patchSOIL_info[,1], function(ii){ unique(patchSOIL_info[ii,2]) })
    for(ii in selectedsoil ){
        for(jj in what[[toString(soilParam[1,ii])]] ){ # soil texture id --> ssurgo mukey
            filename = paste(defsFolder,"/soil_",gsub("\\.","_",colnames(soilParam)[ii]),"_",jj,".def",sep="")
            soilHEADER = c(soilHEADER, paste(filename,'patch_default_filename'))
            filepth = paste(projectFolder,'/',rhessysFolder,'/', filename,sep="")
            ## modify the generic
            cond = ssurgo_info[,1]==jj ## select ONE of the dominated soil_extID
            soil_char = soilParam[, ii]
            soil_char[1] = jj # id
            ## ... below ...  soilParam[,1:2] for checking index
            if(!is.na(ssurgo_info[cond,'soilksat0'])){soil_char[match('Ksat_0',soilParam[,2])] = ssurgo_info[cond,'soilksat0']} # ksat0 (m/day)
            if(!is.na(ssurgo_info[cond,'soilksat0'])){soil_char[match('Ksat_0_v',soilParam[,2])] = ssurgo_info[cond,'soilksat0']}
            if(!is.na(ssurgo_info[cond,'soilksatdecay'])){soil_char[match('m',soilParam[,2])] = ssurgo_info[cond,'soilksatdecay']} # it's meter (horizontal)
            if(!is.na(ssurgo_info[cond,'soilksatdecay'])){soil_char[match('m_z',soilParam[,2])] = ssurgo_info[cond,'soilksatdecay']} # it's meter (vertical)
            if(!is.na(ssurgo_info[cond,'soilpor0'])){soil_char[match('porosity_0',soilParam[,2])] = ssurgo_info[cond,'soilpor0']} # porosity_0
            if(!is.na(ssurgo_info[cond,'soilpordecay'])){soil_char[match('porosity_decay',soilParam[,2])] = ssurgo_info[cond,'soilpordecay']} # porosity_decay (m) <<----- disabled for now June 20th, 2019
            if(!is.na(ssurgo_info[cond,'soilactivedepth'])){soil_char[match('active_zone_z',soilParam[,2])] = ssurgo_info[cond,'soilactivedepth']}
            if(!is.na(ssurgo_info[cond,'soilsoildepth'])){soil_char[match('soil_depth',soilParam[,2])] = ssurgo_info[cond,'soilsoildepth']}
            if(!is.na(ssurgo_info[cond,'soilalbedo'])){soil_char[match('albedo',soilParam[,2])] = ssurgo_info[cond,'soilalbedo']}
            if(!is.na(ssurgo_info[cond,'soilclay'])){soil_char[match('clay',soilParam[,2])] = ssurgo_info[cond,'soilclay']}
            if(!is.na(ssurgo_info[cond,'soilsilt'])){soil_char[match('silt',soilParam[,2])] = ssurgo_info[cond,'soilsilt']}
            if(!is.na(ssurgo_info[cond,'soilsand'])){soil_char[match('sand',soilParam[,2])] = ssurgo_info[cond,'soilsand']}
            if(!is.na(ssurgo_info[cond,'soilpor_size_index'])){soil_char[match('pore_size_index',soilParam[,2])] = ssurgo_info[cond,'soilpor_size_index']}
            if(!is.na(ssurgo_info[cond,'soilpsi_air_entry'])){soil_char[match('psi_air_entry',soilParam[,2])] = ssurgo_info[cond,'soilpsi_air_entry']}
            if(!is.na(ssurgo_info[cond,'soilmaxrootdepth'])){soil_char[match('maxrootdepth',soilParam[,2])] = ssurgo_info[cond,'soilmaxrootdepth']}
            if(!is.na(ssurgo_info[cond,'soilparticledensity'])){soil_char[match('particledensity',soilParam[,2])] = ssurgo_info[cond,'soilparticledensity']}
            if(!is.na(ssurgo_info[cond,'soilsoilc'])){soil_char[match('soilc',soilParam[,2])] = ssurgo_info[cond,'soilsoilc']}
            #if(!is.na(ssurgo_info[cond,'soilomdecay'])){soil_char[match('DOM_decay_rate',soilParam[,2])] = ssurgo_info[cond,'soilomdecay']}
            #if(!is.na(ssurgo_info[cond,'soilomdecay'])){soil_char[match('N_decay',soilParam[,2])] = ssurgo_info[cond,'soilomdecay']}
            if(!is.na(ssurgo_info[cond,'soilomdecay'])){soil_char[match('soilcDecay',soilParam[,2])] = ssurgo_info[cond,'soilomdecay']}
            
            if(!is.na(ssurgo_info[cond,'soilph'])){soil_char[match('PH',soilParam[,2])] = ssurgo_info[cond,'soilph']}
            if(!is.na(ssurgo_info[cond,'soilthetap1'])){soil_char[match('theta_mean_std_p1',soilParam[,2])] = ssurgo_info[cond,'soilthetap1']}
            if(!is.na(ssurgo_info[cond,'soilthetap2'])){soil_char[match('theta_mean_std_p2',soilParam[,2])] = ssurgo_info[cond,'soilthetap2']}
            
            if(as.numeric(templateACTION$outputDefs[2])>0) write.table(cbind(soil_char, soilParam[,2]), filepth,sep="\t",row.names=F,col.names=F, quote=F)
        }#jj
    }#i



	
	## write out selected lulc definition files
    allPatchLand = unique(patchLAND);
	lulcHEADER = NULL
	selectedlulc = lulcParamCOL[match(allPatchLand%%1000, lulcParamCOL[,'lulcID']), 'lulcDefIndex']
    #print(paste('all LULC: ',allPatchLand,selectedlulc))
	for(ii in seq_along(selectedlulc) ){
        ## here need updates
        iii = selectedlulc[ii]
		filename = paste(defsFolder,"/landuse_",gsub("\\.","_",colnames(lulcParam)[iii]),switch(1+(allPatchLand[ii]>1000),NULL,"print"),".def",sep="")
        #print(paste("debug: ",ii,iii,allPatchLand[ii],filename, switch(1+(allPatchLand[ii]<=1000),NULL,"print") ))
		lulcHEADER = c(lulcHEADER, paste(filename,'landuse_default_filename'))
		filepth = paste(projectFolder,'/',rhessysFolder,'/', filename,sep="")
        lulc_char = lulcParam[,iii]
        lulc_char[match('landuse_default_ID',lulcParam[,2])] = allPatchLand[ii]
		if(as.numeric(templateACTION$outputDefs[2])>0) write.table(cbind(lulc_char, lulcParam[,2]), filepth,sep="\t",row.names=F,col.names=F, quote=F)
	}#i
	

    zoneHEADER = NULL
    if(is.null(zoneDefID)){ zoneDefID = 1; }
    selectedzone = zoneParamCOL[match(unique(zoneDefID), zoneParamCOL[,'zoneID']), 'zoneDefIndex']
    for(ii in selectedzone ){
        filename = paste(defsFolder,"/zone_",gsub("\\.","_",colnames(zoneParam)[ii]),".def",sep="")
        zoneHEADER = c(zoneHEADER, paste(filename,'zone_default_filename'))
        filepth = paste(projectFolder,'/',rhessysFolder,'/', filename,sep="")
        if(as.numeric(templateACTION$outputDefs[2])>0) write.table(cbind(zoneParam[, ii], zoneParam[,2]), filepth,sep="\t",row.names=F,col.names=F, quote=F)
    }#i
	
	basinDEF = NULL
	basinDEF = c(basinDEF, '1 basin_default_ID')
	if(as.numeric(templateACTION$outputDefs[2])>0) write(basinDEF, paste(templateACTION$outputDefs[1],"/basin_basin.def",sep=""))
	
    hillHEADER = NULL
    if(is.null(hillDefID)){ hillDefID = 1; }
    selectedhill = hillParamCOL[match(unique(hillDefID), hillParamCOL[,'hillID']), 'hillDefIndex']
    for(ii in selectedhill ){
        filename = paste(defsFolder,"/hill_",gsub("\\.","_",colnames(hillParam)[ii]),".def",sep="")
        hillHEADER = c(hillHEADER, paste(filename,'hillslope_default_filename'))
        filepth = paste(projectFolder,'/',rhessysFolder,'/', filename,sep="")
        if(as.numeric(templateACTION$outputDefs[2])>0) write.table(cbind(hillParam[, ii], hillParam[,2]), filepth,sep="\t",row.names=F,col.names=F, quote=F)
    }#i

	worldHEADER = NULL
	worldHEADER = c(worldHEADER, paste(1,'num_basin_files'))
	worldHEADER = c(worldHEADER, paste(defsFolder,'/basin_basin.def basin_default_filename',sep=''))	
	worldHEADER = c(worldHEADER, paste(length(hillHEADER),'num_hillslope_files'))
    worldHEADER = c(worldHEADER, hillHEADER)
	worldHEADER = c(worldHEADER, paste(length(zoneHEADER),'num_zone_files'))
	worldHEADER = c(worldHEADER, zoneHEADER)
	
	worldHEADER = c(worldHEADER, paste(length(soilHEADER),'num_patch_files'))	
	worldHEADER = c(worldHEADER, soilHEADER)
	worldHEADER = c(worldHEADER, paste(length(lulcHEADER),'num_landuse_files'))	
	worldHEADER = c(worldHEADER, lulcHEADER)
	worldHEADER = c(worldHEADER, paste(length(vegHEADER),'num_stratum_files'))	
	worldHEADER = c(worldHEADER, vegHEADER)
	
	## ... listing all .base files
	zoneStation_uniqueID = unique(zoneStationID)
	worldHEADER = c(worldHEADER, paste(length(zoneStation_uniqueID),'num_base_stations'))
	if(length(zoneStation_uniqueID)==1){
		worldHEADER = c(worldHEADER, paste(climateStationNAME,'base_station_filename'))
	}else{
		for(bii in zoneStation_uniqueID){
			worldHEADER = c(worldHEADER, paste(climateStationNAME,bii,'.base base_station_filename',sep=''))
		}# bii
	}# if else
	
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
    rowneighbor = c(-1,  -1,    -1,     0,     1,    1,    1,    0)
    directEdgeIndex = c(2,4,6,8)
    indirectEdgeIndex = c(1,3,5,7)

    maxCol = max(cols,na.rm=T)
    maskRC = rows*maxCol+cols #paste(rows, cols,sep=':') ## row*[max col]+col (yes: unique ID)
    maskRC_string2Patch_num <- new.env(hash=T)
    list2env(setNames(as.list(patch),maskRC),envir=maskRC_string2Patch_num) #<<---- native R hash
    gridSurroundRC = sapply(rows, FUN=function(x){x+rowneighbor}) * maxCol + sapply(cols, FUN=function(x){x+colneighbor})
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
        numPatch = length(ii);
        if(numPatch==0){numPatch=1;}
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
            
            ## ... surface routing -- stream extension (yes/no)
            strQ = sum(!is.na(stream[ii])),         #11 modeled stream grids
            nonmodelstrgridQ = sum(!is.na(fullstreamExt[ii])),    #21 non-modeled stream grid (treat as land grids)
            
            ## ... surface routing -- road storm drainage
            otherImpQfrac = sum(otherImpFrac[ii],na.rm=T)/numPatch, #<<---
            pavedRoadQfrac = sum(pavedRoadFrac[ii],na.rm=T) * (sum(!is.na(fullstreamExt[ii]))==0)/numPatch,
            roadStormdrainInletQfrac = (sum(!is.na(roadStormDrainInlet[ii]))>0)*sum(pavedRoadFrac[ii],na.rm=T)*(sum(!is.na(fullstreamExt[ii]))==0)/numPatch,
            
            ## ... surface routing -- surface drainage via pipes
            nonstrsurfdrainQfrac = sum(additionalSurfaceDrainInlet[ii],na.rm=T)*(sum(!is.na(fullstreamExt[ii]))==0)/numPatch, # 22 (non-stream) land grids need surface water drain
                
            ## ... surface routing -- housing & drive way
            roofRedirectedQfrac = sum(roofFrac[ii],na.rm=T) * as.numeric(sum(!is.na(roofDrainInlet[ii]))>0)*(sum(!is.na(fullstreamExt[ii]))==0)/numPatch,
            roofQfrac = sum(roofFrac[ii],na.rm=T)/numPatch,
            
            ## ... subsurface drainages -- pipelines and sewers (yes/no)
            sewerdrainQFrac = sum(sewercover[ii],na.rm=T)/numPatch, #17 (checking whether patch contains sewercover grids)
            subsurfpipedrainQ = sum(!is.na(pipecover[ii])), # other non-sewer pipes
            
            ## ... subsurface -- interrcepts
            unpavedRoadQ = sum(!is.na(unpavedroad[ii])), # assume it cuts watertable
            basementQfrac = sum(basement[ii],na.rm=T)/numPatch,
            
            ## ... surface/subsurface -- transfer
            riparianQ = sum(!is.na(riparian[ii])),    #16 (checking whether patch contains riparian grids)
            irrigateQfrac = min(
                sum(irrigationArea[ii],na.rm=T),
                sum(lawnFrac[ii],na.rm=T) + sum(cropFrac[ii],na.rm=T))/numPatch,
                # constrainted by lawn and crop frac
            septicQfrac = min(
            	sum(septic[ii],na.rm=T),
            	sum(lawnFrac[ii],na.rm=T) + sum(cropFrac[ii],na.rm=T))/numPatch,
            ## ... surface -- storage
            waterFracQ = sum(waterFrac[ii],na.rm=T)/numPatch
        );
    })#tapply <--- this output is a list of c() in outputOrder
    patch_info_lowest = patchInfo[[ length(patchInfo) ]] ## assume basin outlet
    patch_info_basinoutlet = patchInfo[[ basinoutlet_orderedPatch_index ]]
    patch_info_suboulet = lapply(suboutlet_orderedPatch_index, function(ii){ patchInfo[[ii]] })
    #cbind(suboutlet_orderedPatch_index_hillID, suboutlet_orderedPatch_index)
    
    #-------- marking down the drainTO destinations / drainIN sources
    patch_info_patchID = tapply(fullLength, INDEX=outputOrder, FUN=function(ii){
        return <- mean(patch[ii])
    })

    if(sum(is.na(spatialAGG))< length(spatialAGG)){
        patchSPAGG=tapply(spatialAGG,INDEX=outputOrder, FUN=most);
        patchSPAGG[is.na(patchSPAGG)] = min(patchSPAGG,na.rm=T)-1
        patchSPAGGindex = match(patchSPAGG, unique(patchSPAGG))-1;
    }else{
        patchSPAGG=NULL;
        patchSPAGGindex=NULL;
    }

    patchInfo_surf_roadStormDrainOutlet = tapply(fullLength, INDEX=outputOrder, FUN=function(ii){
        # "roadStormDrainOutlet" documents the outlet patchID at the road storm drain inlet point.
        tmpCond = !is.na(roadStormDrainOutlet[ii]) & roadStormDrainOutlet[ii]>0
        if(sum(tmpCond)>0){
            return_index = match(roadStormDrainOutlet[ii][tmpCond],patch_info_patchID)
            # return NA when outlet is outside of subcatchment!
            # otherwise, return the index location in the patch_info list
            outlet_index_cond = !is.na(return_index);
            if(sum(outlet_index_cond)>0){return <- return_index[outlet_index_cond]; }else{ return <- NULL;}
        }else{
            return <- NULL
        }
    }) # tapply

    patchInfo_surf_additionalSurfaceDrainOutlet = tapply(fullLength, INDEX=outputOrder, FUN=function(ii){
        tmpCond = !is.na(additionalSurfaceDrainOutlet[ii]) & additionalSurfaceDrainOutlet[ii]>0
        if(sum(tmpCond)>0){
            return_index = match(additionalSurfaceDrainOutlet[ii][tmpCond],patch_info_patchID)
            outlet_index_cond = !is.na(return_index);
            if(sum(outlet_index_cond)>0){return <- return_index[outlet_index_cond]; }else{ return <- NULL;}
        }else{
            return <- NULL
        }
    })##

    patchInfo_surf_roofRedirectedOutlet = tapply(fullLength, INDEX=outputOrder, FUN=function(ii){
        tmpCond = !is.na(roofDrainOutlet[ii]) & roofDrainOutlet[ii]>0
        if(sum(tmpCond)>0){
            return_index = match(roofDrainOutlet[ii][tmpCond],patch_info_patchID)
            outlet_index_cond = !is.na(return_index);
            if(sum(outlet_index_cond)>0){return <- return_index[outlet_index_cond]; }else{ return <- NULL;}
        }else{
            return <- NULL
        }
    })##

    ## ... setpic (lots of warnings)
    patchInfo_septicSource = vector(mode='list',length(fullLength))
    patchInfo_septicSourceMODE = vector(mode='list',length(fullLength))
    patchInfo_septicMaxWithdraw = vector(mode='list',length(fullLength))
    if( length(template$septicInOutTable)>0 ){
        if(!is.na(template$septicInOutTable)){
            tableInOut = read.csv(template$septicInOutTable)
            tableOutIndex = match(tableInOut$outPatch,patch_info_patchID)
            tableInIndex = match(tableInOut$inPatch,patch_info_patchID)
            
            tableInIndexList = tapply(seq_along(tableOutIndex),tableOutIndex,function(ii){
                tmp = tableInIndex[ii]
                tmp = tmp[!is.na(tmp)]
                if(length(tmp)>0){return <- tmp;}else{return <- NULL;}
            });
            tableInModeList = tapply(seq_along(tableOutIndex),tableOutIndex,function(ii){
                tmp = tableInOut$mode[ii]
                tmp = tmp[!is.na(tmp)]
                if(length(tmp)>0){return <- tmp;}else{return <- NULL;}
            });
            tableInMaxWithdrawList = tapply(seq_along(tableOutIndex),tableOutIndex,function(ii){
                tmp = tableInOut$dailymax[ii]
                tmp = tmp[!is.na(tmp)]
                if(length(tmp)>0){return <- tmp;}else{return <- NULL;}
            });
            indexList = tapply(seq_along(tableOutIndex),tableOutIndex,function(ii){
                return <- tableOutIndex[ii][1]
            });
            
            for(ii in seq_along(indexList)){
                patchInfo_septicSource[[ indexList[ii] ]] = tableInIndexList[[ii]]
                patchInfo_septicSourceMODE[[ indexList[ii] ]] = tableInModeList[[ii]]
                patchInfo_septicMaxWithdraw[[ indexList[ii] ]] = tableInMaxWithdrawList[[ii]]
                patchInfo[[ indexList[ii] ]]['septicQfrac'] = 1 #make sure it has septic release
            }# end of for loop ii
        }#if
    }# vector of source patch index in "patch_info_patchID"


    ## ... irrigation (re-use some variables here)
    patchInfo_irrigationSource = vector(mode='list',length(fullLength))
    patchInfo_irrigationSourceMODE = vector(mode='list',length(fullLength))
    patchInfo_irrigationMaxWithdraw = vector(mode='list',length(fullLength))
    if( length(template$irrigationInOutTable)>0 ){
        if( !is.na(template$irrigationInOutTable) ){
            tableInOut = read.csv(template$irrigationInOutTable)
            tableOutIndex = match(tableInOut$irrigationPatchID,patch_info_patchID)
            tableInIndex = match(tableInOut$sourcePatchID,patch_info_patchID)
            # .. group by output patch
            tableInIndexList = tapply(seq_along(tableOutIndex),tableOutIndex,function(ii){
                tmp = tableInIndex[ii]
                tmp = tmp[!is.na(tmp)]
                if(length(tmp)>0){return <- tmp;}else{return <- NULL;}
            });
            tableInModeList = tapply(seq_along(tableOutIndex),tableOutIndex,function(ii){
                tmp = tableInOut$mode[ii]
                tmp = tmp[!is.na(tmp)]
                if(length(tmp)>0){return <- tmp;}else{return <- NULL;}
            });
            tableInMaxWithdrawList = tapply(seq_along(tableOutIndex),tableOutIndex,function(ii){
                tmp = tableInOut$dailymax[ii]
                tmp = tmp[!is.na(tmp)]
                if(length(tmp)>0){return <- tmp;}else{return <- NULL;}
            });
            indexList = tapply(seq_along(tableOutIndex),tableOutIndex,function(ii){
                return <- tableOutIndex[ii][1]
            });
            
            for(ii in seq_along(indexList)){
                patchInfo_irrigationSource[[ indexList[ii] ]] = tableInIndexList[[ii]]
                patchInfo_irrigationSourceMODE[[ indexList[ii] ]] = tableInModeList[[ii]]
                patchInfo_irrigationMaxWithdraw[[ indexList[ii] ]] = tableInMaxWithdrawList[[ii]]
                patchInfo[[ indexList[ii] ]]['irrigateQfrac'] = 1 # just make it positive; irrigation in the patch is still constrainted by lawn fraction
            }# end of for loop ii
        }# if
    }# vector of source patch index in "patch_info_patchID"
    
    

#-----------------------------------------------------------------

    ## part 2: sort by 'elevation' & finding neighbor
    print('starting step II')
    ## .......... Neighbour
    
    patchNeighbourRC_edge = tapply(fullLength, INDEX=outputOrder, FUN=function(jj){
        withinPatchGridRC = rows[jj]*maxCol+cols[jj]; # within
        
        hold = as.vector(gridSurroundRC[directEdgeIndex,jj]);
        hold[hold%in% withinPatchGridRC] = -1
        
        hold2 = as.vector(gridSurroundRC[indirectEdgeIndex,jj]);
        hold2[hold2%in% withinPatchGridRC] = -1
        
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


    ## .......... prefer Neighbour (subsurface)
    patchPreferNeighbourRC = tapply(fullLength, INDEX=outputOrder, FUN=function(ii){
        withinPatchGridRC = rows[ii]*maxCol+cols[ii]; # within
        drainTO_index = cbind(abs(drain[ii]),ii)
        hold3 = as.vector(gridSurroundRC[ drainTO_index ])
        hold3[hold3%in% withinPatchGridRC] = -1
        
        return <- sapply(names(tapply(hold3[hold3>0], hold3[hold3>0],length)),function(x){maskRC_string2maskRC_num[[ x ]]})
    })#

    ## .......... prefer Neighbour (surface)
	patchPreferNeighbourRC_surf = tapply(fullLength, INDEX=outputOrder, FUN=function(ii){
        withinPatchGridRC = rows[ii]*maxCol+cols[ii]; # within
        listOfDrain = sapply(ii,function(x){
            allDir = c(onRoadDraingeDir[x], roofDrainDir[x])
            if(length(allDir)>0){
                condtmp = !is.na(allDir)
                if(sum(condtmp)>0){
                    return <- allDir[condtmp]
                }else{
                    return <- drain[x]
                }
            }else{
                return <- drain[x]
            }
        })
        drainTO_index = cbind(abs(listOfDrain),ii)
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
        withinNeighbourRC_prefer_surf = rep(0,length(withinNeighbourRC))
        withinNeighbourRC_prefer_surf[withinNeighbourRC%in%patchPreferNeighbourRC_surf[[ii]] ] = 1
        index4neighbour = patchNeighbourPatchIndex[[ii]]
        
        current_patch_info = patchInfo[[ii]]
        ## actionCode is mostly for subsurface processes; surface storm drain see below.
        # land (default) = 0
        # impFrac = roofFrac + otherImpFrac + pavedpavedRoadFrac
        # gw_drainage is bounded by imp
        class_stream = 1
        class_road = 2
        actionSTORMDRAIN = 3 # surface water drainage to fixed locations or outlet (for road network)
        actionGWDRAIN = 5 # surface -> deep gw storage
        actionRIPARIAN =7 # receive deep gw seeping
        actionSEWER = 11 # conditional subsurface drain out of system
        actionPIPEDRAIN = 17 # conditional subsurface drain to outlet
        #
        actionIRRIGATION = 13 # = actionFERTILIZER (will be gone)
        actionSEPTIC = 19 # (will be gone)
        actionCode = 	ifelse(	current_patch_info['pavedRoadQfrac']+
        						current_patch_info['roofQfrac']+
        						current_patch_info['otherImpQfrac']>=1,1,actionGWDRAIN) * # GW drain except road/roof/parkinglot
        				ifelse(current_patch_info['sewerdrainQFrac']>0,actionSEWER,1) * # sewer drain (top 3-m)
						ifelse(current_patch_info['subsurfpipedrainQ']>0,actionPIPEDRAIN,1) * # subsurface pipe drain (top 1-m) along the ROAD	
        				## ...		
        				ifelse(current_patch_info['riparianQ']>0,actionRIPARIAN,1) * # riparian
        				## ...
        				ifelse(current_patch_info['irrigateQfrac']>0,actionIRRIGATION,1) * # lawn irrigation & fertilizer
        				## ...
        				ifelse(current_patch_info['septicQfrac']>0,actionSEPTIC,1) * # septic
        				## ...
                    	ifelse(current_patch_info['roadStormdrainInletQfrac']>0,
                    		ifelse(current_patch_info['nonmodelstrgridQ']>0,1,actionSTORMDRAIN), 1)  # count for stream under road bridge
						
		drainage_type = ifelse(current_patch_info['strQ']>0, 1, # class::stream
						ifelse(actionCode>1, actionCode,0))	
        
        
        
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
            
            idiffDEM_surf = current_patch_info['elevation']-neighbor_patch_info['elevation'] + 10*current_patch_info['roofQfrac'] # roof height > 10m
            idiffDEM_surf = ifelse(idiffDEM_surf<0,0, idiffDEM_surf)
            
            surfWeight = min(1,current_patch_info['pavedRoadQfrac']+current_patch_info['otherImpQfrac'])
            
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
               
                rise_surf = as.numeric(idiffDEM_surf)*(1-surfWeight) + as.numeric(idiffDEM_surf)*surfWeight*withinNeighbourRC_prefer_surf[jj],
                    # if a neighbour is prefered by surf drain, then drain to there more
                    # current_patch_info['elevation']
               
                ## ... shared edge
                sharedEdge = sum(withinNeighbourRC_edge[jj]), #edge [7]
                ## info
                neighbor_patch_info['roofQfrac'], # roof 8
                neighbor_patch_info['pavedRoadQfrac'], # road 9
                neighbor_patch_info['otherImpQfrac'], # parking 10
                neighbor_patch_info['irrigateQfrac'] # lawn 11
            )
        }))#tapply
        #allNeighbourInfo['rise',]
        #allNeighbourInfo['riseRegion',]
        #allNeighbourInfo['rise_surf',]
        
        ## local prefer
        slope_jj_l = allNeighbourInfo['rise',]/allNeighbourInfo['dist',] # rise / distance
        gamma_jj_l = slope_jj_l*allNeighbourInfo['sharedEdge',] # edge (width)
        
        ## regional prefer
        slope_jj_r = allNeighbourInfo['riseRegion',]/allNeighbourInfo['dist',] # rise / distance
        gamma_jj_r = slope_jj_r*allNeighbourInfo['sharedEdge',]
        
        cc1 = sum(gamma_jj_l)==0 # use gamma_jj_r
        cc2 = sum(gamma_jj_l < gamma_jj_r)==0  ## if T, all local gamma is > regional gamma: use gamma_jj_l
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
        # cbind(allNeighbourInfo[1,], gamma_jj)
        
        
        ## surface prefer
        final_rise_surf = allNeighbourInfo['rise_surf',]
        if(sum(final_rise_surf)==0){ final_rise_surf=allNeighbourInfo['riseRegion',]; }
        slope_jj_s = final_rise_surf/allNeighbourInfo['dist',] # rise / distance
        gamma_jj_s = slope_jj_s*allNeighbourInfo['sharedEdge',] # edge (width)
        if( sum(gamma_jj_s)==0 ){
            print(paste(ii, current_patch_info['patchID']) )
            gamma_jj_s = rep(1, length(gamma_jj_s))
        }#if
        #cbind(allNeighbourInfo[1,], gamma_jj_s, allNeighbourInfo['rise_surf',], gamma_jj, allNeighbourInfo['rise',])
        # cbind(slope_jj_l,slope_jj_r,slope_jj_s)
        # cbind(gamma_jj_l,gamma_jj_r,gamma_jj_s)
        
        ## ... neighbour gamma fraction
        neighbor_frac_gamma = sapply(gamma_jj,function(x){return <- ifelse(x>0,x,0)})
        neighbor_frac_gamma = neighbor_frac_gamma / ifelse(sum(neighbor_frac_gamma)>0,sum(neighbor_frac_gamma),1)
    
        neighbor_frac_gamma_surf = sapply(gamma_jj_s,function(x){return <- ifelse(x>0,x,0)}) 
        neighbor_frac_gamma_surf = neighbor_frac_gamma_surf / ifelse(sum(neighbor_frac_gamma_surf)>0,sum(neighbor_frac_gamma_surf),1)
        #cbind(allNeighbourInfo[1,], neighbor_frac_gamma_surf, neighbor_frac_gamma)
        
        ## ... total_gamma (subsurface)
        total_perimeter = sum( allNeighbourInfo['sharedEdge', selectedFlow2neigbour] )
        total_gamma = sum(gamma_jj)/total_perimeter*current_patch_info['len']*cellarea; # currrent CF calculation
        if(drainage_type==1) total_gamma = current_patch_info['aveSlope']*current_patch_info['len']*cellarea; # special for stream
            
        septic_num_drainIN = length(patchInfo_septicSource[[ii]])
        irrigation_num_drainIN = length(patchInfo_irrigationSource[[ii]])
        maxWithdrawalDailyWater_mmd = ifelse( length(template$maxWithdrawalDailyWater_mmd)>0,
            template$maxWithdrawalDailyWater_mmd,
            4)
      #-------------- subsurface -------------------# within a for loop
        if(as.numeric(templateACTION$outputSubFlow[2])>0){
            
            if(abs(sum(neighbor_frac_gamma)-1)>1e-10){ print(paste('subsurface',ii,current_patch_info['patchID'],sum(neighbor_frac_gamma))) }
            cat(
				paste(current_patch_info[c('patchID','zoneID','hillID')], collapse=' '),
				paste(sprintf('%.2f',current_patch_info[c('waterFracQ','sewerdrainQFrac')]), collapse=' '),
                paste(sprintf('%.1f', -septic_num_drainIN)), # use negative number because some old file may have 1 here
                paste(sprintf('%.1f', -irrigation_num_drainIN)), # use negative number because some old file may have 1 here
				sprintf('%.2f', ifelse(!is.na(current_patch_info['basementQfrac']),current_patch_info['basementQfrac'],0)*BASEMENT_DEPTH + ifelse(!is.na(current_patch_info['pavedRoadQfrac']),current_patch_info['pavedRoadQfrac'],0)*PAVEDROAD_DEPTH + ifelse(!is.na(current_patch_info['otherImpQfrac']),current_patch_info['otherImpQfrac'],0)*PAVEDROAD_DEPTH), #wttd
				drainage_type,
                total_gamma, length(withinNeighbourRC),switch(1+is.null(patchSPAGG),patchSPAGG[ii],NULL),switch(1+is.null(patchSPAGG),patchSPAGGindex[ii],NULL),'\n', file=subsurfaceflow_table_buff,sep=' ')
              
                
            cat( paste(
				allNeighbourInfo['patchID',],
				allNeighbourInfo['zoneID',],
				allNeighbourInfo['hillID',],
				sprintf('%.5f',neighbor_frac_gamma),
				sprintf('%.2f',allNeighbourInfo['sharedEdge',]/allNeighbourInfo['dist',]),
				sprintf('%.2f',allNeighbourInfo['sharedEdge',]),sep=' '), file=subsurfaceflow_table_buff,sep='\n')
            
            # ... traditional road grid, which cannot be stream or outlet grid
            if(drainage_type==2 & is.na(current_patch_info['subIDindex'])) cat (
				patch_info_lowest['patchID'],
				patch_info_lowest['zoneID'],
				patch_info_lowest['hillID'],
				roadWidth,'\n', file=subsurfaceflow_table_buff,sep=' ')  #cellsize*current_patch_info[15]
            if(drainage_type==2 & !is.na(current_patch_info['subIDindex'])) cat (
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID'],
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['zoneID'],
                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['hillID'],
                roadWidth,'\n', file=subsurfaceflow_table_buff,sep=' ')  #cellsize*current_patch_info[15]
            
            # ... this is for known sources of irrigation / septic water
            # design note: ACTION_flags (irrigation or septic) enable irrigation or septic release to occur
            # 			   If drainIN neighbors present then, the release water is constrained by the source available water
           	#			   If no drainIN neighbors present then, irrigation or septic release is just constrained by the parameter in lulc.def
            #patchID, zoneID, hillID,
			#maxDailyDrain: max withdrawal flux (mm/d)
			#propDrainFrmSurf: drain from surface or SAT at the source --> source Mode: 1 = surf only; 0 = subsurface; 2 = other storage;
			#DrainFrac: fraction among sources
			#
			
			
			## septic
			if( length(patchInfo_septicSource[[ii]])>0 ){
				drainIN_patch_info = do.call(cbind,patchInfo[ patchInfo_septicSource[[ii]] ]);
				num_Source = dim(drainIN_patch_info)[2]; 
				cat( paste(
				drainIN_patch_info['patchID',],
				drainIN_patch_info['zoneID',],
				drainIN_patch_info['hillID',],
				sprintf('%.3f',patchInfo_septicMaxWithdraw[[ii]]),
				sprintf('%.3f',patchInfo_septicSourceMODE[[ii]]), #<<--- check here
				sprintf('%.3f',rep(1/num_Source,num_Source)),sep=' '), file=subsurfaceflow_table_buff,sep='\n')}
			
			## irrigation
			if( length(patchInfo_irrigationSource[[ii]])>0 ){
				drainIN_patch_info = do.call(cbind,patchInfo[ patchInfo_irrigationSource[[ii]] ]);
				num_Source = dim(drainIN_patch_info)[2]; 
				cat( paste(
				drainIN_patch_info['patchID',],
				drainIN_patch_info['zoneID',],
				drainIN_patch_info['hillID',],
				sprintf('%.3f',patchInfo_irrigationMaxWithdraw[[ii]]),
				sprintf('%.3f',patchInfo_irrigationSourceMODE[[ii]]), #<<--- check here
				sprintf('%.3f',rep(1/num_Source,num_Source)),sep=' '), file=subsurfaceflow_table_buff,sep='\n')}
                
            
        }# if subsurface output
        
        #-------------- surface -------------------# within a for loop
        if(as.numeric(templateACTION$outputSurfFlow[2])>0){
            
            if( (current_patch_info['roofRedirectedQfrac']>0 | current_patch_info['roadStormdrainInletQfrac']>0 | current_patch_info['nonstrsurfdrainQfrac']>0) & current_patch_info['strQ']==0 ){
                
                # roof / parking / road on current patch
                stormsurfacedrainFrac = c(
                    ifelse(current_patch_info['roofRedirectedQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0,current_patch_info['roofRedirectedQfrac'],0),  # roof Frac
                    ifelse(current_patch_info['roadStormdrainInletQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0,current_patch_info['roadStormdrainInletQfrac'],0),  # road Frac
                    ifelse(current_patch_info['nonstrsurfdrainQfrac']>0 &
                        current_patch_info['nonmodelstrgridQ']==0,0.9,0) # e.g., surface drain around the roof/road/parkinglot
                ); names(stormsurfacedrainFrac) = c('roof','road','extenddrain')
                if(sum(stormsurfacedrainFrac[1:2])>=1){
                    stormsurfacedrainFrac[1:2] = stormsurfacedrainFrac[1:2]/sum(stormsurfacedrainFrac[1:2]);
                    stormsurfacedrainFrac[3]=0;
                }else if(stormsurfacedrainFrac[3]>0 & sum(stormsurfacedrainFrac[1:3])>=1){ stormsurfacedrainFrac[3] = 1 - sum(stormsurfacedrainFrac[1:2]); }
                
                
                # adjust the gamma fractions
                normal_neighborNum = length(neighbor_frac_gamma_surf)
                normalFrac = 1 - sum(stormsurfacedrainFrac) - ifelse(current_patch_info['nonmodelstrgridQ']>0, 0.4,0.0);
                if(normalFrac<0){
                    print(paste(ii, normalFrac));
                    normalFrac = 0;
                } #<<---- problem to check
                normal_neighbor_frac_gamma = (neighbor_frac_gamma_surf * normalFrac)
                
                #if(abs(sum(normal_neighbor_frac_gamma)-1)>1e-10) print(paste('surface (special)',ii,current_patch_info['patchID'],sum(normal_neighbor_frac_gamma)))
                
                
                ## stop routing from current to roof on surface
                normal_neighbor_frac_gammaSUM = sum(normal_neighbor_frac_gamma)
                normal_neighbor_frac_gamma = normal_neighbor_frac_gamma * sapply( (1.0-allNeighbourInfo['roofQfrac',]+current_patch_info['roofQfrac']),function(x){return <- min(1,x)}) # roofFrac of neighbour patch
                if(sum(normal_neighbor_frac_gamma)>0){
                    normal_neighbor_frac_gamma = normal_neighbor_frac_gamma/sum(normal_neighbor_frac_gamma)*normal_neighbor_frac_gammaSUM;
                }else{
                    normal_neighbor_frac_gamma = rep(0,length(normal_neighbor_frac_gamma))
                }
                
                
                ## counting number of neighbours
                if(current_patch_info['roofRedirectedQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0) normal_neighborNum = normal_neighborNum + max(1, length(patchInfo_surf_roofRedirectedOutlet[[ii]])); # roof (* need update!)
                
                if(current_patch_info['roadStormdrainInletQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0) normal_neighborNum = normal_neighborNum + max(1, length(patchInfo_surf_roadStormDrainOutlet[[ii]])); # road
                
                if(current_patch_info['nonstrsurfdrainQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0) normal_neighborNum = normal_neighborNum + max(1, length(patchInfo_surf_additionalSurfaceDrainOutlet[[ii]])); # surface drain around the roof/road/parkinglot (* need update!)
                
                if( current_patch_info['nonmodelstrgridQ']>0) normal_neighborNum = normal_neighborNum + 1; # stream ext.
                
                cat(
                    paste(current_patch_info[c('patchID','zoneID','hillID')], collapse=' '),
                    paste(sprintf('%.2f',current_patch_info[c('waterFracQ','sewerdrainQFrac')]), collapse=' '),
                    paste(sprintf('%.1f', -septic_num_drainIN)), # use negative number because some old file may have 1 here
               	 	paste(sprintf('%.1f', -irrigation_num_drainIN)), # use negative number because some old file may have 1 here
                    sprintf('%.2f', ifelse(!is.na(current_patch_info['basementQfrac']),current_patch_info['basementQfrac'],0)*BASEMENT_DEPTH + ifelse(!is.na(current_patch_info['pavedRoadQfrac']),current_patch_info['pavedRoadQfrac'],0)*PAVEDROAD_DEPTH + ifelse(!is.na(current_patch_info['otherImpQfrac']),current_patch_info['otherImpQfrac'],0)*PAVEDROAD_DEPTH),
                    drainage_type,
                    total_gamma, normal_neighborNum,switch(1+is.null(patchSPAGG),patchSPAGG[ii],NULL),switch(1+is.null(patchSPAGG),patchSPAGGindex[ii],NULL), '\n', file=surfaceflow_table_buff,sep=' ')
                
                cat( paste(
                    allNeighbourInfo['patchID',],
                    allNeighbourInfo['zoneID',],
                    allNeighbourInfo['hillID',],
                    sprintf('%.5f',normal_neighbor_frac_gamma),
                    sprintf('%.2f',allNeighbourInfo['sharedEdge',]/allNeighbourInfo['dist',]),
                    sprintf('%.2f',allNeighbourInfo['sharedEdge',]),sep=' '), file=surfaceflow_table_buff,sep='\n')
                
                
                # road / storm drain --> sub-catchment outlet ***
                if(current_patch_info['roadStormdrainInletQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0){
                    if( length(patchInfo_surf_roadStormDrainOutlet[[ii]])>0 & sum(is.na(patchInfo_surf_roadStormDrainOutlet[[ii]]))==0){
                        for(outleti in patchInfo_surf_roadStormDrainOutlet[[ii]] ){
                            cat(
                                # ... to designed outlet
                                patchInfo[[ outleti ]]['patchID'],
                                patchInfo[[ outleti ]]['zoneID'],
                                patchInfo[[ outleti ]]['hillID'],
                                # ... to the designed outlet
                                sprintf('%.5f', stormsurfacedrainFrac['road']),
                                sprintf('%.2f',1.0),
                                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                        }# for outleti
                    }else{
                        if(is.na(current_patch_info['subIDindex'])){
                            cat(
                                # ... to baisn outlet
                                patch_info_lowest['patchID'],
                                patch_info_lowest['zoneID'],
                                patch_info_lowest['hillID'],
                                sprintf('%.5f', stormsurfacedrainFrac['road']),
                                sprintf('%.2f',1.0),
                                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                        }else{
                            cat(
                                # ... to sub-basin outlet
                                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID'],
                                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['zoneID'],
                                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['hillID'],
                                sprintf('%.5f', stormsurfacedrainFrac['road']),
                                sprintf('%.2f',1.0),
                                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                        }# if
                    }#else
                }#if
                
                # roof tops --> sub-catchment outlet ***
                if(current_patch_info['roofRedirectedQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0){
                    if( length(patchInfo_surf_roofRedirectedOutlet[[ii]])>0 & sum(is.na(patchInfo_surf_roofRedirectedOutlet[[ii]]))==0){
                        for(outleti in patchInfo_surf_roofRedirectedOutlet[[ii]] ){
                            cat(
                                # ... to designed outlet
                                patchInfo[[ outleti ]]['patchID'],
                                patchInfo[[ outleti ]]['zoneID'],
                                patchInfo[[ outleti ]]['hillID'],
                                # ... to the designed outlet
                                sprintf('%.5f', stormsurfacedrainFrac['roof']),
                                sprintf('%.2f',1.0),
                                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                        }# for outleti
                    }else{
                        if(is.na(current_patch_info['subIDindex'])){
                           cat(
                                # ... to baisn outlet
                                patch_info_lowest['patchID'],
                                patch_info_lowest['zoneID'],
                                patch_info_lowest['hillID'],
                                sprintf('%.5f', stormsurfacedrainFrac['roof']),
                                sprintf('%.2f',1.0),
                                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                        }else{
                            cat(
                                # ... to sub-basin outlet
                                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID'],
                                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['zoneID'],
                                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['hillID'],
                                sprintf('%.5f', stormsurfacedrainFrac['roof']),
                                sprintf('%.2f',1.0),
                                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                        }#if
                    }#else
                }# if
                
                # other imp --> sub-catchment outlet ***
                if(current_patch_info['nonstrsurfdrainQfrac']>0 & current_patch_info['nonmodelstrgridQ']==0){
                    if( length(patchInfo_surf_additionalSurfaceDrainOutlet[[ii]])>0 & sum(is.na(patchInfo_surf_additionalSurfaceDrainOutlet[[ii]]))==0){
                        for(outleti in patchInfo_surf_additionalSurfaceDrainOutlet[[ii]] ){
                            cat(
                                # ... to designed outlet
                                patchInfo[[ outleti ]]['patchID'],
                                patchInfo[[ outleti ]]['zoneID'],
                                patchInfo[[ outleti ]]['hillID'],
                                # ... to the designed outlet
                                sprintf('%.5f', stormsurfacedrainFrac['extenddrain']),
                                sprintf('%.2f',1.0),
                                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                        }# for outleti
                    }else{
                        if(is.na(current_patch_info['subIDindex'])){
                           cat(
                                # ... to baisn outlet
                                patch_info_lowest['patchID'],
                                patch_info_lowest['zoneID'],
                                patch_info_lowest['hillID'],
                                sprintf('%.5f', stormsurfacedrainFrac['extenddrain']),
                                sprintf('%.2f',1.0),
                                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                        }else{
                            cat(
                                # ... to sub-basin outlet
                                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID'],
                                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['zoneID'],
                                patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['hillID'],
                                sprintf('%.5f', stormsurfacedrainFrac['extenddrain']),
                                sprintf('%.2f',1.0),
                                sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                        }
                    }#else
                }# if

                # strExt (surface) & NOT str grid @ outlet
                if(current_patch_info['nonmodelstrgridQ']>0){
                    if(is.na(current_patch_info['subIDindex'])){
                        cat(
                            patch_info_basinoutlet['patchID'],
                            patch_info_basinoutlet['zoneID'],
                            patch_info_basinoutlet['hillID'],
                            sprintf('%.5f', 0.4),
                            sprintf('%.2f',1.0),
                            sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                    }else{
                        if(current_patch_info['patchID']!=patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID']) cat(
                            patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID'],
                            patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['zoneID'],
                            patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['hillID'],
                            #patch_info_lowest['patchID'],
                            #patch_info_lowest['zoneID'],
                            #patch_info_lowest['hillID'],
                            sprintf('%.5f', 0.4),
                            sprintf('%.2f',1.0),
                            sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                        
                        if(current_patch_info['patchID']==patch_info_suboulet[[ current_patch_info['subIDindex'] ]]['patchID']) cat(
                            patch_info_basinoutlet['patchID'],
                            patch_info_basinoutlet['zoneID'],
                            patch_info_basinoutlet['hillID'],
                            sprintf('%.5f', 0.4),
                            sprintf('%.2f',1.0),
                            sprintf('%.2f',1.0),'\n', file=surfaceflow_table_buff,sep=' ')
                    }# if else
                }#if
              
                
                ## septic
                if( length(patchInfo_septicSource[[ii]])>0 ){
                    drainIN_patch_info = do.call(cbind,patchInfo[ patchInfo_septicSource[[ii]] ]);
                    num_Source = dim(drainIN_patch_info)[2];
                    cat( paste(
                    drainIN_patch_info['patchID',],
                    drainIN_patch_info['zoneID',],
                    drainIN_patch_info['hillID',],
                    sprintf('%.3f',patchInfo_septicMaxWithdraw[[ii]]),
                    sprintf('%.3f',patchInfo_septicSourceMODE[[ii]]), #<<--- check here
                    sprintf('%.3f',rep(1/num_Source,num_Source)),sep=' '), file=surfaceflow_table_buff,sep='\n')}
                
                ## irrigation
                if( length(patchInfo_irrigationSource[[ii]])>0 ){
                    drainIN_patch_info = do.call(cbind,patchInfo[ patchInfo_irrigationSource[[ii]] ]);
                    num_Source = dim(drainIN_patch_info)[2];
                    cat( paste(
                    drainIN_patch_info['patchID',],
                    drainIN_patch_info['zoneID',],
                    drainIN_patch_info['hillID',],
                    sprintf('%.3f',patchInfo_irrigationMaxWithdraw[[ii]]),
                    sprintf('%.3f',patchInfo_irrigationSourceMODE[[ii]]), #<<--- check here
                    sprintf('%.3f',rep(1/num_Source,num_Source)),sep=' '), file=surfaceflow_table_buff,sep='\n')}
                
            }else{
                # same as subsurface flow
                normal_neighbor_frac_gamma = neighbor_frac_gamma_surf
                #if(abs(sum(normal_neighbor_frac_gamma)-1)>1e-10) print(paste('surface (normal)',ii,current_patch_info['patchID'],sum(normal_neighbor_frac_gamma)))
                
                    
                ## stop routing from current to roof on surface
                normal_neighbor_frac_gammaSUM = sum(normal_neighbor_frac_gamma)
                normal_neighbor_frac_gamma = normal_neighbor_frac_gamma * sapply( (1.0-allNeighbourInfo['roofQfrac',]+current_patch_info['roofQfrac']),function(x){return <- min(1,x)}) # roofFrac
                if(sum(normal_neighbor_frac_gamma)>0){normal_neighbor_frac_gamma = normal_neighbor_frac_gamma/sum(normal_neighbor_frac_gamma)*normal_neighbor_frac_gammaSUM;}else{ normal_neighbor_frac_gamma = rep(0,length(normal_neighbor_frac_gamma)) }
                    
                cat(
                    paste(current_patch_info[c('patchID','zoneID','hillID')], collapse=' '),
                    paste(sprintf('%.2f',current_patch_info[c('waterFracQ','sewerdrainQFrac')]), collapse=' '),
                    paste(sprintf('%.1f', -septic_num_drainIN)), # use negative number because some old file may have 1 here
                	paste(sprintf('%.1f', -irrigation_num_drainIN)), # use negative number because some old file may have 1 here
                    sprintf('%.2f', ifelse(!is.na(current_patch_info['basementQfrac']),current_patch_info['basementQfrac'],0)*BASEMENT_DEPTH + ifelse(!is.na(current_patch_info['pavedRoadQfrac']),current_patch_info['pavedRoadQfrac'],0)*PAVEDROAD_DEPTH + ifelse(!is.na(current_patch_info['otherImpQfrac']),current_patch_info['otherImpQfrac'],0)*PAVEDROAD_DEPTH),
                    drainage_type,
                    total_gamma,length(withinNeighbourRC),switch(1+is.null(patchSPAGG),patchSPAGG[ii],NULL),switch(1+is.null(patchSPAGG),patchSPAGGindex[ii],NULL),'\n', file=surfaceflow_table_buff,sep=' ')
                
                cat( paste(
                    allNeighbourInfo['patchID',],
                    allNeighbourInfo['zoneID',],
                    allNeighbourInfo['hillID',],
                    sprintf('%.5f', normal_neighbor_frac_gamma),
                    sprintf('%.2f',allNeighbourInfo['sharedEdge',]/allNeighbourInfo['dist',]),
                    sprintf('%.2f',allNeighbourInfo['sharedEdge',]),sep=' '), file=surfaceflow_table_buff,sep='\n')
                    #sprintf('%.2f',allNeighbourInfo[7,]/allNeighbourInfo[4,]),
                    #sprintf('%.2f',allNeighbourInfo[7,]),sep=' '), file=surfaceflow_table_buff,sep='\n')
                
                ## septic
                if( length(patchInfo_septicSource[[ii]])>0 ){
                    drainIN_patch_info = do.call(cbind,patchInfo[ patchInfo_septicSource[[ii]] ]);
                    num_Source = dim(drainIN_patch_info)[2];
                    cat( paste(
                    drainIN_patch_info['patchID',],
                    drainIN_patch_info['zoneID',],
                    drainIN_patch_info['hillID',],
                    sprintf('%.3f',patchInfo_septicMaxWithdraw[[ii]]),
                    sprintf('%.3f',patchInfo_septicSourceMODE[[ii]]), #<<--- check here
                    sprintf('%.3f',rep(1/num_Source,num_Source)),sep=' '), file=surfaceflow_table_buff,sep='\n')}
                
                ## irrigation
                if( length(patchInfo_irrigationSource[[ii]])>0 ){
                    drainIN_patch_info = do.call(cbind,patchInfo[ patchInfo_irrigationSource[[ii]] ]);
                    num_Source = dim(drainIN_patch_info)[2];
                    cat( paste(
                    drainIN_patch_info['patchID',],
                    drainIN_patch_info['zoneID',],
                    drainIN_patch_info['hillID',],
                    sprintf('%.3f',patchInfo_irrigationMaxWithdraw[[ii]]),
                    sprintf('%.3f',patchInfo_irrigationSourceMODE[[ii]]), #<<--- check here
                    sprintf('%.3f',rep(1/num_Source,num_Source)),sep=' '), file=surfaceflow_table_buff,sep='\n')}
                
            }# else

            
        }# surf table print out
    
    }# for loop ii

    if(as.numeric(templateACTION$outputSubFlow[2])>0 ){
       close(subsurfaceflow_table_buff)
    }
    if(as.numeric(templateACTION$outputSurfFlow[2])>0 ){
        close(surfaceflow_table_buff)
    }
	
	
	
	
	
	
	
	
	
	
	
	
	
	
