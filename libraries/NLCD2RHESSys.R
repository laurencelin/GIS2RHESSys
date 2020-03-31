## single basin
## sub-patch/grid setting
#----------------------------------------------------------------------------------------------
	options(scipen=999)	
	arg=commandArgs(T)
	
	
#----------------------------------------------------------------------------------------------
	# read in rast
	library(rgrass7)
	library(rgdal)
	library(sp)
	tryCatch({ use_sp() },error=function(cond){message(cond)},warning=function(cond){message(cond)},finally={message("Please update the rgrass7 package on R")})

	# bounded by GIS mask
	basinMap = 'basin'
	rast0 = readRAST(basinMap)
	mask = !is.na(rast0@data[[1]])
		
	strMAP = 'str'	
	rast3 = readRAST(strMAP)
	str = rast3@data[[1]][mask]	
	
	lulcMAP = 'NLCD' # NLCD coded
	rast4 = readRAST(lulcMAP)
	lulc = rast4@data[[1]][mask]	
		
	
	rast0$vegid = rep(NA,length(mask))
	rast0$landuse = rep(NA,length(mask))
	rast0$lai = rep(NA,length(mask))
	rast0$impervious = rep(NA,length(mask))
	rast0$coverFrac = rep(NA,length(mask))
	
	#--- forest
		cond=(lulc==41 | lulc==43 | lulc==90 | lulc==95) & is.na(str); 
		rast0$vegid[mask][cond] = as.integer(2) # deciduous @ vegCollection.csv
		rast0$landuse[mask][cond] = as.integer(2) # undeveloped @ lulcCollection.csv
		rast0$coverFrac[mask][cond] = 1.0
		rast0$lai[mask][cond] = 4.5
		rast0$impervious[mask][cond] = 0.0
		
		cond=(lulc==42) & is.na(str); 
		rast0$vegid[mask][cond] = as.integer(1) # evergreen @ vegCollection.csv 
		rast0$landuse[mask][cond] = as.integer(2) # undeveloped @ lulcCollection.csv
		rast0$coverFrac[mask][cond] = 1.0
		rast0$lai[mask][cond] = 5.0
		rast0$impervious[mask][cond] = 0.0
	
		cond=(lulc==51 | lulc==52) & is.na(str); 
		rast0$vegid[mask][cond] = as.integer(6) # shrub @ vegCollection.csv 
		rast0$landuse[mask][cond] = as.integer(2) # undeveloped @ lulcCollection.csv
		rast0$coverFrac[mask][cond] = 1.0
		rast0$lai[mask][cond] = 2
		rast0$impervious[mask][cond] = 0.0

	#--- pasture / grass / agriculture 
		cond=(lulc==71 | lulc==72 | lulc==81 | lulc==82) & is.na(str); 
		rast0$vegid[mask][cond] = as.integer(3) # grass @ vegCollection.csv 
		rast0$landuse[mask][cond] = as.integer(1) # grass @ lulcCollection.csv
		rast0$coverFrac[mask][cond] = 1.0
		rast0$lai[mask][cond] = 1.5
		rast0$impervious[mask][cond] = 0.0

	#--- urban / barren
		cond=(lulc==31) & is.na(str); 
		rast0$vegid[mask][cond] = as.integer(6) # no-veg @ vegCollection.csv (may vary by state/county/city/local)
		rast0$landuse[mask][cond] = as.integer(2) # undeveloped @ lulcCollection.csv
		rast0$coverFrac[mask][cond] = 0.15
		rast0$lai[mask][cond] = 3.0
		rast0$impervious[mask][cond] = 0.85
		
		cond=(lulc==21) & is.na(str); 
		rast0$vegid[mask][cond] = as.integer(3) # grass @ vegCollection.csv (may vary by state/county/city/local)
		rast0$landuse[mask][cond] = as.integer(3) # urban @ lulcCollection.csv
		rast0$coverFrac[mask][cond] = 0.8
		rast0$lai[mask][cond] = 1.5
		rast0$impervious[mask][cond] = 0.2
		
		cond=(lulc==22) & is.na(str); 
		rast0$vegid[mask][cond] = as.integer(3) # grass @ vegCollection.csv (may vary by state/county/city/local)
		rast0$landuse[mask][cond] = as.integer(3) # urban @ lulcCollection.csv
		rast0$coverFrac[mask][cond] = 0.5
		rast0$lai[mask][cond] = 1.5
		rast0$impervious[mask][cond] = 0.5
		
		cond=(lulc==23) & is.na(str); 
		rast0$vegid[mask][cond] = as.integer(3) # grass @ vegCollection.csv (may vary by state/county/city/local)
		rast0$landuse[mask][cond] = as.integer(3) # urban @ lulcCollection.csv
		rast0$coverFrac[mask][cond] = 0.2
		rast0$lai[mask][cond] = 1.5
		rast0$impervious[mask][cond] = 0.8
		
		cond=(lulc==24) & is.na(str); 
		rast0$vegid[mask][cond] = as.integer(4) # no-veg @ vegCollection.csv (may vary by state/county/city/local)
		rast0$landuse[mask][cond] = as.integer(3) # urban @ lulcCollection.csv
		rast0$coverFrac[mask][cond] = 1.0
		rast0$lai[mask][cond] = 1.0
		rast0$impervious[mask][cond] = 1.0
	
		cond= !is.na(str); 
		rast0$vegid[mask][cond] = as.integer(4) # no-veg @ vegCollection.csv (may vary by state/county/city/local)
		rast0$landuse[mask][cond] = as.integer(2) # undeveloped @ lulcCollection.csv
		rast0$coverFrac[mask][cond] = 1.0
		rast0$lai[mask][cond] = 0.0
		rast0$impervious[mask][cond] = 0.0
		
	writeRAST(rast0,'vegid',zcol='vegid',overwrite=T)
	writeRAST(rast0,'landuse',zcol='landuse',overwrite=T)
	writeRAST(rast0,'coverFrac',zcol='coverFrac',overwrite=T)
	writeRAST(rast0,'lai',zcol='lai',overwrite=T)
	writeRAST(rast0,'impervious',zcol='impervious',overwrite=T)
		
		
		
		
