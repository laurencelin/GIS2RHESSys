## single basin
## sub-patch/grid setting
#----------------------------------------------------------------------------------------------
	options(scipen=999)	
	arg=commandArgs(T)
	
	
#----------------------------------------------------------------------------------------------
	# read in rast
	library(rgrass7)
	library(rgdal)
	gis = gmeta()
	gridarea = round(gis$nsres * gis$ewres)
	
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
		
	
	rast0$landuse = rep(NA,length(mask))
	rast0$lai = rep(NA,length(mask))
	rast0$impervious = rep(NA,length(mask))
	rast0$coverFrac = rep(NA,length(mask))
	
	#--- forest
		cond=lulc==41 | lulc==43 | lulc==90 | lulc==95; 
		rast0$landuse[mask][cond] = as.integer(2) # deciduous @ vegCollection.csv
		rast0$coverFrac[cond] = 1.0
		rast0$lai[mask][cond] = 4.5
		rast0$impervious[cond] = 0.0
		
		cond=lulc==42; 
		rast0$landuse[mask][cond] = as.integer(1) # evergreen @ vegCollection.csv 
		rast0$coverFrac[cond] = 1.0
		rast0$lai[mask][cond] = 5.0
		rast0$impervious[cond] = 0.0
	
		lulc==51 | lulc==52; 
		rast0$landuse[mask][cond] = as.integer(6) # shrub @ vegCollection.csv 
		rast0$coverFrac[cond] = 1.0
		rast0$lai[mask][cond] = 2
		rast0$impervious[cond] = 0.0

	#--- pasture / grass / agriculture 
		lulc==71 | lulc==72 | lulc==81 | lulc==82; 
		rast0$landuse[mask][cond] = as.integer(3) # grass @ vegCollection.csv 
		rast0$coverFrac[cond] = 1.0
		rast0$lai[mask][cond] = 1.5
		rast0$impervious[cond] = 0.0

	#--- urban / barren
		lulc==31; 
		rast0$landuse[mask][cond] = as.integer(6) # no-veg @ vegCollection.csv (may vary by state/county/city/local)
		rast0$coverFrac[cond] = 0.15
		rast0$lai[mask][cond] = 3.0
		rast0$impervious[cond] = 0.85
		
		lulc==21; 
		rast0$landuse[mask][cond] = as.integer(3) # grass @ vegCollection.csv (may vary by state/county/city/local)
		rast0$coverFrac[cond] = 0.8
		rast0$lai[mask][cond] = 1.5
		rast0$impervious[cond] = 0.2
		
		lulc==22; 
		rast0$landuse[mask][cond] = as.integer(3) # grass @ vegCollection.csv (may vary by state/county/city/local)
		rast0$coverFrac[cond] = 0.5
		rast0$lai[mask][cond] = 1.5
		rast0$impervious[cond] = 0.5
		
		lulc==23; 
		rast0$landuse[mask][cond] = as.integer(3) # grass @ vegCollection.csv (may vary by state/county/city/local)
		rast0$coverFrac[cond] = 0.2
		rast0$lai[mask][cond] = 1.5
		rast0$impervious[cond] = 0.8
		
		lulc==24; 
		rast0$landuse[mask][cond] = as.integer(4) # no-veg @ vegCollection.csv (may vary by state/county/city/local)
		rast0$coverFrac[cond] = 1.0
		rast0$lai[mask][cond] = 1.0
		rast0$impervious[cond] = 1.0
		
	writeRAST(rast0,'landuse',zcol='landuse')
	writeRAST(rast0,'coverFrac',zcol='coverFrac')
	writeRAST(rast0,'lai',zcol='lai')
	writeRAST(rast0,'impervious',zcol='impervious')
		
		
		
		