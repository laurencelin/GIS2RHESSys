## flow table
options(scipen=999)
arg=commandArgs(T)
library(sp)
library(XML)
library(rgrass7)
library(rgdal)
gis = gmeta()
tryCatch({ use_sp() },error=function(cond){message(cond)},warning=function(cond){message(cond)},finally={message("Please update the rgrass7 package on R")})
DtoR = pi/180
RtoD = 1/DtoR
# bounded by GIS mask

	## ... basic maps
	basinMap = 'basin'
	hillslopeMap = 'hill'
	zoneMAP = 'zone'
	xMap = 'xmap'
	yMap = 'ymap'
	patchMAP = 'patch'
	rowMap = 'rowmap' ##<<--- raster calculator row()
	colMap = 'colmap' ##<<--- raster calculator col()
	demMap = 'dem'
	slopeMap = 'slope' ## unit in degree
	rast0 = readRAST(c(basinMap, hillslopeMap, zoneMAP, patchMAP, rowMap, colMap),NODATA=0)
		mask = !is.na(rast0@data[[1]])
		basin = rast0@data[[1]][mask]
		hill = rast0@data[[2]][mask]
		zone = rast0@data[[3]][mask]
		patch = rast0@data[[4]][mask]
		rows = rast0@data[[5]][mask]
		cols = rast0@data[[6]][mask]
	rast1 = readRAST(c(demMap, xMap, yMap, slopeMap))
		dem = rast1@data[[1]][mask]
		xx = rast1@data[[2]][mask]
		yy = rast1@data[[3]][mask]
		slope = rast1@data[[4]][mask]
		

	streamMap = 'str'
	rast2 = readRAST(streamMap)	
		stream = rast2@data[[1]][mask]

	roadMap = 'roads' # mountain dirt road; not paved road
	rast3 = readRAST(roadMap)		
		road = rast3@data[[1]][mask]

	drainMap = 'drain'
	rast4 = readRAST(drainMap)		
		drain = abs(rast4@data[[1]][mask])
		
		
	## assume grids are squares 
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
	colneighbor = c(1,	0,	-1,	-1,	-1,	0,	1,	1)	
	rowneighbor = c(-1,	-1,	-1,	0,	1,	1,	1,	0)	
	directEdgeIndex = c(2,4,6,8)
	indirectEdgeIndex = c(1,3,5,7)
	 
	maxCol = max(cols,na.rm=T) 
	maskRC = rows*maxCol+cols #paste(rows, cols,sep=':') ## row*[max col]+col (yes: unique ID)
	maskRC_string2Patch_num <- new.env(hash=T)
	list2env(setNames(as.list(patch),maskRC),envir=maskRC_string2Patch_num) #<<---- native R hash
	gridSurroundRC = sapply(rows, FUN=function(x){x+rowneighbor})*maxCol+sapply(cols, FUN=function(x){x+colneighbor})
	gridSurroundRC[!(gridSurroundRC %in% maskRC)] = -1
	
	# part 1: gathering information to temporary files
	#patch_title = c('patchID','dem','xx','yy','basin','hill','zone','rr','cc','grid','strQ','roadQ','accgrid','mslope','Mslope')

	fullLength = seq(1,length.out=length(patch))
	patch_info_dem = tapply(dem,INDEX=patch,mean)
	orderedPatch = as.numeric(names(patch_info_dem[order(patch_info_dem,decreasing=T)])) ### patch could be longer than 'orderedPatch'
	outputOrder = match(patch, orderedPatch) # has the same length as 'patch'
		# test = tapply(patch, outputOrder,mean); sum(test==orderedPatch)==length(orderedPatch)
	maskRC_string2outputOrder_num <- new.env(hash=T)
	list2env(setNames(as.list(outputOrder),maskRC),envir=maskRC_string2outputOrder_num) #<<---- native R hash
	maskRC_string2maskRC_num <- new.env(hash=T)
	list2env(setNames(as.list(maskRC),maskRC),envir= maskRC_string2maskRC_num) #<<---- native R hash
	
	print('starting step I')
	patchInfo = tapply(fullLength, INDEX=outputOrder, FUN=function(ii){
		
		return <- c(
			patchID = mean(patch[ii]), 			#1 patchID
			elevation = mean(dem[ii]),				#2 elevation
			xx = mean(xx[ii]),				#3 x coordinate
			yy = mean(yy[ii]),				#4 y coordinate
			hillID = mean(hill[ii]),				#6 hillID
			zoneID = mean(zone[ii]),				#7 zoneID
			rr = mean(rows[ii]),				#8 row index (from left to right)
			cc = mean(cols[ii]),				#9 col index (from top to bottom)
			len = length(ii),					#10 num of cells
			strQ = sum(!is.na(stream[ii])),	#11 strQ (checking whether patch contains stream grids)
			aveSlope = tan(mean(slope[ii])*DtoR),	#13 average slope
			maxSlope = tan(max(slope[ii])*DtoR),	#14 max slope
			mtnRoadQ = sum(!is.na(road[ii]))
			);
	})#tapply <--- this output is a list of c() in outputOrder
	patch_info_lowest = patchInfo[[ length(patchInfo) ]]
	 
	 
	 
	 
	 
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
		})

	
	## ...........................................
	## .......... writing out flow table
	subsurfaceflow_table_buff <- file(arg[1],'w') # open a file connection
	cat( length(patchInfo), '\n', file=subsurfaceflow_table_buff) #,sep='\n'

	
	#silent = sapply(patchNeighbourRC_LEN, function(ii){
	for(ii in patchNeighbourRC_LEN){

		withinNeighbourRC_edge = patchNeighbourRC_edge[[ii]] 	
		withinNeighbourRC = patchNeighbourRC[[ii]]					
		withinNeighbourRC_prefer = rep(0,length(withinNeighbourRC))			##<<------
			withinNeighbourRC_prefer[withinNeighbourRC%in%patchPreferNeighbourRC[[ii]] ] = 1
		index4neighbour = patchNeighbourPatchIndex[[ii]] 
		 			 	
		current_patch_info = patchInfo[[ii]]
		drainage_type = ifelse(current_patch_info['strQ']>0, 1, # class::stream
						ifelse(current_patch_info['mtnRoadQ']>0, 2, 
						0 # LAND in default RHESSys
						))			
		
				
		
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
				riseRegion= ifelse( mean(withinNeighbourRC_prefer[jj])>0, flatDEMdrop, 0), #rise (regional prefer) [6] # zero correct
				## ... shared edge
				sharedEdge = sum(withinNeighbourRC_edge[jj]) #edge [7]
			)
		}))#tapply <<--- not in a right order
		
		## local prefer 
		slope_jj_l = allNeighbourInfo['rise',]/allNeighbourInfo['dist',] # rise / distance
		gamma_jj_l = slope_jj_l*allNeighbourInfo['sharedEdge',] # edge (width)
		
		## regional prefer 
		slope_jj_r = allNeighbourInfo['riseRegion',]/allNeighbourInfo['dist',] # rise / distance
		gamma_jj_r = slope_jj_r*allNeighbourInfo['sharedEdge',]
		
		cc1 = sum(gamma_jj_l)==0 # use gamma_jj_r
		cc2 = sum(gamma_jj_l < gamma_jj_r)==0 	## T: use gamma_jj_l	
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
			
	
		cat(
			paste(current_patch_info[c('patchID','zoneID','hillID')], collapse=' '),
			paste(sprintf('%.1f',current_patch_info[c('rr','cc','elevation')]), collapse=' '),
			sprintf('%.2f',1.0),
			sprintf('%.2f',0.0),
			drainage_type, 
			total_gamma, length(withinNeighbourRC),'\n',
            file=subsurfaceflow_table_buff,sep=' ')
		
		cat( paste(
			allNeighbourInfo['patchID',],
			allNeighbourInfo['zoneID',],
			allNeighbourInfo['hillID',],
			sprintf('%.5f',neighbor_frac_gamma),
			#sprintf('%.2f',allNeighbourInfo['sharedEdge',]/allNeighbourInfo['dist',]),
			#sprintf('%.2f',allNeighbourInfo['sharedEdge',]),
            sep=' '),
            file=subsurfaceflow_table_buff,sep='\n')
		
		if(drainage_type==2) cat (
			patch_info_lowest['patchID'],
			patch_info_lowest['zoneID'],
			patch_info_lowest['hillID'],
			roadWidth,'\n',
            file=subsurfaceflow_table_buff,sep=' ')  #*current_patch_info[16]
			

	}# for loop ii 
	close(subsurfaceflow_table_buff)
	
	## part 3: write out
	#write(subsurfaceflow_table_buff, arg[1], ncolumns=1)
		
		
