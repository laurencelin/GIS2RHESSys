## flow table

arg=commandArgs(T)
library(rgrass7)
library(rgdal)
gis = gmeta()

DtoR = pi/180
RtoD = 1/DtoR
roadWidth = 5; # meter

projectFolder = arg[1]
# bounded by GIS mask
	basinMap = 'basin'
	hillslopeMap = 'hill'
	zoneMAP = 'patch'
	xMap = 'xmap'
	yMap = 'ymap'
	patchMAP = 'patch'
	rowMap = 'rowmap' ##<<--- raster calculator row()
	colMap = 'colmap' ##<<--- raster calculator col()
	demMap = 'dem'
	roadMap = 'roads'
	streamMap = 'str'
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
		
	rast2 = readRAST(streamMap)	
		stream = rast2@data[[1]][mask]
	
	rast3 = readRAST(roadMap)		
		road = rast3@data[[1]][mask]
		
		
	## assume grids are squares 
	cellarea = gis$nsres * gis$ewres
	cellsize = sqrt(cellarea) 
	directEdge = cellsize*0.5
	diagonalEdge = cellsize*sqrt(0.5)	
		
	# E, NE, N, NW, W, SW, S, SE
	rowneighbor = c(1,1,0,-1,-1,-1,0,1)	
	colneighbor = c(0,-1,-1,-1,0,1,1,1)	
	rowDirectneighbor = rowneighbor[c(1,3,5,7)]
	colDirectneighbor = colneighbor[c(1,3,5,7)]
	rowDiagonalneighbor = rowneighbor[c(2,4,6,8)]
	colDiagonalneighbor = colneighbor[c(2,4,6,8)]
	grid_row = sapply(rows, FUN=function(x){x+rowneighbor}) ## col = patch; row = neighbor 
	grid_col = sapply(cols, FUN=function(x){x+colneighbor}) ## col = patch; row = neighbor 
	mask.name = paste(rows, cols,sep=':')
	
	
	# part 1: gathering information to temporary files
	patch_buff = paste('patchID','dem','xx','yy','basin','hill','zone','rr','cc','grid','strQ','roadQ','accgrid','mslope','Mslope',sep=',')
	patch_neighbor_buff = paste('patchID','row','col','edge',sep=',')
	patch_within_buff = paste('patchID','row','col',sep=',')
	silent = tapply(seq(1,length.out=length(patch)), INDEX=patch, FUN=function(ii){
		patchID = mean(patch[ii])
		
		# within
		hold1 = cbind(rows[ii], cols[ii]);
		hold1.name = paste(hold1[,1], hold1[,2],sep=':');
		
		# neighbor
		hold2 = unique(cbind(as.vector(grid_row[,ii]), as.vector(grid_col[,ii])));
		hold2.name = paste(hold2[,1], hold2[,2],sep=':');
		cond = !(hold2.name%in%hold1.name) & hold2.name%in%mask.name;
		hold3 = hold2[cond,]; # neighbor cells
		hold3.name = paste(hold3[,1], hold3[,2],sep=':');
		
		# edges
		edges = rep(NA,sum(cond));
		direct_edge.name = paste(
			as.vector(sapply(hold1[,1], FUN=function(x){x+rowDirectneighbor})), 
			as.vector(sapply(hold1[,2], FUN=function(x){x+colDirectneighbor})),sep=':');
		cond = hold3.name %in% direct_edge.name;
		
		edges[cond] = sapply(1:sum(cond), FUN=function(ii){
			direct.name = paste(hold3[cond,1][ii]+rowDirectneighbor, hold3[cond,2][ii]+colDirectneighbor,sep=':');
			sum(direct.name %in% hold1.name)* directEdge
		}); 	
		
		edges[!cond] = sapply(1:sum(!cond), FUN=function(ii){
			diagonal.name = paste(hold3[!cond,1][ii]+rowDiagonalneighbor, hold3[!cond,2][ii]+colDiagonalneighbor,sep=':');
			sum(diagonal.name %in% hold1.name)* diagonalEdge
		}); 		
		
		# drainage type
		streamQ = sum(!is.na(stream[ii])) # >0  ==> contains stream cells
		roadQ = sum(!is.na(road[ii]))
		
		# write
		patch_buff <<- c(patch_buff, paste(
			patchID, 
			mean(dem[ii]),
			mean(xx[ii]),
			mean(yy[ii]),
			mean(basin[ii]),
			mean(hill[ii]),
			mean(zone[ii]),
			mean(rows[ii]),
			mean(cols[ii]),
			length(ii),
			streamQ,
			roadQ,
			length(ii),
			tan(mean(slope[ii])*DtoR),
			tan(max(slope[ii])*DtoR)
			,sep=','));
		patch_neighbor_buff <<- c(patch_neighbor_buff, paste(patchID, hold3[,1], hold3[,2],edges,sep=','));
		patch_within_buff <<- c(patch_within_buff, paste(patchID, hold1[,1], hold1[,2],sep=','));
		
		return <- 1;
	})#tapply
	
	
	## part 2: sort by 'elevation' & finding neighbor 
	patch_info_ = read.table(textConnection(patch_buff),sep=',',header=T) 
	patch_info_dem = patch_info_[,c('patchID','dem')]
	patch_info = patch_info_[order(patch_info_[,'dem'],decreasing=T),]
	patch_info_lowest = patch_info[dim(patch_info)[1],]
	
	patch_neighbor_ = read.table(textConnection(patch_neighbor_buff),sep=',',header=T) #read.csv(patch_neighbor_name,header=T);
	patch_neighbor = patch_neighbor_[order(patch_info_dem[match(patch_neighbor_[,'patchID'], patch_info_dem[,'patchID']),'dem'],decreasing=T),]
	
	patch_within_ = read.table(textConnection(patch_within_buff),sep=',',header=T) #read.csv(patch_within_name,header=T)
	patch_within = patch_within_[order(patch_info_dem[match(patch_within_[,'patchID'], patch_info_dem[,'patchID']),'dem'],decreasing=T),]
		# sorting is in work here
	
	patch_neighbor.name = paste(patch_neighbor[,2], patch_neighbor[,3],sep=':')
	patch_within.name = paste(patch_within[,2], patch_within[,3],sep=':')
	
	outputOrder = match(patch_neighbor[,'patchID'],unique(patch_neighbor[,'patchID']))
		
	flow_table_buff = paste(dim(patch_info)[1])
	silent = tapply(1:dim(patch_neighbor)[1], INDEX=outputOrder, FUN=function(ii){
		# exmaple: ii = 3:6
		
		# note:
		# match(A,B) => find index of B to match A value in A's order
		# match(1:2,c(3,1,0,1,0,3))
		
		current_patch_id = mean(patch_neighbor[ii,'patchID']);
		current_patch_info = patch_info[patch_info[,'patchID']==current_patch_id,];
		
		neighbor_patch_match = match(patch_neighbor.name[ii], patch_within.name); # this is 1-to-1
		neighbor_patch_match_patch = patch_within[neighbor_patch_match,'patchID'];
		neighbor_patch = unique(neighbor_patch_match_patch);
		neighbor_patch_info = patch_info[match(neighbor_patch, patch_info[,'patchID']),];
		
		
			# 0 = land
			# 1 = stream
			# 2 = road
			# 3 
			# 4 = roof
		drainage_type = 0 + 
			as.numeric(current_patch_info['strQ']>0)*1 +
			as.numeric(current_patch_info['roadQ']>0)*2
				
		total_perimeter = 0;
		gamma = simplify2array(tapply(seq_along(neighbor_patch),INDEX=neighbor_patch,FUN=function(jj){
			islope = -(neighbor_patch_info[jj,'dem']-current_patch_info['dem'])/sqrt((neighbor_patch_info[jj,'xx']-current_patch_info['xx'])^2 + (neighbor_patch_info[jj,'yy']-current_patch_info['yy'])^2);
			islope[islope<0]=0
			shared_perimeter = sum(patch_neighbor[ii,'edge'][neighbor_patch_match_patch==neighbor_patch[jj]]);
			total_perimeter <<- total_perimeter+ ifelse(islope>0,shared_perimeter,0);
			return <- islope*shared_perimeter; 
		})); ## it has different order than neighbor_patch_info
		neighbor_gamma = gamma[match(neighbor_patch_info[,'patchID'],names(gamma))];
		total_gamma = sum(neighbor_gamma)
		neighbor_frac_gamma = neighbor_gamma/ifelse(total_gamma>0, total_gamma,1);
		total_gamma = sum(neighbor_gamma)/total_perimeter*current_patch_info['grid']*cellarea; # currrent CF calculation
		
		if(drainage_type==1) total_gamma = current_patch_info['mslope']*current_patch_info['grid']*cellarea; # special for stream
		
		# current patch:[patch,zone,hill,x,y,z,acc_area,area,drainage_type,gamma,#neighbor]
		flow_table_buff <<- c(flow_table_buff, paste(
			paste(current_patch_info[c('patchID','zone','hill')], collapse=' '),
			paste(sprintf('%.1f',current_patch_info[c('rr','cc','dem')]), collapse=' '),
			paste(sprintf('%.1f',current_patch_info[c('accgrid','grid')]), collapse=' '),
			drainage_type, 
			total_gamma,length(neighbor_patch) ));
		
		# patches to go to [patch,zone,hill,?]
		flow_table_buff <<- c(flow_table_buff, paste(neighbor_patch_info[,'patchID'],neighbor_patch_info[,'zone'],neighbor_patch_info[,'hill'], neighbor_frac_gamma,sep=' '));
		
		if(drainage_type==2) flow_table_buff <<- c(flow_table_buff, 
			paste(patch_info_lowest[,'patchID'], 
			patch_info_lowest[,'zone'], 
			patch_info_lowest[,'hill'], 
			roadWidth,sep=' '))#road
		
		return <- 1;
	})#tapply
	
	
	
	write(flow_table_buff, paste(projectFolder,'/flowtable_sub.txt',sep=''), ncolumns=1)
	
	
	
	## so far, the total_gamma on land and road are correct; but stream total_gamma is ?
	## plux we need to know the downstream direction and outlet.
	## road is special!
	
	
	
	
	
	
	##--------- testing 
	# sapply(b, FUN=function(x){x+ rowneighbor})
	
	
	# a = diag(3) 
	# b = c(1,2,3)
	# a-b
	# b %o% c(1,1,1)	
	
	# unique(	t(b %o% c(1,1,1)) ) # unique rows
	# table( t(b %o% c(1,1,1)) )
	# duplicated( t(b %o% c(1,1,1)) )
	
		
