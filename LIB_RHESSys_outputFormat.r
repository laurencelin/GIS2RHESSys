

source("~/Dropbox/LIB_Rscript/LIB_misc.r")
source("~/Dropbox/LIB_Rscript/LIB_dailytimeseries2.r")
arg=commandArgs(T)

##----------------------------------------
# option A single basin file
# 1) convert a series of rhessys basin output at the same watershed into WSC format
# 2) convert flow mm/day to volumn (maybe scale up) )
# 3) bias correction 
# 4) combine different single basin files into one

# 2) convert a series of rhessys basin output at the same watershed (need to combine multiple sub-basin) into WSC format

## very importance assumption of subbasin.csv
# it must have column names: id, area, grid
# each row is a subbasin
# the format used in the past is very confusing for the propose of this
# suggest the past format should be used for developing GRASS extract and by running GRASS extract it should also yield "subbasin.csv" in the curremt format

##-----------------------------------------

combineSubbasin2Basin=function(prefix, suffix, subbasin, label='', st=NA, ed=NA){
	#prefix = 'output/rhessys'
	#suffix = '_param1'
	
	tryCatch({
		# read the first file
		i=1
		rhessysFile = read.table(paste(prefix,'_sub',subbasin[i,'id'],suffix,'_basin.daily',sep=''),skip=1,header=F )
		rhessys.date = as.Date(paste(rhessysFile[,1], rhessysFile[,2], rhessysFile[,3],sep="-"),format="%d-%m-%Y")
		rhessysCol = ncol(rhessysFile)
		subArea = as.numeric(subbasin[,'area']); totalArea = 1/sum(subArea)
		subGrid = as.numeric(subbasin[,'grid']); totalGrid = 1/sum(subGrid)
		
		if( is.na(st) | is.na(ed)){
			period = rhessys.date
			period.dailytimeSeriesMatch = rep(T,length(period))
			rhessys.dailytimeSeriesMatch = rep(T,length(period))
		}else{
			period=seq.Date(from=as.Date(st), to=as.Date(ed) ,by="day") 
			tmp = match3DailyTimeSeries(rhessys.date, period) ### assume period is the most narrow band
			rhessys.dailytimeSeriesMatch = tmp$xSelect
			period.dailytimeSeriesMatch = tmp$ySelect
		}
		
		
		holding = array(NA, dim=c(
			sum(rhessys.dailytimeSeriesMatch),
			ifelse(rhessysCol>=43,25,22),
			nrow(subbasin)
		))# subbasin#, time, vars
		for(i in 1:nrow(subbasin)){
			if(i>1){ rhessysFile = read.table(paste(prefix,'_sub',subbasin[i,'id'],suffix,'_basin.daily',sep=''),skip=1,header=F ) }
			
			# 1) 7 = sat def z
			# 2) 8 = sat def
			# 3) 9 = rz storage
			# 4) 10 = unsat storage
			# 5) 13 = cap
			# 6) 14 = evap
			# 7) 16 = trans
			# 8) 17 = baseflow
			# 9) 18 = return
			# 10) 19 = streamflow
			# 11) 20 = psn
			# 12) 21 = lai
			# 13) 22 = gw out
			# 14) 23 = gw storage
			# 15) 24 = detention storage
			# 16) 25 = % sat area
			# 17) 26 = litter store
			# 18) 27 = canopy storage
			# 19) 33 = pet
			# 20) 35 = precip
			# 21) 38 = tmax (37)
			# 22) 39 = tmin (38)
			# 23) 40 = tavg (NA)
			# 24) 41 = vpd (NA)
			# 25) 43 = recharge (NA)
			
			if(rhessysCol>=43){
				# 5.20
				holding[,,i]=as.matrix(rhessysFile[rhessys.dailytimeSeriesMatch,c(7,8,9,10,13,14,16,17,18,19,20,21,22,23,24,25,26,27,33,35,38,39,40,41,43)])
			}else{
				# 5.18
				holding[,,i]=as.matrix(rhessysFile[rhessys.dailytimeSeriesMatch,c(7,8,9,10,13,14,16,17,18,19,20,21,22,23,24,25,26,27,33,35,37,38)])	
			}
			
			system(paste("rm ",prefix,'_sub',subbasin[i,'id'],suffix,"*params",sep=''))
			system(paste("rm ",prefix,'_sub',subbasin[i,'id'],suffix,"*monthly",sep=''))
			system(paste("rm ",prefix,'_sub',subbasin[i,'id'],suffix,"*yearly",sep=''))
			system(paste("rm ",prefix,'_sub',subbasin[i,'id'],suffix,"*hourly",sep=''))
		}#i
		
		if(rhessysCol>=43){
			basin = cbind(
				as.numeric(format(period[period.dailytimeSeriesMatch],"%d")),#1
				as.numeric(format(period[period.dailytimeSeriesMatch],"%m")),#2
				as.numeric(format(period[period.dailytimeSeriesMatch],"%Y")),#3
				rep(0,length(period[period.dailytimeSeriesMatch])),#4
				rep(0,length(period[period.dailytimeSeriesMatch])),#5
				rep(0,length(period[period.dailytimeSeriesMatch])),#6
				(holding[,1,]%*% subArea)*totalArea,#7 satz
				(holding[,2,]%*% subArea)*totalArea,#8 satdef
				(holding[,3,]%*% subArea)*totalArea,#9 rz
				(holding[,4,]%*% subArea)*totalArea,#10 unsat
				rep(0,length(period[period.dailytimeSeriesMatch])),#11
				rep(0,length(period[period.dailytimeSeriesMatch])),#12
				(holding[,5,]%*% subArea)*totalArea,#13 cap
				(holding[,6,]%*% subArea)*totalArea,#14 evap
				rep(0,length(period[period.dailytimeSeriesMatch])),#15
				(holding[,7,]%*% subArea)*totalArea,#16 trans
				(holding[,8,]%*% subArea)*totalArea,#17 baseflow
				(holding[,9,]%*% subArea)*totalArea,#18 returnflow
				(holding[,10,]%*% subArea)*totalArea,#19 flow
				(holding[,11,]%*% subArea)*totalArea,#20 psn
				(holding[,12,]%*% subArea)*totalArea,#21 LAI
				(holding[,13,]%*% subArea)*totalArea,#22 gwq
				(holding[,14,]%*% subArea)*totalArea,#23 gw store
				(holding[,15,]%*% subArea)*totalArea,#24 detention store
				(holding[,16,]%*% subArea)*totalArea,#25 sat area
				(holding[,17,]%*% subArea)*totalArea,#26 litter store
				(holding[,18,]%*% subArea)*totalArea,#27 canopy store
				rep(0,length(period[period.dailytimeSeriesMatch])),#28
				rep(0,length(period[period.dailytimeSeriesMatch])),#29
				rep(0,length(period[period.dailytimeSeriesMatch])),#30
				rep(0,length(period[period.dailytimeSeriesMatch])),#31
				rep(0,length(period[period.dailytimeSeriesMatch])),#32
				(holding[,19,]%*% subArea)*totalArea,#33 pet
				rep(0,length(period[period.dailytimeSeriesMatch])),#34
				(holding[,20,]%*% subArea)*totalArea,#35 rain
				rep(0,length(period[period.dailytimeSeriesMatch])),#36
				rep(0,length(period[period.dailytimeSeriesMatch])),#37
				(holding[,21,]%*% subArea)*totalArea,#38 tmax
				(holding[,22,]%*% subArea)*totalArea,#39 tmin
				(holding[,23,]%*% subArea)*totalArea,#40 tavg
				(holding[,24,]%*% subArea)*totalArea,#41 vpd
				rep(0,length(period[period.dailytimeSeriesMatch])),#42
				(holding[,25,]%*% subArea)*totalArea #43 recharge
			)
			colnames(basin)=c(
			"day",#1
			"month",#2
			"year",#3
			'',#4
			'',#5
			'',#6
			'satz',#7
			'satdef',#8
			'rz',#9
			'unsat',#10
			'',#11
			'',#12
			'cap',#13
			'evap',#14
			'',#15
			'trans',#16
			'baseflow',#17
			'returnflow',#18
			'streamflow',#19
			'psn',#20
			'lai',#21
			'gwq',#22
			'gwstore',#23
			'detentionstore',#24
			'satarea',#25
			'litterstore',#26
			'canopystore',#27
			'',#28
			'',#29
			'',#30
			'',#31
			'',#32
			'pet',#33
			'',#34
			'precip',#35
			'',#36
			'',#37
			'tmax',#38
			'tmin',#39
			'tavg',#40
			'vpd',#41
			'',#42
			'rechargre'#43
			)
		}else{
			basin = cbind(
				as.numeric(format(period,"%d")),#1
				as.numeric(format(period,"%m")),#2
				as.numeric(format(period,"%Y")),#3
				rep(0,length(period[period.dailytimeSeriesMatch])),#4
				rep(0,length(period[period.dailytimeSeriesMatch])),#5
				rep(0,length(period[period.dailytimeSeriesMatch])),#6
				(holding[,1,]%*% subArea)*totalArea,#7 satz
				(holding[,2,]%*% subArea)*totalArea,#8 satdef
				(holding[,3,]%*% subArea)*totalArea,#9 rz
				(holding[,4,]%*% subArea)*totalArea,#10 unsat
				rep(0,length(period[period.dailytimeSeriesMatch])),#11
				rep(0,length(period[period.dailytimeSeriesMatch])),#12
				(holding[,5,]%*% subArea)*totalArea,#13 cap
				(holding[,6,]%*% subArea)*totalArea,#14 evap
				rep(0,length(period[period.dailytimeSeriesMatch])),#15
				(holding[,7,]%*% subArea)*totalArea,#16 trans
				(holding[,8,]%*% subArea)*totalArea,#17 baseflow
				(holding[,9,]%*% subArea)*totalArea,#18 returnflow
				(holding[,10,]%*% subArea)*totalArea,#19 flow
				(holding[,11,]%*% subArea)*totalArea,#20 psn
				(holding[,12,]%*% subArea)*totalArea,#21 LAI
				(holding[,13,]%*% subArea)*totalArea,#22 gwq
				(holding[,14,]%*% subArea)*totalArea,#23 gw store
				(holding[,15,]%*% subArea)*totalArea,#24 detention store
				(holding[,16,]%*% subArea)*totalArea,#25 sat area
				(holding[,17,]%*% subArea)*totalArea,#26 litter store
				(holding[,18,]%*% subArea)*totalArea,#27 canopy store
				rep(0,length(period[period.dailytimeSeriesMatch])),#28
				rep(0,length(period[period.dailytimeSeriesMatch])),#29
				rep(0,length(period[period.dailytimeSeriesMatch])),#30
				rep(0,length(period[period.dailytimeSeriesMatch])),#31
				rep(0,length(period[period.dailytimeSeriesMatch])),#32
				(holding[,19,]%*% subArea)*totalArea,#33 pet
				rep(0,length(period[period.dailytimeSeriesMatch])),#34
				(holding[,20,]%*% subArea)*totalArea#35 rain
			)
			colnames(basin)=c(
			"day",#1
			"month",#2
			"year",#3
			'',#4
			'',#5
			'',#6
			'satz',#7
			'satdef',#8
			'rz',#9
			'unsat',#10
			'',#11
			'',#12
			'cap',#13
			'evap',#14
			'',#15
			'trans',#16
			'baseflow',#17
			'returnflow',#18
			'streamflow',#19
			'psn',#20
			'lai',#21
			'gwq',#22
			'gwstore',#23
			'detentionstore',#24
			'satarea',#25
			'litterstore',#26
			'canopystore',#27
			'',#28
			'',#29
			'',#30
			'',#31
			'',#32
			'pet',#33
			'',#34
			'precip'#35
			)
		}
		write.table(basin,paste(prefix,suffix, label,"_basin.daily",sep=""),row.names=F,col.names=T)
		
	}, error = function(e){
		print(paste(subbasin[i,'id']," is not here.",e,sep=""))
	})#try blocks
		
}#function

SingleBasinSeries2WSC=function( prefix, Jindex, outputPrefix, sitename=NA, period=NA){
	# first step (everything is in terms of mm/day )
	# 1) convert a series of rhessys basin output at the same watershed into WSC format
	
	outputname = paste(prefix,Jindex[1],'_basin.daily',sep='' ) 
	tmp = read.table(outputname,header=F,skip=1) 
	if(is.na(period)){
		print('use RHESSys output period')
		period = as.Date(paste(tmp[,1], tmp[,2], tmp[,3],sep="-"),format="%d-%m-%Y")
		rhessys.dailytimeSeriesMatch = rep(T,length(period))
		period.dailytimeSeriesMatch = rep(T,length(period))
		print(range(period))
	}else{
		rhessys.date = as.Date(paste(tmp[,1], tmp[,2], tmp[,3],sep="-"),format="%d-%m-%Y")
		tmp = match2DailyTimeSeries(rhessys.date, period) ### assume period is the most narrow band
		rhessys.dailytimeSeriesMatch = tmp$xSelect
		period.dailytimeSeriesMatch = tmp$ySelect
	}
	tmp = unlist(strsplit(prefix,split='/'))
	if(length(tmp)>1){location = paste(tmp[1:(length(tmp)-1)],collapse='/')}else{location='.'}
	if(is.na(sitename)){sitename = tmp[length(tmp)]}
	
	jMax = length(Jindex)
	holding = matrix(NA,sum(period.dailytimeSeriesMatch), jMax)
	holding_et = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_pet = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_precip = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_tmax = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_tmin = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_tavg = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_vpd = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_recharge = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	NewVersion=F
	for(j in 1:jMax){
		outputname = paste(prefix,Jindex[j],'_basin.daily',sep='' ) 
		tmp = read.table(outputname,header=F,skip=1) 
		
		if(ncol(tmp)>=43){
			# 5.20
			holding[,j]=tmp[rhessys.dailytimeSeriesMatch,19]
			holding_et[,j]=tmp[rhessys.dailytimeSeriesMatch,14]+tmp[rhessys.dailytimeSeriesMatch,16]
			holding_pet[,j]=tmp[rhessys.dailytimeSeriesMatch,33]
			holding_precip[,j] = tmp[rhessys.dailytimeSeriesMatch,35]# rain mm 
			tmax_ = tmp[rhessys.dailytimeSeriesMatch,38]# tmax C 
			tmin_ = tmp[rhessys.dailytimeSeriesMatch,39]# tmin C
			holding_tmax[,j] = colMaxs( rbind(tmax_,tmin_))
			holding_tmin[,j] = colMins( rbind(tmax_,tmin_))
			
			holding_tavg[,j] = tmp[rhessys.dailytimeSeriesMatch,40]# tavg
			holding_vpd[,j] = tmp[rhessys.dailytimeSeriesMatch,41]# vpd
			holding_recharge[,j] = tmp[rhessys.dailytimeSeriesMatch,43]# recharge
			NewVersion=T
		}else{
			# 5.18
			holding[,j]=tmp[rhessys.dailytimeSeriesMatch,19]
			holding_et[,j]=tmp[rhessys.dailytimeSeriesMatch,14]+tmp[rhessys.dailytimeSeriesMatch,16]
			holding_pet[,j]=tmp[rhessys.dailytimeSeriesMatch,33]
			holding_precip[,j] = tmp[rhessys.dailytimeSeriesMatch,35]# rain mm 
			tmax_ = tmp[rhessys.dailytimeSeriesMatch,37]# tmax C 
			tmin_ = tmp[rhessys.dailytimeSeriesMatch,38]# tmin C
			holding_tmax[,j] = colMaxs( rbind(tmax_,tmin_))
			holding_tmin[,j] = colMins( rbind(tmax_,tmin_))
		}
	}#j
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding
	)
	colnames(result) = c('year','month','day', paste('streamflowmm',1:jMax,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,'_streamflowmm.csv',sep=''),row.names=F)
	
	
	result = cbind(
		format(period,format='%Y'),
		format(period,format='%m'),
		format(period,format='%d'),
		holding_et
	)
	colnames(result) = c('year','month','day', paste('et',1:jMax,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,'_et.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period,format='%Y'),
		format(period,format='%m'),
		format(period,format='%d'),
		holding_pet
	)
	colnames(result) = c('year','month','day', paste('pet',1:jMax,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,'_pet.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period,format='%Y'),
		format(period,format='%m'),
		format(period,format='%d'),
		holding_precip
	)
	colnames(result) = c('year','month','day', paste('precip',1:jMax,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,'_precip.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period,format='%Y'),
		format(period,format='%m'),
		format(period,format='%d'),
		holding_tmax
	)
	colnames(result) = c('year','month','day', paste('tmax',1:jMax,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,'_tmax.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period,format='%Y'),
		format(period,format='%m'),
		format(period,format='%d'),
		holding_tmin
	)
	colnames(result) = c('year','month','day', paste('tmin',1:jMax,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,'_tmin.csv',sep=''),row.names=F)
	
	
	if(NewVersion){
		result = cbind(
			format(period,format='%Y'),
			format(period,format='%m'),
			format(period,format='%d'),
			holding_tavg
		)
		colnames(result) = c('year','month','day', paste('tavg',1:jMax,sep='_')  )
		write.csv(result,paste(location,'/',outputPrefix, sitename,'_tavg.csv',sep=''),row.names=F)
		
		result = cbind(
			format(period,format='%Y'),
			format(period,format='%m'),
			format(period,format='%d'),
			holding_vpd
		)
		colnames(result) = c('year','month','day', paste('vpd',1:jMax,sep='_')  )
		write.csv(result,paste(location,'/',outputPrefix, sitename,'_vpd.csv',sep=''),row.names=F)
		
		result = cbind(
			format(period,format='%Y'),
			format(period,format='%m'),
			format(period,format='%d'),
			holding_recharge
		)
		colnames(result) = c('year','month','day', paste('recharge',1:jMax,sep='_')  )
		write.csv(result,paste(location,'/',outputPrefix, sitename,'_recharge.csv',sep=''),row.names=F)
	}
	
	
	
}#function



WSC_combineUSGS2Lake=function( prefix, dailytimeSeriesMatch, replication, arealweight, outPrefix, scaler=1, NewVersion=F){
	# assume usgs files
	num = length(prefix)
	time = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])),3)
	hold_flow = array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	hold_pet= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	hold_et= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	hold_rain= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	hold_tmax= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	hold_tmin= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	
	hold_satz= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	hold_unsat= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	hold_evap= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	hold_snow= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	hold_return= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	hold_psn= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	hold_lai= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	
	hold_tavg= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	hold_vpd= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	hold_recharge= array(NA,dim=c(sum(unlist(dailytimeSeriesMatch[1])),num, replication) )
	
	## missing tavg, VPD, recharge
	
	for(i in 1:num){
		tmp = as.matrix(read.csv( paste(prefix[i],'_streamflowmm.csv',sep='')))
		if(i==1){time = tmp[unlist(dailytimeSeriesMatch[i]),1:3] }
		hold_flow[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		
		tmp = as.matrix(read.csv( paste(prefix[i],'_pet.csv',sep='')))
		hold_pet[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		
		tmp = as.matrix(read.csv( paste(prefix[i],'_et.csv',sep='')))
		hold_et[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		
		tmp = as.matrix(read.csv( paste(prefix[i],'_precip.csv',sep='')))
		hold_rain[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		
		tmp = as.matrix(read.csv( paste(prefix[i],'_tmax.csv',sep='')))
		hold_tmax[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		
		tmp = as.matrix(read.csv( paste(prefix[i],'_tmin.csv',sep='')))
		hold_tmin[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		
		
		
		tmp = as.matrix(read.csv( paste(prefix[i],'_satz.csv',sep='')))
		hold_satz[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		
		tmp = as.matrix(read.csv( paste(prefix[i],'_unsat.csv',sep='')))
		hold_unsat[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		
		tmp = as.matrix(read.csv( paste(prefix[i],'_evap.csv',sep='')))
		hold_evap[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		
		tmp = as.matrix(read.csv( paste(prefix[i],'_snow.csv',sep='')))
		hold_snow[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		
		tmp = as.matrix(read.csv( paste(prefix[i],'_return.csv',sep='')))
		hold_return[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		
		tmp = as.matrix(read.csv( paste(prefix[i],'_psn.csv',sep='')))
		hold_psn[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		
		tmp = as.matrix(read.csv( paste(prefix[i],'_lai.csv',sep='')))
		hold_lai[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		
		
		if(NewVersion){
			tmp = as.matrix(read.csv( paste(prefix[i],'_tavg.csv',sep='')))
			hold_tavg[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
			
			tmp = as.matrix(read.csv( paste(prefix[i],'_vpd.csv',sep='')))
			hold_vpd[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
			
			tmp = as.matrix(read.csv( paste(prefix[i],'_recharge.csv',sep='')))
			hold_recharge[,i,] = tmp[unlist(dailytimeSeriesMatch[i]),4:(replication+3)]
		}
	}#i
	
	comb_flow = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	comb_pet = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	comb_et = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	comb_rain = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	comb_tmax = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	comb_tmin = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	
	comb_satz = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	comb_unsat = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	comb_evap = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	comb_snow = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	comb_return = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	comb_psn = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	comb_lai = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	
	comb_tavg = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	comb_vpd = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	comb_recharge = matrix(NA,sum(unlist(dailytimeSeriesMatch[1])), replication)
	
	for(j in 1: replication){
		comb_flow[,j] = hold_flow[,,j]%*% arealweight/sum(arealweight)*scaler*0.001	
		comb_pet[,j] = hold_pet[,,j]%*% arealweight/sum(arealweight)	
		comb_et[,j] = hold_et[,,j]%*% arealweight/sum(arealweight)	
		comb_rain[,j] = hold_rain[,,j]%*% arealweight/sum(arealweight)	
		comb_tmax[,j] = hold_tmax[,,j]%*% arealweight/sum(arealweight)	
		comb_tmin[,j] = hold_tmin[,,j]%*% arealweight/sum(arealweight)
		
		comb_satz[,j] = hold_satz[,,j]%*% arealweight/sum(arealweight)
		comb_unsat[,j] = hold_unsat[,,j]%*% arealweight/sum(arealweight)
		comb_evap[,j] = hold_evap[,,j]%*% arealweight/sum(arealweight)
		comb_snow[,j] = hold_snow[,,j]%*% arealweight/sum(arealweight)
		comb_return[,j] = hold_return[,,j]%*% arealweight/sum(arealweight)
		comb_psn[,j] = hold_psn[,,j]%*% arealweight/sum(arealweight)
		comb_lai[,j] = hold_lai[,,j]%*% arealweight/sum(arealweight)
		
		if(NewVersion){
			comb_tavg[,j] = hold_tavg[,,j]%*% arealweight/sum(arealweight)
			comb_vpd[,j] = hold_vpd[,,j]%*% arealweight/sum(arealweight)
			comb_recharge[,j] = hold_recharge[,,j]%*% arealweight/sum(arealweight)
		}	
	}#j
	
	result = cbind(time,comb_flow)
	colnames(result) = c('year','month','day', paste('flowcmd',1:replication,sep='_')  )
	write.csv(result,paste(outPrefix,'_flowcmd.csv',sep=''),row.names=F)
	
	result = cbind(time,comb_pet)
	colnames(result) = c('year','month','day', paste('pet',1:replication,sep='_')  )
	write.csv(result,paste(outPrefix,'_pet.csv',sep=''),row.names=F)
	
	result = cbind(time,comb_et)
	colnames(result) = c('year','month','day', paste('et',1:replication,sep='_')  )
	write.csv(result,paste(outPrefix,'_et.csv',sep=''),row.names=F)
	
	result = cbind(time,comb_rain)
	colnames(result) = c('year','month','day', paste('precip',1:replication,sep='_')  )
	write.csv(result,paste(outPrefix,'_precip.csv',sep=''),row.names=F)
	
	result = cbind(time,comb_tmax)
	colnames(result) = c('year','month','day', paste('tmax',1:replication,sep='_')  )
	write.csv(result,paste(outPrefix,'_tmax.csv',sep=''),row.names=F)
	
	result = cbind(time,comb_tmin)
	colnames(result) = c('year','month','day', paste('tmin',1:replication,sep='_')  )
	write.csv(result,paste(outPrefix,'_tmin.csv',sep=''),row.names=F)
	
	
	result = cbind(time,comb_satz)
	colnames(result) = c('year','month','day', paste('satz',1:replication,sep='_')  )
	write.csv(result,paste(outPrefix,'_satz.csv',sep=''),row.names=F)
	
	result = cbind(time,comb_unsat)
	colnames(result) = c('year','month','day', paste('unsat',1:replication,sep='_')  )
	write.csv(result,paste(outPrefix,'_unsat.csv',sep=''),row.names=F)
	
	result = cbind(time,comb_evap)
	colnames(result) = c('year','month','day', paste('evap',1:replication,sep='_')  )
	write.csv(result,paste(outPrefix,'_evap.csv',sep=''),row.names=F)
	
	result = cbind(time,comb_snow)
	colnames(result) = c('year','month','day', paste('snow',1:replication,sep='_')  )
	write.csv(result,paste(outPrefix,'_snow.csv',sep=''),row.names=F)
	
	result = cbind(time,comb_return)
	colnames(result) = c('year','month','day', paste('return',1:replication,sep='_')  )
	write.csv(result,paste(outPrefix,'_return.csv',sep=''),row.names=F)
	
	result = cbind(time,comb_psn)
	colnames(result) = c('year','month','day', paste('psn',1:replication,sep='_')  )
	write.csv(result,paste(outPrefix,'_psn.csv',sep=''),row.names=F)
	
	result = cbind(time,comb_lai)
	colnames(result) = c('year','month','day', paste('lai',1:replication,sep='_')  )
	write.csv(result,paste(outPrefix,'_lai.csv',sep=''),row.names=F)
	
	if(NewVersion){
		result = cbind(time,comb_tavg)
		colnames(result) = c('year','month','day', paste('tavg',1:replication,sep='_')  )
		write.csv(result,paste(outPrefix,'_tavg.csv',sep=''),row.names=F)
		
		result = cbind(time,comb_vpd)
		colnames(result) = c('year','month','day', paste('vpd',1:replication,sep='_')  )
		write.csv(result,paste(outPrefix,'_vpd.csv',sep=''),row.names=F)
		
		result = cbind(time,comb_recharge)
		colnames(result) = c('year','month','day', paste('recharge',1:replication,sep='_')  )
		write.csv(result,paste(outPrefix,'_recharge.csv',sep=''),row.names=F)
	}
	
}#function

WSC_cutPeriod=function( prefix, period, label, prefixBias_ ='', NewVersion=F){
	# after bias correction	
	namelist = c(
		'_flowcmd_bias',
		'_flowcmd',
		'_et',
		'_pet',
		'_precip',
		'_tmax',
		'_tmin',
		'_satz',
		'_unsat',
		'_evap',
		'_snow',
		'_return',
		'_psn',
		'_lai'
	)
	
	if(prefixBias_ == ''){ prefixBias = prefix; }else{ prefixBias = prefixBias_; }  
	hold = as.matrix(read.csv( paste(prefixBias, namelist[1],'.csv',sep='')))
	time = as.Date(paste(hold[,3], hold[,2], hold[,1],sep="-"),format="%d-%m-%Y")
	
	hold2 = as.matrix(read.csv( paste(prefix, namelist[2],'.csv',sep='')))
	time2 = as.Date(paste(hold2[,3], hold2[,2], hold2[,1],sep="-"),format="%d-%m-%Y")
	
	tmp = match3DailyTimeSeries(time, time2, period) ### assume period is the most narrow band
	hold.dailytimeSeriesMatch = tmp$xSelect
	hold2.dailytimeSeriesMatch = tmp$ySelect
	#write.csv(hold[hold.dailytimeSeriesMatch,],paste(prefix, namelist[1],'_',label,'.csv',sep=''),row.names=F)
	#write.csv(hold2[hold2.dailytimeSeriesMatch,],paste(prefix, namelist[2],'_',label,'.csv',sep=''),row.names=F)
	
	write.csv(hold[hold.dailytimeSeriesMatch,],paste(prefixBias, label,'_',namelist[1],'.csv',sep=''),row.names=F)
	write.csv(hold2[hold2.dailytimeSeriesMatch,],paste(prefix, label,'_',namelist[2],'.csv',sep=''),row.names=F)
	
	for(i in 3:length(namelist)){
		hold = as.matrix(read.csv( paste(prefix, namelist[i],'.csv',sep='')))
		write.csv(hold[hold2.dailytimeSeriesMatch,],paste(prefix, label,'_',namelist[i],'.csv',sep=''),row.names=F)
	}#i
	
	if(NewVersion){
		hold = as.matrix(read.csv( paste(prefix, '_tavg','.csv',sep='')))
		write.csv(hold[hold2.dailytimeSeriesMatch,],paste(prefix, label,'_','_tavg','.csv',sep=''),row.names=F)
		
		hold = as.matrix(read.csv( paste(prefix, '_vpd','.csv',sep='')))
		write.csv(hold[hold2.dailytimeSeriesMatch,],paste(prefix, label,'_','_vpd','.csv',sep=''),row.names=F)
		
		hold = as.matrix(read.csv( paste(prefix, '_recharge','.csv',sep='')))
		write.csv(hold[hold2.dailytimeSeriesMatch,],paste(prefix, label,'_','_recharge','.csv',sep=''),row.names=F)
	}
		
		
}#function


MultipleBasinSeries2WSC=function( prefix, sub, Jindex, outputPrefix, sitename=NA, period=NULL, subPrefix=NA, toLake=F,lakeArea=NA,label=''){
	# first step (everything is in terms of mm/day )
	# convert a series of rhessys basin output at the same watershed (need to combine multiple sub-basin) into WSC format
	
	# ls -l world_subbasin_??? | awk '{print $9}'
	subFile = read.csv(paste(sub,sep=''),stringsAsFactors=F)
	
	tmp = unlist(strsplit(prefix,split='/'))
	if(length(tmp)>1){location = paste(tmp[1:(length(tmp)-1)],collapse='/')}else{location='.'}
	if(is.na(sitename)){sitename = tmp[length(tmp)]}
	if(is.na(subPrefix)){subPrefix = '_world_subbasin_'}
	if(is.na(lakeArea)){lakeArea=sum(subFile[,'area'])}
	
	outputname = paste(prefix, sitename, subPrefix,subFile[1,'id'],'_',Jindex[1],'_basin.daily',sep='' )
	tmp = read.table(outputname,header=F,skip=1) 
	if(is.null(period)){
		print('use RHESSys output period')
		period = as.Date(paste(tmp[,1], tmp[,2], tmp[,3],sep="-"),format="%d-%m-%Y")
		rhessys.dailytimeSeriesMatch = rep(T,length(period))
		period.dailytimeSeriesMatch = rep(T,length(period))
		print(range(period))
	}else{
		rhessys.date = as.Date(paste(tmp[,1], tmp[,2], tmp[,3],sep="-"),format="%d-%m-%Y")
		tmp = match2DailyTimeSeries(rhessys.date, period) ### assume period is the most narrow band
		rhessys.dailytimeSeriesMatch = tmp$xSelect
		period.dailytimeSeriesMatch = tmp$ySelect
	}
	
	
	jMax = length(Jindex)
	holding = matrix(NA,sum(period.dailytimeSeriesMatch), jMax) #flow
	holding_et = matrix(NA, sum(period.dailytimeSeriesMatch), jMax) 
	holding_pet = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_precip = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_tmax = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_tmin = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_satz = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_unsat = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_evap = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_snow = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_return = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_psn = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_lai = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_tavg = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_vpd = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_recharge = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	for(j in 1:jMax){
		
		# multiple sub-catchment
		secondholding = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_et = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_pet = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_rain = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_tmax = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_tmin = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_satz = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_unsat = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_evap = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_snow = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_return = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_psn = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_lai = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_tavg = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_vpd = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_recharge = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		NewVersion=F
		for(kk in 1:nrow(subFile)){
			
			tryCatch({
				outputname = paste(prefix, sitename, subPrefix,subFile[kk,'id'],'_',Jindex[j],'_basin.daily',sep='' )
				tmp = read.table(outputname,header=F,skip=1) 
				if(ncol(tmp)>=43){
					# 5.20
					secondholding[,kk]=tmp[rhessys.dailytimeSeriesMatch,19]
					secondholding_et[,kk]=tmp[rhessys.dailytimeSeriesMatch,14]+tmp[rhessys.dailytimeSeriesMatch,16]
					secondholding_pet[,kk]=tmp[rhessys.dailytimeSeriesMatch,33]
					secondholding_rain[,kk]=tmp[rhessys.dailytimeSeriesMatch,35]# rain mm 
					tmax_ = tmp[rhessys.dailytimeSeriesMatch,38]# tmax C 
					tmin_ = tmp[rhessys.dailytimeSeriesMatch,39]# tmin C
					secondholding_tmax[,kk]=colMaxs( rbind(tmax_,tmin_))  
					secondholding_tmin[,kk]=colMins( rbind(tmax_,tmin_))
					secondholding_satz[,kk]=tmp[rhessys.dailytimeSeriesMatch,7]# satz
					secondholding_unsat[,kk]=tmp[rhessys.dailytimeSeriesMatch,10]# unsat
					secondholding_evap[,kk]=tmp[rhessys.dailytimeSeriesMatch,14]# evap
					secondholding_snow[,kk]=tmp[rhessys.dailytimeSeriesMatch,15]# snowpack
					secondholding_return[,kk]=tmp[rhessys.dailytimeSeriesMatch,18]# return flow
					secondholding_psn[,kk]=tmp[rhessys.dailytimeSeriesMatch,20]# psn
					secondholding_lai[,kk]=tmp[rhessys.dailytimeSeriesMatch,21]# lai
					secondholding_tavg[,kk]=tmp[rhessys.dailytimeSeriesMatch,40]# tavg
					secondholding_vpd[,kk]=tmp[rhessys.dailytimeSeriesMatch,41]# vpd
					secondholding_recharge[,kk]=tmp[rhessys.dailytimeSeriesMatch,43]# recharge
					NewVersion=T
				}else{
					# 5.18
					secondholding[,kk]=tmp[rhessys.dailytimeSeriesMatch,19]
					secondholding_et[,kk]=tmp[rhessys.dailytimeSeriesMatch,14]+tmp[rhessys.dailytimeSeriesMatch,16]
					secondholding_pet[,kk]=tmp[rhessys.dailytimeSeriesMatch,33]
					secondholding_rain[,kk]=tmp[rhessys.dailytimeSeriesMatch,35]# rain mm 
					tmax_ = tmp[rhessys.dailytimeSeriesMatch,37]# tmax C 
					tmin_ = tmp[rhessys.dailytimeSeriesMatch,38]# tmin C
					secondholding_tmax[,kk]=colMaxs( rbind(tmax_,tmin_))  
					secondholding_tmin[,kk]=colMins( rbind(tmax_,tmin_))
					secondholding_satz[,kk]=tmp[rhessys.dailytimeSeriesMatch,7]# satz
					secondholding_unsat[,kk]=tmp[rhessys.dailytimeSeriesMatch,10]# unsat
					secondholding_evap[,kk]=tmp[rhessys.dailytimeSeriesMatch,14]# evap
					secondholding_snow[,kk]=tmp[rhessys.dailytimeSeriesMatch,15]# snowpack
					secondholding_return[,kk]=tmp[rhessys.dailytimeSeriesMatch,18]# return flow
					secondholding_psn[,kk]=tmp[rhessys.dailytimeSeriesMatch,20]# psn
					secondholding_lai[,kk]=tmp[rhessys.dailytimeSeriesMatch,21]# lai
				}}, 
				warning = function(w) {
				    print(kk)
				    print(outputname)
				    print(w)
				}, error = function(e) {
				    print(kk)
				    print(outputname)
				    print(e)
				}
			
			)#tryCatch
			
			
		}#kk
		
		holding[,j] = secondholding %*%subFile[,'grid']/sum(subFile[,'grid'])
		holding_et[,j] = secondholding_et %*%subFile[,'grid']/sum(subFile[,'grid'])
		holding_pet[,j] = secondholding_pet %*%subFile[,'grid']/sum(subFile[,'grid'])
		
		holding_precip[,j] = secondholding_rain%*%subFile[,'grid']/sum(subFile[,'grid'])# rain mm 
		holding_tmax[,j] = secondholding_tmax%*%subFile[,'grid']/sum(subFile[,'grid'])# tmax C 
		holding_tmin[,j] = secondholding_tmin%*%subFile[,'grid']/sum(subFile[,'grid'])# tmin C 
	
		holding_satz[,j] = secondholding_satz%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_unsat[,j] = secondholding_unsat%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_evap[,j] = secondholding_evap%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_snow[,j] = secondholding_snow%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_return[,j] = secondholding_return%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_psn[,j] = secondholding_psn%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_lai[,j] = secondholding_lai%*%subFile[,'grid']/sum(subFile[,'grid'])#
		
		if(NewVersion){
			holding_tavg[,j] = secondholding_tavg%*%subFile[,'grid']/sum(subFile[,'grid'])#
			holding_vpd[,j] = secondholding_vpd%*%subFile[,'grid']/sum(subFile[,'grid'])#
			holding_recharge[,j] = secondholding_recharge%*%subFile[,'grid']/sum(subFile[,'grid'])#	
		}
		
	}#j
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding
	)
	colnames(result) = c('year','month','day', paste('streamflowmm', Jindex,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,label,'_streamflowmm.csv',sep=''),row.names=F)
	
	if(toLake){
		result = cbind(
			format(period[period.dailytimeSeriesMatch],format='%Y'),
			format(period[period.dailytimeSeriesMatch],format='%m'),
			format(period[period.dailytimeSeriesMatch],format='%d'),
			holding*0.001*lakeArea
		)
		colnames(result) = c('year','month','day', paste('flowcmd', Jindex,sep='_')  )
		write.csv(result,paste(location,'/',outputPrefix, sitename,'_flowcmd.csv',sep=''),row.names=F)
	}
	
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_et
	)
	colnames(result) = c('year','month','day', paste('et', Jindex,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,'_et.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_pet
	)
	colnames(result) = c('year','month','day', paste('pet', Jindex,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,'_pet.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_precip
	)
	colnames(result) = c('year','month','day', paste('precip', Jindex,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,'_precip.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_tmax
	)
	colnames(result) = c('year','month','day', paste('tmax', Jindex,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,'_tmax.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_tmin
	)
	colnames(result) = c('year','month','day', paste('tmin', Jindex,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,'_tmin.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_satz
	)
	colnames(result) = c('year','month','day', paste('satz', Jindex,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,label,'_satz.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_unsat
	)
	colnames(result) = c('year','month','day', paste('unsat', Jindex,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,label,'_unsat.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_evap
	)
	colnames(result) = c('year','month','day', paste('evap', Jindex,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,label,'_evap.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_snow
	)
	colnames(result) = c('year','month','day', paste('snow', Jindex,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,label,'_snow.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_return
	)
	colnames(result) = c('year','month','day', paste('return', Jindex,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,label,'_return.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_psn
	)
	colnames(result) = c('year','month','day', paste('psn', Jindex,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,label,'_psn.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_lai
	)
	colnames(result) = c('year','month','day', paste('lai', Jindex,sep='_')  )
	write.csv(result,paste(location,'/',outputPrefix, sitename,label,'_lai.csv',sep=''),row.names=F)
	
	
	if(NewVersion){
		
		result = cbind(
			format(period[period.dailytimeSeriesMatch],format='%Y'),
			format(period[period.dailytimeSeriesMatch],format='%m'),
			format(period[period.dailytimeSeriesMatch],format='%d'),
			holding_tavg
		)
		colnames(result) = c('year','month','day', paste('tavg', Jindex,sep='_')  )
		write.csv(result,paste(location,'/',outputPrefix, sitename,label,'_tavg.csv',sep=''),row.names=F)
		
		result = cbind(
			format(period[period.dailytimeSeriesMatch],format='%Y'),
			format(period[period.dailytimeSeriesMatch],format='%m'),
			format(period[period.dailytimeSeriesMatch],format='%d'),
			holding_vpd
		)
		colnames(result) = c('year','month','day', paste('vpd', Jindex,sep='_')  )
		write.csv(result,paste(location,'/',outputPrefix, sitename,label,'_vpd.csv',sep=''),row.names=F)
		
		result = cbind(
			format(period[period.dailytimeSeriesMatch],format='%Y'),
			format(period[period.dailytimeSeriesMatch],format='%m'),
			format(period[period.dailytimeSeriesMatch],format='%d'),
			holding_recharge
		)
		colnames(result) = c('year','month','day', paste('recharge', Jindex,sep='_')  )
		write.csv(result,paste(location,'/',outputPrefix, sitename,label,'_recharge.csv',sep=''),row.names=F)
		
		
	}
	
	
}#function

# lakeID	name	area_m2	grid	simulation	percent
# 4	657		buttner	72981900	81091	70761	0.87261225
# 1	865		michie	432610200	480678	472280	0.982528845
# 7	948		westfor	23639400	26266	16277	0.61969847
# 6	1009	orange	22968000	25520	21294	0.834404389
# 5	1127	rogers	44847900	49831	46498	0.933113925
# 2	1146	little	249788700	277543	267050	0.962193246
# 3	1439	fall	1099539900	1221711	1089327	0.891640494
# 8	2046	cane	80128800	89032	78132	0.877572109
# 9	2317	university	76376700 	84863	80021	0.942943332
# 10	2435	jordan	946452600	1051614	945619	0.899207314 (include newhope) 
# total jordan lake with HAW 4369799700 4855333
# total falls lake 1996320600 2218134
# swift 1.7172e+8 m2

# 101	flat	385145100	427939
# 102	little	202600800	225112
# 103	mtn	20677500	22975
# 104	morgan	21401100	23779
# 105	cane	19687500	21875
# 106	newhope	198703800	220782 --> 198868500 (model)
# 107	eno	365411700	406013
# 108	ellerbeClub	13273200	14748
# 109	ellerbeGorman	41607900	46231
# 110	lick	9989100	11099
# 111	northeast	55437300	61597
# 112   swift	54394200	60438



##-------------------------- outdated functions
WSC_usgs2lake=function( inputname, scaler, outputname){
	
	tmp = unlist(strsplit(inputname,split='/'))
	if(length(tmp)>1){location = paste(tmp[1:(length(tmp)-1)],collapse='/')}else{location='.'}	
	inputnamePrefix = tmp[length(tmp)]
	print(location)
	print(inputnamePrefix)
	
	tmp = read.csv(paste(inputname,'_streamflowmm.csv',sep=''))
	tmp[,4:ncol(tmp)] = tmp[,4:ncol(tmp)]*0.001*scaler
	write.csv(tmp,paste(location,'/',outputname,'_flowcmd.csv',sep=''),row.names=F)
	
	system(paste('cp ', inputname, '_pet.csv ',location,'/',outputname,'_pet.csv', sep=''))
	system(paste('cp ', inputname, '_precip.csv ',location,'/',outputname,'_precip.csv', sep=''))
	system(paste('cp ', inputname, '_tmax.csv ',location,'/',outputname,'_tmax.csv', sep=''))
	system(paste('cp ', inputname, '_tmin.csv ',location,'/',outputname,'_tmin.csv', sep=''))
}#function


ConditionMultipleBasinSeries2WSC =function(CONDS,prefix, sub, Jindex, outputPrefix, sitename=NA, period=NULL, subPrefix=NA, toLake=F,lakeArea=NA,label=''){
	# assuming more than 1 condition
	# CONDS: multiple conditions based on sub.csv (T/F-matrix: nrow = sub, col=conds)
	# prefix: multiple prefixes (should correspond to CONDS)
	# Jidex: index matrix: nrow=index, and col=cond
	
	# first step (everything is in terms of mm/day )
	# convert a series of rhessys basin output at the same watershed (need to combine multiple sub-basin) into WSC format
	
	# ls -l world_subbasin_??? | awk '{print $9}'
	subFile = read.csv(paste(sub,sep=''),stringsAsFactors=F)
	
	
	location = rep(NA, length(prefix))
	for(i in 1:length(location)){
		tmp = unlist(strsplit(prefix[i],split='/'))
		if(length(tmp)>1){location[i] = paste(tmp[1:(length(tmp)-1)],collapse='/')}else{location[i]='.'}
	}#i
	
	if(is.na(sitename)){sitename = tmp[length(tmp)]}
	if(is.na(subPrefix)){subPrefix = '_world_subbasin_'}
	if(is.na(lakeArea)){lakeArea=sum(subFile[,'area'])}
	
	prefixuseIndex = which(CONDS[1,]==T)
	outputname = paste(prefix[prefixuseIndex], sitename, subPrefix,subFile[1,'id'],'_',Jindex[1, prefixuseIndex],'_basin.daily',sep='' )
	tmp = read.table(outputname,header=F,skip=1) 
	if(is.null(period)){
		print('use RHESSys output period')
		period = as.Date(paste(tmp[,1], tmp[,2], tmp[,3],sep="-"),format="%d-%m-%Y")
		rhessys.dailytimeSeriesMatch = rep(T,length(period))
		period.dailytimeSeriesMatch = rep(T,length(period))
		print(range(period))
	}else{
		rhessys.date = as.Date(paste(tmp[,1], tmp[,2], tmp[,3],sep="-"),format="%d-%m-%Y")
		tmp = match2DailyTimeSeries(rhessys.date, period) ### assume period is the most narrow band
		rhessys.dailytimeSeriesMatch = tmp$xSelect
		period.dailytimeSeriesMatch = tmp$ySelect
	}
	
	
	jMax = nrow(Jindex)
	holding = matrix(NA,sum(period.dailytimeSeriesMatch), jMax) #flow
	holding_et = matrix(NA, sum(period.dailytimeSeriesMatch), jMax) 
	holding_pet = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_precip = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_tmax = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_tmin = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_satz = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_unsat = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_evap = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_snow = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_return = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_psn = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_lai = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	for(j in 1:jMax){
		
		# multiple sub-catchment
		secondholding = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_et = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_pet = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_rain = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_tmax = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_tmin = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_satz = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_unsat = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_evap = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_snow = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_return = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_psn = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_lai = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		for(kk in 1:nrow(subFile)){
			
			tryCatch({
				prefixuseIndex = which(CONDS[kk,]==T)
				outputname = paste(prefix[prefixuseIndex], sitename, subPrefix,subFile[kk,'id'],'_',Jindex[j, prefixuseIndex],'_basin.daily',sep='' )
				tmp = read.table(outputname,header=F,skip=1) 
				if(ncol(tmp)>70){
					# 5.20
					secondholding[,kk]=tmp[rhessys.dailytimeSeriesMatch,19]
					secondholding_et[,kk]=tmp[rhessys.dailytimeSeriesMatch,14]+tmp[rhessys.dailytimeSeriesMatch,16]
					secondholding_pet[,kk]=tmp[rhessys.dailytimeSeriesMatch,33]
					secondholding_rain[,kk]=tmp[rhessys.dailytimeSeriesMatch,35]# rain mm 
					tmax_ = tmp[rhessys.dailytimeSeriesMatch,38]# tmax C 
					tmin_ = tmp[rhessys.dailytimeSeriesMatch,39]# tmin C
					secondholding_tmax[,kk]=colMaxs( rbind(tmax_,tmin_))  
					secondholding_tmin[,kk]=colMins( rbind(tmax_,tmin_))
					secondholding_satz[,kk]=tmp[rhessys.dailytimeSeriesMatch,7]# satz
					secondholding_unsat[,kk]=tmp[rhessys.dailytimeSeriesMatch,10]# unsat
					secondholding_evap[,kk]=tmp[rhessys.dailytimeSeriesMatch,14]# evap
					secondholding_snow[,kk]=tmp[rhessys.dailytimeSeriesMatch,15]# snowpack
					secondholding_return[,kk]=tmp[rhessys.dailytimeSeriesMatch,18]# return flow
					secondholding_psn[,kk]=tmp[rhessys.dailytimeSeriesMatch,20]# psn
					secondholding_lai[,kk]=tmp[rhessys.dailytimeSeriesMatch,21]# lai
				}else{
					# 5.18
					secondholding[,kk]=tmp[rhessys.dailytimeSeriesMatch,19]
					secondholding_et[,kk]=tmp[rhessys.dailytimeSeriesMatch,14]+tmp[rhessys.dailytimeSeriesMatch,16]
					secondholding_pet[,kk]=tmp[rhessys.dailytimeSeriesMatch,33]
					secondholding_rain[,kk]=tmp[rhessys.dailytimeSeriesMatch,35]# rain mm 
					tmax_ = tmp[rhessys.dailytimeSeriesMatch,37]# tmax C 
					tmin_ = tmp[rhessys.dailytimeSeriesMatch,38]# tmin C
					secondholding_tmax[,kk]=colMaxs( rbind(tmax_,tmin_))  
					secondholding_tmin[,kk]=colMins( rbind(tmax_,tmin_))
					secondholding_satz[,kk]=tmp[rhessys.dailytimeSeriesMatch,7]# satz
					secondholding_unsat[,kk]=tmp[rhessys.dailytimeSeriesMatch,10]# unsat
					secondholding_evap[,kk]=tmp[rhessys.dailytimeSeriesMatch,14]# evap
					secondholding_snow[,kk]=tmp[rhessys.dailytimeSeriesMatch,15]# snowpack
					secondholding_return[,kk]=tmp[rhessys.dailytimeSeriesMatch,18]# return flow
					secondholding_psn[,kk]=tmp[rhessys.dailytimeSeriesMatch,20]# psn
					secondholding_lai[,kk]=tmp[rhessys.dailytimeSeriesMatch,21]# lai
				}}, 
				warning = function(w) {
				    print(kk)
				    print(outputname)
				    print(w)
				}, error = function(e) {
				    print(kk)
				    print(outputname)
				    print(e)
				}
			
			)#tryCatch
			
			
		}#kk
		
		holding[,j] = secondholding %*%subFile[,'grid']/sum(subFile[,'grid'])
		holding_et[,j] = secondholding_et %*%subFile[,'grid']/sum(subFile[,'grid'])
		holding_pet[,j] = secondholding_pet %*%subFile[,'grid']/sum(subFile[,'grid'])
		
		holding_precip[,j] = secondholding_rain%*%subFile[,'grid']/sum(subFile[,'grid'])# rain mm 
		holding_tmax[,j] = secondholding_tmax%*%subFile[,'grid']/sum(subFile[,'grid'])# tmax C 
		holding_tmin[,j] = secondholding_tmin%*%subFile[,'grid']/sum(subFile[,'grid'])# tmin C 
	
		holding_satz[,j] = secondholding_satz%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_unsat[,j] = secondholding_unsat%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_evap[,j] = secondholding_evap%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_snow[,j] = secondholding_snow%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_return[,j] = secondholding_return%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_psn[,j] = secondholding_psn%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_lai[,j] = secondholding_lai%*%subFile[,'grid']/sum(subFile[,'grid'])#
	}#j
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding
	)
	colnames(result) = c('year','month','day', paste('streamflowmm',1:jMax,sep='_')  )
	write.csv(result,paste(location[1],'/',outputPrefix, sitename,label,'_streamflowmm.csv',sep=''),row.names=F)
	
	if(toLake){
		result = cbind(
			format(period[period.dailytimeSeriesMatch],format='%Y'),
			format(period[period.dailytimeSeriesMatch],format='%m'),
			format(period[period.dailytimeSeriesMatch],format='%d'),
			holding*0.001*lakeArea
		)
		colnames(result) = c('year','month','day', paste('flowcmd',1:jMax,sep='_')  )
		write.csv(result,paste(location[1],'/',outputPrefix, sitename,'_flowcmd.csv',sep=''),row.names=F)
	}
	
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_et
	)
	colnames(result) = c('year','month','day', paste('et',1:jMax,sep='_')  )
	write.csv(result,paste(location[1],'/',outputPrefix, sitename,'_et.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_pet
	)
	colnames(result) = c('year','month','day', paste('pet',1:jMax,sep='_')  )
	write.csv(result,paste(location[1],'/',outputPrefix, sitename,'_pet.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_precip
	)
	colnames(result) = c('year','month','day', paste('precip',1:jMax,sep='_')  )
	write.csv(result,paste(location[1],'/',outputPrefix, sitename,'_precip.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_tmax
	)
	colnames(result) = c('year','month','day', paste('tmax',1:jMax,sep='_')  )
	write.csv(result,paste(location[1],'/',outputPrefix, sitename,'_tmax.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_tmin
	)
	colnames(result) = c('year','month','day', paste('tmin',1:jMax,sep='_')  )
	write.csv(result,paste(location[1],'/',outputPrefix, sitename,'_tmin.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_satz
	)
	colnames(result) = c('year','month','day', paste('satz',1:jMax,sep='_')  )
	write.csv(result,paste(location[1],'/',outputPrefix, sitename,label,'_satz.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_unsat
	)
	colnames(result) = c('year','month','day', paste('unsat',1:jMax,sep='_')  )
	write.csv(result,paste(location[1],'/',outputPrefix, sitename,label,'_unsat.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_evap
	)
	colnames(result) = c('year','month','day', paste('evap',1:jMax,sep='_')  )
	write.csv(result,paste(location[1],'/',outputPrefix, sitename,label,'_evap.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_snow
	)
	colnames(result) = c('year','month','day', paste('snow',1:jMax,sep='_')  )
	write.csv(result,paste(location[1],'/',outputPrefix, sitename,label,'_snow.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_return
	)
	colnames(result) = c('year','month','day', paste('return',1:jMax,sep='_')  )
	write.csv(result,paste(location[1],'/',outputPrefix, sitename,label,'_return.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_psn
	)
	colnames(result) = c('year','month','day', paste('psn',1:jMax,sep='_')  )
	write.csv(result,paste(location[1],'/',outputPrefix, sitename,label,'_psn.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_lai
	)
	colnames(result) = c('year','month','day', paste('lai',1:jMax,sep='_')  )
	write.csv(result,paste(location[1],'/',outputPrefix, sitename,label,'_lai.csv',sep=''),row.names=F)
	
}#function

ConditionII_MultipleBasinSeries2WSC =function(CONDS,prefix, sub, Jindex, outputPrefix, sitename=NA, period=NULL, subPrefix=NA, toLake=F,lakeArea=NA,label=''){
	# assuming more than 1 condition
	# CONDS: multiple conditions based on sub.csv (T/F-matrix: nrow = sub, col=conds)
	# prefix: multiple prefixes (should correspond to CONDS)
	# Jidex: index matrix: nrow=index, and col=cond
	
	# first step (everything is in terms of mm/day )
	# convert a series of rhessys basin output at the same watershed (need to combine multiple sub-basin) into WSC format
	
	# ls -l world_subbasin_??? | awk '{print $9}'
	subFile = read.csv(paste(sub,sep=''),stringsAsFactors=F)
	
	
	# location = rep(NA, length(prefix))
	# for(i in 1:length(location)){
		# tmp = unlist(strsplit(prefix[i],split='/'))
		# if(length(tmp)>1){location[i] = paste(tmp[1:(length(tmp)-1)],collapse='/')}else{location[i]='.'}
	# }#i
	
	if(is.na(sitename)){sitename = tmp[length(tmp)]}
	if(is.na(subPrefix)){subPrefix = '_world_subbasin_'}
	if(is.na(lakeArea)){lakeArea=sum(subFile[,'area'])}
	
	prefixuseIndex = which(CONDS[1,]==T) #find out which cond the first file is on
	outputname = paste(prefix[1,prefixuseIndex], sitename, subPrefix,subFile[1,'id'],'_',Jindex[1, prefixuseIndex],'_basin.daily',sep='' )
	tmp = read.table(outputname,header=F,skip=1) 
	if(is.null(period)){
		print('use RHESSys output period')
		period = as.Date(paste(tmp[,1], tmp[,2], tmp[,3],sep="-"),format="%d-%m-%Y")
		rhessys.dailytimeSeriesMatch = rep(T,length(period))
		period.dailytimeSeriesMatch = rep(T,length(period))
		print(range(period))
	}else{
		rhessys.date = as.Date(paste(tmp[,1], tmp[,2], tmp[,3],sep="-"),format="%d-%m-%Y")
		tmp = match2DailyTimeSeries(rhessys.date, period) ### assume period is the most narrow band
		rhessys.dailytimeSeriesMatch = tmp$xSelect
		period.dailytimeSeriesMatch = tmp$ySelect
	}
	
	
	jMax = nrow(Jindex)
	holding = matrix(NA,sum(period.dailytimeSeriesMatch), jMax) #flow
	holding_et = matrix(NA, sum(period.dailytimeSeriesMatch), jMax) 
	holding_pet = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_precip = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_tmax = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_tmin = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_satz = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_unsat = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_evap = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_snow = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_return = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_psn = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	holding_lai = matrix(NA, sum(period.dailytimeSeriesMatch), jMax)
	for(j in 1:jMax){
		
		# multiple sub-catchment
		secondholding = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_et = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_pet = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_rain = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_tmax = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_tmin = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_satz = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_unsat = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_evap = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_snow = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_return = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_psn = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		secondholding_lai = matrix(NA,sum(period.dailytimeSeriesMatch), nrow(subFile))
		for(kk in 1:nrow(subFile)){
			
			tryCatch({
				prefixuseIndex = which(CONDS[kk,]==T)
				outputname = paste(prefix[j,prefixuseIndex], sitename, subPrefix,subFile[kk,'id'],'_',Jindex[j, prefixuseIndex],'_basin.daily',sep='' )
				tmp = read.table(outputname,header=F,skip=1) 
				if(ncol(tmp)>70){
					# 5.20
					secondholding[,kk]=tmp[rhessys.dailytimeSeriesMatch,19]
					secondholding_et[,kk]=tmp[rhessys.dailytimeSeriesMatch,14]+tmp[rhessys.dailytimeSeriesMatch,16]
					secondholding_pet[,kk]=tmp[rhessys.dailytimeSeriesMatch,33]
					secondholding_rain[,kk]=tmp[rhessys.dailytimeSeriesMatch,35]# rain mm 
					tmax_ = tmp[rhessys.dailytimeSeriesMatch,38]# tmax C 
					tmin_ = tmp[rhessys.dailytimeSeriesMatch,39]# tmin C
					secondholding_tmax[,kk]=colMaxs( rbind(tmax_,tmin_))  
					secondholding_tmin[,kk]=colMins( rbind(tmax_,tmin_))
					secondholding_satz[,kk]=tmp[rhessys.dailytimeSeriesMatch,7]# satz
					secondholding_unsat[,kk]=tmp[rhessys.dailytimeSeriesMatch,10]# unsat
					secondholding_evap[,kk]=tmp[rhessys.dailytimeSeriesMatch,14]# evap
					secondholding_snow[,kk]=tmp[rhessys.dailytimeSeriesMatch,15]# snowpack
					secondholding_return[,kk]=tmp[rhessys.dailytimeSeriesMatch,18]# return flow
					secondholding_psn[,kk]=tmp[rhessys.dailytimeSeriesMatch,20]# psn
					secondholding_lai[,kk]=tmp[rhessys.dailytimeSeriesMatch,21]# lai
				}else{
					# 5.18
					secondholding[,kk]=tmp[rhessys.dailytimeSeriesMatch,19]
					secondholding_et[,kk]=tmp[rhessys.dailytimeSeriesMatch,14]+tmp[rhessys.dailytimeSeriesMatch,16]
					secondholding_pet[,kk]=tmp[rhessys.dailytimeSeriesMatch,33]
					secondholding_rain[,kk]=tmp[rhessys.dailytimeSeriesMatch,35]# rain mm 
					tmax_ = tmp[rhessys.dailytimeSeriesMatch,37]# tmax C 
					tmin_ = tmp[rhessys.dailytimeSeriesMatch,38]# tmin C
					secondholding_tmax[,kk]=colMaxs( rbind(tmax_,tmin_))  
					secondholding_tmin[,kk]=colMins( rbind(tmax_,tmin_))
					secondholding_satz[,kk]=tmp[rhessys.dailytimeSeriesMatch,7]# satz
					secondholding_unsat[,kk]=tmp[rhessys.dailytimeSeriesMatch,10]# unsat
					secondholding_evap[,kk]=tmp[rhessys.dailytimeSeriesMatch,14]# evap
					secondholding_snow[,kk]=tmp[rhessys.dailytimeSeriesMatch,15]# snowpack
					secondholding_return[,kk]=tmp[rhessys.dailytimeSeriesMatch,18]# return flow
					secondholding_psn[,kk]=tmp[rhessys.dailytimeSeriesMatch,20]# psn
					secondholding_lai[,kk]=tmp[rhessys.dailytimeSeriesMatch,21]# lai
				}}, 
				warning = function(w) {
				    print(kk)
				    print(outputname)
				    print(w)
				}, error = function(e) {
				    print(kk)
				    print(outputname)
				    print(e)
				}
			
			)#tryCatch
			
			
		}#kk
		
		holding[,j] = secondholding %*%subFile[,'grid']/sum(subFile[,'grid'])
		holding_et[,j] = secondholding_et %*%subFile[,'grid']/sum(subFile[,'grid'])
		holding_pet[,j] = secondholding_pet %*%subFile[,'grid']/sum(subFile[,'grid'])
		
		holding_precip[,j] = secondholding_rain%*%subFile[,'grid']/sum(subFile[,'grid'])# rain mm 
		holding_tmax[,j] = secondholding_tmax%*%subFile[,'grid']/sum(subFile[,'grid'])# tmax C 
		holding_tmin[,j] = secondholding_tmin%*%subFile[,'grid']/sum(subFile[,'grid'])# tmin C 
	
		holding_satz[,j] = secondholding_satz%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_unsat[,j] = secondholding_unsat%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_evap[,j] = secondholding_evap%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_snow[,j] = secondholding_snow%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_return[,j] = secondholding_return%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_psn[,j] = secondholding_psn%*%subFile[,'grid']/sum(subFile[,'grid'])#
		holding_lai[,j] = secondholding_lai%*%subFile[,'grid']/sum(subFile[,'grid'])#
	}#j
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding
	)
	colnames(result) = c('year','month','day', paste('streamflowmm',1:jMax,sep='_')  )
	write.csv(result,paste(outputPrefix, sitename,label,'_streamflowmm.csv',sep=''),row.names=F)
	
	if(toLake){
		result = cbind(
			format(period[period.dailytimeSeriesMatch],format='%Y'),
			format(period[period.dailytimeSeriesMatch],format='%m'),
			format(period[period.dailytimeSeriesMatch],format='%d'),
			holding*0.001*lakeArea
		)
		colnames(result) = c('year','month','day', paste('flowcmd',1:jMax,sep='_')  )
		write.csv(result,paste(outputPrefix, sitename,'_flowcmd.csv',sep=''),row.names=F)
	}
	
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_et
	)
	colnames(result) = c('year','month','day', paste('et',1:jMax,sep='_')  )
	write.csv(result,paste(outputPrefix, sitename,'_et.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_pet
	)
	colnames(result) = c('year','month','day', paste('pet',1:jMax,sep='_')  )
	write.csv(result,paste(outputPrefix, sitename,'_pet.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_precip
	)
	colnames(result) = c('year','month','day', paste('precip',1:jMax,sep='_')  )
	write.csv(result,paste(outputPrefix, sitename,'_precip.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_tmax
	)
	colnames(result) = c('year','month','day', paste('tmax',1:jMax,sep='_')  )
	write.csv(result,paste(outputPrefix, sitename,'_tmax.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_tmin
	)
	colnames(result) = c('year','month','day', paste('tmin',1:jMax,sep='_')  )
	write.csv(result,paste(outputPrefix, sitename,'_tmin.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_satz
	)
	colnames(result) = c('year','month','day', paste('satz',1:jMax,sep='_')  )
	write.csv(result,paste(outputPrefix, sitename,label,'_satz.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_unsat
	)
	colnames(result) = c('year','month','day', paste('unsat',1:jMax,sep='_')  )
	write.csv(result,paste(outputPrefix, sitename,label,'_unsat.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_evap
	)
	colnames(result) = c('year','month','day', paste('evap',1:jMax,sep='_')  )
	write.csv(result,paste(outputPrefix, sitename,label,'_evap.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_snow
	)
	colnames(result) = c('year','month','day', paste('snow',1:jMax,sep='_')  )
	write.csv(result,paste(outputPrefix, sitename,label,'_snow.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_return
	)
	colnames(result) = c('year','month','day', paste('return',1:jMax,sep='_')  )
	write.csv(result,paste(outputPrefix, sitename,label,'_return.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_psn
	)
	colnames(result) = c('year','month','day', paste('psn',1:jMax,sep='_')  )
	write.csv(result,paste(outputPrefix, sitename,label,'_psn.csv',sep=''),row.names=F)
	
	result = cbind(
		format(period[period.dailytimeSeriesMatch],format='%Y'),
		format(period[period.dailytimeSeriesMatch],format='%m'),
		format(period[period.dailytimeSeriesMatch],format='%d'),
		holding_lai
	)
	colnames(result) = c('year','month','day', paste('lai',1:jMax,sep='_')  )
	write.csv(result,paste(outputPrefix, sitename,label,'_lai.csv',sep=''),row.names=F)
	
}#function




##--------------------------------------------------------------------------------------------------------------------
##---------    below are exmaples 
##--------------------------------------------------------------------------------------------------------------------
if(F){
	
	#---------------------------------------------------- new hope Bias

	MultipleBasinSeries2WSC(
		prefix=paste("output_newhopeBias/FIAnlcdlocal_2010",sep=''),
		sub='newhope_sub.csv',
		Jindex = paste('param', c(1,2,3,4),sep=''),
		outputPrefix=paste('regionalNewhopeBias',sep=''),
		sitename='',
		period=seq.Date(from=as.Date('1940-1-1'), to=as.Date('2010-10-1'), by="day") ,
		subPrefix='_sub',
		toLake=T,
		lakeArea=198868500
	)


	MultipleBasinSeries2WSC(
		prefix=paste("output_jordanBias/FIAnlcdlocal_2010",sep=''),
		sub='jordan_sub.csv',
		Jindex = paste('param', c(1,2,3,4),sep=''),
		outputPrefix=paste('regionalJordanBias',sep=''),
		sitename='',
		period=seq.Date(from=as.Date('1940-1-1'), to=as.Date('2010-10-1'), by="day") ,
		subPrefix='_sub',
		toLake=T,
		lakeArea=747584100 #area excluding newhope
	)
	
	
	
	
	#---------------------------------------------------- michie regional simulation
	# MultipleBasinSeries2WSC(
		# prefix='output_michie_FIAnlcdlocal_proj2_s3/FIAnlcdlocal_2060',
		# sub='flat_region_sub.csv',
		# Jindex = paste('param', c(1,2,3,4),sep=''),
		# outputPrefix='regionalFlat2060',
		# sitename='',
		# period=NULL,
		# subPrefix='_sub',
		# toLake=T,
		# lakeArea=432610200
	# )
	
	projhh = c(1) #c(2,3,12,13)	
	allperiod = matrix(c(
		'2020','1990-10-1','2051-9-30',
		'2030','2000-10-1','2061-9-30',
		'2040','2010-10-1','2071-9-30',
		'2050','2020-10-1','2081-9-30',
		'2060','2030-10-1','2090-9-30'
	),nrow=5,ncol=3,byrow=T)
	
	for(jj in 1:1){  #nrow(allperiod) 
		for(hh in projhh){
			
			MultipleBasinSeries2WSC(
				prefix=paste("output_michie_FIAnlcdlocal_proj",hh,"_csiroRCP6r7/FIAnlcdlocal_",allperiod[jj,1],sep=''),
				sub='flat_region_sub.csv',
				Jindex = paste('param', c(1,2,3,4),sep=''),
				outputPrefix=paste('regionalFlat',allperiod[jj,1],sep=''),
				sitename='',
				period=seq.Date(from=as.Date(allperiod[jj,2]), to=as.Date(allperiod[jj,3]), by="day") ,
				subPrefix='_sub',
				toLake=T,
				lakeArea=432610200
			)
			
		}#hh
	}#jj
	
	
	
	
	
	# period set to avoid 5 spin up years
	# flatsub = read.csv('flat_region_sub.csv')
	# combineSubbasin2Basin(
		# prefix='output_michieBias/FIAnlcdlocal_2010',
		# suffix='_param3',
		# subbasin=flatsub
	# )
	
	#---------------------------------------------------- owasa regional simulation
	
	# allperiod = matrix(c(
		# '2010','1980-10-1','2041-9-30',
		# '2020','1990-10-1','2051-9-30',
		# '2030','2000-10-1','2061-9-30',
		# '2040','2010-10-1','2071-9-30',
		# '2050','2020-10-1','2081-9-30',
		# '2060','2030-10-1','2090-9-30'
	# ),nrow=6,ncol=3,byrow=T)
	allperiod = matrix(c(
		'2060','1980-10-1','2041-9-30'
		#'2010','1980-10-1','2040-9-30'
	),nrow=1,ncol=3,byrow=T)
	for(jj in 1:nrow(allperiod)){
		
		MultipleBasinSeries2WSC(
			prefix=paste('output_owasa_FIAnlcdlocal_proj1_s3_nonForest_climateperiod2010/FIAnlcdlocal_',allperiod[jj,1],sep=''),
			sub='cane_regional_sub.csv',
			Jindex = paste('param', c(1),sep=''),
			outputPrefix=paste('regionalCane',allperiod[jj,1],sep=''),
			sitename='',
			period=seq.Date(from=as.Date(allperiod[jj,2]), to=as.Date(allperiod[jj,3]), by="day"),
			subPrefix='_sub',
			toLake=T,
			lakeArea=80128800
		)
		MultipleBasinSeries2WSC(
			prefix=paste('output_owasa_FIAnlcdlocal_proj1_s3_nonForest_climateperiod2010/FIAnlcdlocal_',allperiod[jj,1],sep=''),
			sub='morgan_regional_sub.csv',
			Jindex = paste('param', c(1),sep=''),
			outputPrefix=paste('regionalMorgan',allperiod[jj,1],sep=''),
			sitename='',
			period=seq.Date(from=as.Date(allperiod[jj,2]), to=as.Date(allperiod[jj,3]), by="day"),
			subPrefix='_sub',
			toLake=T,
			lakeArea=76376700
		)
		
		ww=read.csv(paste("output_owasa_FIAnlcdlocal_proj1_s3_nonForest_climateperiod2010/regionalCane",allperiod[jj,1],"_streamflowmm.csv",sep=''))
		ww.date = as.Date(paste(ww[,3], ww[,2], ww[,1],sep="-"),format="%d-%m-%Y")
		tmp = match2DailyTimeSeries(
			ww.date, 
			seq.Date(from=as.Date(allperiod[jj,2]), to=as.Date(allperiod[jj,3]), by="day") 
		)
		period.dailytimeSeriesMatch = tmp$xSelect
		WSC_combineUSGS2Lake(
			prefix = c(
				paste('output_owasa_FIAnlcdlocal_proj1_s3_nonForest_climateperiod2010/regionalCane',allperiod[jj,1],sep=''), 
				paste('output_owasa_FIAnlcdlocal_proj1_s3_nonForest_climateperiod2010/regionalMorgan',allperiod[jj,1],sep='')
			),
			dailytimeSeriesMatch = list( period.dailytimeSeriesMatch, period.dailytimeSeriesMatch ),
			replication = 1,
			arealweight = c(70318800, 72018900),
			outPrefix = paste('output_owasa_FIAnlcdlocal_proj1_s3_nonForest_climateperiod2010/lake_owasa',allperiod[jj,1],sep=''),
			scaler = 80128800+ 76376700
		)
		
	}#jj

##----------- landuse	
projhh = c(1)
allperiod = matrix(c(
	'2020','1990-10-1','2051-9-30',
	'2030','2000-10-1','2061-9-30',
	'2040','2010-10-1','2071-9-30',
	'2050','2020-10-1','2081-9-30',
	'2060','2030-10-1','2090-9-30'
),nrow=5,ncol=3,byrow=T)
for(jj in 1:nrow(allperiod)){
	for(hh in projhh){
		
		
		
		MultipleBasinSeries2WSC(
			prefix=paste("output_owasa_FIAnlcdlocal_proj",hh,"_csiroRCP6r7/FIAnlcdlocal_",allperiod[jj,1],sep=''),
			sub='cane_regional_sub.csv',
			Jindex = paste('param', c(1,2,3,4),sep=''),
			outputPrefix=paste('regionalCane',allperiod[jj,1],sep=''),
			sitename='',
			period=seq.Date(from=as.Date(allperiod[jj,2]), to=as.Date(allperiod[jj,3]), by="day"),
			subPrefix='_sub',
			toLake=T,
			lakeArea=80128800
		)
		
		MultipleBasinSeries2WSC(
			prefix=paste("output_owasa_FIAnlcdlocal_proj",hh,"_csiroRCP6r7/FIAnlcdlocal_",allperiod[jj,1],sep=''),
			sub='morgan_regional_sub.csv',
			Jindex = paste('param', c(1,2,3,4),sep=''),
			outputPrefix=paste('regionalMorgan',allperiod[jj,1],sep=''),
			sitename='',
			period=seq.Date(from=as.Date(allperiod[jj,2]), to=as.Date(allperiod[jj,3]), by="day"),
			subPrefix='_sub',
			toLake=T,
			lakeArea=76376700
		)
		
		ww=read.csv(paste("output_owasa_FIAnlcdlocal_proj",hh,"_csiroRCP6r7/regionalCane",allperiod[jj,1],"_streamflowmm.csv",sep=''))
		ww.date = as.Date(paste(ww[,3], ww[,2], ww[,1],sep="-"),format="%d-%m-%Y")
		tmp = match2DailyTimeSeries(
			ww.date, 
			seq.Date(from=as.Date(allperiod[jj,2]), to=as.Date(allperiod[jj,3]), by="day") 
		)
		period.dailytimeSeriesMatch = tmp$xSelect
		WSC_combineUSGS2Lake(
			prefix = c(
				paste('output_owasa_FIAnlcdlocal_proj',hh,'_csiroRCP6r7/regionalCane',allperiod[jj,1],sep=''), 
				paste('output_owasa_FIAnlcdlocal_proj',hh,'_csiroRCP6r7/regionalMorgan',allperiod[jj,1],sep='')
			),
			dailytimeSeriesMatch = list( period.dailytimeSeriesMatch, period.dailytimeSeriesMatch ),
			replication = 4,
			arealweight = c(70318800, 72018900),
			outPrefix = paste('output_owasa_FIAnlcdlocal_proj',hh,'_csiroRCP6r7/lake_owasa',allperiod[jj,1],sep=''),
			scaler = 80128800+ 76376700
		)
	}#hh
}#jj





	canesub = read.csv('cane_regional_sub.csv')
	combineSubbasin2Basin(
		prefix='output_owasaTesting/FIAnlcdlocal_2010',
		suffix='_param1',
		subbasin=canesub
	)

	canesub = read.csv('morgan_regional_sub.csv')
	combineSubbasin2Basin(
		prefix='output_owasaTesting/FIAnlcdlocal_2010',
		suffix='_param1',
		label='morgan',
		subbasin=canesub
	)
	
	
	#------------------------------------------------
	#"-st 1980 10 1 1 -ed 2041 9 30 1", #<<--- 2010
	#"-st 1990 10 1 1 -ed 2051 9 30 1", #<<--- 2020
	#"-st 2000 10 1 1 -ed 2061 9 30 1", #<<--- 2030
	#"-st 2010 10 1 1 -ed 2071 9 30 1", #<<--- 2040
	#"-st 2020 10 1 1 -ed 2081 9 30 1", #<<--- 2050
	#"-st 2030 10 1 1 -ed 2090 9 30 1", #<<--- 2060
	#WSC_cutPeriod0
	# problem here is that "WSC_cutPeriod" produced a different name scheme
	
	WSC_cutPeriod( 
		prefix='output_owasa_csiroRCP6r7/lake_owasa',
		period=seq.Date(from=as.Date('1980-10-1'), to=as.Date('2041-9-30') ,by="day"), 
		label='2010')
	
	WSC_cutPeriod( 
		prefix='output_owasa_csiroRCP6r7/lake_owasa',
		period=seq.Date(from=as.Date('1990-10-1'), to=as.Date('2051-9-30') ,by="day"), 
		label='2020')
	
	WSC_cutPeriod( 
		prefix='output_owasa_csiroRCP6r7/lake_owasa',
		period=seq.Date(from=as.Date('2000-10-1'), to=as.Date('2061-9-30') ,by="day"), 
		label='2030')
		
	WSC_cutPeriod( 
		prefix='output_owasa_csiroRCP6r7/lake_owasa',
		period=seq.Date(from=as.Date('2010-10-1'), to=as.Date('2071-9-30') ,by="day"), 
		label='2040')
		
	WSC_cutPeriod( 
		prefix='output_owasa_csiroRCP6r7/lake_owasa',
		period=seq.Date(from=as.Date('2020-10-1'), to=as.Date('2081-9-30') ,by="day"), 
		label='2050')
		
	WSC_cutPeriod( 
		prefix='output_owasa_csiroRCP6r7/lake_owasa',
		period=seq.Date(from=as.Date('2030-10-1'), to=as.Date('2090-9-30') ,by="day"), 
		label='2060')
	
	#"-st 1980 10 1 1 -ed 2041 9 30 1", #<<--- 2010
	#"-st 1990 10 1 1 -ed 2051 9 30 1", #<<--- 2020
	#"-st 2000 10 1 1 -ed 2061 9 30 1", #<<--- 2030
	#"-st 2010 10 1 1 -ed 2071 9 30 1", #<<--- 2040
	#"-st 2020 10 1 1 -ed 2081 9 30 1", #<<--- 2050
	#"-st 2030 10 1 1 -ed 2090 9 30 1", #<<--- 2060
	
	
	#---------------------------------------------------- morgan

	SingleBasinSeries2WSC('morgan_',c(5,6,8,10),'usgs_','morgan' )
	WSC_usgs2lake('usgs_morgan', 21401100,'lake_university' ) #wrong 76376700
	
	SingleBasinSeries2WSC('../output_cmip5/morgan_',c(5,6,8,10),'usgs_', 'morgan' )
	WSC_usgs2lake('../output_cmip5/usgs_morgan', 21401100,'lake_university' )
	
	ww=read.csv("usgs_morgan_pet.csv")
	WSC_combineUSGS2Lake(
		prefix = c('usgs_morgan', '~/Dropbox/Myself/UNC/WSC/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/needed_coop/usgs_cane'),
		dailytimeSeriesMatch = list( rep(T,nrow(ww)), rep(T,nrow(ww)) ),
		replication = 4,
		arealweight = c(21401100, 19687500),
		outPrefix = 'lake_owasa',
		scaler = 80128800+ 76376700
	)
	
	WSC_combineUSGS2Lake(
		prefix = c('../output_cmip5/usgs_morgan', '~/Dropbox/Myself/UNC/WSC/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/needed_cmip5/usgs_cane'),
		dailytimeSeriesMatch = list( rep(T,nrow(ww)), rep(T,nrow(ww)) ),
		replication = 4,
		arealweight = c(21401100, 19687500),
		outPrefix = '../output_cmip5/lake_owasa',
		scaler = 80128800+ 76376700
	)
	
	
	#---------------------------------------------------- cane
	SingleBasinSeries2WSC(
		'~/Dropbox/Myself/UNC/WSC/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/needed_coop/rhessys',
		c(16,20,41,42),
		'usgs_','cane' )
	WSC_usgs2lake(
		'~/Dropbox/Myself/UNC/WSC/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/needed_coop/usgs_cane',
		19687500,
		'lake_cane')
	
	
	SingleBasinSeries2WSC(
		'~/Dropbox/Myself/UNC/WSC/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/needed_cmip5/rhessys',
		c(16,20,41,42),
		'usgs_' ,'cane')
	WSC_usgs2lake(
		'~/Dropbox/Myself/UNC/WSC/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/needed_cmip5/usgs_cane',
		19687500,
		'lake_cane')
	
	
	#---------------------------------------------------- flat
	MultipleBasinSeries2WSC(
		prefix='/Users/laurencelin/Desktop/master_FIA/rhessys/output_green_cmip/case05_',
		sub='flat_sub.csv',
		Jindex =c(1,2,3,4,5),
		outputPrefix='usgs_flat',
		sitename='flat',
		period=NULL
	)
	WSC_usgs2lake('/Users/laurencelin/Desktop/master_FIA/rhessys/output_green_cmip/usgs_flatflat', 432610200,'lake_michie')
	
	
	MultipleBasinSeries2WSC(
		prefix='/Users/laurencelin/Desktop/master_FIA/rhessys/output_green_coop/case05_',
		sub='flat_sub.csv',
		Jindex =c(1,2,3,4,5),
		outputPrefix='usgs_flat',
		sitename='flat',
		period=NULL
	)
	WSC_usgs2lake('/Users/laurencelin/Desktop/master_FIA/rhessys/output_green_coop/usgs_flatflat', 432610200,'lake_michie')
	## still working
}# not exe


