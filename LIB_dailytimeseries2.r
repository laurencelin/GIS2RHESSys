
source("~/Dropbox/LIB_Rscript/LIB_misc.r")


fillingTimeGap = function(x){
	#assume x is Date
	return <- seq.Date(x[1], to=x[3],by="day")
	
}#function


match2DailyTimeSeries_backup=function(x,y){
	#assume both x and y are date object
	
	x.select = rep(NA,length(x))
	for(i in 1:length(x)){
		x.select[i] = x[i]%in%y
	}#i
	
	y.select = rep(NA,length(y))
	for(i in 1:length(y)){
		y.select[i] = y[i]%in%x
	}#i
	
	return <- list( xSelect=x.select, ySelect=y.select)
}#function

match2DailyTimeSeries=function(x,y){
	#assume both x and y are date object
	return <- list( xSelect=(!is.na(match(x,y))), ySelect=(!is.na(match(y,x))))
}#function




match3DailyTimeSeries_backup=function(x,y,z){
	#assume both x and y are date object
	
	x.select = rep(NA,length(x))
	for(i in 1:length(x)){
		x.select[i] = x[i]%in%y & x[i]%in%z
	}#i
	
	y.select = rep(NA,length(y))
	for(i in 1:length(y)){
		y.select[i] = y[i]%in%x & y[i]%in%z
	}#i
	
	z.select = rep(NA,length(z))
	for(i in 1:length(z)){
		z.select[i] = z[i]%in%x & z[i]%in%y
	}#i
	
	return <- list( xSelect=x.select, ySelect=y.select, zSelect=z.select)
}#function

match3DailyTimeSeries=function(x,y,z){
	#assume both x and y are date object
	
	return <- list( 
				xSelect=(!is.na(match(x,y)) & !is.na(match(x,z)) ), 
				ySelect=(!is.na(match(y,x)) & !is.na(match(y,z)) ),
				zSelect=(!is.na(match(z,x)) & !is.na(match(z,y)) )
			)
}#function






match4DailyTimeSeries_backup=function(x,y,z,w){
	#assume both x and y are date object
	
	x.select = rep(NA,length(x))
	for(i in 1:length(x)){
		x.select[i] = x[i]%in%y & x[i]%in%z & x[i]%in%w
	}#i
	
	y.select = rep(NA,length(y))
	for(i in 1:length(y)){
		y.select[i] = y[i]%in%x & y[i]%in%z  & y[i]%in%w
	}#i
	
	z.select = rep(NA,length(z))
	for(i in 1:length(z)){
		z.select[i] = z[i]%in%x & z[i]%in%y  & z[i]%in%w
	}#i
	
	w.select = rep(NA,length(w))
	for(i in 1:length(w)){
		w.select[i] = w[i]%in%x & w[i]%in%y  & w[i]%in%z
	}#i
	
	return <- list( xSelect=x.select, ySelect=y.select, zSelect=z.select, wSelect=w.select)
}#function

match4DailyTimeSeries=function(x,y,z,w){
	#assume both x and y are date object
	
		return <- list( 
				xSelect=(!is.na(match(x,y)) & !is.na(match(x,z)) & !is.na(match(x,w)) ), 
				ySelect=(!is.na(match(y,x)) & !is.na(match(y,z)) & !is.na(match(y,w)) ),
				zSelect=(!is.na(match(z,x)) & !is.na(match(z,y)) & !is.na(match(z,w)) ),
				wSelect=(!is.na(match(w,x)) & !is.na(match(w,y)) & !is.na(match(w,z)) )
			)
}#function





LIBas.Date=function(x,y,z){
	return <- as.Date(paste(x,y,z,sep="-"),format="%Y-%m-%d")
}

setDay=function(x,dd){
	return <- as.Date(paste(format(x,"%Y"),format(x,"%m"),dd,sep="-"))
}

setMonth=function(x,mm){
	return <- as.Date(paste(format(x,"%Y"),mm,format(x,"%d"),sep="-"))
}

setYear=function(x,yyyy){
	return <- as.Date(paste(yyyy,format(x,"%mm"),format(x,"%d"),sep="-"))
}



wateryearStartDate=function(x){
	#assume x is Date
	# i don't know how to do?
	hold = LIBas.Date(as.numeric(format(x,"%Y")),10,1)	
	if(hold>x){
		return <- LIBas.Date(as.numeric(format(x,"%Y"))-1,10,1)
	}else{
		return <- hold
	}
}

wateryearEndDate=function(x){
	#assume x is Date
	# i don't know how to do?
	hold = LIBas.Date(as.numeric(format(x,"%Y")),9,30)	
	if(hold<x){
		return <- LIBas.Date(as.numeric(format(x,"%Y"))+1,9,30)
	}else{
		return <- hold
	}
}

getYear=function(x){
	return <- as.numeric(format(x,"%Y"))
}

##-------------------------------------------------------------------
seasonalPatterns=function(x,grp_month,season){
	result = matrix(NA,length(season),9)
	for(i in 1:length(season)){
		tmp = x[grp_month==season[i]]
		if( length(tmp)>0 ){
			result[i,]=c(
				mean(tmp, na.rm=T),
				quantile(tmp,probs=c(0.025),na.rm=T),
				quantile(tmp,probs=c(0.975),na.rm=T),
				max(tmp, na.rm=T),
				min(tmp, na.rm=T),
				quantile(tmp,probs=c(0.5),na.rm=T),
				sd(tmp,na.rm=T),
				sd(tmp,na.rm=T)/sum(!is.na(tmp)),
				length(tmp)
			)
		}else{
			result[i,]=rep(0,9)
		}
		
	}#i
	colnames(result)=c("mean","q025","q975","max","min","med","sd","stderr","count")
	
	return <- result
}

seasonalAccumPatterns=function(x,grp_month,grp_yyyy,season){
	yy = unique(grp_yyyy)
	hold = matrix(NA,length(yy),length(season) )
	tmp = rep(NA,length(season))
	for(i in 1:nrow(hold)){
		cond = grp_yyyy==yy[i] 
		for(j in 1:length(season)){
			tmp[j] = x[cond][grp_month[cond]==season[j]]
		}#j
		hold[i,] = accumulate(tmp)
	}#i
	return <- rowPattern(hold)
}


seasonalAccum=function(x,grp_month,grp_yyyy,season){
	yy = unique(grp_yyyy)
	hold = matrix(NA,length(yy),length(season) )
	tmp = rep(NA,length(season))
	for(i in 1:nrow(hold)){
		cond = grp_yyyy==yy[i] 
		for(j in 1:length(season)){
			tmp[j] = x[cond][grp_month[cond]==season[j]]
		}#j
		hold[i,] = accumulate(tmp)
	}#i
	return <- hold
}



dailyQQBiasCorrection = function(obs_,calibrate,obsDate, correcting, correctingDate,window=15){
	
	#obs.dailytimeSeries = dailyTimeSeries(obsDate)
	
	periodCalibration.doy = as.POSIXlt(obsDate)$yday+1  ##<<-------- julian day or DoY
	period.doy = as.POSIXlt(correctingDate)$yday+1 #<<------- full series!!
	newGCM = correcting #<<------- full series!!
	for(i in 1:366){
		cond = abs(periodCalibration.doy-i) <= window | abs(periodCalibration.doy-i-366) <= window
		dayGroup = unique(periodCalibration.doy[cond]) #<<<------------------------- selecting days on multiple year; use unique to narrow exact day
		specificDay = unique(period.doy[period.doy==i]) #<<------- full series!!
		
		obs = obs_[periodCalibration.doy%in% dayGroup]#observed
		gcm = calibrate[periodCalibration.doy%in% dayGroup]#model
		gcmSP = correcting[period.doy%in% specificDay] #<<------- full series!!
		# qqplot(gcm,obs)
		
		gcmECDF = ecdf(gcm)
		p = gcmECDF(gcmSP) #<<------- full series!!
		correctTerm = rep(NA,length(p)) #<<------- full series!!
		for(j in 1:length(p)){
			correctTerm[j] = quantile(obs,p[j],na.rm=T)- gcmSP[j] #<<----
		}#j
		newGCM[period.doy==i] = newGCM[period.doy==i] + correctTerm #<<------- full series!!
		 
	}#i
	return <- newGCM
}# function

monthlyQQBiasCorrection = function(obs_,calibrate,obsDate, correcting, correctingDate){
	
}#

dailyMovingMean = function(ts, ts_date){
	
	ts_new = rep(NA,length(ts))
	jday = as.POSIXlt(ts_date)$yday+1
	yyyy = as.POSIXlt(ts_date)$year+1900
	for(i in 1:length(ts_new)){
		jtoday = jday[i]
		ytoday = yyyy[i]
		cond = (abs(jday-jtoday) <=15 | abs(jday-jtoday-366) <=15) & yyyy==ytoday
		if(sum(cond)==0){
			print(paste('problem at ', ts_date[i]))
			ts_new[i] = ts[i]
		}else if(sum(cond)==1){
			print(paste('one at ', ts_date[i]))
			ts_new[i] = ts[i]
		}else{
			ts_new[i] = mean(ts[cond])
		}
	}#i
	return <- ts_new
	
}#function


seasonalMovingMean = function(ts, ts_date){

	jday = as.POSIXlt(ts_date)$yday+1
	yyyy = as.POSIXlt(ts_date)$year+1900
	centralMonthJday = c(15,46,74,105,135,166,196,227,258,288,319,349) # assume no leap year from J to D
	seasonal = matrix(NA,12,4)
	for(i in 1:12){
		jtoday = centralMonthJday[i]
		cond = (abs(jday-jtoday) <=21 | abs(jday-jtoday-366) <=21)
		if(sum(cond)==0){
			print(paste('problem at ', ts_date[i]))
			seasonal[i,] = c(i,NA,NA,NA,NA)
		}else if(sum(cond)<3){
			print(paste('less than 3 at ', ts_date[i]))
			seasonal[i,] = c(i,mean(ts[cond],na.rm=T),mean(ts[cond],na.rm=T),mean(ts[cond],na.rm=T))
		}else{
			seasonal[i,] = c(	
				i,
				mean(ts[cond],na.rm=T),
				quantile(ts[cond], probs=c(0.025),na.rm=T),
				quantile(ts[cond], probs=c(0.975),na.rm=T)
			 )
		}
	}#i
	colnames(seasonal)=c('month','mean','q025','q975')
	return <- seasonal
	
}#function





dailyTimeSeries = function(x){
	
	key_month_len = matrix(c(31,29,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31),nrow=4,ncol=12, byrow=T) #non leap year
	
	# assume x = Date
	firstDay = wateryearStartDate(x[1])
	lastDay = wateryearEndDate(x[length(x)])
	PerfectX = seq.Date(from=firstDay, to=lastDay ,by="day") 
	Perfectdd = as.numeric(format(PerfectX,"%d"))
	Perfectmm = as.numeric(format(PerfectX,"%m"))
	Perfectyyyy = as.numeric(format(PerfectX,"%Y"))
	
	tmp=match2DailyTimeSeries(x, PerfectX)
	xMatch=tmp$xSelect
	PerfectXMatch=tmp$ySelect
	
	Perfectgrp_wateryearYYYY = getYear(firstDay):(getYear(lastDay)-1)
	Perfectgrp_wateryearLen = rep(365,length(Perfectgrp_wateryearYYYY)) + LIBbinaryRev((Perfectgrp_wateryearYYYY+1)%%4)
	Perfectgrp_wateryear = LIBrep(1:length(Perfectgrp_wateryearLen), each= Perfectgrp_wateryearLen)
	yearMatch = grpSums(PerfectXMatch,Perfectgrp_wateryear)
	
	

	Perfectgrp_dayYYYY = Perfectyyyy
	Perfectgrp_dayWY = Perfectgrp_wateryearYYYY[Perfectgrp_wateryear]
	
	#ithdayisendWY = accumulate(table(Perfectgrp_dayWY)) #ith day is the end of WY <<--------- need imperfect 
	ithdayisendWY = accumulate(yearMatch[yearMatch>0]) 
	ithdayisendWYlbl = Perfectgrp_dayWY[ithdayisendWY]
	ithdayisbeginWY = c(1,ithdayisendWY[1:(length(ithdayisendWY)-1)]+1)
	ithdayisbeginWYlbl= Perfectgrp_dayWY[ithdayisbeginWY]
	
	ithdayisendYYYY = accumulate(table(Perfectgrp_dayYYYY)) #ith day is the end of WY
	ithdayisendYYYYlbl = Perfectgrp_dayYYYY[ithdayisendYYYY]
	ithdayisbeginYYYY = c(1, ithdayisendYYYY[1:(length(ithdayisendYYYY)-1)]+1)
	ithdayisbeginYYYYlbl = Perfectgrp_dayYYYY[ithdayisbeginYYYY]

	
	Perfectweekday = as.POSIXlt(PerfectX)$wday 
	sunStart = which(Perfectweekday ==0)
	numWeek = length(sunStart); 
	if(sunStart[1]>1){numWeek= numWeek+1}
	Perfectgrp_week = rep(numWeek,length(PerfectX))
	for(i in length(sunStart):1){
		numWeek = numWeek-1
		if(sunStart[i]>1){Perfectgrp_week[1:(sunStart[i]-1)] = numWeek}
	}#i
	#Perfectgrp_weekLen = what=grpSums(rep(1,length(PerfectX)), Perfectgrp_week)
	Perfectgrp_weekMM = c(Perfectmm[sunStart[1]-1],Perfectmm[sunStart])
	Perfectgrp_weekYYYY = c(Perfectyyyy[sunStart[1]-1],Perfectyyyy[sunStart])
	Perfectgrp_weekWY= Perfectgrp_wateryearYYYY[c(Perfectgrp_wateryear[sunStart[1]-1], Perfectgrp_wateryear[sunStart])]
	weekMatch=grpSums(PerfectXMatch, Perfectgrp_week)
	Perfectgrp_weekTH= LIBfirstEachGrpX(Perfectgrp_week,grpSums(rep(1,length(PerfectX)), Perfectgrp_week) )$lbl
	
	ithweekisendWY = accumulate(table(Perfectgrp_weekWY)) #ith week is the end of WY
	ithweekisendWYlbl = Perfectgrp_weekWY[ithweekisendWY]
	ithweekisbeginWY = c(1,ithweekisendWY[1:(length(ithweekisendWY)-1)]+1)
	ithweekisbeginWYlbl= Perfectgrp_weekWY[ithweekisbeginWY]
	
	ithweekisendYYYY = accumulate(table(Perfectgrp_weekYYYY)) #ith week is the end of WY
	ithweekisendYYYYlbl = Perfectgrp_weekYYYY[ithweekisendYYYY]
	ithweekisbeginYYYY = c(1, ithweekisendYYYY[1:(length(ithweekisendYYYY)-1)]+1)
	ithweekisbeginYYYYlbl = Perfectgrp_weekYYYY[ithweekisbeginYYYY]
	
	firstStart = which(Perfectdd == 1)
	Perfectgrp_month = rep(NA,length(PerfectX))
	count=0
	for(i in 2:length(firstStart)){
		count=count+1
		Perfectgrp_month[firstStart[i-1]:(firstStart[i]-1)] = count
	}#i
	count=count+1
	Perfectgrp_month[firstStart[i]:length(Perfectgrp_month)]=count
	
	Perfectgrp_monthMM = Perfectmm[firstStart]
	Perfectgrp_monthYYYY = Perfectyyyy[firstStart]
	Perfectgrp_monthWY = Perfectgrp_wateryearYYYY[Perfectgrp_wateryear[firstStart]]
	monthMatch=grpSums(PerfectXMatch, Perfectgrp_month)
	Perfectgrp_monthTH= LIBfirstEachGrpX(Perfectgrp_month,grpSums(rep(1,length(PerfectX)), Perfectgrp_month) )$lbl

	ithmonthisendWY = accumulate(table(Perfectgrp_monthWY)) #ith month is the end of WY
	ithmonthisendWYlbl = Perfectgrp_monthWY[ithmonthisendWY]
	ithmonthisbeginWY = c(1, ithmonthisendWY[1:(length(ithmonthisendWY)-1)]+1)
	ithmonthisbeginWYlbl = Perfectgrp_monthWY[ithmonthisbeginWY]
	
	ithmonthisendYYYY = accumulate(table(Perfectgrp_monthYYYY)) #ith month is the end of WY
	ithmonthisendYYYYlbl = Perfectgrp_monthYYYY[ithmonthisendYYYY]
	ithmonthisbeginYYYY = c(1, ithmonthisendYYYY[1:(length(ithmonthisendYYYY)-1)]+1)
	ithmonthisbeginYYYYlbl = Perfectgrp_monthYYYY[ithmonthisbeginYYYY]
	
	## returned result is discontinuous because the original series is discontinuous
	## 
	return <- list(
		grp_wateryear= Perfectgrp_wateryear[PerfectXMatch], #data length
		grp_wateryearDefaultLen = Perfectgrp_wateryearLen[yearMatch>0],
		grp_wateryearLen = yearMatch[yearMatch>0],
		grp_wateryearYYYY = Perfectgrp_wateryearYYYY[yearMatch>0],
		ithdayisendWY = ithdayisendWY,
		ithdayisendWYlbl= ithdayisendWYlbl,
		ithdayisbeginWY = ithdayisbeginWY,
		ithdayisbeginWYlbl = ithdayisbeginWYlbl,
		ithdayisendYYYY = ithdayisendYYYY,
		ithdayisendYYYYlbl= ithdayisendYYYYlbl,
		ithdayisbeginYYYY = ithdayisbeginYYYY,
		ithdayisbeginYYYYlbl = ithdayisbeginYYYYlbl,
		
		weekday = Perfectweekday[PerfectXMatch],
		grp_week = Perfectgrp_week[PerfectXMatch],
		grp_weekLen = weekMatch[weekMatch>0],
		grp_weekMM = Perfectgrp_weekMM[weekMatch>0],
		grp_weekYYYY = Perfectgrp_weekYYYY[weekMatch>0],
		grp_weekWY = Perfectgrp_weekWY[weekMatch>0],
		grp_weekTH = Perfectgrp_weekTH[weekMatch>0], #showing these discontinuous weeks in the continuous time line
		ithweekisendWY = ithweekisendWY,
		ithweekisendWYlbl= ithweekisendWYlbl,
		ithweekisbeginWY = ithweekisbeginWY,
		ithweekisbeginWYlbl = ithweekisbeginWYlbl,
		ithweekisendYYYY = ithweekisendYYYY,
		ithweekisendYYYYlbl= ithweekisendYYYYlbl,
		ithweekisbeginYYYY = ithweekisbeginYYYY,
		ithweekisbeginYYYYlbl = ithweekisbeginYYYYlbl,
		
		grp_month = Perfectgrp_month[PerfectXMatch],
		grp_monthLen = monthMatch[monthMatch>0],
		grp_monthDefaultLen = grpSums(rep(1,length(PerfectX)), Perfectgrp_month)[monthMatch>0],
		grp_monthMM = Perfectgrp_monthMM[monthMatch>0], 
		grp_monthYYYY = Perfectgrp_monthYYYY[monthMatch>0],
		grp_monthWY = Perfectgrp_monthWY[monthMatch>0],
		grp_monthTH = Perfectgrp_monthTH[monthMatch>0],
		ithmonthisendWY = ithmonthisendWY,
		ithmonthisendWYlbl= ithmonthisendWYlbl,
		ithmonthisbeginWY = ithmonthisbeginWY,
		ithmonthisbeginWYlbl= ithmonthisbeginWYlbl,
		ithmonthisendYYYY = ithmonthisendYYYY,
		ithmonthisendYYYYlbl= ithmonthisendYYYYlbl,
		ithmonthisbeginYYYY = ithmonthisbeginYYYY,
		ithmonthisbeginYYYYlbl= ithmonthisbeginYYYYlbl
		)
		
		
		
}






