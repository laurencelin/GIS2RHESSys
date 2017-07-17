source("~/Dropbox/LIB_Rscript/LIB_misc.r")

#### -------------- baseflow separation from daily hydrograph

## isep : Lim et al. 2005 (WHAT)
## hysep: Sloto and Crouse 1996 (USGS)
## 5 day minima
## qt = q0 exp(-kt)

fivedayblockbaseflow = function(x){
	## 5 day block
	num5dblock=as.integer(length(x)/5)
	blockID = c(rep(1: num5dblock,each=5),rep(num5dblock+1,(length(x)-5* num5dblock) ))
	blockIDx_ = c(rep(1:5, num5dblock),1:(length(x)-5* num5dblock) )
	blockIDx = which(blockIDx_==3) ##<<< ----- missing one at the end
	if(length(blockIDx)<max(blockID)){ blockIDx = append(blockIDx, length(x)) }
	
	minima = grpMins(x, blockID)
	baseline = rep(NA,length(minima))
	for(i in 2:(length(baseline)-1)){
		if(0.9*minima[i]<minima[i-1] | 0.9*minima[i]<minima[i+1] ){baseline[i]=minima[i]}
	}#i
	i=length(baseline);baseline[i]=minima[i]; #if(0.9*minima[i]<minima[i-1]){baseline[i]=minima[i]}
	i=1; baseline[i]=minima[i]; #if(0.9*minima[i]<minima[i+1]){baseline[i]=minima[i]}
	
	#plot(x,type='l')
	#points(blockIDx, baseline,col="red")
	
	cond = !is.na(baseline)
	baseflowPt = baseline[cond]
	baseflowPtx = blockIDx[cond]
	tmp =approx(x= baseflowPtx,y=baseflowPt,xout=1:length(x),yleft=baseflowPt[1],yright=baseflowPt[length(baseflowPt)] )
	baseflow=tmp$y
	for(i in 1:length(baseflow)){
		if(!is.na(x[i])){if(baseflow[i]>x[i]){baseflow[i]=x[i]}}
	}

	return <- baseflow
	#lines(baseflow,lty=2,col="green")
}


allBias=function(obs,monthID,y){ 
	obs.baseflow = fivedayblockbaseflow(obs); obs.stormflow = obs-obs.baseflow
	y.baseflow = fivedayblockbaseflow(y); y.stormflow = y - y.baseflow
	
	cond = monthID==12 | monthID==1 | monthID==2
	obs.winterflow = obs[cond]
	y.winterflow = y[cond]
	
	cond = monthID==6 | monthID==7 | monthID==8
	obs.summerflow = obs[cond]
	y.summerflow = y[cond]
	
	ABias = (sum(y)-sum(obs))/sum(obs)
	WBias = (sum(y.winterflow)-sum(obs.winterflow))/sum(obs.winterflow)
	SBias = (sum(y.summerflow)-sum(obs.summerflow))/sum(obs.summerflow)
	BBias = (sum(y.baseflow)-sum(obs.baseflow))/sum(obs.baseflow)
	Wstat = (WBias+1)/(ABias+1)
	Sstat = (SBias+1)/(ABias+1)
	Bstat = (BBias+1)/(ABias+1)
	
	nonZero = y.baseflow>0 & obs.baseflow>0
	obs.baseflowNZ = obs.baseflow[nonZero]
	y.baseflowNZ = y.baseflow[nonZero]
	len = length(y.baseflowNZ)
	BaveRI =  mean( y.baseflowNZ[2:len] / y.baseflowNZ[1:(len-1)] ) / mean( obs.baseflowNZ[2:len] / obs.baseflowNZ[1:(len-1)] )
	
	nonZero = y.stormflow>0 & obs.stormflow>0
	obs.stormflowNZ = obs.stormflow[nonZero]
	y.stormflowNZ = y.stormflow[nonZero]
	len = length(y.stormflowNZ)
	QaveRI =  mean( y.stormflowNZ[2:len] / y.stormflowNZ[1:(len-1)] ) / mean( obs.stormflowNZ[2:len] / obs.stormflowNZ[1:(len-1)] )
	
	
	condx = rep(NA,length(obs)); condx[1]=F; condx[length(obs)]=F
	condy = rep(NA,length(y)); condy[1]=F; condy[length(obs)]=F
	for(i in 2:(length(obs)-1)){
		 condx[i] = obs[i-1]<obs[i] & obs[i]> obs[i+1]
		 condy[i] = y[i-1]<y[i] & y[i]>y[i+1]
	}#i
	cond = condx & condy # hope it's over 50
	PBias=(sum(y[cond])-sum(obs[cond]))/sum(obs[cond])
	
	VPBias = (sum(y.stormflow)-sum(obs.stormflow))/sum(obs.stormflow)
	
	return <- list(
		bias= ABias,
		wbias= WBias,
		sbias= SBias,
		bbias= BBias,
		pbias= PBias,
		vpbias= VPBias,
		baveri= BaveRI,  ## <1 = a) predicted not raising as fast & b) falling too quickly;  >1 = a) predicted raising too quickly & falling too slowly
		qaveri= QaveRI,
		wstat = Wstat,  ## 1 meaning?
		sstat = Sstat,
		bstat = Bstat,
		obsbaseflow=obs.baseflow,
		obsstormflow=obs.stormflow,
		ybaseflow=y.baseflow,
		ystormflow=y.stormflow
		)
	
}





### ---- VPbias for storm flow





