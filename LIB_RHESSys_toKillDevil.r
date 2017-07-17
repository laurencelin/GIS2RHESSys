source("~/Dropbox/LIB_Rscript/LIB_misc.r")
arg=commandArgs(T)


generateBashRunScriptSingleBasin = function(serverCMD,RHESSysPreCMD, RHESSysCMD_worldfiles, RHESSysCMD_flow, RHESSysCMD_output, param_, parallel=F){
	#return Bash script ready-submit commands in text vector (not yet write out to file) !
	# serverCMD = paste('bsub -q day -G lband_pi -M 20 -J joblabel -o output',sep='')
	# RHESSysPreCMD = './rhessys5.18.r2.correction -st -ed -b -t tecfiles/tec_daily.txt'
	# RHESSysCMD_worldfiles = -w 'worldfiles/world'
	# RHESSysCMD_flow = -r 'flow/world.flow'
	# RHESSysCMD_output = -pre 'output/rhessys' (prefix)
	
	if(parallel){endCMD = '&'}else{endCMD=''}
	
	
	#### if param is a vector, not matrix !!! use param[1,,drop=F]
	
	w=rep(NA,nrow(param_))
	for(i in 1:nrow(param_)){
		w[i]=paste(
			serverCMD,
			RHESSysPreCMD, 
			paste('-w ', RHESSysCMD_worldfiles, sep=''),
			paste('-r ', RHESSysCMD_flow, sep=''),
			paste('-pre ', RHESSysCMD_output,'_param',i, sep=''),
			paste("-s", param_[j,'s1'], param_[j,'s2'], param_[j,'s3']),
			paste("-sv", param_[j,'sv1'], param_[j,'sv2']),
			paste("-gw", param_[j,'gw1'], param_[j,'gw2']),
			endCMD,
			sep=" "
		)
	}#i
	
	
	return<-list(
		bashHead = "#!/bin/bash",
		CMDs = w,
		toFile = w
	)
	
}#function

generateBashRunScriptMultipleSubBasins = function(serverCMD,RHESSysPreCMD, RHESSysCMD_worldfiles, RHESSysCMD_flow, RHESSysCMD_output, param_, subbasin, parallel=F, RHESSysCMD_worldfilesHDRAddress=NA, RHESSysCMD_worldfilesHDR=NA, optParam=NA, indexParam_=NULL){
	#return Bash script ready-submit commands in text vector (not yet write out to file) !
	# serverCMD = paste('bsub -q day -G lband_pi -M 20 -o output',sep='')
	# RHESSysPreCMD = './rhessys5.18.r2.correction -st -ed -t tecfiles/tec_daily.txt'
	# RHESSysCMD_worldfiles = -w 'worldfiles/world_subbasin_' (prefix)
	# RHESSysCMD_flow = -r 'flow/world_subbasin_' (prefix)
	# RHESSysCMD_output = -pre 'output/rhessys' (prefix)
	# RHESSysCMD_worldfilesHDR = '' (suffix)
	
	
	
	nrowParam = 1
	if(is.vector(param_)){ 
		nrowParam=1; 
		param = matrix(NA,nrow=1,ncol=length(param_))
		param[1,] = param_;
		colnames(param) = names(param_)
	}else{ nrowParam=nrow(param_); param = as.matrix(param_) }
	
	if(is.null(indexParam_)){ indexParam = 1:nrowParam; }else{indexParam = indexParam_; }
	
	if(parallel){endCMD = '&'}else{endCMD=''}
	if(is.na(RHESSysCMD_worldfilesHDR)){RHESSysCMD_worldfilesHDR=''}
	if(is.na(RHESSysCMD_worldfilesHDRAddress)){RHESSysCMD_worldfilesHDRAddress=RHESSysCMD_worldfiles}
	w=matrix(NA,nrow=nrow(subbasin), ncol=nrowParam)
	
	for(j in 1:nrowParam){
		for(i in 1:nrow(subbasin)){
			
			if(is.na(optParam)){
				optParamCMD = ''
			}else{
				optParamCMD = ''
				for(ff in 1:length(optParam)){
					optParamCMD = paste(optParamCMD, "-", optParam[ff],' ', param_[j,optParam[ff]],' ')
				}#ff
			}
			
			w[i,j]=paste(
				serverCMD,
				RHESSysPreCMD, 
				paste('-w ', RHESSysCMD_worldfiles,subbasin[i,'id'], sep=''),
				paste('-whdr ', RHESSysCMD_worldfilesHDRAddress,subbasin[i,'id'], RHESSysCMD_worldfilesHDR,'.hdr', sep=''),
				paste('-r ', RHESSysCMD_flow, subbasin[i,'id'],'.flow', sep=''),
				paste('-pre ', RHESSysCMD_output,'_sub', subbasin[i,'id'],'_param', indexParam[j], sep=''),
				paste("-s", param[j,'s1'], param[j,'s2'], param[j,'s3']),
				paste("-sv", param[j,'sv1'], param[j,'sv2']),
				paste("-gw", param[j,'gw1'], param[j,'gw2']),
				optParamCMD,
				endCMD,
				sep=" "
			)

		}#i
	}#j
	
	ww = w[,1]
	if(ncol(w)>1){
		for(j in 2:nrowParam){
			ww = c(ww,w[,j])
		}#j
	}#ncol
	
	return<-list(
		bashHead = "#!/bin/bash",
		CMDs = w,
		toFile = ww
	)
	
}#function



###----------------------------------------------
if(F){
	dd = read.csv("allSiteFitParam_info.csv")
	paramSet =  dd[,c('site','itr','s1','s2','s3','sv1','sv2','gw1','gw2')]
	siteID = unique(dd[,'site'])
	
	#runningSite = c(5,3,4,8) #small catchment
	runningSite = c(1) #large catchment need break down 6,2
	ww = read.csv('check.csv', stringsAsFactors=F)
	
	ww[runningSite,]
	##-----------------------------------------


	numRuns = 1
	numCore = 10000
	cluster = 1
	numRunsCheck=1
	prefix = 'killdevilcase05Big'
	maxLine = 10000 
	
	paramSet= paramSet[paramSetRID==1,]
	runScript = paste(prefix,"run_",cluster,".sh",sep='')
	
	write("#!/bin/bash",file= runScript,append=F)
	for(i in runningSite){ #1:length(siteID)
		
		if( i %in% runningSite){
			sitename = ww[ww[,'site']==i,'name']
			siteST = ww[ww[,'site']==i,'st']
			siteED = ww[ww[,'site']==i,'ed']
			siteRun = ifelse(ww[ww[,'site']==i,'day']==1,'day','week')
			siteMem = ww[ww[,'site']==i,'memory']
			siteTec = ww[ww[,'site']==i,'climate'] ##<<--- based on climate file
			siteBreak = ww[ww[,'site']==i,'partition']
			params = paramSet[paramSet[,'site']==siteID[i],]
			jMax = min(nrow(params),20)
			for(j in 1:jMax){
				
				if(numRuns>1 & numRuns%%maxLine ==1){
					write("wait",file= runScript,append=T) ## close previous
					
					cluster= cluster+1
					runScript = paste("run_",cluster,".sh",sep='') ## open a new
					write("#!/bin/bash",file= runScript,append=F) 
					numRuns=1
				}#if
				
				
				
				if(siteBreak==0){
					#single catchment
					worldname = paste(sitename,sep='' )
					flowname = paste(sitename,'.flow',sep='' )
					outputname = paste('output/case05', sitename,j,sep='_' ) 
					cmd=paste(
						paste("bsub -q ",siteRun,' -G lband_pi -M ',siteMem,' -o output',sep=''),
						"./rhessys5.18.r2.correction",
						paste("-st ", siteST," -ed ", siteED,sep=''), #<<--- 2060
						"-b",
						paste("-t tecfiles/tec_daily_", siteTec,".txt",sep=''),
						paste("-w worldfiles/", worldname,sep=""),
						paste("-r flow/", flowname,sep=""),
						paste("-pre ", outputname,sep=""), 
						paste("-s",params[j,'s1'],params[j,'s2'],params[j,'s3']),
						paste("-sv",params[j,'sv1'],params[j,'sv2']),
						paste("-gw",params[j,'gw1'],params[j,'gw2']),
						#"&",
						sep=" "
					)
					write(cmd,file= runScript,append=T)
					if(numRuns%% numCore==0){write("wait",file= runScript,append=T)}
					numRuns= numRuns+1
					numRunsCheck = numRunsCheck +1
				}else{
					# multiple sub-catchment
					# ls -l world_subbasin_??? | awk '{print $9}'
					subFile = read.csv(paste(sitename,'_sub.csv',sep=''))
					for(kk in 1:nrow(subFile)){
						worldname = paste(subFile[kk,'name'],sep='' )
						flowname = paste(subFile[kk,'name'],'.flow',sep='' )
						outputname = paste('output/case05', sitename, worldname,j,sep='_' ) 
						cmd=paste(
							paste("bsub -q ",siteRun,' -G lband_pi -M ',siteMem,' -o output',sep=''),
							"./rhessys5.18.r2.correction",
							paste("-st ", siteST," -ed ", siteED,sep=''), #<<--- 2060
							"-b",
							paste("-t tecfiles/tec_daily_", siteTec,".txt",sep=''),
							paste("-w worldfiles_", sitename,"/", worldname,sep=""),
							paste("-r flow_", sitename,"/", flowname,sep=""),
							paste("-pre ", outputname,sep=""), 
							paste("-s",params[j,'s1'],params[j,'s2'],params[j,'s3']),
							paste("-sv",params[j,'sv1'],params[j,'sv2']),
							paste("-gw",params[j,'gw1'],params[j,'gw2']),
							#"&",
							sep=" "
						)
						write(cmd,file= runScript,append=T)
						if(numRuns%% numCore==0){write("wait",file= runScript,append=T)}
						numRuns= numRuns+1
						numRunsCheck = numRunsCheck +1
					}#kk
				}
				
			}#j
		}#end of if
		
	}#i
	#write("wait",file= runScript,append=T)
	
}



















