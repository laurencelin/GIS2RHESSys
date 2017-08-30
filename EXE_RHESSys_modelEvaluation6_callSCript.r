source("./LIB_misc.r")
source("./LIB_dailytimeseries2.r")
source("./LIB_hydro.r")
source("./LIB_RHESSys_modelFit_6_working.r")
source("./LIB_RHESSys_modelBehavior_6_working.r")
source("./LIB_RHESSys_modelPlot_6_working.r")

arg=commandArgs(T)

##--------------------------------------------------------------------------------------------## WS18
# arg=c(
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18",
	# "Qobs_18_r.csv",
	# "1", "1", "500", "1991-10-1","1996-9-30","rhessys","output_OriginalCalibration","OriginalCalibration"
# );paste(arg,collapse=" ")




proj = arg[1]
obsfile = arg[2]
sessionID= as.numeric(arg[3])
Itr = as.numeric(arg[4]):as.numeric(arg[5])

startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; 
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<----- for years starting from 19XX
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=20) ##<<----- for years starting from 20XX


i=1
rhessys_SingleFile = read.table(paste(proj,"/", arg[8],"/", arg[9],"/SESSION_", sessionID,"_","world_ITR_",Itr[i],"/rhessys_basin.daily" ,sep=''),header=F,skip=1,sep=' ')
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")
tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
matchYears=range(calobs.dailytimeSeries$grp_wateryearYYYY)

holdingFit = matrix(NA,length(Itr),22)
holdingBeh = matrix(NA,length(Itr),19)
holdingPar = matrix(NA,length(Itr),7)
for(i in 1:length(Itr)){
	tryCatch({
		rhessys_SingleFile = read.table(paste(proj,"/", arg[8],"/", arg[9],"/SESSION_", sessionID,"_","world_ITR_",Itr[i],"/rhessys_basin.daily" ,sep=''),header=F,skip=1,sep=' ')
		
		##......... function calling
		w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)	
		holdingFit[i,]=c(Itr[i],w$FittnessList)
		if(i==1){colnames(holdingFit) = c('itr',names(w$FittnessList)) }
		
		ww = modelBehavior(rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)
		holdingBeh[i,] = ww$AnnualList
		if(i==1){colnames(holdingBeh) = names(ww$AnnualList) }
		
		outputName = paste(proj,"/",arg[8],"/",arg[9],"/SESSION_",sessionID,"_world_ITR_",Itr[i],"/rhessys_itr",Itr[i],"_plot_", matchYears[1],"_",matchYears[2],"_style2.pdf",sep="")
		modelPlotStyle2( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries, outputName)
		
		
		# #............ reading parameters from log file
		logfile = list.files(path=paste(proj,"/",arg[8],"/",arg[9],"/SESSION_",sessionID,"_world_ITR_",Itr[i],sep="") )[1]
		con=file(paste(proj,"/",arg[8],"/",arg[9],"/SESSION_",sessionID,"_world_ITR_",Itr[i],"/", logfile,sep=""))
		w=unlist(strsplit(readLines(con)," +"))
		starting = which(w=='-s')[1]
		holdingPar[i,]=c(as.numeric(w[(starting+1):(starting+3)]), as.numeric(w[(starting+5):(starting+6)]), as.numeric(w[(starting+8)]), as.numeric(gsub("[^0-9.]","",w[(starting+9)])) )
		close(con)
		if(i==1){colnames(holdingPar)=c('s1','s2','s3','sv1','sv2','gw1','gw2')}
	}, error = function(e){
		#nothing
	})#try blocks
}#i

result = cbind(holdingFit, holdingPar, holdingBeh)
output = paste(proj,"/",arg[10],"_session", sessionID,"_itr",as.numeric(arg[4]),"_",as.numeric(arg[5]),"_fittingEvaluation_", matchYears[1], "_",matchYears[2],".csv",sep="")
write.csv(result, output, row.names=F)


	
	
	
	
