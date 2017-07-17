source("~/Dropbox/LIB_Rscript/LIB_misc.r")
source("~/Dropbox/LIB_Rscript/LIB_dailytimeseries2.r")
source("~/Dropbox/LIB_Rscript/LIB_hydro.r")

arg=commandArgs(T)

modelFittness = function( calobs_, rhessys_, dailytimeSeries_, DailyThreshold_=0){
	print(DailyThreshold_)
	
	calobsDayFlow = calobs_
	calobsWeekFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_week )
	calobsMonthFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_month )
	calobsYearFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_wateryear )


	dailyDataQuality = (calobsDayFlow> DailyThreshold_) 
	dailyDataQuality.weight = dailyDataQuality/sum(dailyDataQuality)
	dailyobsMean = sum(dailyDataQuality.weight *calobsDayFlow)
	dailyobsSS = sum( dailyDataQuality.weight*(calobsDayFlow - dailyobsMean)^2 )
	dailyobsLogMean = sum( dailyDataQuality.weight *log(calobsDayFlow) ) 
	dailyobsLogSS = sum( dailyDataQuality.weight*(log(calobsDayFlow) - dailyobsLogMean)^2 )
	
	weeklyDataQuality = dailytimeSeries_ $grp_weekLen/7 * (calobsWeekFlow> DailyThreshold_*7) 
	weeklyDataQuality.weight = weeklyDataQuality/sum(weeklyDataQuality)
	weeklyobsMean = sum(weeklyDataQuality.weight *calobsWeekFlow)
	weeklyobsSS = sum( weeklyDataQuality.weight*(calobsWeekFlow - weeklyobsMean)^2 )
	weeklyobsLogMean = sum( weeklyDataQuality.weight *log(calobsWeekFlow) ) 
	weeklyobsLogSS = sum( weeklyDataQuality.weight*(log(calobsWeekFlow) - weeklyobsLogMean)^2 )

	monthlyDataQuality = dailytimeSeries_ $grp_monthLen/dailytimeSeries_ $grp_monthDefaultLen * (calobsMonthFlow> dailytimeSeries_ $grp_monthDefaultLen* DailyThreshold_)
	monthlyDataQuality.weight = monthlyDataQuality/sum(monthlyDataQuality)
	monthlyobsMean=sum(monthlyDataQuality.weight*calobsMonthFlow)
	monthlyobsSS = sum( monthlyDataQuality.weight*(calobsMonthFlow - monthlyobsMean)^2 )
	
	yearlyDataQuality = dailytimeSeries_ $grp_wateryearLen/dailytimeSeries_ $grp_wateryearDefaultLen * (calobsYearFlow> dailytimeSeries_ $grp_wateryearDefaultLen* DailyThreshold_)
	yearlyDataQuality.weight = yearlyDataQuality/sum(yearlyDataQuality)		
	yearlyobsMean=sum(yearlyDataQuality.weight* calobsYearFlow)
	yearlyobsSS = sum( yearlyDataQuality.weight*(calobsYearFlow - yearlyobsMean)^2 )
	
	minWeekCond = calobsWeekFlow> DailyThreshold_*7
	flowpt = exp(seq(round(log(min(calobsWeekFlow[minWeekCond]))),round(log(max(calobsWeekFlow[minWeekCond]))), 0.1)) 
	weeklyCDF=ecdf(calobsWeekFlow)
	weeklyCDFresult = weeklyCDF(flowpt)
	
	dailyflowpt = exp(seq(round(log(min(calobsDayFlow))),round(log(max(calobsDayFlow))), 0.1)) 
	dailyCDF=ecdf(calobsDayFlow)
	dailyCDFresult = dailyCDF(dailyflowpt)



	#--------------------------------------------
	fittnessList = rep(NA,26)
	names(fittnessList)=c('one','two','three',
		"dailyNSE","dailyLogNSE","meanAnnualFlushObs","meanAnnualFlushRHESSys",
		"weeklyNSE","weeklyLogNSE","inversedweeklyNSE","weeklyCDFfitr2",
		"monthlyNSE","monthlySAE",
		"yearlyNSE","yearlySAE",
		"bias","wbias","sbias",
		"totPrecip","totET","totFlow","totFlowObs","RHESSysRunoffRatio","obsRunoffRatio",'ETbias','flashCOMP')
	
	rhessysDayFlow = rhessys_[,19]#flow
	rhessysDayRain = rhessys_[,35]#rain
	rhessysDayET = rhessys_[,14]+rhessys_[,16]#et

	# ... / ... NSE
			rhessysSS = sum( dailyDataQuality.weight*(calobsDayFlow - rhessysDayFlow)^2 )
			rhessysNSE = 1 - rhessysSS/dailyobsSS
			rhessysDayResidue = calobsDayFlow - rhessysDayFlow
			
			# ... / ... log NSE
			rhessysLogSS = sum( dailyDataQuality.weight*(log(calobsDayFlow) - log(rhessysDayFlow))^2 )
			rhessysLogNSE = 1 - rhessysLogSS/dailyobsLogSS
		
			# ... / ... SAE --> change to flushness index (annually) << **** >>
			#rhessysSAE = sum( abs(calobsDayFlow - rhessysDayFlow)) #<<--------------- original
			flushness = matrix(NA, nrow=length(unique(calobs.dailytimeSeries$grp_wateryear)), ncol=2)
			for(fik in unique(calobs.dailytimeSeries$grp_wateryear)){
				tmpCond = calobs.dailytimeSeries$grp_wateryear==fik
				flushness[fik,] = c(
					sum(abs( calobsDayFlow[tmpCond][2:sum(tmpCond)]-calobsDayFlow[tmpCond][1:(sum(tmpCond)-1)] ))/sum(calobsDayFlow[tmpCond]),
					sum(abs( rhessysDayFlow[tmpCond][2:sum(tmpCond)]-rhessysDayFlow[tmpCond][1:(sum(tmpCond)-1)] ))/sum(rhessysDayFlow[tmpCond])
				)
			}#fik
			rhessysSAE = mean(flushness[,1]) #obs
		
			# ... / ... dailyCDFfit --> change to flushness index (annually) << **** >>
			#rhessysdailyCDF=ecdf(rhessysDayFlow)
			#rhessysdailyCDFresult = rhessysdailyCDF(dailyflowpt)
			#rhessysdailyCDFfit = sum(abs(dailyCDFresult-rhessysdailyCDFresult))
			rhessysdailyCDFfit = mean(flushness[,2]) #predicted
			
			# ___________ 1:3
			fittnessList[4:7]=c(rhessysNSE, rhessysLogNSE, rhessysSAE, rhessysdailyCDFfit ) #4<<------------------
				
		# ... weekly
			rhessysWeekFlow =grpSums(rhessysDayFlow, calobs.dailytimeSeries$grp_week )
		
			# ... / ... NSE
			rhessysSS = sum( weeklyDataQuality.weight*(calobsWeekFlow - rhessysWeekFlow)^2 )
			rhessysNSE = 1 - rhessysSS/weeklyobsSS
			rhessysWeekResidue = calobsWeekFlow - rhessysWeekFlow
			
			# ... / ... log NSE
			rhessysLogSS = sum( weeklyDataQuality.weight*(log(calobsWeekFlow) - log(rhessysWeekFlow))^2 )
			rhessysLogNSE = 1 - rhessysLogSS/weeklyobsLogSS
			
			# ... / ... SAE --> change to inversedNSE << **** >>
			# rhessysSAE = sum(weeklyDataQuality.weight*abs(calobsWeekFlow - rhessysWeekFlow)) #<<--------------- original
			rhessysSAE = 1- sum( weeklyDataQuality.weight*(1/calobsWeekFlow - 1/rhessysWeekFlow)^2 ) / sum( weeklyDataQuality.weight*(1/calobsWeekFlow - mean(1/calobsWeekFlow))^2 ) 
			
			
			rhessysweeklyCDF=ecdf(rhessysWeekFlow[minWeekCond])
			rhessysweeklyCDFresult = rhessysweeklyCDF(flowpt)
			rhessysweeklyCDFfit = 1-5*sum((weeklyCDFresult-rhessysweeklyCDFresult)^2 ) / sum((weeklyCDFresult- mean(weeklyCDFresult))^2 ) # --> r2 format << **** >>
			#sum(weeklyCDFresult) #sum(abs(weeklyCDFresult-rhessysweeklyCDFresult))#/sum(weeklyCDFresult)
			
			
			# ___________ 1:3
			fittnessList[8:11]=c(rhessysNSE, rhessysLogNSE, rhessysSAE, rhessysweeklyCDFfit ) #<<-------------------
		
		
		
		
		# ... monthly
			rhessysMonthFlow =grpSums(rhessysDayFlow, calobs.dailytimeSeries$grp_month )
			rhessysSS = sum( monthlyDataQuality.weight*(calobsMonthFlow - rhessysMonthFlow)^2 )
			rhessysNSE = 1 - rhessysSS/monthlyobsSS
			rhessysSAE = sum(monthlyDataQuality.weight*abs(calobsMonthFlow - rhessysMonthFlow))
			fittnessList[12:13]=c(rhessysNSE, rhessysSAE) #<----------------------
		
		
		
		# ... yearly (water year)
			rhessysYearFlow =grpSums(rhessysDayFlow, calobs.dailytimeSeries$grp_wateryear )
			rhessysYearET =grpSums(rhessysDayET, calobs.dailytimeSeries$grp_wateryear )
			rhessysYearRain = grpSums(rhessysDayRain, calobs.dailytimeSeries$grp_wateryear )
			rhessysSS = sum( yearlyDataQuality.weight*(calobsYearFlow - rhessysYearFlow)^2 )
			rhessysNSE = 1 - rhessysSS/yearlyobsSS
			rhessysSAE = sum(yearlyDataQuality.weight*abs(calobsYearFlow - rhessysYearFlow))
			rhessysYearResidue = calobsYearFlow - rhessysYearFlow
			fittnessList[14:15]=c(rhessysNSE, rhessysSAE) #<----------------------
		
		### -----------------------------------------
		# "bias","wbias","sbias", 18
		# "runoffRatio","ETratio","ETcor","annualBalance", 22
		# "s1","s2","s3","sv1","sv2","gw1","gw2")
		# ... / ... hydro
			#condWinter = calobs.dailytimeSeries$grp_monthMM==12|calobs.dailytimeSeries$grp_monthMM==1|calobs.dailytimeSeries$grp_monthMM==2
			
			#holding = allBias(calobsWeekFlow, dailytimeSeries_ $grp_weekMM, rhessysWeekFlow)
			holding = allBias(calobsMonthFlow, dailytimeSeries_ $grp_monthMM, rhessysMonthFlow)
			fittnessList[16:18] = c(holding$bias,holding$wbias,holding$sbias)
		
		
		# total flux and ratios
		fittnessList[19:24]=c(
			sum(rhessysYearRain),sum(rhessysYearET),sum(rhessysYearFlow),sum(calobsYearFlow), 
			mean(rhessysYearFlow/rhessysYearRain), 
			mean(calobsYearFlow/rhessysYearRain) 
		)


		approxET = sum(rhessysDayRain) - sum(calobsYearFlow)
		fittnessList[25] = (sum(rhessysDayET)-approxET)/approxET
		
		fittnessList[26] = fittnessList['meanAnnualFlushObs']-fittnessList['meanAnnualFlushRHESSys']

		MCMC_fittnessList = rep(NA,10)
		MCMC_fittnessList[1] = fittnessList['bias']
		MCMC_fittnessList[2] = fittnessList['wbias']
		MCMC_fittnessList[3] = fittnessList['sbias']
		MCMC_fittnessList[4] = fittnessList['dailyNSE']
		MCMC_fittnessList[5] = fittnessList['weeklyNSE']
		MCMC_fittnessList[6] = fittnessList['monthlyNSE']
		MCMC_fittnessList[7] = fittnessList['yearlyNSE']
		MCMC_fittnessList[8] = fittnessList['weeklyCDFfitr2']
		MCMC_fittnessList[9] = fittnessList['weeklyLogNSE']
		MCMC_fittnessList[10] = fittnessList['ETbias']

		MCMC_fittnessList2 = rep(NA,13)
		MCMC_fittnessList2[1] = fittnessList['bias']
		MCMC_fittnessList2[2] = fittnessList['wbias']
		MCMC_fittnessList2[3] = fittnessList['sbias']
		MCMC_fittnessList2[4] = fittnessList['inversedweeklyNSE'] #<<----- 4
		MCMC_fittnessList2[5] = fittnessList['weeklyNSE']
		MCMC_fittnessList2[6] = fittnessList['monthlyNSE']
		MCMC_fittnessList2[7] = fittnessList['yearlyNSE']
		MCMC_fittnessList2[8] = fittnessList['weeklyCDFfitr2'] #<<-----8
		MCMC_fittnessList2[9] = fittnessList['weeklyLogNSE']
		MCMC_fittnessList2[10] = fittnessList['ETbias']
		MCMC_fittnessList2[11] = fittnessList['dailyNSE']
		MCMC_fittnessList2[12] = fittnessList['dailyLogNSE']
		MCMC_fittnessList2[13] = fittnessList['flashCOMP']
		

	return<-list(
		FittnessList=fittnessList[4:length(fittnessList)],
		MCMC_fittnessList= MCMC_fittnessList,
		MCMC_fittnessList2=MCMC_fittnessList2
	)
	
}#

if(length(arg)>0){

##--------------------------------------------------------------------------------------------## WS18
arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18_mcmc", 
	"Qobs_18_r.csv",
	"rhessys_tree", "output_ws18_manuscript_TDR", "manu15Basin_basin.daily",
	"1990-10-1","2011-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_ws18_manuscript_TDR/test_it28_ws18_basin_plot_1980_2013_style2.pdf"
)

arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt", 
	"Qobs_18_r.csv",
	"rhessys", "output_ws18_manuscript_TDR", "manu18Basin_basin.daily",
	"1980-10-1","1991-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output_ws18_manuscript_calibration/test_it28_ws18_basin_plot_1980_2013_style2.pdf"
)



##--------------------------------------------------------------------------------------------## WS14 Coweeta
arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc", 
	"ws14dailyflow_19710101_20120703mm.csv",
	"rhessys", "output/SESSION_2_world_ITR_28", "rhessys_basin.daily",
	"2005-10-1","2011-9-30", 
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc/rhessys/output/SESSION_2_world_ITR_28/test_it28_basin_plot_1980_2013_style2.pdf"
)
arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc", 
	"ws14dailyflow_19710101_20120703mm.csv",
	"rhessys", "output/SESSION_2_world_ITR_447", "rhessys_basin.daily",
	"2005-10-1","2011-9-30", 
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc/rhessys/output/SESSION_2_world_ITR_28/test_it28_basin_plot_1980_2013_style2.pdf"
)
arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt", 
	"ws14dailyflow_19710101_20120703mm.csv",
	"rhessys", "output", "test_it28_basin.daily",
	"2005-10-1","2011-9-30", #"1980-10-1","1991-9-30"
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output/test_it28_basin_plot_1980_2013_style2.pdf"
)
arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows", 
	"ws14dailyflow_19710101_20120703mm.csv",
	"rhessys", "outputKilldevil/SESSION_2_world_ITR_21", "rhessysLONG_basin.daily",
	"2005-10-1","2011-9-30", #"1980-10-1","1991-9-30"
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows/rhessys/outputKilldevil/SESSION_2_world_ITR_21/test_it21_basin_plot_1980_2013_style2.pdf"
)

proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])

# modelFittness = function( calobs_, rhessys_, dailytimeSeries_)
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)
round(w$FittnessList,3)
as.numeric(w$FittnessList)


round(w$FittnessList,5)
w$MCMC_fittnessList
w$MCMC_fittnessList2


##--------------------------------------------------------------------------------------------## NY shelter
arg=c(
	"/Volumes/storage/RHESSys_watershed/NY_ShelterCreek/workflows10m", 
	"USGS01434092_19921001_20070930.csv",
	"rhessys_veg_stream", "output_mcmc_corTime3/SESSION_2_world_ITR_40", "soil8testV_basin.daily",
	"1992-10-1","1995-9-30",
	"baseline_basin_annualTable.csv"
)

proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])

# modelFittness = function( calobs_, rhessys_, dailytimeSeries_)
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)
round(w$FittnessList,3)


round(w$FittnessList,5)
w$MCMC_fittnessList
w$MCMC_fittnessList2

##--------------------------------------------------------------------------------------------## cane debug testing
arg=c(
	"/Users/laurencelin/Desktop/cane_testingMCMC_sept15/cane_FIA_nlcd_local", 
	"canecreek_19881101_20150826.csv",
	"rhessys", "output_h3/SESSION_2_world_ITR_1", "rhessys_basin.daily",
	"2000-10-1","2004-9-30",
	"baseline_basin_annualTable.csv"
)

proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])

# modelFittness = function( calobs_, rhessys_, dailytimeSeries_)
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)
round(w$FittnessList,3)
#--------------------------------- ITR 1
             # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE 
             # 0.269              0.356           1010.386              9.926              0.560 
      # weeklyLogNSE          weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE 
             # 0.469              0.045              8.517              0.782              9.340 
         # yearlyNSE          yearlySAE               bias              wbias              sbias 
             # 0.976             33.459              0.058              0.206             -0.038 
         # totPrecip              totET            totFlow         totFlowObs RHESSysRunoffRatio 
          # 4690.872           3464.575           1145.768           1083.439              0.207 
    # obsRunoffRatio 
             # 0.195 
#--------------------------------- ITR 2
         # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE 
             # 0.121              0.498            925.788              2.834              0.566 
      # weeklyLogNSE          weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE 
             # 0.588              0.119              1.580              0.684             10.207 
         # yearlyNSE          yearlySAE               bias              wbias              sbias 
             # 0.984             21.337              0.000              0.303             -0.348 
         # totPrecip              totET            totFlow         totFlowObs RHESSysRunoffRatio 
          # 4690.872           3514.655           1082.929           1083.439              0.190 
    # obsRunoffRatio 
             # 0.195 
#--------------------------------- ITR 4 -- 4-2-2
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE 
             # 0.303              0.432            884.121              8.728              0.667 
      # weeklyLogNSE          weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE 
             # 0.518              0.048              8.239              0.809              9.067 
         # yearlyNSE          yearlySAE               bias              wbias              sbias 
             # 0.992             17.191             -0.006              0.156              0.017 
         # totPrecip              totET            totFlow         totFlowObs RHESSysRunoffRatio 
          # 4690.872           3544.382           1076.635           1083.439              0.194 
    # obsRunoffRatio 
             # 0.195 







##--------------------------------------------------------------------------------------------## Ellerbe
arg=c(
	"/Users/laurencelin/Downloads/ellerbe/EllerbeCk_Gorman_workflows_100th", 
	"USGS_02086849.csv",
	"rhessys", "output", "rhessys_basin.daily",
	"2008-10-1","2013-9-30",
	"baseline_basin_annualTable.csv"
)

proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile = read.table(paste("/Users/laurencelin/Desktop/WSC/watersheds/cane/cane_FIA/cane_FIA_MCMC_c_output/SESSION_2_world_ITR_10/rhessys_basin.daily" ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])

# modelFittness = function( calobs_, rhessys_, dailytimeSeries_)
rhessys_SingleFileTest = rhessys_SingleFile
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)

##--------------------------------------------------------------------------------------------## morgan
arg=c(
	"/Users/laurencelin/Desktop/master_FIA", 
	"morgancreek_19981101_20150817.csv",
	"rhessys", "output_h3/SESSION_2_world_ITR_1", "rhessys_basin.daily",
	"2000-10-1","2004-9-30",
	"baseline_basin_annualTable.csv"
)


proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
rhessys_SingleFileTest = rhessys_SingleFile	
#rhessys_SingleFileTest[rhessys.dailytimeSeriesMatch,19] = calobs[calobs.dailytimeSeriesMatch,2]	
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFileTest[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)	

	
##--------------------------------------------------------------------------------------------## Cane
arg=c(
	"/Users/laurencelin/Desktop/WSC/watersheds/cane/cane_FIA", 
	"canecreek_19881101_20150826.csv",
	".", "cane_FIA_MCMC_c_output", "rhessys_basin.daily",
	"1998-10-1","2011-9-30",
	"baseline_basin_annualTable.csv"
)


proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile = read.table(paste("/Users/laurencelin/Desktop/WSC/watersheds/cane/cane_FIA/cane_FIA_MCMC_c_output/SESSION_2_world_ITR_10/rhessys_basin.daily" ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
rhessys_SingleFileTest = rhessys_SingleFile	
rhessys_SingleFileTest[rhessys.dailytimeSeriesMatch,19] = calobs[calobs.dailytimeSeriesMatch,2]	
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFileTest[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)	




arg=c(
	"/Users/laurencelin/Dropbox/Myself/UNC/WSC/cane_FIA_version2_realization", 
	"canecreek_19881101_20150826.csv",
	"rhessys", "output", "combineclimateB2010LULC_basin.daily",
	"2000-10-1","2004-9-30",
	"combineFIAPID1_annualTable.csv"
)


proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)	
round(w$FittnessList,5)

##------------------------------- rhessys_basin.daily itr29 MCMC
        # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
           # 0.34744            0.45559          881.89662            8.65878 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
           # 0.68528            0.53803            3.33474            7.99024 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 0.81902            8.89841            0.99449           14.20089 
              # bias              wbias              sbias          totPrecip 
           # 0.00355            0.13100            0.01628         4690.87200 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
        # 3606.80371         1087.28656         1083.43902            0.19581 
    # obsRunoffRatio 
           # 0.19472

##------------------------------- combineFIAPID1_basin.daily
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
           # 0.34999            0.41125          902.97637            9.34265 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
           # 0.69072            0.49859            3.38292            8.69756 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 0.82570            8.93628            0.99069           20.11569 
              # bias              wbias              sbias          totPrecip 
           # 0.04574            0.16943            0.11749         4690.87200 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
        # 3577.30011         1132.99256         1083.43902            0.20536 
    # obsRunoffRatio 
           # 0.19472 
  
 ##------------------------------- combineFIAPID9_basin.daily  
            # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
           # 0.34833            0.41850          895.54285            9.18925 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
           # 0.68796            0.50649            3.37105            8.51220 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 0.81974            9.02517            0.99281           17.91923 
              # bias              wbias              sbias          totPrecip 
           # 0.02773            0.14151            0.08640         4690.87200 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
        # 3609.65518         1113.48128         1083.43902            0.20133 
    # obsRunoffRatio 
           # 0.19472          
           
##------------------------------- combineclimateB2010LULC_basin.daily -------------------> need calibration?
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
           # 0.33000            0.17926         1083.30608           12.52473 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
           # 0.69599            0.29331            3.81324           11.93659 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 0.76842           10.60924            0.79505           90.78185 
              # bias              wbias              sbias          totPrecip 
           # 0.33190            0.41865            0.72779         4690.87200 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
        # 3528.60469         1443.02745         1083.43902            0.26892 
    # obsRunoffRatio 
           # 0.19472           
           
           
           

arg=c(
	"~/Dropbox/Myself/UNC/WSC/cane_FIA_version2_agu", 
	"canecreek_19881101_20150826.csv",
	"rhessys", "output", "e_cross_4_6_basin.daily",
	"2000-10-1","2004-9-30",
	"combineFIAPID1_annualTable.csv"
)


proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)	
round(w$FittnessList,5)

 ##------------------------------- e_cross_4_3_basin.daily  
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
        # 0.16183201         0.63592954       810.43829864         4.28960573 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
        # 0.34267689         0.65098005         3.64329501         5.37560976 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
        # 0.49109016        13.11974478         0.23691754       141.71902957 
              # bias              wbias              sbias          totPrecip 
       # -0.51137546        -0.56579468        -0.50398471      4690.87200000 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
     # 4166.68582500       529.39489200      1083.43902033         0.09891235 
    # obsRunoffRatio 
        # 0.19472357 

 ##------------------------------- e_cross_4_4_basin.daily 
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
           # 0.16193            0.63582          810.85100            4.28387 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
           # 0.34308            0.65099            3.64427            5.37073 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 0.49226           13.11261            0.23998          141.44950 
              # bias              wbias              sbias          totPrecip 
          # -0.51041           -0.56530           -0.50186         4690.87200 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
        # 4142.30887          530.44303         1083.43902            0.09908 
    # obsRunoffRatio 
           # 0.19472 

 ##------------------------------- e_cross_4_5_basin.daily
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
           # 0.13707            0.61201          828.87072            4.64588 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
           # 0.30423            0.62639            3.76908            5.63902 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 0.46813           13.41043            0.21228          144.35944 
              # bias              wbias              sbias          totPrecip 
          # -0.52095           -0.59616           -0.50189         4690.87200 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
        # 4174.55468          519.01768         1083.43902            0.09692 
    # obsRunoffRatio 
           # 0.19472
           
 ##------------------------------- e_cross_4_6_basin.daily     
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
           # 0.13705            0.61212          828.67268            4.64659 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
           # 0.30423            0.62632            3.76850            5.65366 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 0.46762           13.41319            0.21098          144.49344 
              # bias              wbias              sbias          totPrecip 
          # -0.52143           -0.59647           -0.50289         4690.87200 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
        # 4197.94016          518.49936         1083.43902            0.09683 
    # obsRunoffRatio 
           # 0.19472            
         



arg=c(
	"~/Dropbox/Myself/UNC/WSC/cane_FIA_nlcd_local", 
	"canecreek_19881101_20150826.csv",
	"rhessys", "output_agu", "h_cross_29_0_basin.daily",
	"2000-10-1","2004-9-30",
	"combineFIAPID1_annualTable.csv"
)


proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)	
round(w$FittnessList,5)

 ##------------------------------- h_cross_24_0_basin.daily 
        # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
           # 0.33788           -0.07320         1211.00554           15.23871 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
           # 0.63600            0.09046            4.59417           13.98537 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 0.77786           12.24980            0.72429          110.00875 
              # bias              wbias              sbias          totPrecip 
           # 0.39986            0.30471            0.90986         4690.87200 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
        # 3071.93863         1516.66221         1083.43902            0.28478 
    # obsRunoffRatio 
           # 0.19472 
 ##------------------------------- h_cross_25_0_basin.daily
               # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
           # 0.30733           -0.05995         1241.75169           15.16918 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
           # 0.59074            0.11986            4.71990           13.64390 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 0.76158           12.18995            0.72313          109.06714 
              # bias              wbias              sbias          totPrecip 
           # 0.39627            0.30106            0.74607         4690.87200 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
        # 3069.24108         1512.77288         1083.43902            0.28314 
    # obsRunoffRatio 
           # 0.19472     
 ##------------------------------- h_cross_28_0_basin.daily
  # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
           # 0.34885            0.03375         1162.78326           14.23441 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
           # 0.67164            0.17350            4.32860           13.24878 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 0.78146           11.71062            0.73067          106.40975 
              # bias              wbias              sbias          totPrecip 
           # 0.38570            0.30357            0.87543         4690.87200 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
        # 3085.14673         1501.32681         1083.43902            0.27955 
    # obsRunoffRatio 
           # 0.19472 
 ##------------------------------- h_cross_29_0_basin.daily
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
           # 0.34563            0.00213         1181.84305           14.56631 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
           # 0.67073            0.14436            4.40307           13.64878 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 0.77357           11.98108            0.70517          112.69578 
              # bias              wbias              sbias          totPrecip 
           # 0.40950            0.33979            0.89647         4690.87200 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
        # 3064.56786         1527.10924         1083.43902            0.28621 
    # obsRunoffRatio 
           # 0.19472 





arg=c(
	"~/Desktop/cane_FIA_version2", 
	"canecreek_19881101_20150826.csv",
	"rhessys", "output", "validationTest_basin.daily",
	#"2000-10-1","2004-9-30", calibrated
	"2004-10-1","2008-9-30",
	"validationTest_DRT_annualTable.csv"
)
proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<----- WY 1989 - 2014

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")
# 1990 - 2010 WY

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)	
round(w$FittnessList,5)

##---------------------------------------- cane paper 2
# "2000-10-1","2008-9-30",
         # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
           # 0.34524            0.35647         1476.27809            9.51052 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
           # 0.65055            0.44825            3.05593            8.78906 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 0.75384            9.03088            0.95138           28.21077 
              # bias              wbias              sbias          totPrecip 
           # 0.11159            0.26053            0.08127         8996.68000 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
        # 6875.28445         1926.75338         1733.33692            0.19796 
    # obsRunoffRatio 
           # 0.17653 
# "2004-10-1","2008-9-30",
         # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
           # 0.33217            0.22389          594.37318           10.54598 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
           # 0.49161            0.32171            2.73521            9.93333 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 0.46783            9.21904            0.29659           44.46630 
              # bias              wbias              sbias          totPrecip 
           # 0.29262            0.44959            0.22420         4305.80800 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
        # 3269.33886          840.07263          649.89790            0.20043 
    # obsRunoffRatio 
           # 0.15834


#### forward 2005 is ok, starting 2006 is not so ok.
# "2000-10-1","2004-9-30"
        # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.34704            0.45733          881.90491            8.62222            0.68469            0.53949 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 3.33470            7.95610            0.81996            8.86657            0.99441           14.01578 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.00299            0.13260            0.01459         4690.87200         3605.94560         1086.68075 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
        # 1083.43902            0.19550            0.19472 


# "1990-10-1","2004-9-30"
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.34528            0.44521         3497.52502            8.33380            0.43297            0.53013 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 3.90678            7.31535            0.47608           12.26044            0.53698           66.77274 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.09473            0.19513           -0.14233        17802.51788        12986.38280         4708.44726 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
        # 4301.01584            0.24941            0.23310 

# "1995-10-1","2004-9-30"
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.28841            0.44967         2418.27118            8.71474            0.39127            0.53216 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 4.25034            7.86147            0.35466           13.34088            0.69052           56.55227 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.16126            0.33263           -0.08630        11717.18588         8352.72988         3302.39110 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
        # 2843.79919            0.26186            0.23006

# "1997-10-1","2004-9-30"
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.41871            0.40891         1774.49998            9.35620            0.57968            0.50796 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 3.84499            8.52514            0.63669           11.22484            0.89212           33.17617 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.09114            0.08697           -0.17817         8812.69588         6397.16145         2277.84323 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
        # 2087.58931            0.23735            0.21949 

# "1996-10-1","2004-9-30"
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.42349            0.43106         1958.09553            9.27608            0.58204            0.51999 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 3.74367            8.51220            0.64439           10.72639            0.88663           33.72737 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.09475            0.09030           -0.11636         9891.17988         7321.38783         2631.41364 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
        # 2403.65618            0.24866            0.22869 

# "1996-10-1","2005-9-30"  
           # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.42709            0.43270         2066.03332            8.94878            0.58601            0.51910 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 3.50769            8.21834            0.65259           10.07452            0.88827           33.70887 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.10114            0.09080           -0.05348        10875.93788         8131.62016         2844.92337 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
        # 2583.61095            0.24512            0.22359  
  
# "1996-10-1","2006-9-30"
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.42405            0.41046         2183.18798            9.04354            0.58374            0.49732 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 3.40155            8.30632            0.65067            9.95760            0.89804           31.48285 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.10057            0.09872           -0.14983        11939.43588         9018.10935         2967.12045 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
        # 2695.97519            0.23210            0.21179 

# "1996-10-1","2007-9-30"        
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.42038            0.41127         2411.41673            9.26320            0.58136            0.50055 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 3.45333            8.49818            0.64550           10.18557            0.89257           32.44700 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.10623            0.14566           -0.12819        12909.20788         9774.10694         3274.79491 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
        # 2960.31874            0.23984            0.21732         
        
# "1996-10-1","2008-9-30"        
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.41477            0.37879         2552.46870            9.65361            0.57710            0.47213 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 3.43756            8.94058            0.63523           10.26812            0.87559           36.99199 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.13687            0.16071           -0.05002        14196.98788        10590.72668         3471.48627 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
        # 3053.55408            0.23258            0.20524         
        
# "1996-10-1","2009-9-30" 
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.41144            0.34343         2777.51772           10.25487            0.57286            0.44310 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 3.45274            9.51014            0.63592           10.30782            0.83852           43.22855 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.16436            0.19724           -0.04470        15183.26988        11508.42243         3758.35718 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
        # 3227.82676            0.23707            0.20305 
  
 # "1996-10-1","2010-9-30"       
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.40583            0.34874         3040.39202           10.40471            0.56774            0.44809 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 3.55154            9.62209            0.63575           10.56386            0.83487           43.25032 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.16206            0.19521           -0.01330        16332.36588        12218.21998         4116.62428 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
        # 3542.53512            0.24240            0.20811        
       
# 2004-2005
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.59627            0.44405          107.93779            6.24179            0.74388            0.49986 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 1.53786            5.57143            0.85221            4.64245               -Inf           33.55496 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.18646            0.09763            2.63503          984.75800          810.23233          213.50973 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
         # 179.95478            0.21681            0.18274 
		
# 2004-2006		
		 # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.37789            0.26929          225.09246            8.19817            0.49571            0.32998 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 1.94436            7.75258            0.57110            6.68856            0.45489           21.94703 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.14843            0.16404           -0.30171         2048.25600         1696.72152          335.70681 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
         # 292.31901            0.16586            0.14420         
      
# 2004-2007        
         # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.38494            0.33779          453.32120            9.29240            0.54848            0.42573 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 2.60888            8.43972            0.59840            8.61626            0.72915           28.73380 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.15578            0.37942           -0.18084         3018.02800         2452.71912          643.38127 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
         # 556.66256            0.21633            0.18699      

# 2004 - 2008   
        # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.33217            0.22389          594.37318           10.54598            0.49161            0.32171 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 2.73521            9.93333            0.46783            9.21904            0.29659           44.46630 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.29262            0.44959            0.22420         4305.80800         3269.33886          840.07263 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
         # 649.89790            0.20043            0.15834     

# 2004 - 2009
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.31986            0.14788          819.42219           12.01394            0.46625            0.26674 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 2.93744           11.25862            0.50374            9.56820           -0.55850           60.01663 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.36737            0.55196            0.14514         5292.09000         4187.03460         1126.94354 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
         # 824.17059            0.21852            0.16202    

# 2004 - 2010         
      # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit          weeklyNSE       weeklyLogNSE 
           # 0.32628            0.21493         1082.29650           12.07370            0.48853            0.32997 
         # weeklySAE       weeklyCDFfit         monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
           # 3.26651           11.29749            0.57265           10.32361            0.18825           57.32791 
              # bias              wbias              sbias          totPrecip              totET            totFlow 
           # 0.30410            0.38873            0.25484         6441.18600         4896.83215         1485.21064 
        # totFlowObs RHESSysRunoffRatio     obsRunoffRatio 
        # 1138.87894            0.23406            0.18066   
                
                
 arg=c(
	"/Users/laurencelin/Dropbox/Myself/UNC/WSC/cane_cmip5_test", 
	"canecreek_19881101_20150826.csv",
	"rhessys", "output/SESSION_2_world_ITR_1241", "rhessys_basin.daily",
	"2000-10-1","2004-9-30",
	"combineFIAPID1_annualTable.csv"
)


proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)	
round(w$FittnessList,5)               
                
                
##--------------------------------------------------------------------------------------------## Eno
arg=c(
	"/Users/laurencelin/Downloads/eno/enodurham3000", 
	"eno_19630901_20150904.csv",
	"rhessys", "output", "",
	"1998-10-1","2010-9-30",
	"."
)


proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]
sessionNum=3

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
#calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=20) ##<<-----

Itr = 1:500
holding = matrix(NA,length(Itr),22)
for(i in 1:length(Itr)){
	rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/SESSION_",sessionNum,"_","world_ITR_",Itr[i],"/rhessys_basin.daily" ,sep=''),header=F,skip=1,sep=' ')
	rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,3], rhessys_SingleFile[,2],sep="-"),format="%d-%m-%Y")
	
	tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
	calobs.dailytimeSeriesMatch = tmp$xSelect
	rhessys.dailytimeSeriesMatch = tmp$ySelect
	calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
	fakeRHESSysOutput = matrix(NA,nrow(rhessys_SingleFile),35)
	fakeRHESSysOutput[,19] = rhessys_SingleFile[,4] #flow
	fakeRHESSysOutput[,14] = rhessys_SingleFile[,6] #evap
	fakeRHESSysOutput[,16] = rhessys_SingleFile[,7] #trans
	fakeRHESSysOutput[,35] = rhessys_SingleFile[,5] #precip
	w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], fakeRHESSysOutput[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)	
	holding[i,]=c(Itr[i],w$FittnessList)
}#i
colnames(holding)=c("itr",names(w$FittnessList))
write.csv(holding,"test.csv",row.names=F)


##--------------------------------------------------------------------------------------------##
arg=c(
	"/Users/laurencelin/Desktop/WS18/cwt18_growth", 
	"Qobs_18_r.csv",
	"rhessys", "output", "",
	"1991-10-1","2010-9-30",
	"."
)


proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
#calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=20) ##<<-----


rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,'growthTest_basin.daily' ,sep=''),header=F,skip=1,sep=' ')
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,3], rhessys_SingleFile[,2],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])


Itr = 1:500
holding = matrix(NA,length(Itr),22)
for(i in 1:length(Itr)){
	
	
	fakeRHESSysOutput = matrix(NA,nrow(rhessys_SingleFile),35)
	fakeRHESSysOutput[,19] = rhessys_SingleFile[,4] #flow
	fakeRHESSysOutput[,14] = rhessys_SingleFile[,6] #evap
	fakeRHESSysOutput[,16] = rhessys_SingleFile[,7] #trans
	fakeRHESSysOutput[,35] = rhessys_SingleFile[,5] #precip
	w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], fakeRHESSysOutput[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)	
	holding[i,]=c(Itr[i],w$FittnessList)
}#i
colnames(holding)=c("itr",names(w$FittnessList))
write.csv(holding,"test.csv",row.names=F)

##--------------------------------------------------------------------------------------------##
arg=c(
	"~/Dropbox/Myself/UNC/WSC/cane_NLCD", 
	"canecreek_19881101_20150826.csv",
	"rhessys", "output", "testing_basin.daily",
	"2000-10-1","2004-9-30",
	"testing_basin_annualTable.csv"
)


proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)
	
# $FittnessList
          # dailyNSE        dailyLogNSE           dailySAE        dailyCDFfit 
         # 0.3214866          0.4724009        854.0953189          5.4207885 
         # weeklyNSE       weeklyLogNSE          weeklySAE       weeklyCDFfit 
         # 0.6032705          0.5333373          3.5186120          5.7512195 
        # monthlyNSE         monthlySAE          yearlyNSE          yearlySAE 
         # 0.6868861         11.5968627          0.7684152         94.7601310 
              # bias              wbias              sbias          totPrecip 
        # -0.3382641         -0.3162847         -0.2305386       4690.8720000 
             # totET            totFlow         totFlowObs RHESSysRunoffRatio 
      # 3983.7207700        716.9504990       1083.4390203          0.1245834 
    # obsRunoffRatio 
         # 0.1947236 

# $MCMC_fittnessList
 # [1] -0.3382641 -0.3162847 -0.2305386  0.3214866  0.6032705  0.6868861
 # [7]  0.7684152  0.4724009  0.5333373  0.1043090

##--------------------------------------------------------------------------------------------## NY
arg=c(
	"/Users/laurencelin/Dropbox/Myself/UNC/NY_ShelterCreek/workflows10m", 
	"USGS01434092_19921001_20070930.csv",
	"rhessys_veg", "output_/SESSION_2_world_ITR_1", "rhessys_basin.daily",
	"1992-10-1","1995-9-30",
	"combineFIAPID1_annualTable.csv"
)


proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)	
round(w$FittnessList,5)



##--------------------------------------------------------------------------------------------##  flat
 arg=c(
	"/Users/laurencelin/Desktop/master_FIA", 
	"flat_19251001_20130930.csv",
	"rhessys", "output_flatTest", "rhessys_basin.daily",
	"2000-10-1","2004-9-30",
	"combineFIAPID1_annualTable.csv"
)
	 ###-------- static phenology baseline, 3,4,5,6
	 arg=c(
		"/Volumes/storage/desktop_laura/master_FIA", 
		"flat_19251001_20130930.csv",
		"rhessys", "output_mcmcIIII/SESSION_1_world_ITR_3", "rhessys_basin.daily",
		"2000-10-1","2004-9-30",
		"combineFIAPID1_annualTable.csv"
	 )
	 
	 ###-------- dynamic phenology overlap with baseline period (in climate change content)
	 arg=c(
		"/Volumes/storage/WSC_storage/WSC_regional_FIA2LAI", 
		"flat_19251001_20130930.csv",
		"rhessys2010_climateB", "output_michieCsiro", "regionalFlatAll",
		"2000-10-1","2004-9-30",
		"combineFIAPID1_annualTable.csv"
	 )


proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----
	
	## regular
	rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
	rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

	## special for WSC output format [43 columns]
	rhessys_SingleFile_flowmm = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName,'_streamflowmm.csv' ,sep=''),header=F,skip=1,sep=',') # assume csv
	rhessys_SingleFile_flowet = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName,'_et.csv' ,sep=''),header=F,skip=1,sep=',') # assume csv
	rhessys_SingleFile_flowprecip = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName,'_precip.csv' ,sep=''),header=F,skip=1,sep=',') # assume csv
	rhessys_SingleFile = matrix(NA, nrow(rhessys_SingleFile_flowmm),43)
	rhessys_SingleFile[,19] = rhessys_SingleFile_flowmm[,5]
	rhessys_SingleFile[,16] = rhessys_SingleFile_flowet[,5]
	rhessys_SingleFile[,35] = rhessys_SingleFile_flowprecip[,5]
	rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile_flowmm[,3], rhessys_SingleFile_flowmm[,2], rhessys_SingleFile_flowmm[,1],sep="-"),format="%d-%m-%Y")


tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)	
round(w$FittnessList,5)   

# #               dailyNSE            dailyLogNSE     meanAnnualFlushObs 
              # -0.82948                0.45446                0.64458 
# meanAnnualFlushRHESSys              weeklyNSE           weeklyLogNSE 
               # 0.74815               -0.03327                0.50963 
     # inversedweeklyNSE         weeklyCDFfitr2             monthlyNSE 
               # 0.14946                0.71054               -0.32476 
            # monthlySAE              yearlyNSE              yearlySAE 
              # 23.32954               -0.37706              269.78946 
                  # bias                  wbias                  sbias 
               # 0.83813                0.58411                1.37545 
             # totPrecip                  totET                totFlow 
            # 5119.30000             2724.10734             2366.73500 
            # totFlowObs     RHESSysRunoffRatio         obsRunoffRatio 
            # 1287.57717                0.39575                0.21049 
# itr	dailyNSE	dailyLogNSE	meanAnnualFlushObs	meanAnnualFlushRHESSys	weeklyNSE	weeklyLogNSE	inversedweeklyNSE	weeklyCDFfitr2	monthlyNSE	monthlySAE	yearlyNSE	yearlySAE	bias	wbias	sbias	totPrecip	totET	totFlow	totFlowObs	RHESSysRunoffRatio	obsRunoffRatio	s1	s2	s3	sv1	sv2	gw1	gw2
# 46	-0.181789671	0.288520208	0.644577918	0.546335315	0.350898606	0.369879337	0.010855381	0.272374397	0.001918814	20.16096883	0.025304524	236.13759	0.733587379	0.411419438	1.235537587	5119.3	2844.086274	2232.127529	1287.577169	0.377229295	0.210491614	0.883443537	21.00798439	20	0.827432064	135.8660387	0	0
  
##--------------------------------------------------------------------------------------------##  WS14
arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc", 
	"ws14dailyflow_19710101_20120703mm.csv",
	"rhessys", "output", "vegbolstad_bgcNew_basin.daily",
	"2005-10-1","2011-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc/rhessys/output/vegbolstad_bgcNew_basin_annualTable.csv"
)
arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc", 
	"ws14dailyflow_19710101_20120703mm.csv",
	"rhessys", "output", "vegbolstad_rhd5_bgcNew2_basin.daily",
	"2005-10-1","2011-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc/rhessys/output/vegbolstad_bgcNew_basin_annualTable.csv"
)
arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows", 
	"ws14dailyflow_19710101_20120703mm.csv",
	"rhessys", "outputKilldevil/SESSION_2_world_ITR_21", "rhessys_basin.daily",
	"2005-10-1","2011-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows/rhessys/outputKilldevil/SESSION_2_world_ITR_21/vegbolstad_bgcNew_basin_annualTable.csv"
)

proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[8]

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; 
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) 
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
w = modelFittness( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)	
round(w$FittnessList,5)    
  
 ##........................ vegbolstad_bgcNew_basin
                # dailyNSE            dailyLogNSE     meanAnnualFlushObs meanAnnualFlushRHESSys 
               # 0.65744                0.85668                0.17957                0.20189 
             # weeklyNSE           weeklyLogNSE      inversedweeklyNSE         weeklyCDFfitr2 
               # 0.82221                0.86267                0.77481                0.90277 
            # monthlyNSE             monthlySAE              yearlyNSE              yearlySAE 
               # 0.84261               13.00266                0.85556               76.67287 
                  # bias                  wbias                  sbias              totPrecip 
               # 0.09428                0.16356               -0.09139            10588.67337 
                 # totET                totFlow             totFlowObs     RHESSysRunoffRatio 
            # 5430.53562             5339.69107             4879.65384                0.50208 
        # obsRunoffRatio 
               # 0.45922 
               
 ##........................ vegbolstad_rhd5_bgcNew2_basin               
                # dailyNSE            dailyLogNSE     meanAnnualFlushObs meanAnnualFlushRHESSys 
               # 0.68305                0.85312                0.17957                0.20413 
             # weeklyNSE           weeklyLogNSE      inversedweeklyNSE         weeklyCDFfitr2 
               # 0.85010                0.85840                0.73041                0.90239 
            # monthlyNSE             monthlySAE              yearlyNSE              yearlySAE 
               # 0.87476               11.87585                0.92722               49.82223 
                  # bias                  wbias                  sbias              totPrecip 
               # 0.06074                0.13521               -0.10658            10588.67337 
                 # totET                totFlow             totFlowObs     RHESSysRunoffRatio 
            # 5590.96694             5176.04229             4879.65384                0.48618 
        # obsRunoffRatio 
               # 0.45922 
               
 ##........................ setup_rhessys_5_20_debug2_workflows                  
                # dailyNSE            dailyLogNSE     meanAnnualFlushObs meanAnnualFlushRHESSys 
               # 0.63668                0.86418                0.17957                0.19409 
             # weeklyNSE           weeklyLogNSE      inversedweeklyNSE         weeklyCDFfitr2 
               # 0.79243                0.86752                0.80891                0.88909 
            # monthlyNSE             monthlySAE              yearlyNSE              yearlySAE 
               # 0.81578               13.84510                0.75714              103.17472 
                  # bias                  wbias                  sbias              totPrecip 
               # 0.12686                0.11781               -0.00456            10588.67337 
                 # totET                totFlow             totFlowObs     RHESSysRunoffRatio 
            # 5316.22484             5498.70214             4879.65384                0.51790 
        # obsRunoffRatio 
               # 0.45922 
  
##--------------------------------------------------------------------------------------------##	
}#



	
	
	
	