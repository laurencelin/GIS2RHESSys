source("~/Dropbox/LIB_Rscript/LIB_misc.r")
source("~/Dropbox/LIB_Rscript/LIB_dailytimeseries2.r")
source("~/Dropbox/LIB_Rscript/LIB_hydro.r")

arg=commandArgs(T)

modelBehavior = function( rhessys_, dailytimeSeries_ ){
	
	mostcol = ncol(rhessys_)
	# ... assume rhessys_ is a matrix and already "matched"
	
	# ... rhessys_ balance
	brhessys_DayFlow =rhessys_[,19]#flow
	brhessys_DayRain =rhessys_[,35]#rain
	brhessys_DayET =rhessys_[,14]+rhessys_[,16]#et
	
	brhessys_DayInf = ifelse(43<=mostcol,rhessys_[,43],rep(0,nrow(rhessys_)))# recharge (infiltration)
	brhessys_DayUnsatDrain =rhessys_[,12]# unsat drain
	brhessys_DayCap =rhessys_[,13]# cap		

	brhessys_DayRZ =rhessys_[,9]#rz
	brhessys_DayUnsat =rhessys_[,10]#unsat
	brhessys_DaySatdef =rhessys_[,8]#satdef
	brhessys_DaySatz =rhessys_[,7]#satz
	
	brhessys_DayCanopy =rhessys_[,27]# canopy store
	brhessys_DayLitter =rhessys_[,26]# litter store
	brhessys_DayGWS =rhessys_[,23]# groundwater store
	brhessys_DayDets =rhessys_[,24]# detention store
	
	brhessys_DayPSN =rhessys_[,20]# psn
	brhessys_DaySatArea =rhessys_[,25]# sat area
	brhessys_DayReturn =rhessys_[,18]# return
	brhessys_DayBaseflow =rhessys_[,17]# baseflow (sub-surface)
	brhessys_DayGWq =rhessys_[,22]# gwq

	### ----------------------------------------- model behavorial assessment 
	# ... annual flux + balance
	brhessys_YearFlow =grpSums(brhessys_DayFlow, dailytimeSeries_ $grp_wateryear )
	brhessys_YearRain =grpSums(brhessys_DayRain, dailytimeSeries_ $grp_wateryear )
	brhessys_YearET =grpSums(brhessys_DayET, dailytimeSeries_ $grp_wateryear )
	
	brhessys_YearRZ = brhessys_DayRZ[dailytimeSeries_ $ithdayisendWY] - brhessys_DayRZ[dailytimeSeries_ $ithdayisbeginWY]
	brhessys_YearUnsat = brhessys_DayUnsat[dailytimeSeries_ $ithdayisendWY] - brhessys_DayUnsat[dailytimeSeries_ $ithdayisbeginWY]
	brhessys_YearSatdef = brhessys_DaySatdef[dailytimeSeries_ $ithdayisendWY] - brhessys_DaySatdef[dailytimeSeries_ $ithdayisbeginWY]
	brhessys_YearCanopy = brhessys_DayCanopy[dailytimeSeries_ $ithdayisendWY] - brhessys_DayCanopy[dailytimeSeries_ $ithdayisbeginWY]
	brhessys_YearLitter = brhessys_DayLitter[dailytimeSeries_ $ithdayisendWY] - brhessys_DayLitter[dailytimeSeries_ $ithdayisbeginWY]
	brhessys_YearGWS = brhessys_DayGWS[dailytimeSeries_ $ithdayisendWY] - brhessys_DayGWS[dailytimeSeries_ $ithdayisbeginWY]
	brhessys_YearDets = brhessys_DayDets[dailytimeSeries_ $ithdayisendWY] - brhessys_DayDets[dailytimeSeries_ $ithdayisbeginWY]
	
	brhessys_YearBalance = brhessys_YearRain-brhessys_YearET-brhessys_YearFlow-brhessys_YearRZ-brhessys_YearUnsat+brhessys_YearSatdef-brhessys_YearCanopy-brhessys_YearLitter-brhessys_YearGWS-brhessys_YearDets
	
	brhessys_YearInf =grpSums(brhessys_DayInf, dailytimeSeries_ $grp_wateryear )
	brhessys_YearUnsatDrain =grpSums(brhessys_DayUnsatDrain, dailytimeSeries_ $grp_wateryear )
	brhessys_YearCap =grpSums(brhessys_DayCap, dailytimeSeries_ $grp_wateryear )
	brhessys_YearGWq =grpSums(brhessys_DayGWq, dailytimeSeries_ $grp_wateryear )
	brhessys_YearPSN =grpSums(brhessys_DayPSN, dailytimeSeries_ $grp_wateryear )
	brhessys_YearSatz =grpMeans(brhessys_DaySatz, dailytimeSeries_ $grp_wateryear )
	
	
	# ... weekly flux
	brhessys_WeekPrecip = grpSums(brhessys_DayRain, dailytimeSeries_ $grp_week )
	brhessys_WeekStreamflow = grpSums(brhessys_DayFlow, dailytimeSeries_ $grp_week )
	brhessys_WeekReturn = grpSums(brhessys_DayReturn, dailytimeSeries_ $grp_week )
	brhessys_WeekGWq = grpSums(brhessys_DayGWq, dailytimeSeries_ $grp_week )
	brhessys_WeekBaseflow = brhessys_WeekStreamflow-brhessys_WeekReturn-brhessys_WeekGWq  #subsurface
	

	brhessys_WeekSatArea = grpMeans(brhessys_DaySatArea, dailytimeSeries_ $grp_week )
	brhessys_WeekSatz = grpMeans(brhessys_DaySatz, dailytimeSeries_ $grp_week )
	brhessys_WeekUnsat = grpMeans(brhessys_DayUnsat, dailytimeSeries_ $grp_week )
	
	weekyyyy = unique(dailytimeSeries_ $grp_weekWY)
		hhresult = matrix(NA,length(weekyyyy),10)
		for(hh in 1:length(weekyyyy)){
			hhcond = dailytimeSeries_ $grp_weekWY == weekyyyy[hh]
			hhresult[hh,]=c(
				cor(brhessys_WeekReturn[hhcond]/brhessys_WeekStreamflow[hhcond], brhessys_WeekPrecip[hhcond] ),
				cor(brhessys_WeekBaseflow[hhcond]/brhessys_WeekStreamflow[hhcond], brhessys_WeekPrecip[hhcond]),
				cor(brhessys_WeekGWq[hhcond]/brhessys_WeekStreamflow[hhcond], brhessys_WeekPrecip[hhcond]),
				cor(brhessys_WeekUnsat[hhcond], brhessys_WeekPrecip[hhcond]),
				cor(brhessys_WeekSatz[hhcond], brhessys_WeekPrecip[hhcond]),
				cor(brhessys_WeekSatArea[hhcond], brhessys_WeekPrecip[hhcond]),
				min(brhessys_DaySatArea[hhcond]),
				median(brhessys_DaySatArea[hhcond]),
				max(brhessys_DaySatArea[hhcond]),
				sd(brhessys_DaySatArea[hhcond])/mean(brhessys_DaySatArea[hhcond])
			)
		}#hh
		
		
		annualTable=cbind(
			dailytimeSeries_$grp_wateryearYYYY,
			brhessys_YearRain,
			brhessys_YearET,
			brhessys_YearFlow,
			brhessys_YearCanopy,
			brhessys_YearLitter,
			brhessys_YearDets,
			brhessys_YearRZ,
			brhessys_YearUnsat,
			brhessys_YearSatdef,
			brhessys_YearGWS,
			(brhessys_YearET+ brhessys_YearFlow)/brhessys_YearRain,
			##----------------------
			brhessys_YearInf,
			brhessys_YearUnsatDrain, 
			brhessys_YearCap,
			brhessys_YearGWq,
			brhessys_YearPSN,
			brhessys_YearSatz,
			##----------------------
			hhresult
			##----------------------
		);
		colnames(annualTable)=c(
		"WY","rain","rhessys_ET","rhessys_Flow","rhessys_Canopy","rhessys_Litter","rhessys_Detention","rhessys_RZ","rhessys_Unsat","rhessys_Satdef","rhessys_GW","rhessys_Balance",
		"annualInf","annualUnsatDrain","annualCap","annualGWq","annualPSN","annualSatz",
		"returnflow_precip","subflow_precip","gwq_precip","unsat_precip","satz_precip","satArea_precip","minSatArea","medianSatArea","maxSatArea","SDSatArea"
		)
		
		annualList=c(
			mean((brhessys_YearET+ brhessys_YearFlow)/brhessys_YearRain),
			mean(brhessys_YearInf),
			mean(brhessys_YearUnsatDrain),
			mean(brhessys_YearCap),
			mean(brhessys_YearSatz),
			mean(brhessys_YearPSN),
			
			mean(brhessys_YearFlow/brhessys_YearRain),
			mean(brhessys_YearET/brhessys_YearRain),
			cor(brhessys_YearET,brhessys_YearRain),
			cor(brhessys_YearFlow,brhessys_YearRain),
			
			mean(brhessys_YearGWq/brhessys_YearFlow),
			cor(brhessys_WeekReturn/brhessys_WeekStreamflow, brhessys_WeekPrecip),
			cor(brhessys_WeekBaseflow/brhessys_WeekStreamflow, brhessys_WeekPrecip),
			cor(brhessys_WeekGWq/brhessys_WeekStreamflow, brhessys_WeekPrecip),
			cor(brhessys_WeekUnsat, brhessys_WeekPrecip),
			cor(brhessys_WeekSatz, brhessys_WeekPrecip),
			cor(brhessys_WeekSatArea, brhessys_WeekPrecip),
			mean(brhessys_DaySatArea),
			sd(brhessys_DaySatArea)/mean(brhessys_DaySatArea)
		)
		names(annualList)=c('ETFpcp','inf','unsatDrain','cap','satz','PSN','runoffRatio','ETratio','ETpcpCor','FlowpcpCor','GWQratio','returnPcp','subflowPcp','GWpcp','UnsatPcp','satzPcp','satAreaPcp','satArea','satAreaSD')
		
		return<-list(
			AnnualTable=annualTable,
			AnnualList=annualList
		)
		# add summary
	
}#end of function

###--------------------------------------------------------------------------------------------------------------------------------- single file warp

if(length(arg)>0){

###----------------------------------------------------------- WS18, NC
# arg=c(
	# "/Users/laurencelin/Desktop/WS18/cwt18", "rhessys_", "output_test", "soil_debug2_basin.daily.csv",
	# "1991-10-1","2010-9-30",
	# "what.csv"
# )
# arg=c(
	# "/Users/laurencelin/Desktop/WS18/cwt18", "rhessys_", "output_test", "soil_debug2_basin.daily.csv",
	# "1991-10-1","2010-9-30",
	# "soil_debug2_basin_annualTable.csv"
# )
# arg=c(
	# "/Users/laurencelin/Desktop/WS18/cwt18", "rhessys_", "output_test", "soil_debug2_deep_basin.daily.csv",
	# "1991-10-1","2010-9-30",
	# "soil_debug2_deep_basin_annualTable.csv"
# )
# arg=c(
	# "/Users/laurencelin/Desktop/WS18/cwt18", "rhessys_", "output_test", "soil_debug2_shallow_basin.daily.csv",
	# "1991-10-1","2010-9-30",
	# "soil_debug2_shallow_basin_annualTable.csv"
# )
# arg=c(
	# "/Users/laurencelin/Desktop/WS18/cwt18", "rhessys_", "output_test", "soil_debug2_k05_basin.daily.csv",
	# "1991-10-1","2010-9-30",
	# "soil_debug2_k05_basin_annualTable.csv"
# )


# arg=c(
	# "/Users/laurencelin/Desktop/WS18/cwt18", "rhessys_", "output_test", "soil_debug2_kv1mv025_basin.daily.csv",
	# "1991-10-1","2010-9-30",
	# "soil_debug2_kv1mv025_basin_annualTable.csv"
# )



# arg=c(
	# "/Users/laurencelin/Desktop/WS18/cwt18", "rhessys_", "output_test", "veg_debug2_k300_basin.daily.csv",
	# "1991-10-1","2010-9-30",
	# "veg_debug2_k300_basin_annualTable.csv"
# )



###----------------------------------------------------------- cane, NC
# arg=c(
	# "/Users/laurencelin/Dropbox/Myself/UNC/WSC/cane_FIA", "rhessys_", "output", "baselineSoil_basin.daily.csv",
	# "2000-10-1","2004-9-30",
	# "baselineSoil_basin_annualTable.csv"
# )
# arg=c(
	# "/Users/laurencelin/Dropbox/Myself/UNC/WSC/cane_FIA", "rhessys_", "output", "baselineSoil_debug2_basin.daily.csv",
	# "2000-10-1","2004-9-30",
	# "baselineSoil_debug2_basin_annualTable.csv"
# )
# arg=c(
	# "/Users/laurencelin/Dropbox/Myself/UNC/WSC/cane_FIA", "rhessys_", "output", "baseline_debug2_basin.daily.csv",
	# "2000-10-1","2004-9-30",
	# "baseline_debug2_basin_annualTable.csv"
# )
# arg=c(
	# "/Users/laurencelin/Dropbox/Myself/UNC/WSC/cane_FIA", "rhessys_", "output", "baseline_basin.daily.csv",
	# "2000-10-1","2004-9-30",
	# "baseline_basin_annualTable.csv"
# )


arg=c(
	"/Users/laurencelin/Dropbox/Myself/UNC/WSC/cane_FIA", "rhessys", "output", "baseline_debug2B_basin.daily.csv",
	"2000-10-1","2011-9-30",
	"baseline_debug2B_basin_annualTable.csv"
)
arg=c(
	"/Users/laurencelin/Dropbox/Myself/UNC/WSC/cane_FIA", "rhessys", "output", "baselineSoil_debug2B_basin.daily.csv",
	"2000-10-1","2011-9-30",
	"baselineSoil_debug2B_basin_annualTable.csv"
)
arg=c(
	"/Users/laurencelin/Dropbox/Myself/UNC/WSC/cane_FIA", "rhessys", "output", "baseline_debug2C_basin.daily.csv",
	"2000-10-1","2011-9-30",
	"baseline_debug2C_basin_annualTable.csv"
)
arg=c(
	"/Users/laurencelin/Dropbox/Myself/UNC/WSC/cane_FIA", "rhessys", "output", "FIA_FT_debug2B_basin.daily.csv",
	"2000-10-1","2011-9-30",
	"FIA_FT_debug2B_basin_annualTable.csv"
)



###----------------------------------------------------------- cane reservior, NC
# arg=c(
	# "/Users/laurencelin/Dropbox/Myself/UNC/WSC/cane_reservoir_FIA", "rhessys_", "output", "baselineSoil_basin.daily.csv",
	# "2000-10-1","2004-9-30",
	# "baselineSoil_basin_annualTable.csv"
# )
# arg=c(
	# "/Users/laurencelin/Dropbox/Myself/UNC/WSC/cane_reservoir_FIA", "rhessys_", "output", "baseline_basin.daily.csv",
	# "2000-10-1","2004-9-30",
	# "baseline_basin_annualTable.csv"
# )
# arg=c(
	# "~/Dropbox/Myself/UNC/WSC/cane_reservoir_FIA", "rhessys", "output", "baselineSoil_debug2_basin.daily.csv",
	# "2000-10-1","2004-9-30",
	# "baselineSoil_debug2_basin_annualTable.csv"
# )
# arg=c(
	# "~/Dropbox/Myself/UNC/WSC/cane_reservoir_FIA", "rhessys", "output", "baseline_debug2_basin.daily.csv",
	# "2000-10-1","2004-9-30",
	# "baseline_debug2_basin_annualTable.csv"
# )

arg=c(
	"~/Dropbox/Myself/UNC/WSC/cane_reservoir_FIA", "rhessys", "output", "baseline_debug2B_basin.daily.csv",
	"2000-10-1","2011-9-30",
	"baseline_debug2B_basin_annualTable.csv"
)
arg=c(
	"~/Dropbox/Myself/UNC/WSC/cane_reservoir_FIA", "rhessys", "output", "baselineSoil_debug2B_basin.daily.csv",
	"2000-10-1","2011-9-30",
	"baselineSoil_debug2B_basin_annualTable.csv"
)
arg=c(
	"~/Dropbox/Myself/UNC/WSC/cane_reservoir_FIA", "rhessys", "output", "baseline_debug2C_basin.daily.csv",
	"2000-10-1","2011-9-30",
	"baseline_debug2C_basin_annualTable.csv"
)
arg=c(
	"~/Dropbox/Myself/UNC/WSC/cane_reservoir_FIA", "rhessys", "output", "FIA_FT_debug2B_basin.daily.csv",
	"2000-10-1","2011-9-30",
	"FIA_FT_debug2B_basin_annualTable.csv"
)



arg=c(
	"~/Dropbox/Myself/UNC/WSC/cane_reservoir_FIA", "rhessys", "output", "baseline_debug2B1_basin.daily.csv",
	"2000-10-1","2011-9-30",
	"baseline_debug2B1_basin_annualTable.csv"
)
arg=c(
	"~/Dropbox/Myself/UNC/WSC/cane_reservoir_FIA", "rhessys", "output", "baseline_debug2B2_basin.daily.csv",
	"2000-10-1","2011-9-30",
	"baseline_debug2B2_basin_annualTable.csv"
)

arg=c(
	"~/Dropbox/Myself/UNC/WSC/cane_reservoir_FIA", "rhessys", "output", "baseline_debug2B1Ring_basin.daily.csv",
	"2000-10-1","2011-9-30",
	"baseline_debug2B1Ring_basin_annualTable.csv"
)
arg=c(
	"~/Dropbox/Myself/UNC/WSC/cane_reservoir_FIA", "rhessys", "output", "baseline_debug2B1Tracheid_basin.daily.csv",
	"2000-10-1","2011-9-30",
	"baseline_debug2B1Tracheid_basin_annualTable.csv"
)

arg=c(
	"~/Dropbox/Myself/UNC/WSC/cane_reservoir_FIA", "rhessys", "output", "baseline_debug2B2B_basin.daily.csv",
	"2000-10-1","2011-9-30",
	"baseline_debug2B2B_basin_annualTable.csv"
)

##------------------------------------------------- cane FIA version2 AGU
arg=c(
	"~/Dropbox/Myself/UNC/WSC/cane_FIA_version2_agu", 
	"rhessys", "output", "e_cross_4_3_basin.daily",
	"2000-10-1","2004-9-30",
	"e_cross_4_3_basin_annualTable.csv"
)

proj = arg[1]
rhessys_Folder = arg[2]
outputFolder = arg[3]
fileName = arg[4]
startingDate=as.Date(arg[5]) #, 
endingDate=as.Date(arg[6]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputName = arg[7]


rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")
tmp = match2DailyTimeSeries(rhessys_SingleFile.date, period) ### assume period is the most narrow band
Balancerhessys_.dailytimeSeries_Match = tmp$xSelect
Balancerhessys_.dailytimeSeries_ = dailyTimeSeries(rhessys_SingleFile.date[Balancerhessys_.dailytimeSeries_Match])


w = modelBehavior(rhessys_SingleFile[Balancerhessys_.dailytimeSeries_Match,], Balancerhessys_.dailytimeSeries_)
		
write.csv(w$AnnualTable,paste(proj,"/", rhessys_Folder,"/", outputFolder,"/",outputName ,sep=''),row.names=F)

	
}

			
			
			
			
			
			
			
			
			
			
			