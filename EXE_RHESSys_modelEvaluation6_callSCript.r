source("~/Dropbox/LIB_Rscript/LIB_misc.r")
source("~/Dropbox/LIB_Rscript/LIB_dailytimeseries2.r")
source("~/Dropbox/LIB_Rscript/LIB_hydro.r")
source("~/Dropbox/LIB_Rscript/LIB_RHESSys_modelFit_6_working.r")
source("~/Dropbox/LIB_Rscript/LIB_RHESSys_modelBehavior_6_working.r")
source("~/Dropbox/LIB_Rscript/LIB_RHESSys_modelPlot_6_working.r")

arg=commandArgs(T)

##--------------------------------------------------------------------------------------------## flat
# arg=c(
	# '/Volumes/storage/WSC_storage/flatriver/flatbahama2',
	# "flat_19251001_20130930.csv",
	# "2", "1", "500", "2000-10-1","2004-9-30","rhessys","output","NLCD_flat"
# );paste(arg,collapse=" ")

# arg=c(
	# "/Users/laurencelin/Desktop/master_FIA",
	# "flat_19251001_20130930.csv",
	# "1", "1", "500", "2000-10-1","2004-9-30","rhessys","output_flat_shortgun","flat_FIAnlcdlocal"
# );paste(arg,collapse=" ")

# arg=c(
	# "/Users/laurencelin/Desktop/master_FIA",
	# "flat_19251001_20130930.csv",
	# "1", "1", "500", "1998-10-1","2004-9-30","rhessys","output_flat_shortgun","flat_FIAnlcdlocal"
# );paste(arg,collapse=" ")

##--------------------------------------------------------------------------------------------## cane
# arg=c(
	# "/Volumes/storage/WSC_storage/cane_link_nov8_2016/calibration_ch2w_ruleMod_1992-2012",
	# "canecreek_19881101_20150826.csv",
	# "2", "1", "2000", "2000-10-1","2004-9-30","rhessys","output_cal","NLCD_cane"
# );paste(arg,collapse=" ")

# arg=c(
	# "/Volumes/storage/WSC_storage/cane_link_nov8_2016/calibration_ch2w_ruleMod_1992-2012",
	# "canecreek_19881101_20150826.csv",
	# "2", "5", "8", "2000-10-1","2004-9-30","rhessys","output","NLCD_caneParameterTest"
# );paste(arg,collapse=" ")

# arg=c(
	# "~/Desktop/WSC/watersheds/cane/cane_FIA/",
	# "canecreek_19881101_20150826.csv",
	# "2", "1", "2000", "2000-10-1","2004-9-30","rhessys_modeC_debug2","output","modeC_debug2_cane"
# );paste(arg,collapse=" ")

# arg=c(
	# "~/Desktop/WSC/watersheds/cane/cane_FIA/cane_FIA_version2",
	# "canecreek_19881101_20150826.csv",
	# "2", "1", "300", "2000-10-1","2004-9-30","rhessys","output_sensitivity2","output_sensitivity2"
# );paste(arg,collapse=" ")

# arg=c(
	# "~/Desktop/WSC/watersheds/cane/cane_FIA/cane_FIA_nlcd_local",
	# "canecreek_19881101_20150826.csv",
	# "2", "1", "300", "2000-10-1","2004-9-30","rhessys","output_sensitivity","output_sensitivity"
# );paste(arg,collapse=" ")

# arg=c(
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WSC/watersheds_link_nov8_2016/cane/cane_FIA/cane_FIA_version2_7tree",
	# "canecreek_19881101_20150826.csv",
	# "3", "1", "5", "2000-10-1","2004-9-30","rhessys","output","parametertesting"
# );paste(arg,collapse=" ")


# arg=c(
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WSC/watersheds_link_nov8_2016/cane/cane_FIA/cane_FIA_version2",
	# "canecreek_19881101_20150826.csv",
	# "3", "1", "5", "2000-10-1","2004-9-30","rhessys","output_parameterTest","parametertesting"
# );paste(arg,collapse=" ")


# arg=c(
	# "/Volumes/storage/WSC_storage/cane_link_nov8_2016/calibration_ch2w_ruleMod_1992-2012",
	# "canecreek_19881101_20150826.csv",
	# "2", "1", "500", "2000-10-1","2004-9-30","rhessys","output_d2III_random","d2III_random"
# );paste(arg,collapse=" ")

##--------------------------------------------------------------------------------------------## little
# arg=c(
	# "~/Desktop/WSC/watersheds/littleriver/calibration_oct20",
	# "litteriver_19870930_20150904.csv",
	# "1", "1", "2000", "2000-10-1","2005-9-30","rhessys","output","NLCD_little"
# );paste(arg,collapse=" ")

##--------------------------------------------------------------------------------------------## morgan
# arg=c(
	# "~/Desktop/WSC/watersheds/morgancreek/calibration_ch2w91-10_rulesMod_1992-2012",
	# "morgancreek_19981101_20150817.csv",
	# "2", "1", "2000", "2000-10-1","2004-9-30","rhessys","output","NLCD_morgan"
# );paste(arg,collapse=" ")

# arg=c(
	# "/Users/laurencelin/Desktop/master_FIA",
	# "morgancreek_19981101_20150817.csv",
	# "2", "1", "10", "2000-10-1","2004-9-30","rhessys","output_h3","morgan"
# );paste(arg,collapse=" ")

##--------------------------------------------------------------------------------------------## mtn
# arg=c(
	# "~/Desktop/WSC/watersheds/mtncreek/calibration/mtncreek_calibration_flat",
	# "USGS208524090_19941001_20150902.csv",
	# "1", "1", "2000", "2000-10-1","2004-9-30","rhessys","output","NLCD_mtn"
# );paste(arg,collapse=" ")

## overall 1998 - 2011 WY 14yr
## normal  2008 - 2010 WY 3yr

## extreme 2001 - 2002 WY 2yr
## normal  2006 - 2010 WY 5yr
## mid  2000 - 2003 WY 5yr

##--------------------------------------------------------------------------------------------## new hope
# arg=c(
	# "/Users/laurencelin/Downloads/newhope",
	# "newhope_19821001_2015.csv",
	# "1", "1", "500", "2000-10-1","2004-9-30","rhessys","output","NLCD_newhope"
# );paste(arg,collapse=" ")


##--------------------------------------------------------------------------------------------## Eno
# arg=c(
	# "/Volumes/storage/WSC_storage/eno/enodurham3000",
	# "eno_19630901_20150904.csv",
	# "3", "1", "500", "2000-10-1","2004-9-30","rhessys","output","NLCD_eno"
# );paste(arg,collapse=" ")

# arg=c(
	# "/Volumes/storage/WSC_storage/eno/enodurham3000",
	# "eno_19630901_20150904.csv",
	# "3", "1", "500", "1998-10-1","2010-9-30","rhessys","output","NLCD_eno"
# );paste(arg,collapse=" ")

##--------------------------------------------------------------------------------------------## WS14
# arg=c(
	# "/Users/laurencelin/Desktop/WS14_dynamicNdep/setup_rhessys_5_18",
	# "hwc_19910101_19971231.csv",
	# "2", "1", "500", "1991-10-1","1997-9-30","rhessys","output","setup_rhessys_5_18NewScript"
# );paste(arg,collapse=" ")

# arg=c(
	# "/Volumes/Laurence/WS14_brain/Rhessys520_src_Laurence/RHESSys5.20.source-70-master-nighttime-evapFinal",
	# "ws14dailyflow_19710101_20120703mm.csv",
	# "4", "1", "1000", "2005-10-1","2011-9-30","rhessys","output_str4103_calibration","killdevilRuns_str4103"
# );paste(arg,collapse=" ")

# arg=c(
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc",
	# "ws14dailyflow_19710101_20120703mm.csv",
	# "2", "1", "1000", "2005-10-1","2011-9-30","rhessys","output","world_vegbolstad_rhd5_bgcNew2"
# );paste(arg,collapse=" ")

# arg=c(
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys518_src_Scaife/setup_rhessys_5_18_2015_workflows",
	# "ws14dailyflow_19710101_20120703mm.csv",
	# "1", "1", "1000", "2005-10-1","2011-9-30","rhessys","output","rhessys_5_18_2015b"
# );paste(arg,collapse=" ")

##--------------------------------------------------------------------------------------------## NY
# arg=c(
	# "~/Dropbox/Myself/UNC/NY_ShelterCreek/workflows10m",
	# "USGS01434092_19921001_20070930.csv",
	# "8", "1", "500", "1992-10-1","1995-9-30","rhessys_veg","output_soilz1","rhessy_veg_soil"
# );paste(arg,collapse=" ")

# # arg=c(
	# "~/Dropbox/Myself/UNC/NY_ShelterCreek/workflows10m",
	# "USGS01434092_19921001_20070930.csv",
	# "4", "1", "1500", "1992-10-1","1995-9-30","rhessys_veg","output_soilz1_lai","rhessy_veg_soil_lai"
# );paste(arg,collapse=" ")


arg=c(
	"/Volumes/storage/RHESSys_watershed/NY_ShelterCreek/workflows10m",
	"USGS01434092_19921001_20070930.csv",
	"4", "993", "993", "1992-10-1","1995-9-30","rhessys_veg","output_soilz1_lai","rhessy_veg_soil_lai993"
);paste(arg,collapse=" ")


##--------------------------------------------------------------------------------------------## Swift
# arg=c(
	# "/Users/laurencelin/Downloads/swift",
	# "swift2002_2016.csv",
	# "2", "1", "1000", "2000-10-1","2004-9-30","rhessys","output","swift"
# );paste(arg,collapse=" ")

##--------------------------------------------------------------------------------------------## WS18
# arg=c(
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18",
	# "Qobs_18_r.csv",
	# "1", "1", "500", "1991-10-1","1996-9-30","rhessys","output_OriginalCalibration","OriginalCalibration"
# );paste(arg,collapse=" ")


##--------------------------------------------------------------------------------------------## Ellerbe Creek Club Blvd
# arg=c(
	# "/Volumes/storage/WSC_storage/ellerbe/EllerbeCk_ClubBlvd",
	# "EllerbeCk_discharge_WY2008-2013.csv",
	# "2", "1", "5000", "2009-10-1","2012-9-30","rhessys","output","EllerbeCk_ClubBlvd"
# );paste(arg,collapse=" ")
#-st 2007 1 1 1 -ed 2014 1 1 1

##--------------------------------------------------------------------------------------------## Pond10m
# arg=c(
	# "/Volumes/band_group/Users/jmduncan/Pond10m",
	# "POBR_ObsQmm.csv",
	# "1", "1", "1000", "2004-10-1","2010-9-30","","output_revised_5.20_code","Pond10mCodefix"
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
calobs.date0 = convertDateExcelMDY(calobs[,1]) ##<<-----
#calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=20) ##<<-----


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


	
	
	
	