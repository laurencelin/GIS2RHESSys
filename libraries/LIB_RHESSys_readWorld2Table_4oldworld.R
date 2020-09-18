#source("~/LIB_misc.r")
options(scipen=999)
arg=commandArgs(T)
inputfile = arg[1]
skip = as.numeric(arg[2])
output=arg[3]


con=file(inputfile)
open(con)

worldid=0; 
num_basin=0;
num_hill=0;
num_zone=0;
num_patch=0;

title=c(
	"worldID",
	"basinID","x","y","z","basinDefID","latitude","basinBase", #8
	
	"hillslope_ID","x","y","z","hillDefID","hillgws","hillgwno3","hillBase", #16
	
	"zoneID","x","y","z","zoneDefID","zoneArea","zoneSlope","zoneAspect","zoneIso","zoneEH","zoneWH", "zoneBase","zoneBaseID", #29
	
	"patchID","patchX","patchY","patchZ","patchsoilID","patchLandID","patchArea","patchSlope","patchLNA","patchKsat","patchMpar",
	"patchRZstorage","patchUnsat","patchSat",
	"patchSnowEZ","patchSnowZ","patchSnowT","patchSnowAge","patchSnowED", 
	"patchLittercfrac","patchLitterStorage","patchLitterc1","patchLittern1","patchLitterc2","patchLitterc3","patchLitterc4",
	"patchSoilc1","patchSoilsminn","patchSoilNO3","patchSoilc2","patchSoilc3","patchSoilc4","patchBase",
	
	"strateID","stratePlantID","stratecfrac","strateGap","strateRZ","strateSnowStored","strateRainStored",
	"stratecpool","strateleafc","stratedleafc","strateleafcstore","strateleafctrans","stratelstemc","stratelstemcstore","stratelstemctrans",
	"stratedstemc","stratedstemcstore","stratedstemctrans","stratelrootc","stratelrootcstore","stratelrootctrans",
	"stratedrootc","stratedrootcstore","stratedrootctrans","stratefrootc","stratefrootcstore","stratefrootctrans",
	"stratecwdc","strateEPVleafcalloc",
	"stratenpool","strateleafn","stratedleafn","strateleafnstore","strateleafntrans","stratelstemn","stratelstemnstore","stratelstemntrans",
	"stratedstemn","stratedstemnstore","stratedstemntrans","stratelrootn","stratelrootnstore","stratelrootntrans",
	"stratedrootn","stratedrootnstore","stratedrootntrans","stratefrootn","stratefrootnstore","stratefrootntrans",
	"stratecwdn","strateRetransn","epv_wstress_days","epv_max_fparabs","epv_min_vwc","strateBase"
)
write(title,file=output,sep=",",ncolumns=8+8+13+33+55, append=F)


#skip headers
if(skip>0){w=readLines(con,n=skip,warn=F)}

# https://stackoverflow.com/questions/25707647/merge-multiple-spaces-to-single-space-remove-trailing-leading-spaces
w = read.table(text=sapply(readLines(con,warn=F),function(x){ gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl=TRUE)}))
colnames(w) = c('value','key')

w[1:10,]

while()


# world
line=readLines(con,n=1,warn=F)
worldid = as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[1]))); ###<<---------------------- worldID

#basin
line=readLines(con,n=1,warn=F) 
num_basin = as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[1]))) ###<<----------
for(bi in 1:num_basin){
	
	line=readLines(con,n=8,warn=F)
	basinObj = c(
		worldid,
		num_basin,
		as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[1]))), #<<---- basin ID
		as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[2]))), #<<---- basin x
		as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[3]))), #<<---- basin y
		as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[4]))), #<<---- basin z
		as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[5]))), #<<---- basin def ID
		as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[6]))), #<<---- basin latitude
		as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[7]))), #<<---- basin #bnasestation
		as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[8])))  #<<---- # hill
	)
	#as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[1])))
	
	num_hill= basinObj[10];
	
	for(hi in 1:num_hill){
		
		line=readLines(con,n=9,warn=F)
		hillObj =c(
			as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[1]))), #<<---- hill ID
			as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[2]))), #<<---- hill x
			as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[3]))), #<<---- hill y
			as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[4]))), #<<---- hill z
			as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[5]))), #<<---- hill def ID
			as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[6]))), #<<---- hill gw.storage
			as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[7]))), #<<---- hill gw.no3 <<--- this 3
			as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[8]))), #<<---- hill #basestation [what is it? always zero!]
			as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[9]))) #<<---- # zone
		)
		num_zone= hillObj[9];
		
		for(zi in 1:num_zone){
			
			line=readLines(con,n=14,warn=F)
			zoneObj=c(
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[1]))), #<<---- zone ID
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[2]))), #<<---- zone x
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[3]))), #<<---- zone y
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[4]))), #<<---- zone z 
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[5]))), #<<---- zone def ID 5
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[6]))), #<<---- zone area
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[7]))), #<<---- zone slope
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[8]))), #<<---- zone aspect
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[9]))), #<<---- zone isohyet [how does it work with def?]
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[10]))), #<<---- zone e_horizon 10
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[11]))), #<<---- zone w_horizon
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[12]))), #<<---- zone #basestation
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[13]))), #<<---- zone p basestation ID
				as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[14]))) #<<---- # patch 14
			)
			num_patch= zoneObj[14];
			
			
			for(pi in 1:num_patch){
				
				line=readLines(con,n=34,warn=F)
				patchObj=c(
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[1]))), #<<---- patch ID
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[2]))), #<<---- patch x
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[3]))), #<<---- patch y
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[4]))), #<<---- patch z
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[5]))), #<<---- patch soil ID
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[6]))),  #<<---- patch land ID
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[7]))),  #<<---- patch area
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[8]))),  #<<---- patch slope
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[9]))),  #<<---- patch lna [what is it?]
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[10]))),  #<<---- patch ksat
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[11]))),  #<<---- patch m_par 
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[12]))),  #<<---- patch rz_storage
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[13]))),  #<<---- patch unsat_storage
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[14]))),  #<<---- patch sat_deficit
					
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[15]))),  #<<---- patch snowpack water Eq depth
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[16]))),  #<<---- patch snowpack water depth
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[17]))),  #<<---- patch snowpack T
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[18]))),  #<<---- patch snowpack surface age
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[19]))),  #<<---- patch snowpack energy deficit
					
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[20]))),  #<<---- patch litter cover fraction
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[21]))),  #<<---- patch litter rain stored
					
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[22]))), #<<---- patch litter_cs.litr1c
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[23]))), #<<---- patch litter_ns.litr1n
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[24]))), #<<---- patch litter_cs.litr2c
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[25]))), #<<---- patch litter_cs.litr3c
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[26]))), #<<---- patch litter_cs.litr4c
					
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[27]))), #<<---- patch soil_cs.soil1c
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[28]))), #<<---- patch soil_ns.sminn
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[29]))), #<<---- patch soil_ns.nitrate
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[30]))), #<<---- patch soil_cs.soil2c
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[31]))), #<<---- patch soil_cs.soil3c
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[32]))), #<<---- patch soil_cs.soil4c 
					
					
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[33]))), #<<---- patch #basestation
					as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[34]))) #<<---- # strate
				)
				
				#assume zone = patch = canopy_strata_ID
				num_strate = patchObj[34];
				for(si in 1:num_strate){
					line=readLines(con,n=55,warn=F)
					strateObj=c(
					
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[1]))), #<<---- # canopy ID
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[2]))), #<<---- # canopy plant ID
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[3]))), #<<---- # canopy cover fraction
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[4]))), #<<---- # canopy gap_fraction
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[5]))), #<<---- # canopy root_depth
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[6]))), #<<---- # canopy snow stored
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[7]))), #<<---- # canopy rain stored
						
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[8]))), #<<---- # canopy cs_cpool
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[9]))), #<<---- # canopy cs_leafc
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[10]))), #<<---- # canopy cs_dead_leafc
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[11]))), #<<---- # canopy cs_leafc_store
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[12]))), #<<---- # canopy cs_leafc_transfer
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[13]))), #<<---- # canopy cs_live_stemc
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[14]))), #<<---- # canopy cs_livestemc_store
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[15]))), #<<---- # canopy cs_livestemc_transfer
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[16]))), #<<---- # canopy cs_dead_stemc
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[17]))), #<<---- # canopy cs_deadstemc_store
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[18]))), #<<---- # canopy cs_deadstemc_transfer
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[19]))), #<<---- # canopy cs_live_crootc
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[20]))), #<<---- # canopy cs_livecrootc_store
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[21]))), #<<---- # canopy cs_livecrootc_transfer
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[22]))), #<<---- # canopy cs_dead_crootc
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[23]))), #<<---- # canopy cs_deadcrootc_store
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[24]))), #<<---- # canopy cs_deadcrootc_transfer			
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[25]))), #<<---- # canopy cs_frootc
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[26]))), #<<---- # canopy cs_frootc_store
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[27]))), #<<---- # canopy cs_frootc_transfer
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[28]))), #<<---- # canopy cs_cwdc
						
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[29]))), #<<---- # canopy epv.prev_leafcalloc [what is it?]
						
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[30]))), #<<---- # canopy ns_npool
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[31]))), #<<---- # canopy ns_leafn
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[32]))), #<<---- # canopy ns_dead_leafn
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[33]))), #<<---- # canopy ns_leafn_store
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[34]))), #<<---- # canopy ns_leafn_transfer
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[35]))), #<<---- # canopy ns_live_stemn
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[36]))), #<<---- # canopy ns_livestemn_store
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[37]))), #<<---- # canopy ns_livestemn_transfer
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[38]))), #<<---- # canopy ns_dead_stemn
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[39]))), #<<---- # canopy ns_deadstemn_store
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[40]))), #<<---- # canopy ns_deadstemn_transfer
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[41]))), #<<---- # canopy ns_live_crootn
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[42]))), #<<---- # canopy ns_livecrootn_store
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[43]))), #<<---- # canopy ns_livecrootn_transfer
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[44]))), #<<---- # canopy ns_dead_crootn
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[45]))), #<<---- # canopy ns_deadcrootn_store
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[46]))), #<<---- # canopy ns_deadcrootn_transfer			
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[47]))), #<<---- # canopy ns_frootn
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[48]))), #<<---- # canopy ns_frootn_store
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[49]))), #<<---- # canopy ns_frootn_transfer
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[50]))), #<<---- # canopy ns_cwdn
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[51]))), #<<---- # canopy ns_retransn [what is it?]
						
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[52]))), #<<---- # canopy epv_wstress_days [what is it?]
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[53]))), #<<---- # canopy epv_max_fparabs [what is it?]
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[54]))), #<<---- # canopy epv_max_fparabs [what is it?]
						as.numeric(gsub("[^0-9.-]","",gsub("[[:lower:]]\\..","",line[55]))) #<<---- # canopy # basestation
					)
					#tmp=c(basinObj,hillObj,zoneObj,patchObj,strateObj) 122
					#tmp=c(basinObj[c(1,3:9)],hillObj[1:8],zoneObj[1:13],patchObj[1:33],strateObj) 117
					
					write(
						c(basinObj[c(1,3:9)],hillObj[1:8],zoneObj[1:13],patchObj[1:33],strateObj),
						file=output,sep=",",ncolumns=117, append=T
					)
				}#si			
			}#pi
		}#zone
	}#hill
}#basin

close(con)
