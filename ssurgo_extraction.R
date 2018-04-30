mapunitTitle=c('musym','muname','mukind','mustatus','muacres','mapunitlfw_l','mapunitlfw_r','mapunitlfw_h','mapunitpfa_l','mapunitpfa_r','mapunitpfa_h','farmlndcl','muhelcl','muwathelcl','muwndhelcl','interpfocus','invesintens','iacornsr','nhiforsoigrp','nhspiagr','vtsepticsyscl','mucertstat','lkey','mukey')
compTitle=c('comppct_l','comppct_r','comppct_h','compname','compkind','majcompflag','otherph','localphase','slope_l','slope_r','slope_h','slopelenusle_l','slopelenusle_r','slopelenusle_h','runoff','tfact','wei','weg','erocl','earthcovkind1','earthcovkind2','hydricon','hydricrating','drainagecl','elev_l','elev_r','elev_h','aspectccwise','aspectrep','aspectcwise','geomdesc','albedodry_l','albedodry_r','albedodry_h','airtempa_l','airtempa_r','airtempa_h','map_l','map_r','map_h','reannualprecip_l','reannualprecip_r','reannualprecip_h','ffd_l','ffd_r','ffd_h','nirrcapcl','nirrcapscl','nirrcapunit','irrcapcl','irrcapscl','irrcapunit','cropprodindex','constreeshrubgrp','wndbrksuitgrp','rsprod_l','rsprod_r','rsprod_h','foragesuitgrpid','wlgrain','wlgrass','wlherbaceous','wlshrub','wlconiferous','wlhardwood','wlwetplant','wlshallowwat','wlrangeland','wlopenland','wlwoodland','wlwetland','soilslippot','frostact','initsub_l','initsub_r','initsub_h','totalsub_l','totalsub_r','totalsub_h','hydgrp','corcon','corsteel','taxclname','taxorder','taxsuborder','taxgrtgroup','taxsubgrp','taxpartsize','taxpartsizemod','taxceactcl','taxreaction','taxtempcl','taxmoistscl','taxtempregime','soiltaxedition','castorieindex','flecolcomnum','flhe','flphe','flsoilleachpot','flsoirunoffpot','fltemik2use','fltriumph2use','indraingrp','innitrateleachi','misoimgmtgrp','vasoimgtgrp','mukey','cokey')
chorizonTitle = c('hzname','desgndisc','desgnmaster','desgnmasterprime','desgnvert','hzdept_l','hzdept_r','hzdept_h','hzdepb_l','hzdepb_r','hzdepb_h','hzthk_l','hzthk_r','hzthk_h','fraggt10_l','fraggt10_r','fraggt10_h','frag3to10_l','frag3to10_r','frag3to10_h','sieveno4_l','sieveno4_r','sieveno4_h','sieveno10_l','sieveno10_r','sieveno10_h','sieveno40_l','sieveno40_r','sieveno40_h','sieveno200_l','sieveno200_r','sieveno200_h','sandtotal_l','sandtotal_r','sandtotal_h','sandvc_l','sandvc_r','sandvc_h','sandco_l','sandco_r','sandco_h','sandmed_l','sandmed_r','sandmed_h','sandfine_l','sandfine_r','sandfine_h','sandvf_l','sandvf_r','sandvf_h','silttotal_l','silttotal_r','silttotal_h','siltco_l','siltco_r','siltco_h','siltfine_l','siltfine_r','siltfine_h','claytotal_l','claytotal_r','claytotal_h','claysizedcarb_l','claysizedcarb_r','claysizedcarb_h','om_l','om_r','om_h','dbtenthbar_l','dbtenthbar_r','dbtenthbar_h','dbthirdbar_l','dbthirdbar_r','dbthirdbar_h','dbfifteenbar_l','dbfifteenbar_r','dbfifteenbar_h','dbovendry_l','dbovendry_r','dbovendry_h','partdensity','ksat_l','ksat_r','ksat_h','awc_l','awc_r','awc_h','wtenthbar_l','wtenthbar_r','wtenthbar_h','wthirdbar_l','wthirdbar_r','wthirdbar_h','wfifteenbar_l','wfifteenbar_r','wfifteenbar_h','wsatiated_l','wsatiated_r','wsatiated_h','lep_l','lep_r','lep_h','ll_l','ll_r','ll_h','pi_l','pi_r','pi_h','aashind_l','aashind_r','aashind_h','kwfact','kffact','caco3_l','caco3_r','caco3_h','gypsum_l','gypsum_r','gypsum_h','sar_l','sar_r','sar_h','ec_l','ec_r','ec_h','cec7_l','cec7_r','cec7_h','ecec_l','ecec_r','ecec_h','sumbases_l','sumbases_r','sumbases_h','ph1to1h2o_l','ph1to1h2o_r','ph1to1h2o_h','ph01mcacl2_l','ph01mcacl2_r','ph01mcacl2_h','freeiron_l','freeiron_r','freeiron_h','feoxalate_l','feoxalate_r','feoxalate_h','extracid_l','extracid_r','extracid_h','extral_l','extral_r','extral_h','aloxalate_l','aloxalate_r','aloxalate_h','pbray1_l','pbray1_r','pbray1_h','poxalate_l','poxalate_r','poxalate_h','ph2osoluble_l','ph2osoluble_r','ph2osoluble_h','ptotal_l','ptotal_r','ptotal_h','excavdifcl','excavdifms','cokey','chkey')
chtexgrpTitle=c('texture','stratextsflag','rvindicator','texdesc','chkey','chtgkey')
chtexturTitle=c('texcl','lieutex','chtgkey','chtkey')
extractVar = c('permeability','H2Ocapacity','bulkdensity','SHC','erodibility','FC','porosity','thickness','OM')

target = 'VA113/tabular'

mapunit = read.table(paste(target,'/mapunit.txt',sep=''),sep='|') #mukey
colnames(mapunit) = mapunitTitle
	## 1 mukey to many componments [cokey]
comp = read.table(paste(target,'/comp.txt',sep=''),sep='|') #cokey
colnames(comp) = compTitle
	## 1 cokey to many horizons [chkey]
chorizon = read.table(paste(target,'/chorizon.txt',sep=''),sep='|') #chkey
colnames(chorizon) = chorizonTitle

chtexgrp = read.table(paste(target,'/chtexgrp.txt',sep=''),sep='|',stringsAsFactors=F) #chkey
colnames(chtexgrp) = chtexgrpTitle

chtextur = read.table(paste(target,'/chtextur.txt',sep=''),sep='|',stringsAsFactors=F) #chkey
colnames(chtextur) = chtexturTitle

## mapunit:component:horizon
## extract (weighted average[thickness] of soil horizon in a soil component; then weighted [%composition] by componments):
	# 1 permeability
	# 2 watercapacity
	# 3 bulkdensity
	# 4 saturatedhydraulicconductivity
	# 5 erodibility
	# 6 field capacity
	# 7 porosity
	# 8 soilthickness
	# 9 organ matter 
	
 	
	
soilscoreNames = c('Clay','Silty clay','Silty clay loam','Sandy clay','Sandy clay loam','Clay loam','Silt','Silt loam','Loam','Sand','Loamy sand','Sandy loam')		
soilscore = c(1,2,3,4,5,6,7,8,9,10,11,12); names(soilscore)=soilscoreNames
	
mukey = mapunit[,'mukey']	
mukeyHold = matrix(NA,length(mukey), 3+1+7); 
colnames(mukeyHold) = c('mukey','texture','soilname','depth','sand','slit','clay','ksat','porosity','fc','awc')
for(i in 1:length(mukey)){
	
	cond1 = comp[,'mukey']== mukey[i]
	icokey = comp[cond1,'cokey']
	icokeyPercent = comp[cond1,'comppct_r'] ## may need adjust to make it sum to one
	icokeyWeight = icokeyPercent/sum(icokeyPercent)
	
	icokeyHold = matrix(NA,length(icokey),1+7)
	colnames(icokeyHold)=c('jchkeyThinkness','sandtotal_r','silttotal_r','claytotal_r','ksat_r','wsatiated_r','wthirdbar_r','awc_r')
	for(j in 1:length(icokey)){
		cond2 = chorizon[,'cokey']==icokey[j]
			# chorizon[cond2,]
		jchkey = chorizon[cond2,'chkey']
		jchkeyThinkness =  chorizon[cond2,'hzdepb_r']-chorizon[cond2,'hzdept_r'] 
		jchkeyVarMatrix = as.matrix(chorizon[cond2,c('sandtotal_r','silttotal_r','claytotal_r','ksat_r','wsatiated_r','wthirdbar_r','awc_r')])#7
		for(jj in 1:ncol(jchkeyVarMatrix)){
			layerSelect = !is.na(jchkeyThinkness) & !is.na(jchkeyVarMatrix[,jj])
			jchkeyWeight = jchkeyThinkness[layerSelect]/sum(jchkeyThinkness[layerSelect])
			
			icokeyHold[j,jj+1] = sum(jchkeyWeight * jchkeyVarMatrix[layerSelect,jj])
		}#jj horizons
		icokeyHold[j,1] = sum(jchkeyThinkness,na.rm=T)
			
	}#j componments
	
	cond25 = chorizon[,'cokey']%in%icokey
	cond3 = chtexgrp[,'chkey'] %in% chorizon[cond25,'chkey']
	cond4 = chtextur[,'chtgkey'] %in% chtexgrp[cond3,'chtgkey']
	#cbind(chtextur[cond4,1],match(chtextur[cond4,1], soilscoreNames))
	tmp = table(soilscore[match(chtextur[cond4,1], soilscoreNames)])
	soilID_ = ifelse(length(tmp)>0, as.numeric(names(which.max(tmp))), NA)
	soilID_name = ifelse(length(tmp)>0, soilscoreNames[soilID_], NA) 
	mukeyHold[i,] = c(mukey[i], soilID_, soilID_name, icokeyWeight %*% icokeyHold)
	
	# https://water.usgs.gov/GIS/metadata/usgswrd/XML/ds866_ssurgo_variables.xml
	# https://github.com/selimnairb/EcohydroLib/blob/bde84bb7c5839781038484ffc2159755b60ffb1e/ecohydrolib/ssurgo/attributequery.py#L95
	# 'depth' = 'jchkeyThinkness'
	# 'sand' = 'sandtotal_r'
	# 'slit' = 'silttotal_r'
	# 'clay' = 'claytotal_r'
	# 'ksat' = 'ksat_r'
	# 'porosity' = 'wsatiated_r'
	# 'fc' = 'wthirdbar_r'
	# 'awc' = 'awc_r'
	
}#i

write.csv(mukeyHold,'VA113_ssurgo_extract.csv',row.names=F)









