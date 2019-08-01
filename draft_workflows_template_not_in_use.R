



template = list()
template$action = list()
template$elevation = list()
template$slope = list()
template$aspect = list()
template$twi = list()
template$horizonE = list()
template$horizonW = list()
template$isohyet = list()
template$riparian = list()
template$UnpavedRoad = list()
template$soil_texture = list()
template$lulc = list()
template$Patch_forestFrac = list()
template$Patch_shrubFrac = list()
template$Patch_lawnFrac = list()
template$catchmentStructure = list()
template$hillslopeIni = list()
template$zoneIni = list()
template$patchIni = list()

##############################################################################
##############################################################################
# edits below


############# Actions #############
template$action$buildGIS = F # just import files to GIS and 
template$action$G2W = T # extract GIS information to build worldfile
template$action$CF_subsurfaceRouting = F
template$action$CF_surfaceRouting = F

############# project #############
template$project$path = ''
template$project$buildStructure = T

############# Data inputs #############
## option 0: when import raw data, resample raw data, reproject raw data, and semi-autometically generate topographic information and spatial hierarchy structure
## option 1: when import processed data from files into GIS
## option 2: when working with a (completedly) processed GIS dataset, assume masking and regioning are all set and correct.

template$elevation$selectedOption = 2 # [0 or 1 or 2]
	template$elevation$option0_RAWelevationFile = '../dem.tif'
	template$elevation$option0_elevationOutputName = 'dem'
	template$elevation$option0_res = 10 # spatial resolution in maters
	template$elevation$option0_proj = '' # 
	template$elevation$option0_elevationOutputName = 'dem' 
	template$elevation$option1_fromFile = 'processed_dem.tif'
	template$elevation$option1_toGIS = 'dem'
	template$elevation$option2_fromGIS = 'procssed_dem'

template$slope$selectedOption = 0 # [0 or 1 or 2]; 0=auto
	template$slope$option0_slopeOutputName = 'slope'
	template$slope$option1_fromFile = 'processed_slope.tif'
	template$slope$option1_toGIS = 'slope'
	template$slope$option2_fromGIS = 'processed_slope'

template$aspect$selectedOption = 0 # [0 or 1 or 2]; 0=auto
	template$aspect$option0_aspectOutputName = 'aspect'
	template$aspect$option1_fromFile = 'processed_aspect.tif'
	template$aspect$option1_toGIS = 'aspect'
	template$aspect$option2_fromGIS = 'processed_aspect'

template$twi$selectedOption = 0 # [0 or 1 or 2]; 0=auto
	template$twi$option0_twiOutputName = 'twi'
	template$twi$option1_fromFile = 'processed_twi.tif'
	template$twi$option1_toGIS = 'twi'
	template$twi$option2_fromGIS = 'processed_twi'

template$horizonE$selectedOption = 0 # [0 or 1 or 2]; 0=auto
	template$horizonE$option0_horizonEOutputName = 'horizonE'
	template$horizonE$option1_fromFile = 'processed_horizonE.tif'
	template$horizonE$option1_toGIS = 'horizonE'
	template$horizonE$option2_fromGIS = 'processed_horizonE'

template$horizonW$selectedOption = 0 # [0 or 1 or 2]; 0=auto
	template$horizonW$option0_horizonWOutputName = 'horizonW'
	template$horizonW$option1_fromFile = 'processed_horizonW.tif'
	template$horizonW$option1_toGIS = 'horizonW'
	template$horizonW$option2_fromGIS = 'procssed_horizonW'

template$isohyet$selectedOption = 0 # [0 or 1 or 2]; 0 = none => isohyet is one everywhere
	template$isohyet$option0_isohyetOutputName = 'isohyet'
	template$isohyet$option1_fromFile = 'processed_isohyet.tif'
	template$isohyet$option1_toGIS = 'riparian'
	template$isohyet$option2_fromGIS = 'processed_isohyet'

template$riparian$selectedOption = 0 # [0 or 1 or 2]; 0 = none 
	template$riparian$option0_riparianOutputName = 'riparian'
	template$riparian$option1_fromFile = 'processed_riparian.tif'
	template$riparian$option1_toGIS = 'riparian'
	template$riparian$option2_fromGIS = 'processed_riparian'

template$UnpavedRoad$selectedOption = 0 # [0 or 1 or 2]; 0 = none
	template$UnpavedRoad$option0_UnpavedRoadOutputName = 'unpavedRoads'
	template$UnpavedRoad$option1_fromFile = 'processed_unpavedRoads.tif'
	template$UnpavedRoad$option1_toGIS = 'unpavedRoads'
	template$UnpavedRoad$option2_fromGIS = 'processed_unpavedRoads'

############# Soil data inputs #############
template$soil_texture$selectedOption = 0 # [0 or 1 or 2]; 0 = import from ssurgo tubulars and mukey.shp 
	template$soil_texture$option0_ssurgoImport = "../MD005"
	template$soil_texture$option0_soilOutputName = 'soil_texture'
	template$soil_texture$option1_fromFile = 'processed_soil_texture.tif'
	template$soil_texture$option1_toGIS = 'soil_texture'
	template$soil_texture$option2_fromGIS = 'processed_soil_texture'

############# LULC, imperviousness, and strata inputs #############
template$lulc$selectedOption = 0 #[0 or 1 or 2]; 0 = reclass RAW lulc code to RHESSys classes
	template$lulc$option0_RAWlulcFile = '../raw_data/lulc.tif' 	
	template$lulc$option0_lulcOutputName = 'lulc'
	template$lulc$option0_RAWlulcCodeinfomation = '../lulcCodeInfo.csv' # table needs user to provide
		# To create this Codeinformation, please first run ...
		# for example,
		# 
	template$lulc$option1_fromFile = 'processed_lulc.tif'
	template$lulc$option1_toGIS = 'lulc'
	template$lulc$option2_fromGIS = 'processed_lulc'

template$Patch_forestFrac$maxNumOfStrata = 3
	template$Patch_forestFrac$strataType = c(1,2,102) #these are IDs in https://github.com/laurencelin/GIS2RHESSys/blob/master/vegCollection.csv
	template$Patch_forestFrac$strataLAI = c('map:lai1','value:1.5','file:/usr/xx/Downloads/lai3.tif')
	template$Patch_forestFrac$strataMaxRoot = c('map:rtz1','value:0.5','file:/usr/xx/Downloads/rtz3.tif') # meters

template$Patch_shrubFrac$maxNumOfStrata = 0
	template$Patch_shrubFrac$strataType = c(NA) #these are IDs in https://github.com/laurencelin/GIS2RHESSys/blob/master/vegCollection.csv
	template$Patch_shrubFrac$strataLAI = c(NA)
	template$Patch_shrubFrac$strataMaxRoot = c(NA)

template$Patch_lawnFrac$maxNumOfStrata = 1
	template$Patch_lawnFrac$strataType = c(3) #these are IDs in https://github.com/laurencelin/GIS2RHESSys/blob/master/vegCollection.csv
	template$Patch_lawnFrac$strataLAI = c('value:1.5')
	template$Patch_lawnFrac$strataMaxRoot = c('value:0.2')


############# model spatial hierarchy structure settings #############
template$catchmentStructure$selectedOption = 0 # [0 or 1]; 
	# 0=auto; 1=preiously defined from file; 2= preiously defined from GIS
	template$catchmentStructure$option0_outlet = 'latlong:12.3,78.4' # 'latlong:12.3,78.4' or 'file:../point.shp'
	template$catchmentStructure$option0_threshold = 1200 # (int; num of grid)
	template$catchmentStructure$option0_drainageArea = 124.34 #(m2)
	template$catchmentStructure$option0_BasinOutputName = 'basin'
	template$catchmentStructure$option0_HillslopeOutputName = 'hill'
	template$catchmentStructure$option0_ZoneOutputName = 'zone'
	template$catchmentStructure$option0_PatchOutputName = 'patch'
	template$catchmentStructure$option1_Basin_fromFile = 'basin.tif'
	template$catchmentStructure$option1_Hillslope_fromFile = 'hill.tif'
	template$catchmentStructure$option1_Zone_fromFile = 'zone.tif'
	template$catchmentStructure$option1_Patch_fromFile = 'patch.tif'
	template$catchmentStructure$option2_Basin_fromGIS = 'basin.tif'
	template$catchmentStructure$option2_Hillslope_fromGIS = 'hill.tif'
	template$catchmentStructure$option2_Zone_fromGIS = 'zone.tif'
	template$catchmentStructure$option2_Patch_fromGIS = 'patch.tif'

	# enter a value or a GIS map (not a file) below
	template$hillslopeIni$gwstorage = 0
	template$hillslopeIni$gwno3storage = 0
	
	template$patchIni$litter_coverfraction = 1
	template$patchIni$litter_litr1c = 0.001
	template$patchIni$litter_litr1n = 2.857143e-05
	template$patchIni$litter_litr2c = 0.001
	template$patchIni$litter_litr3c = 0.001
	template$patchIni$litter_litr4c = 0.001
	
	template$zoneIni$stationID = 'value:101'
	
	template$patchIni$soil_sminn = 2.1
	template$patchIni$soil_nitrate = 0.1
	template$patchIni$soil_c1 = 0.1
	template$patchIni$soil_c2 = 0.1
	template$patchIni$soil_c3 = 2
	template$patchIni$soil_c4 = 8 

##############################################################################
##############################################################################
# no modifications below
##############################################################################








