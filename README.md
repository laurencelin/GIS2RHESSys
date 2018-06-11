# GIS2RHESSys

Two major functions: 1) extracting spatial information from GIS to RHESSys worldfile (RHESSys input file) and 2) calculating subsurface and surface D-inf flow distribution.

step 1:   setup/create GRASS 7.x database with downloadeded and processed elevation data. 
          elevation has to be projected to UTM and depression filled. 
          
step 2:   grass_cmd.sh to delinearate catchment with user provided gage location

step 3:   user needs to provide the following information:
          a)soil type (USDA classes); often based on SSURGO dataset
          b)LAI (Leaf Area Index)
          c)RHESSys vegetation types/IDs (please refers to rhessys_veg_default.csv) 
          d)REHSSys LU settings (e.g., fertilizing rate and surface water detention storage size; please refers to "defs_workflows"
          
step 4:   g2w_default.R (already listed in the end of the grass_cmd.sh) to extracting spatial information from GIS to RHESSys worldfile ;

step 5:   cf_default.R (already listed in the end of the grass_cmd.sh) to calculate drinage directions and weights

               



