# GIS2RHESSys

This tool set is a beta version with limited automation. Two major functions for this tool set: 1) extracting spatial information from GIS to RHESSys worldfile (RHESSys input file) and 2) calculating subsurface and surface D-inf flow distribution. This tool set will continuously receive updates for fixing issues and providing more functionality/automation.  More importantly, the shell/R scripts here serve as templates for advanced/highly-customized RHESSys setup.

step 1:   setup/create GRASS 7.x database with downloadeded and processed elevation data. 
          elevation has to be projected to UTM and depression filled. 
          
step 2:   grass_cmd.sh to delinearate catchment with user provided gage location

step 3:   user needs to provide the following information:
          a)soil type (USDA classes); often based on SSURGO dataset
          b)LAI (Leaf Area Index)
          c)RHESSys vegetation types/IDs (please refers to rhessys_veg_default.csv) 
          d)REHSSys LU settings (e.g., fertilizing rate and surface water detention storage size; please refers to "defs_workflows"
          
step 4:   g2w_default.R (already listed in the end of the grass_cmd.sh) to extracting spatial information from GIS to RHESSys worldfile ;

step 5:   cf_default.R (already listed in the end of the grass_cmd.sh) to calculate subsurface drinage directions and weights (with limited surface flow routing). urban surface flow routing R script will be released as a seperate tool in the future.

               
Future directions:

a) extend the functionality grass_cmd.sh to help users to setup and project elevation and LULC once user provide these information in RAW form.

b) merge this tool set to UVA Jupyter Notebook, interact with RHESSys workflows (python)

c) add in SSURGO soil tool (R script: https://github.com/laurencelin/ssurgo_extraction)


Other links:

RHESSys github: https://github.com/RHESSys/RHESSys

RHESSys wiki: https://github.com/RHESSys/RHESSys/wiki

RHESSys tools by Will from U.S. west coast https://github.com/RHESSys/RHESSys/tree/master/RHESSysPreprocessing
