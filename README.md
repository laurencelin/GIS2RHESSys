# GIS2RHESSys

This tool set is a beta version with limited automation. Two major functions for this tool set: 1) extracting spatial information from GIS to RHESSys worldfile (RHESSys input file) and 2) calculating subsurface and surface D-inf flow distribution. This tool set will continuously receive updates for fixing issues and providing more functionality/automation.  More importantly, the shell/R scripts here serve as templates for advanced/highly-customized RHESSys setup.

step 0:   PROJDIR (LINE 19 @ automation_workflows.sh) is the pathway to the project directory (e.g., ~/Desktop/project). Note that in the current beta version we DO NOT put the GIS2RHESSys scripts inside the PROJDIR. Perhaps, we will make some changes here to better organize all the scripts inside the PROJDIR.

step 1:   downloaded and process elevation and LULC data (see LINE 31-32 @ automation_workflows.sh);
          identify the Lat/Long of the catchment outlet (see LINE 36-37 @ automation_workflows.sh);
          find the NAD83 UTM zone and the corresponding EPSG code for the catchment (see LINE 38 @ automation_workflows.sh);
          define "threshold value" for stream channel (in terms of number of grids, not area; see LINE 39 @ automation_workflows.sh)
          download https://github.com/laurencelin/ssurgo_extraction and put the scripts inside the downloaded GIS2RHESSys folder (have to do this manually for now because ssurgo_extraction is considered as a seperated tool).
          
step 2:   edit automation_workflows.sh with the information above, and run the script in terminals at the location of the script.

automation_workflows.sh is designed to create the GRASS GIS database, delineate catchment, setup watershed model variables (e.g., impervious, lai, and vegetation type based on NLCD classification and default rules), and generate RHESSys model input files (worldfile and flowtable). Note that, user may experience "error"/unexpected result when catchment outlet Lat/Long is not well align with the flow accumution. 

grass_setup.sh is a script containing all grass commands to delineate catchment, and it is called by the automation_workflows.sh.  Regular users have no need to modify it.

g2w_default.R and cf_default.R are R scripts to replace the C-coded old programs used for GRASS 6.4 to generate RHESSys input files. Regular users have no need to modify them. However, they are excellent templates for advanced users to highly customize the RHESSys setup. Note that this calculation will take some time, specially when catchment is large. 
               
Future directions:

a) merge this tool set to UVA Jupyter Notebook, interact with RHESSys workflows (python)

b) add in SSURGO soil tool (R script: https://github.com/laurencelin/ssurgo_extraction) [Updated: tool is added in the automation_workflow.sh; perhaps need some test runs.]

c) provide linkage/guide to RHESSys calibration: https://github.com/laurencelin/R-coded-scripts-for-RHESSys-calibration

Other links:

RHESSys github: https://github.com/RHESSys/RHESSys

RHESSys wiki: https://github.com/RHESSys/RHESSys/wiki

RHESSys tools by Will from U.S. west coast https://github.com/RHESSys/RHESSys/tree/master/RHESSysPreprocessing
Will's RHESSys tools are also written in R with R-studio. Major difference between GIS2RHESSys and Will's is that GIS2RHESSys is designed to simplify the existing code and be friendly to command line/Jupyter notebook/server-client environment.
