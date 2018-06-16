# GIS2RHESSys

This tool set is a beta version with limited automation. Two major functions for this tool set: 1) extracting spatial information from GIS to RHESSys worldfile (RHESSys input file) and 2) calculating subsurface and surface D-inf flow distribution. This tool set will continuously receive updates for fixing issues and providing more functionality/automation.  More importantly, the shell/R scripts here serve as templates for advanced/highly-customized RHESSys setup.

step 1:   downloaded and process elevation and LULC data;
          identify the Lat/Long of the catchment outlet;
          find the NAD83 UTM zone and the corresponding EPSG code for the catchment.
          
step 2:   edit automation_workflows.sh with the information above, and run the script in terminals at the location of the script.

automation_workflows.sh is designed to create the GRASS GIS database, delineate catchment, setup watershed model variables (e.g., impervious, lai, and vegetation type based on NLCD classification and default rules), and generate RHESSys model input files (worldfile and flowtable).

               
Future directions:

a) merge this tool set to UVA Jupyter Notebook, interact with RHESSys workflows (python)

b) add in SSURGO soil tool (R script: https://github.com/laurencelin/ssurgo_extraction)

c) provide linkage/guide to RHESSys calibration: https://github.com/laurencelin/R-coded-scripts-for-RHESSys-calibration

Other links:

RHESSys github: https://github.com/RHESSys/RHESSys

RHESSys wiki: https://github.com/RHESSys/RHESSys/wiki

RHESSys tools by Will from U.S. west coast https://github.com/RHESSys/RHESSys/tree/master/RHESSysPreprocessing
Will's RHESSys tools are also written in R with R-studio. Major difference between GIS2RHESSys and Will's is that GIS2RHESSys is designed to simplify the existing code and be friendly to command line/Jupyter notebook/server-client environment.
