# GIS2RHESSys

- Master branch serves a collection of scripts and datasets that help constructing RHESSys eco-hydrological model 
- Datasets include soil, vegeation, and LULC parameter values
- Most tools here are coded in R with pacakage "rgrass7" to access GRASS GIS data
For start, please browse into the following branches
- Jupyter notebook interfaces are newly added in the "Jupyter notebook" branch
- standalone (working ...)

![Alt text](rhessys_filesystem.png?raw=true "Title")


The workflows.sh is a bash/shell script to 1) set the system library paths; 2) make grass datasets and GIS data post-processing; 3) delineate catchment; and 4) extract GIS information to build RHESSys models. Certain procedures are organized in groups in workflows.sh. User should customize the workflows.sh for his/her tasks. Advanced users should also look into the "g2w" and "createFlowRouting" for more controls.  

Below diagram shows the relationship between GRASS dataset and RHESSys model.

![Alt text](GIS2RHESSys.png?raw=true "Title")

Other links:

RHESSys github: https://github.com/RHESSys/RHESSys

RHESSys wiki: https://github.com/RHESSys/RHESSys/wiki

RHESSys tools by Will from U.S. west coast https://github.com/RHESSys/RHESSys/tree/master/RHESSysPreprocessing
Will's RHESSys tools are also written in R with R-studio. Major difference between GIS2RHESSys and Will's is that GIS2RHESSys is designed to simplify the existing code and be friendly to command line/Jupyter notebook/server-client environment.
