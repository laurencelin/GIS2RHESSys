# GIS2RHESSys Readme

Introduction

There are two branches under this repository. Master branch (default) holds a set of shell / R scripts for processing GIS data and assemble RHESSys model inputs. Tools are designed to run locally on Mac / Linux.  The usage of the scripts are organized by modules documented in workflow guide (links are below).  The second branch is "JupyterNotebook" developed and maintained by Lukey (Choi).  Lukey's goal is to create a user-friendly workflow interface on JupyterNotebook that utilizes the same scripts held on master branch. JupyterNotebooks have been developed for UVA Rivanna and HydroShare Jupyter App.

Requirements / Dependencies:
* GRASS GIS 7.x.x installed and its associated dependencies and libraries (please see offical GRASS GIS website)
* R 3.6.x and above installed
* Bash scripting environment installed or supported by the OS

Recommended backgrounds for using GIS2RHESSys to setup RHESSysEastCoast model:
* Any experiences in GIS, including finding data sources, pre- and post- processing data
* Experiences in command lines, being comfortable to use commands to operate computer
* Backgrounds in Hydrology and Ecology; After all, RHESSys is an eco-hydrological model used for hydrology and ecosystem studies.

Workflow guide for running commandline tools on local Mac / Linux (one of the links below):
https://nbviewer.jupyter.org/github/laurencelin/GIS2RHESSys/blob/master/GIS2RHESSys_workflow_script_master.ipynb 
https://colab.research.google.com/github/laurencelin/GIS2RHESSys/blob/master/GIS2RHESSys_workflow_script_master.ipynb


Directory structures for RHESSys modeling:
When setup a RHESSys model for a catchment, we are expecting certain directory structure (Figure below).
* Project directory holds all raw data, GIS dataset, RHESSys model inputs, and observations.
* Raw data directory (named "raw_data" ) holds any data that are used for model setup, including GIS data, vegetation physiological dataset, and soil data.  
* GRASS database (named "grass_dataset") holds all the GRASS GIS databases that are developed by the commandline workflow; One GRASS database for one RHESSys model; 
* RHESSys model directory (could be named as anything) holds all input files for RHESSys; Sources / binary codes of RHESSys are downloaded and compiled seperatedly. 
* Observation directory (named "obs" ) usually holds streamflow time series or other data for later model calibrations.

![Alt text](rhessys_filesystem.png?raw=true "Title")


Minimum data requirements:
* Elevation 
* Landuse & landcover
* Soil texture
* Climate time series (daily) 
Below diagram shows the relationship between GRASS dataset and RHESSys model.
![Alt text](GIS2RHESSys.png?raw=true "Title")



Other links:

RHESSysEastCoast https://github.com/laurencelin/RHESSysEastCoast

General RHESSys github: https://github.com/RHESSys/RHESSys

General RHESSys wiki: https://github.com/RHESSys/RHESSys/wiki

General RHESSys tools by Will from U.S. west coast https://github.com/RHESSys/RHESSys/tree/master/RHESSysPreprocessing
Will's RHESSys tools are written R-studio and designed for general RHESSys model.
