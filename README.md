# GIS2RHESSys Readme

## Introduction

There are two branches under this repository. Master branch (default) holds a set of shell / R scripts for processing GIS data and assemble RHESSys model inputs. Tools are designed to run locally on Mac / Linux.  The usage of the scripts are organized by modules documented in workflow guide (links are below).  The second branch is "JupyterNotebook" developed and maintained by Lukey (Choi).  Lukey's goal is to create a user-friendly workflow interface on JupyterNotebook that utilizes the same scripts held on master branch. JupyterNotebooks have been developed for UVA Rivanna and HydroShare Jupyter App.

## Requirements / Dependencies:
* GRASS GIS 7.x.x installed and its associated dependencies and libraries (please see offical GRASS GIS website)
* R 3.6.x and above installed
* Bash scripting environment installed or supported by the OS

## Recommended backgrounds for using GIS2RHESSys to setup RHESSysEastCoast model:
* Any experiences in GIS, including finding data sources, pre- and post- processing data
* Experiences in command lines, being comfortable to use commands to operate computer
* Backgrounds in Hydrology and Ecology; After all, RHESSys is an eco-hydrological model used for hydrology and ecosystem studies.

## Usage of the tools here
This is a two-step process from scratch:
1) Setup a workflow script (see below) to construct GIS database for eco-hydrological modeling, processing the raw gis data into the database, and calculating the necessary information for the model. 
2) Run the script in command line, e.g., bash <script file> if you have installed and configured the require software and library on your computer. 

Correcting / updating / modifying the pre-existing workflow script (many options here): 
* option 1: you can open and edit the pre-existing workflow script; Take this approach, you are going to rebuild the evreything from scratch again
* option 2: you can make a second workflow script with the same project directory and gis settings (e.g., resolution and projection) and put in the new calculations; Take this approach, you are building new information onto the existing GIS database (not overwriting). By updating or making new "template" (see guide below), you can build a new model. 


## Workflow script guides (one of the links below):
* https://nbviewer.jupyter.org/github/laurencelin/GIS2RHESSys/blob/master/GIS2RHESSys_workflow_script_master.ipynb 
* https://colab.research.google.com/github/laurencelin/GIS2RHESSys/blob/master/GIS2RHESSys_workflow_script_master.ipynb

## Directory structures for RHESSys modeling:
When setup a RHESSys model for a catchment, we are expecting certain directory structure (Figure below).
* Project directory holds all raw data, GIS dataset, RHESSys model inputs, and observations.
* Raw data directory (named "raw_data" ) holds any data that are used for model setup, including GIS data, vegetation physiological dataset, and soil data.  
* GRASS database (named "grass_dataset") holds all the GRASS GIS databases that are developed by the commandline workflow; One GRASS database for one RHESSys model; 
* RHESSys model directory (could be named as anything) holds all input files for RHESSys; Sources / binary codes of RHESSys are downloaded and compiled seperatedly. 
* Observation directory (named "obs" ) usually holds streamflow time series or other data for later model calibrations.

![Alt text](rhessys_filesystem.png?raw=true "Title")

## Minimum data requirements:
* Elevation 
* Landuse & landcover
* Soil texture
* Climate time series (daily) 
Below diagram shows the relationship between GRASS dataset and RHESSys model.
![Alt text](GIS2RHESSys.png?raw=true "Title")

## Other links:
* RHESSysEastCoast https://github.com/laurencelin/RHESSysEastCoast
* RHESSys model calibration https://github.com/laurencelin/R-coded-scripts-for-RHESSys-calibration
* General RHESSys github: https://github.com/RHESSys/RHESSys
* General RHESSys wiki: https://github.com/RHESSys/RHESSys/wiki General RHESSys tools by Will from U.S. west coast https://github.com/RHESSys/RHESSys/tree/master/RHESSysPreprocessing
Will's RHESSys tools are written R-studio and designed for general RHESSys model.
