# GIS2RHESSys

master branch serves a general purposed collection of scripts help constructing RHESSys eco-hydrological model

![Alt text](rhessys_filesystem.png?raw=true "Title")


There are two branches of "GIS2RHESSys" handling different GIS data structures. The master branch handles the most straightforward GIS data (see below), while 1mUrban branch deals with high resolution GIS data (e.g., 1 m) by auto-aggregating data to 3-m or 10-m resolution and build RHESSys patch object by the multiple LULC fractions. 

![Alt text](branches_difference.png?raw=true "Title")

There may be more than one 'workflows.sh' in the "GIS2RHESSys". The workflows.sh is a bash/shell script to 1) set the system library paths; 2) make grass datasets and GIS data post-processing; 3) delineate catchment; and 4) extract GIS information to build RHESSys models. Certain procedures are organized in groups in workflows.sh (it's similar to Jupyter Notebook). The workflows.sh for building RHESSys from RAW GIS data (i.e., from scratch) has the most procedures while workflows.sh for rebuild RHESSys from existing GRASS GIS dataset could have a fewer procedures. User should customize the workflows.sh for his/her tasks. Advanced users should also look into the "g2w" and "cf" for more controls.  For example, one modifies the existing LULC maps in the GRASS GIS dataset and saves the edited information as a new map "newLULC". Then one should edit the "g2w" and "cf" scripts to look up the "newLULC" for LULC information. The "template" file is fully imbedded into the "g2w" and "cf" scripts. 

![Alt text](procedure.png?raw=true "Title")

Other links:

RHESSys github: https://github.com/RHESSys/RHESSys

RHESSys wiki: https://github.com/RHESSys/RHESSys/wiki

RHESSys tools by Will from U.S. west coast https://github.com/RHESSys/RHESSys/tree/master/RHESSysPreprocessing
Will's RHESSys tools are also written in R with R-studio. Major difference between GIS2RHESSys and Will's is that GIS2RHESSys is designed to simplify the existing code and be friendly to command line/Jupyter notebook/server-client environment.
