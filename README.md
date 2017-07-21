# R-coded-scripts-for-RHESSys-calibration
The main script is CUS_RHESSys_MultipleCalibration_MCMC_parallel_MCMC_killdevil.r
All other script with names began with "LIB_" are libraries of functions that would be used by the main script.
Particularly, this MCMC calibration script is designed for UNC Killdevil computer clusters. 
However, it can be generalized for other computer clusters and local personal desktop/mainframe computer. 

It is not recommended to run this calibration script without knowing the MCMC and probability likelihood concepts. 
There are several areas in the script that need to be customized/paid attention.
1) iniParam and RHESSYS_PARAMS -- initial values of RHESSys parameters: s1, s2, s3, sv1, sv2, gw1, and gw2. Some additional parameters, e.g., pondz, snowT, RTz, and CAPr, are for custom RHESSys build. Users can edit the parameter list.
2) param.fittingNames -- select parameters for calibration
3) paramBoundary -- ranges of parameters
4) precmd -- RHESSys model, start time, end time, worldfile, flow table, and other RHESSys runtime flags
5) FITTNESS_NAMES and betaShape1 -- set fittness criteria and likelihood probability; All criteria will be mulitpled through likelihood probabilities. 
6) arg -- command line arguments (when using Rscript): project directory, observation file, session ID, iternation ID (starting and ending), RHESSys model directory, model output directory, number of CPU cores, cluster job ID. 
