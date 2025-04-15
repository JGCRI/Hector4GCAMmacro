# Hector 4 GCAM macro
Scripts and materials related to emulating select CMIP6 ESM with Hector as part of the GCAM macro project. 

## Scripts 

Where all of the scripts related to the training Hector as an emulator, preparing 
STITCHES input, to the STITICHES pipeline live. The following table describes the 
order to run the scripts for this workflow. 


| Run Order|Name                             | Description                                              |  
|:---------|--------------------------------:|---------------------------------------------------------:|
|1         | 0.prep_ESM_compdata.R           | Format and prepare the ESM CMIP6 output data that will be used in the calibration process, processed ESM data from Leeya Pressburger, & Kalyn R. Dorheim. (2022). Only needs to be run once.|  
|2         | 1.calibrate.R                   | Tune key Hector parameters in set Hector up as an emulator for the selected ESMs.|
|3         | 2.run_hector_stitching_prep-dev.R |Place holder for the GCAM run step, uses the parameter best fits to run Hector for the SSP245 scenario pathway.  | 
|4         | 3.stitching-dev.py              |Place holder for the script that makes the STITCHES products| 


The A.constants.R and A.fxns_calibration.R are scripts that set up the R environment for the .R scripts. 