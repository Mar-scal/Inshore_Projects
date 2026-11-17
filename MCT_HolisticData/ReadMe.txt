R script used for Holistic Data QAQC and production of MCT Figures.


Tips and order of operations for running scripts:

 -  Ensure data entry for Holistic_Samples.xlsx for desired area/year is complete and that metadata tab is 1st and data tab 2nd. Files are located in their respective "Data" folders within "Y:\Projects\Holistic_sampling_Inshore"

 - Run "SpatialDataSetup.R" first. Spatial data is required for condition, gonad, and health maps. For simplicity all R scripts pull from spatial file, not original. 

- Run Preliminary_results.R next for QAQC and basic figures on shell height, sex/maturity, GSI, etc. IF AN EDIT IS MADE DURING QAQC, MAKE IT IN ORIGINAL EXCEL FILE AND THEN RERUN SPATIAL SETUP SCRIPT. DO NOT JUST EDIT THE SPATIAL FILE.

- Run Gonad, Condition, and Scallop Health scripts in any order. 

- For quicker re-making of Condition and Gonad maps, you can simply load csv for mapping. To do this, run set up section, skip modelling section and start again at line ~140 (look for header "load csv/file for mapping").