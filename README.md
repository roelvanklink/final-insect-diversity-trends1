# final-insect-diversity-trends1
reproducable code for review for the manuscript called 'Widespread declines of dominant insect species are changing the structure of insect assemblages '


files and their functions: 

In order of use: 
1) load and check data 20201216.R
This will load all the original datafiles, clean them and arrange them so, as to produce the datafram on which all main analyses are done. The raw data files are only partially provided on KNB, but all results are available in the file "completedata202021pure.rds".

Dependencies: 
- Calculate metrics.R # this script calculates all biodiversity metrics, including the density kernels, used in the manuscript and more.
- calculate expected beta diversity.R # this script calculates the expected Beta diversity based on randomizations of the species pool
- effort rarefaction.R # this script is used to rarefy communities based on a species X time matrix. Originally written by Thore Engel, adapted by Roel van Klink
- function_cleaning_taxon_names.R # this function probabilistically allocates unidentified individuals to the species of its genus found in the time series Written by Alban Sagouis

 ) Load and prep density data.R
This creates the dataframe 



2) allMetricsInlaModels.R
this is an array script for use on a high performance cluster. it runs all INLA models in the main text (and a few more). Approximate memmory use: 200gb

3) AllSensitivityInaModels.R
This script runs the models needed to produce the supplementary / extended data graphs. It requires a number of additional input dataframes that are created in the script XXXXXXXX

 ) randomContinentInlaModels.R
This runs the inla models for the continent-specific models 


4) Inla graphs richness.R 
This script takes the output files of the models and plots them to produce the graphs in the manuscript

Dependencies: 
map_preparation.R # a script for preparing the map properties for plotting 











 other files
Greenland data processing rarefaction 2021.R 
This file contains the processing code for the Greenland data, which may not be shared in a derived form. 



randomContinentInlaModels