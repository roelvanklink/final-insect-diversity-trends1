# final-insect-diversity-trends1
reproducable code for review for the manuscript called 'Widespread declines of dominant insect species are changing the structure of insect assemblages '


files and their functions: 

In order of use: 
1) load and check data 20201216.R
This will load all the original datafiles, clean them and arrange them so, as to produce the datafram on which all main analyses are done. The raw data files are only partially provided on KNB, but all results are available in the file "completedata202021pure.rds".

Dependencies: 



2) allMetricsInlaModels.R
this is an array script for use on a high performance cluster. it runs all INLA models in the main text (and a few more). Approximate memmory use: 200gb

3) AllSensitivityInaModels.R
This script runs the models needed to produce the supplementary / extended data graphs. It requires a number of additional input dataframes that are created in the script XXXXXXXX

4) Inla graphs richness.R 
This script takes the output files of the models and plots them to produce the graphs in 
