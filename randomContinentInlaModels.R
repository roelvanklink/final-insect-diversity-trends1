suppressPackageStartupMessages( library (INLA))
INLA:::inla.dynload.workaround()



library(tidyverse)
library(reshape2)
parameters<- read.csv("RandomContinent.csv", stringsAsFactors = F)


args <- commandArgs(trailingOnly = T)
output_dir <- args[1]
taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
threads =  as.integer(Sys.getenv("NSLOTS", "1"))
load("completeData2021pure.RData") 

exptPlots<- c(5, # alaska
							921, 922, 924,925, #smes
							643, 644, 646, 647, # hemlock removal
							137, 138, 139  #brazil fragmentation experiment
)
exptDatasources<- c(300,1364, 1357,1410) #Kellogg, Luiquillo CTE, Cedar creek big bio, some german grassland

completeData2021pure<- completeData2021pure[!completeData2021pure$Datasource_ID %in% exptDatasources, ]
completeData2021pure<- completeData2021pure[!completeData2021pure$Plot_ID %in% exptPlots, ]
dim(completeData2021pure)


completeData2021pure$pltYr<- paste0(completeData2021pure$Plot_ID, "_", completeData2021pure$Year) # unique ID for year/plot combination fo that we exclude some years 

startEndYears<- completeData2021pure%>% 
	group_by(Plot_ID) %>%
	summarise(
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T), 
		nYrs = length(unique(Year)),
	)
startEndYears$pltStartYr<- paste0(startEndYears$Plot_ID, "_", startEndYears$Start_year)


# rename 
completeData2021<- completeData2021pure
completeData2021$Unit[completeData2021$Unit == "richness"   & completeData2021$Datasource_ID == 1394  ] <- "rarefiedRichness"


# assign random slope and random intercept parameters 
completeData2021$Continent_4INLA <- as.numeric(factor(completeData2021$Continent))
completeData2021$Continent_4INLAs <- completeData2021$Continent_4INLA+max(completeData2021$Continent_4INLA)


cDrichness <- subset(completeData2021, Unit == "richness"); dim(cDrichness)
cDabund    <- subset(completeData2021, Unit == "abundance"); dim(cDabund)
cDenspie   <- subset(completeData2021, Unit == "ENSPIE")   ;dim(cDenspie)
cDenspie$Number[is.infinite(cDenspie$Number)] <- 0



all.df<-list(cDrichness,
						 cDabund,
						 cDenspie
)
names(all.df)<- c("cDrichness",  "cDabund" , "cDenspie" )



taskID

print("model name:")
metric<- parameters$model_name[taskID]
metric

Sys.time()

dat<- all.df[[as.character(parameters$input_file[taskID])]] 
dim(dat)
#str(dat)


formul<-as.formula(paste(parameters$y[taskID], " ~ ", parameters$model_formula[taskID]   ,
												 "+f(Plot_ID_4INLA,model='iid')+
                            f(Location_4INLA,model='iid')+
                            f(Datasource_ID_4INLA,model='iid')+
                            f(Continent_4INLA,model='iid')+
														f(Period_4INLA,model='iid')+
                            f(Plot_ID_4INLAs,iYear,model='iid')+
                         		f(Location_4INLAs,iYear,model='iid')+
                            f(Datasource_ID_4INLAs,iYear,model='iid')+
                            f(Continent_4INLAs, iYear,model='iid')+
                            f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA))" ))


model <- inla( formul,
							 control.compute = list(config = TRUE, 
							 												dic=TRUE,
							 												waic=TRUE, 
							 												cpo = TRUE), 
							 #control.inla = list(h = 0.484761), 
							 control.predictor = list(link = 1) , verbose = F, 
							 quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,    
							 data=dat)

fixed <- model$summary.fixed

parameters$model_name[taskID]
print("done:")
Sys.time()
#assign(as.character(parameters$model_name[i]), model)

model_file <- file.path(output_dir, paste0(as.character(metric),"TEST.rds"))
fixed_file <- file.path(output_dir, paste0(as.character(metric),"SUMMARY.rds"))

saveRDS (model, file =  model_file)
saveRDS (fixed, file = fixed_file)




# extract posterior marginals and sample posteriors
m <-  data.frame(Metric = metric, 
								 Realm = "Freshwater",
								 inla.smarginal(model$marginals.fixed$`RealmFreshwater:cYear`, factor = 50))
n <-  data.frame(Metric = metric, 
								 Realm = "Terrestrial",
								 inla.smarginal(model$marginals.fixed$`RealmTerrestrial:cYear`, factor = 50))
marg<- rbind(m,n)

marg$y80<- marg$y
marg$y80  [ marg$Realm == "Freshwater"  & marg$x< fixed["RealmFreshwater:cYear", "0.1quant"] ]<- 0 # allocate 0 to evrything below the 80% quantile 
marg$y80  [ marg$Realm == "Freshwater"  & marg$x> fixed["RealmFreshwater:cYear", "0.9quant"] ]<- 0
marg$y80  [ marg$Realm == "Terrestrial" & marg$x< fixed["RealmTerrestrial:cYear", "0.1quant"] ]<- 0 # allocate 0 to evrything below the 80% quantile 
marg$y80  [ marg$Realm == "Terrestrial" & marg$x> fixed["RealmTerrestrial:cYear", "0.9quant"] ]<- 0
marg$y90<- marg$y
marg$y90  [ marg$Realm == "Freshwater"  & marg$x< fixed["RealmFreshwater:cYear", "0.05quant"] ]<- 0 # allocate 0 to evrything below the 90% quantile 
marg$y90  [ marg$Realm == "Freshwater"  & marg$x> fixed["RealmFreshwater:cYear", "0.95quant"] ]<- 0
marg$y90  [ marg$Realm == "Terrestrial" & marg$x< fixed["RealmTerrestrial:cYear", "0.05quant"] ]<- 0 # allocate 0 to evrything below the 90% quantile 
marg$y90  [ marg$Realm == "Terrestrial" & marg$x> fixed["RealmTerrestrial:cYear", "0.95quant"] ]<- 0
marg$y95<- marg$y
marg$y95  [ marg$Realm == "Freshwater"  & marg$x< fixed["RealmFreshwater:cYear", "0.025quant"] ]<- 0 # allocate 0 to evrything below the 95% quantile 
marg$y95  [ marg$Realm == "Freshwater"  & marg$x> fixed["RealmFreshwater:cYear", "0.975quant"] ]<- 0
marg$y95  [ marg$Realm == "Terrestrial" & marg$x< fixed["RealmTerrestrial:cYear", "0.025quant"] ]<- 0 # allocate 0 to evrything below the 95% quantile 
marg$y95  [ marg$Realm == "Terrestrial" & marg$x> fixed["RealmTerrestrial:cYear", "0.975quant"] ]<- 0


marg_file <- file.path(output_dir, paste0(metric,"Marginal.rds"))

saveRDS (marg, file =  model_file)

