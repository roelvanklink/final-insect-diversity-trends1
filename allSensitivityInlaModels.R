suppressPackageStartupMessages( library (INLA))
INLA:::inla.dynload.workaround()



library(tidyverse)
library(reshape2)
parameters<- read.csv("sensitivityanalyses.csv", stringsAsFactors = F)


args <- commandArgs(trailingOnly = T)
output_dir <- args[1]
taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
threads =  as.integer(Sys.getenv("NSLOTS", "1"))
load("completeData2021pure.RData") ; dim(completeData2021pure)
completeData2021max10plots<- readRDS(file = "completeData2021max10plots.rds")  ; dim(completeData2021max10plots)
completeData2021max20plots<- readRDS(file = "completeData2021max20plots.rds")  ; dim(completeData2021max20plots)
completeData2021max50plots<- readRDS(file = "completeData2021max50plots.rds")  ; dim(completeData2021max50plots)
completeData2021short<- readRDS(file = "completeData2021short.rds");dim(completeData2021short)
completeData2021long<- readRDS(file = "completeData2021long.rds") ; dim(completeData2021long)





exptPlots<- c(5, # alaska
							921, 922, 924,925, #smes
							643, 644, 646, 647, # hemlock removal
							137, 138, 139  #brazil fragmentation experiment
)
exptDatasources<- c(300,1364, 1357,1410) #Kellogg, Luiquillo CTE, Cedar creek big bio, some german grassland

completeData2021pure<- completeData2021pure[!completeData2021pure$Datasource_ID %in% exptDatasources, ]
completeData2021pure<- completeData2021pure[!completeData2021pure$Plot_ID %in% exptPlots, ]; dim(completeData2021pure)

completeData2021max50plots<- completeData2021max50plots[!completeData2021max50plots$Plot_ID %in% exptPlots, ]; dim(completeData2021max50plots)
completeData2021max50plots<- completeData2021max50plots[!completeData2021max50plots$Datasource_ID %in% exptDatasources, ]; dim(completeData2021max50plots)

completeData2021max20plots<- completeData2021max20plots[!completeData2021max20plots$Plot_ID %in% exptPlots, ]; dim(completeData2021max20plots)
completeData2021max20plots<- completeData2021max20plots[!completeData2021max20plots$Datasource_ID %in% exptDatasources, ]; dim(completeData2021max20plots)

completeData2021max10plots<- completeData2021max10plots[!completeData2021max10plots$Plot_ID %in% exptPlots, ]; dim(completeData2021max10plots)
completeData2021max10plots<- completeData2021max10plots[!completeData2021max10plots$Datasource_ID %in% exptDatasources, ]; dim(completeData2021max10plots)

completeData2021short<- completeData2021short[!completeData2021short$Plot_ID %in% exptPlots, ]; dim(completeData2021short)
completeData2021short<- completeData2021short[!completeData2021short$Datasource_ID %in% exptDatasources, ]; dim(completeData2021short)

completeData2021long<- completeData2021long[!completeData2021long$Plot_ID %in% exptPlots, ]; dim(completeData2021long)
completeData2021long<- completeData2021long[!completeData2021long$Datasource_ID %in% exptDatasources, ]; dim(completeData2021long)

# for beta metrics: 
completeData2021pure$pltYr<- paste0(completeData2021pure$Plot_ID, "_", completeData2021pure$Year)

startEndYears<- completeData2021pure%>% 
	group_by(Plot_ID) %>%
	summarise(
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T), 
		nYrs = length(unique(Year)),
	)
startEndYears$pltStartYr<- paste0(startEndYears$Plot_ID, "_", startEndYears$Start_year)
startEndYears$pltEndYr<- paste0(startEndYears$Plot_ID, "_", startEndYears$End_year)




# rename 
completeData2021<- completeData2021pure

# create needed objects by selecting from dataframes
completeData2021max50plotsRichness <- subset(completeData2021max50plots, Unit == "richness"); dim(completeData2021max50plotsRichness)
completeData2021max20plotsRichness <- subset(completeData2021max20plots, Unit == "richness"); dim(completeData2021max20plotsRichness)
completeData2021max10plotsRichness <- subset(completeData2021max10plots, Unit == "richness"); dim(completeData2021max10plotsRichness)
completeData2021max50plotsAbundance <- subset(completeData2021max50plots, Unit == "abundance"); dim(completeData2021max50plotsAbundance)
completeData2021max20plotsAbundance <- subset(completeData2021max20plots, Unit == "abundance"); dim(completeData2021max20plotsAbundance)
completeData2021max10plotsAbundance <- subset(completeData2021max10plots, Unit == "abundance"); dim(completeData2021max10plotsAbundance)
completeData2021max50plotsENSPIE <-    subset(completeData2021max50plots, Unit == "ENSPIE"); dim(completeData2021max50plotsENSPIE)
completeData2021max50plotsENSPIE$Number[completeData2021max50plotsENSPIE$Number == Inf] <- NA
completeData2021max20plotsENSPIE <-    subset(completeData2021max20plots, Unit == "ENSPIE"); dim(completeData2021max20plotsENSPIE)
completeData2021max20plotsENSPIE$Number[completeData2021max20plotsENSPIE$Number == Inf] <- NA
completeData2021max10plotsENSPIE <-    subset(completeData2021max10plots, Unit == "ENSPIE"); dim(completeData2021max10plotsENSPIE)
completeData2021max10plotsENSPIE <-  completeData2021max10plotsENSPIE[!is.infinite(completeData2021max10plotsENSPIE$Number),] ; dim(completeData2021max10plotsENSPIE)
completeData2021max10plotsENSPIE$Number[completeData2021max10plotsENSPIE$Number == Inf] <- NA
completeData2021shortRichness <-  	 subset(completeData2021short, Unit == "richness"); dim(completeData2021shortRichness)
completeData2021shortAbundance <- 	 subset(completeData2021short, Unit == "abundance"); dim(completeData2021shortAbundance)
completeData2021shortENSPIE <- 		subset(completeData2021short, Unit == "ENSPIE"); dim(completeData2021shortENSPIE)
completeData2021shortENSPIE$Number[completeData2021shortENSPIE$Number == Inf] <- NA; dim(completeData2021shortENSPIE)
completeData2021longRichness <- 	subset(completeData2021long, Unit == "richness"); dim(completeData2021longRichness)
completeData2021longAbundance <-  subset(completeData2021long, Unit == "abundance"); dim(completeData2021longAbundance)
completeData2021longENSPIE <- 		subset(completeData2021long, Unit == "ENSPIE"); dim(completeData2021longENSPIE)
completeData2021longENSPIE$Number[completeData2021longENSPIE$Number == Inf] <- NA

cDhorn     <- subset(completeData2021, Unit == "HornLast"); dim(cDhorn)
cDhorn$dif <-  cDhorn$Number - cDhorn$ExpectedBeta
cDhorn$SES <- (cDhorn$Number - cDhorn$ExpectedBeta) / cDhorn$SDexpectedBeta
cDhorn<- cDhorn[!cDhorn$pltYr %in% startEndYears$pltEndYr, ] ;dim(cDhorn) # remove last year (0 dissimilarity) 
cDbray     <- subset(completeData2021, Unit == "BrayLast"); dim(cDbray)
cDbray$dif <-  cDbray$Number - cDbray$ExpectedBeta
cDbray$SES <- (cDbray$Number - cDbray$ExpectedBeta) / cDbray$SDexpectedBeta
cDbray<- cDbray[!cDbray$pltYr %in% startEndYears$pltEndYr, ]; dim(cDbray)
cDjaccard  <- subset(completeData2021, Unit == "JaccardLast") ; dim(cDjaccard)
cDjaccard$Number[is.nan(cDjaccard$Number)] <- NA
cDjaccard$dif <-  cDjaccard$Number - cDjaccard$ExpectedBeta
cDjaccard$SES <- (cDjaccard$Number - cDjaccard$ExpectedBeta) / cDjaccard$SDexpectedBeta
cDjaccard<- cDjaccard[!cDjaccard$pltYr %in% startEndYears$pltEndYr, ] ; dim(cDjaccard)




all.df<-list(completeData2021max50plotsRichness = completeData2021max50plotsRichness,
						 completeData2021max20plotsRichness = completeData2021max20plotsRichness, 
						 completeData2021max10plotsRichness = completeData2021max10plotsRichness,
						 completeData2021max50plotsAbundance =completeData2021max50plotsAbundance,
						 completeData2021max20plotsAbundance =completeData2021max20plotsAbundance,
						 completeData2021max10plotsAbundance = completeData2021max10plotsAbundance,
						 completeData2021max50plotsENSPIE =completeData2021max50plotsENSPIE,
						 completeData2021max20plotsENSPIE =completeData2021max20plotsENSPIE,
						 completeData2021max10plotsENSPIE =completeData2021max10plotsENSPIE,
						 completeData2021shortRichness =completeData2021shortRichness,
						 completeData2021shortAbundance =completeData2021shortAbundance,
						 completeData2021shortENSPIE =completeData2021shortENSPIE,
						 completeData2021longRichness =completeData2021longRichness,
						 completeData2021longAbundance =completeData2021longAbundance,
						 completeData2021longENSPIE =completeData2021longENSPIE,
						 cDjaccard =cDjaccard,
						 cDhorn =cDhorn,
						 cDbray = cDbray
						 )




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
                            f(Period_4INLA,model='iid')+
                            f(Plot_ID_4INLAs,iYear,model='iid')+
                         		f(Location_4INLAs,iYear,model='iid')+
                            f(Datasource_ID_4INLAs,iYear,model='iid')+
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


# 
# #samples:
# my_list <- list("RealmFreshwater:cYear" = 1 ,
# 								"RealmTerrestrial:cYear" = 1)
# 
# lm1.samples = inla.posterior.sample(1000, model, selection = my_list)
# #extract the bits we need using the line below
# Ter.samp <- as.vector(inla.posterior.sample.eval(function(...) { `RealmTerrestrial:cYear` },lm1.samples))
# FW.samp <- as.vector(inla.posterior.sample.eval(function(...) { `RealmFreshwater:cYear` },lm1.samples))
# 
# sampT_file<- file.path(output_dir, paste0(metric,"TerSamples.rds"))
# sampFW_file<- file.path(output_dir, paste0(metric,"FWSamples.rds"))
# 
# saveRDS(Ter.samp, file = sampT_file)
# saveRDS(FW.samp, file = sampFW_file)
# 
# 
# dif<- Ter.samp - FW.samp
# print(dataname)
# print("difference between realms")
# print(quantile(dif,c(0,0.025, 0.05, 0.1 ,0.5,0.9, 0.95, 0.975,1)) )


















