suppressPackageStartupMessages( library (INLA))
INLA:::inla.dynload.workaround()



library(tidyverse)
library(reshape2)
parameters<- read.csv("metrics.csv", stringsAsFactors = F)


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

cDrichness <- subset(completeData2021, Unit == "richness"); dim(cDrichness)
cDrarRichness <- subset(completeData2021, Unit == "rarefiedRichness"); dim(cDrarRichness)
cDabund    <- subset(completeData2021, Unit == "abundance"); dim(cDabund)
cDenspie   <- subset(completeData2021, Unit == "ENSPIE")   ;dim(cDenspie)
	cDenspie$Number[is.infinite(cDenspie$Number)] <- 0
cDhorn     <- subset(completeData2021, Unit == "Horn"); dim(cDhorn)
	cDhorn$dif <-  cDhorn$Number - cDhorn$ExpectedBeta
	cDhorn$SES <- (cDhorn$Number - cDhorn$ExpectedBeta) / cDhorn$SDexpectedBeta
	cDhorn<- cDhorn[!cDhorn$pltYr %in% startEndYears$pltStartYr, ] ;dim(cDhorn)
cDbray     <- subset(completeData2021, Unit == "Bray"); dim(cDbray)
	cDbray$dif <-  cDbray$Number - cDbray$ExpectedBeta
	cDbray$SES <- (cDbray$Number - cDbray$ExpectedBeta) / cDbray$SDexpectedBeta
	cDbray<- cDbray[!cDbray$pltYr %in% startEndYears$pltStartYr, ]; dim(cDbray)
cDjaccard  <- subset(completeData2021, Unit == "Jaccard") ; dim(cDjaccard)
	cDjaccard$Number[is.nan(cDjaccard$Number)] <- NA
	cDjaccard$dif <-  cDjaccard$Number - cDjaccard$ExpectedBeta
	cDjaccard$SES <- (cDjaccard$Number - cDjaccard$ExpectedBeta) / cDjaccard$SDexpectedBeta
	cDjaccard<- cDjaccard[!cDjaccard$pltYr %in% startEndYears$pltStartYr, ] ; dim(cDjaccard)
cDlogNr10   <- subset(completeData2021, Unit == "logNr10"    ); dim(cDlogNr10)  
cDlogNr90   <- subset(completeData2021, Unit ==  "logNr90"   ); dim(cDlogNr90)  
cDlogNr020   <- subset(completeData2021, Unit ==  "logNr020"   ); dim(cDlogNr020)  
cDlogNr2040   <- subset(completeData2021, Unit ==  "logNr2040"   ); dim(cDlogNr2040)  
cDlogNr4060   <- subset(completeData2021, Unit ==  "logNr4060"   ); dim(cDlogNr4060)  
cDlogNr6080   <- subset(completeData2021, Unit ==  "logNr6080"   ); dim(cDlogNr6080)  
cDlogNr80100   <- subset(completeData2021, Unit ==  "logNr80100"   ); dim(cDlogNr80100)  



cDdom       <- subset(completeData2021, Unit == "dominanceRel"); dim(cDdom)  
cDshan  <- subset(completeData2021, Unit == "Shannon"); dim(cDshan)  
cDpilou <- subset(completeData2021, Unit == "Pielou"|Unit == "Pilou" ); dim(cDpilou)  



all.df<-list(cDrichness,
cDrarRichness ,
             cDabund,
             cDenspie,
             cDhorn,
             cDbray,
             cDjaccard,
		cDshan, 
		cDpilou, 
		cDlogNr10,
		cDlogNr90,
		cDlogNr020,
		cDlogNr2040,
		cDlogNr4060 ,
		cDlogNr6080  ,
		cDlogNr80100  ,
		cDdom
)
names(all.df)<- c("cDrichness", "cDrarRichness",  "cDabund" , "cDenspie" , "cDhorn" ,"cDbray" , "cDjaccard", 
									"cDshan", "cDpilou",  "cDlogNr10", "cDlogNr90", 
									"cDlogNr020",		"cDlogNr2040",	"cDlogNr4060" ,	"cDlogNr6080"  , 	"cDlogNr80100"  ,"cDdom")



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
               control.compute = list(	config = TRUE, 
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



# samples: 
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









