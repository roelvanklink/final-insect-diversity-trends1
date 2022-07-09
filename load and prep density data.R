#Load kernel density data
rm(list=ls()) 
library(reshape2)
library(tidyverse)
library(beepr)


setwd("C:/Users/roelv/Dropbox/Insect Biomass Trends/csvs") # home
setwd("C:\\Dropbox\\Insect Biomass Trends/csvs") # work # work

taxa<-read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/taxa5.2.csv"); dim(taxa)
plots<-read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/PlotData 5.0.csv"); dim(plots)
UKfwPlots<- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/UKfwSites.csv")
plots<- rbind(plots[, names(UKfwPlots)], UKfwPlots)

samples <-read.csv( file = "Sample_Info 5.2.csv"); dim(samples)
database <-read.csv( file = "Data 5.2.csv"); dim(database)
database<- subset(database, Note != "remove");dim(database)
unique(database$Datasource_name)
studies<-read.csv(file = "studies 5.2.csv", header = T); dim(studies)

load("completeData2021.RData")
completeData2021$Flag_taxonomy[is.na(completeData2021$Flag_taxonomy)  ]<- "" # homogenize lacking flags 


#Load datasets
# load rarefied densitoes 
	DatabaseDensities<- readRDS( file = "all  densities calculated from database 20210903.rds ")
  CzbeetlesDens<- read.csv(file = "Czech beetles mean densities.csv")
  ECNbuttDens<- read.csv( file = "ECN butterflies mean densities 20210715.csv")[, -3]
  ECNmothsDens<- read.csv(file = "ECN moths mean densities.csv")
  ECNgbDens<- read.csv( file = "ECN GB mean densities.csv")
  IRbuttDens<- read.csv( file = "IR butterflies mean densities 20210715.csv")[, -c(3,7)]
  KelloggDens<- read.csv(file = "Kellogg mean densities 20210724.csv", header = T) # replacement
  IndianaDens <- read.csv(file = "IndianaMosquitos mean densities 20210825.csv")
  iowaDens<- read.csv( file = "IowaMosquitosRarefiedmeanDensities 20210715.csv", header = T) # 
  AZfwDens<- read.csv( file = "AZfw mean densities0718.csv", header = T)
  Luquillo1Dens<- read.csv(file = "luquillo canopyall RarefiedmeanDensities 20210803.csv", header = T)
  LuquilloCTEdens<- read.csv(  file = "luquillo canopyCTE meanDensities 20210803.csv", header = T)    
  SwengelButterfliesDens<- read.csv( file = "Swengel Butterflies mean densities20220226.csv", header = T)
  GreenlandDens<- read.csv( file = "greenland meanDensities 20210731.csv")        # checked 19-12-19
  SpaindungbeetlesDens<- read.csv(file = "Spain dungbeetles mean densities.csv", header = T) 
  AustrAntsDens<-read.csv( file = "Austalia ants 3 meanDensities 20210731.csv", header = T)
  HongkongfwDens<- read.csv( file = "Hongkong fw Densities 20210802.csv", header = T)
  GeorgiaCaddisDens<- read.csv( file = "GeorgiaCaddisflies mean densities20210804.csv")
  KonzaDens<- read.csv(file = "KonzaRarefied mean densities20210715.csv")
  TDbuttDens<- read.csv( file = "TamDaoButterflies mean densities.csv")
  ChicagoMosquitosDens<- read.csv( file = "ChicagoMosquitosRarefiedmeanDensities 20210726.csv")
  VirginiaMosquitosDens<- read.csv( file = "VirginiaMosquitosRarefied mean densities20210727.csv")
  MontanaMosquitosDens <- read.csv( file = "MontanaMosquitosRarefiedmeanDensities 20210731.csv")
  NDmosquitosDens<- read.csv( file = "NDMosquitosRarefiedmeanDensities 20210804.csv")
  AZdens<- read.csv( file = "c:\\Dropbox\\Insect Biomass Trends/csvs/LTERArizonaRarefiedmeanDensities 20210806.csv", header = T)
  IdahoMosquitosDens  <- read.csv( file = "IdahoMosquitosRarefiedmeanDensities 20210806.csv")
  FloridaMosquitosDens<- read.csv( file = "Florida Mosquitos mean densities 20210812.csv")
brazilbeesdens<- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends\\csvs/Brazil bees 3 mean densities.csv")
  
  


















#Combine datasets

    
    # combine all densities of datasets where Plot is denoted by ID number
allDensitiesID<- rbind(
	cbind (X = seq(1: nrow(DatabaseDensities)), DatabaseDensities),
  CzbeetlesDens, 
  ECNbuttDens,
  ECNmothsDens,
  ECNgbDens,
  IRbuttDens,
  iowaDens, 
  AZfwDens, 
  KelloggDens,
  Luquillo1Dens, 
  LuquilloCTEdens, 
  SwengelButterfliesDens, 
  SpaindungbeetlesDens, 
  AustrAntsDens, 
  HongkongfwDens, 
  GeorgiaCaddisDens, 
  KonzaDens,
  TDbuttDens, 
  ChicagoMosquitosDens, 
  IdahoMosquitosDens , 
  VirginiaMosquitosDens, 
  MontanaMosquitosDens,
  NDmosquitosDens, 
	brazilbeesdens); dim(allDensitiesID)  # 68978
#   write_rds(allDensitiesID, file = "allDensities 20210812.rds")
allDensitiesID<- merge(allDensitiesID, plots) # merge in plot info
dim(allDensitiesID)
  # combine datasets that have plots denoted by  plot_name: 
allDensitiesName<- rbind(GreenlandDens, 
  AZdens,
  IndianaDens[, -(6)] ,
  FloridaMosquitosDens[, -(6)]  ) ; dim(allDensitiesName)# remove column 'run' 
#write_rds(providedDensitiesName, file = "Densities by name 20210812.rds")
  allDensitiesName<-       merge(allDensitiesName, plots) ; dim(allDensitiesName) # merge in plot info
  

 #combine  
allDensities<-   rbind(allDensitiesID, allDensitiesName)
  dim(allDensities) # 73736   123
  length(unique(allDensities$Plot_ID)) # 1857 plots
  length(unique(allDensities$Datasource_ID)) # 83 datasets
  
allDensities<- merge(allDensities, studies, by = "Datasource_ID"); dim(allDensities)
# remove column X
  allDensities<- allDensities[, -(which(names(allDensities) == "X"))]
    

  metadata_per_plotD<-  allDensities %>% 
  group_by(Plot_ID) %>%
  summarise(
    Datasource_ID = unique(Datasource_ID),
      Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
     )

too.short<- subset(metadata_per_plotD, Duration<9)
allDensities<- subset(allDensities, !Plot_ID %in% too.short$Plot_ID )
  
  
  
dim(allDensities)
# what do we lose when we merge in the data we use in the rest of the  analysis?



  write_rds(allDensities, file = "allDensities 20220304.rds")
  
  
  
  
  
  
  
  
  
  
  
  
# prep for inla   #####
  completeDens2021<- NULL
for(i in 1:length(unique(allDensities$Plot_ID))){ # 1843
  
  plt<- sort(unique(allDensities$Plot_ID))[i]
  myData<- allDensities[allDensities$Plot_ID == plt , ]
  
  #expand grid to include NAs  # note that the 'date' variable is removed here. 
  # Date plays no role in the analysis, 
  # and in case multiple weeks were sampled withing a month, these are thus seen as "replicates" within a month. 
  # month is accounted for as random effect
  constantData <- unique(myData[,c("Plot_ID","Datasource_ID", "densityMode", "correction" )])#these are defo unique
  allgrid <- expand.grid(Plot_ID = unique(myData$Plot_ID),
                         Year= min(myData$Year):max(myData$Year),
                         densityMode = unique(myData$densityMode), 
  											 correction = unique(myData$correction))
  
  allgrid <- merge(allgrid,constantData,all.x=T)
  
  #add observed data
  myData1 <- merge(allgrid, myData[, 1:105],  #"classes",
                   by=c("Datasource_ID", "Year","Plot_ID", "densityMode", "correction" ),all=T)
  if (nrow(myData1) > nrow(allgrid)){ print("WARNING: something's going wrong here")}
  # add descriptors
  myData <- merge(myData1, unique(myData[ ,c("Plot_ID",  "Location", "Datasource_name", "Realm",
                                             "Continent",  "Country", "Country_State", "Region" )]),
                  by="Plot_ID",all=T)
  if (nrow(myData) > nrow(myData1)){ print("WARNING: something's going wrong here")}
  #print(plt)
  

  completeDens2021<-rbind (completeDens2021,myData)
  print(plt)
}






dim(completeDens2021) #  
beep(2)



addIndicies <- function(myData){
  
  #year covariates
  myData$cYear <- myData$Year - floor(median(myData$Year))
  myData$iYear <- myData$Year - min(myData$Year) + 1
  myData$rYear <- myData$iYear
  myData$rYear2 <- myData$iYear
  
  #random intercept indices (these are nested)
  myData$Plot_ID_4INLA <- interaction(myData$Datasource_ID,myData$Plot_ID)
  myData$Plot_ID_4INLA <- as.numeric(factor(myData$Plot_ID_4INLA))   
  myData$Datasource_ID_4INLA <- as.numeric(factor(myData$Datasource_ID))
  myData$Country_State_4INLA <- as.numeric(factor(myData$Country_State))
  
  # This is now a crossed random effect: accounting for datasets that were collected at the same location
  #  myData$Location[is.na(myData$Location)] <- 1#dummy value
  # myData$Location_4INLA <- interaction(myData$Datasource_ID,myData$Location) # this is not necessary anymore
  myData$Location_4INLA <- as.numeric(factor(myData$Location))
  
  #random slope indices
  myData$Plot_ID_4INLAs <- myData$Plot_ID_4INLA+max(myData$Plot_ID_4INLA)
  myData$Datasource_ID_4INLAs <- myData$Datasource_ID_4INLA+max(myData$Datasource_ID_4INLA)
  myData$Location_4INLAs <- myData$Location_4INLA+max(myData$Location_4INLA)
  myData$Country_State_4INLAs <- myData$Country_State_4INLA+max(myData$Country_State_4INLA)
  


  
  
  
  return(myData)
}

completeDens2021 <- addIndicies(completeDens2021)

 
dim(completeDens2021) #657345
save(completeDens2021, file = "completeDens2021.RData")

unique(completeDens2021$Datasource_ID)
  
  