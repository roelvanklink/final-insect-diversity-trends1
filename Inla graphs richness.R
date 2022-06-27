rm(list=ls()) 

library(INLA)
library(brinla)
library(ggplot2)
library(ggnewscale)
library(tidyverse)
library(reshape2)
#source("D:/work/2017 iDiv/2018 insect biomass/final-insect-diversity-trends1/plotting functions.R")


setwd("") # home

figure_path<- "/final-insect-diversity-trends1/Figures/"


completeData2021<- read_rds("C:/Dropbox/Insect Biomass Trends/csvs/Van Klink insects Full final dataframe.rds"); dim(completeData2021) # 18.8.20 66249 lines,  1676 plots

#add unique ID for plot_year combination
completeData2021$pltYr<- paste0(completeData2021$Plot_ID, "_", completeData2021$Year)
# rename south and central America to be Latin America
completeData2021$Continent[completeData2021$Continent == "Central America" | completeData2021$Continent == "South America" ] <- "Latin America"






# table S1
metadata_per_dataset <-completeData2021 %>% 
	group_by(Datasource_ID) %>%
	summarise(
		Datasource_name = unique(Datasource_name),
		Country = unique(Country),
		Country_State  = unique(Country_State),
		Start = min(Year), 
		End = max(Year),
		Time_span_yrs = (max(Year) - min(Year))+1,
		Nr_yrs_data = length(unique(Year)),
		Nb_plots = length(unique(Plot_ID))
			
	)
subset(metadata_per_dataset, Time_span_yrs == max(metadata_per_dataset$Time_span_yrs)) # longest time series
subset(metadata_per_dataset, Nb_plots == max(metadata_per_dataset$Nb_plots)) # largest dataset
subset(metadata_per_dataset, Nb_plots > 50) # which dataset have over 50 plots?
sum(metadata_per_dataset$Nb_plots)

metadata_per_dataset_per_metric<- completeData2021 %>% 
	group_by(Datasource_ID, Metric) %>%
	summarise(
		Datasource_name = unique(Datasource_name), 
		count = 1)

# add columns for counting which metrics are available for each dataset 
md<- dcast(metadata_per_dataset_per_metric, Datasource_ID ~ Metric, fill = "0" )
md$ab<- ""; md$ab [md$abundance == 1]<- "A" # 
md$Rich<-  ""; md$Rich [md$richness == 1]<- "R"
md$comp<- "" ; md$comp [md$ENSPIE == 1]<- "C"
md$Metrics<- paste0(md$ab, md$Rich, md$comp) # code for metrics

metadata_per_dataset<- merge( metadata_per_dataset, md[, c(1, ncol(md))], all = T)
write.csv(metadata_per_dataset, file = "/Table S1.csv")


# metadata per metric: 
metadata_per_metric<- completeData2021 %>% 
	group_by(Metric, Realm) %>%
	summarise(
		Nb_datasets = length(unique(Datasource_ID	)), 
		Nb_plots = length(unique(Plot_ID))
	)
print(metadata_per_metric, n = Inf)

dcast(metadata_per_metric, Metric ~ Realm, value.var = "Nb_datasets")
dcast(metadata_per_metric, Metric ~ Realm, value.var = "Nb_plots")

# object with start and end years of each dataset 
startEndYears<- completeData2021 %>% 
  group_by(Plot_ID) %>%
  summarise(
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T), 
    nYrs = length(unique(Year)),
    )
startEndYears$pltStartYr<- paste0(startEndYears$Plot_ID, "_", startEndYears$Start_year)




cDrichness <- subset(completeData2021, Metric == "richness"); 
cDrichness <- subset(cDrichness, Datasource_ID != 1394); dim(cDrichness)
cDabund    <- subset(completeData2021, Metric == "abundance"); dim(cDabund)
cDenspie   <- subset(completeData2021, Metric == "ENSPIE")   ;dim(cDenspie)
	cDenspie$Number[is.infinite(cDenspie$Number)] <- 0
cDhorn     <- subset(completeData2021, Metric == "Horn"); dim(cDhorn)
	cDhorn$dif <-  cDhorn$Number - cDhorn$ExpectedBeta
	cDhorn$SES <- (cDhorn$Number - cDhorn$ExpectedBeta) / cDhorn$SDexpectedBeta
	cDhorn<- cDhorn[!cDhorn$pltYr %in% startEndYears$pltStartYr, ] ;dim(cDhorn)
cDbray     <- subset(completeData2021, Metric == "Bray"); dim(cDbray)
	cDbray$dif <-  cDbray$Number - cDbray$ExpectedBeta
	cDbray$SES <- (cDbray$Number - cDbray$ExpectedBeta) / cDbray$SDexpectedBeta
	cDbray<- cDbray[!cDbray$pltYr %in% startEndYears$pltStartYr, ]; dim(cDbray)
cDjaccard  <- subset(completeData2021, Metric == "Jaccard") ; dim(cDjaccard)
	cDjaccard$Number[is.nan(cDjaccard$Number)] <- NA
	cDjaccard$dif <-  cDjaccard$Number - cDjaccard$ExpectedBeta
	cDjaccard$SES <- (cDjaccard$Number - cDjaccard$ExpectedBeta) / cDjaccard$SDexpectedBeta
	cDjaccard<- cDjaccard[!cDjaccard$pltYr %in% startEndYears$pltStartYr, ] ; dim(cDjaccard)
cDlogNr80   <- subset(completeData2021, Metric ==  "logNr80"   )       ; dim(cDlogNr80)   
cDlogNr20   <- subset(completeData2021, Metric == "logNr20"    ); dim(cDlogNr20)  
cDlogNr10   <- subset(completeData2021, Metric == "logNr10"    ); dim(cDlogNr10)  
cDlogNr90   <- subset(completeData2021, Metric ==  "logNr90"   ); dim(cDlogNr90)  
cDdom       <- subset(completeData2021, Metric == "dominanceRel"); dim(cDdom)  
cDshan  <- subset(completeData2021, Metric == "Shannon"); dim(cDshan)  
cDlogNr020   <- subset(completeData2021, Metric ==  "logNr020"   ); dim(cDlogNr020)  
cDlogNr2040   <- subset(completeData2021, Metric ==  "logNr2040"   ); dim(cDlogNr2040)  
cDlogNr4060   <- subset(completeData2021, Metric ==  "logNr4060"   ); dim(cDlogNr4060)  
cDlogNr6080   <- subset(completeData2021, Metric ==  "logNr6080"   ); dim(cDlogNr6080)  
cDlogNr80100   <- subset(completeData2021, Metric ==  "logNr80100"   ); dim(cDlogNr80100)  
cDlogQ1   <- subset(completeData2021, Metric ==  "logNrQ1"   ); dim(cDlogQ1)  
cDlogQ2   <- subset(completeData2021, Metric ==  "logNrQ2"   ); dim(cDlogQ2)  
cDlogQ3   <- subset(completeData2021, Metric ==  "logNrQ3"   ); dim(cDlogQ3)  
cDlogQ4   <- subset(completeData2021, Metric ==  "logNrQ4"   ); dim(cDlogQ4)  



# how many datasets and sites per coninent for each of the main metrics? 

metadata_abundance<- cDabund %>%
	group_by (Datasource_ID, Datasource_name) %>%
	summarise(
		Continent = unique(Continent), 
		Region = unique(Region),
		Realm = unique(Realm), 
		n_plots= length(unique(Plot_ID))
	)
ab<- data.frame(metric = "Abundance",  table(metadata_abundance$Realm, metadata_abundance$Continent))


metadata_richness<- cDrichness %>%
	group_by (Datasource_ID) %>%
	summarise(
		Continent = unique(Continent), 
		Region = unique(Region),
		Realm = unique(Realm), 
		n_plots= length(unique(Plot_ID))
	)
rich<- data.frame(metric = "Richness",  table(metadata_richness$Realm, metadata_richness$Continent))
dcast(metadata_richness, Continent ~ Realm, value.var = "n_plots" , sum)

propEurNA<- table(metadata_richness$Continent)

metadata_PIE<- cDenspie %>%
	group_by (Datasource_ID) %>%
	summarise(
		Continent = unique(Continent), 
		Region = unique(Region),
		Realm = unique(Realm), 
		n_plots= length(unique(Plot_ID))
	)
comp<- data.frame(metric = "Composition",  table(metadata_PIE$Realm, metadata_PIE$Continent))
data_availability<- dcast(rbind(ab, rich, comp), factor(Var2)  ~  #, levels = c("North America" "Africa"        "Oceania"       "Europe"        "Latin America" "Asia" )
				factor(metric, levels = c("Abundance", "Richness", "Composition" )) + 
				factor(Var1, levels = c("Terrestrial", "Freshwater")   )); data_availability
colSums(data_availability[, -1], na.rm = T)

metadata_beta<- cDhorn %>%
	group_by (Datasource_ID) %>%
	summarise(
		Continent = unique(Continent), 
		Region = unique(Region),
		Realm = unique(Realm), 
		n_plots= length(unique(Plot_ID))
	)
table(metadata_beta$Continent, metadata_beta$Realm)




#load("all.results.RData")
load("metadata_per_dataset richness 20220228.RData")
load("metadata_per_plot richness 20220228.RData")
all.selectedIns<-  readRDS("all.selectedIns.RDS")

# clean theme
theme_clean<- theme_grey() + theme(panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), 
                                 axis.line = element_line(colour = "black") , 
                                 legend.key=element_blank())

col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4")
col.scheme.realm2 <- c(  "Freshwater"  = "dodgerblue2", "Freshwater2"  = "dodgerblue2",  "Terrestrial" = "chocolate4", "Terrestrial2" = "chocolate4")
col.scheme.realm3<-c(  "Fw"  = "dodgerblue2", "Ter" = "chocolate4")

#col.scheme.realm<- c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4", "Both realms" = "grey50")  #coral4
shps<- c("Freshwater" = 24, "Terrestrial" = 21 )#, "Both realms" = 22)

col.scheme.global<- c(  "Global"  = "grey10", "Observed" = "grey70")  #
col.scheme.black<- c(  "Global"  = "black", "Observed" = "black")  #

sz = 0.5# symbol size
brks<- c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
e<- c("","","","","","","")
lims<- c(-0.03,0.036)


# set working directory to wherever the outputs from the Inla models are stored
setwd("/model outputs richness paper")




#Table S1 Data availability: #####

# total datasets
length(unique(subset(completeData2021, Metric != "Biomass")$Datasource_ID)) #171


metadata_metrics<- completeData2021 %>%
	group_by (Metric, Realm) %>%
	summarise(
		n_datasets = length(unique(Datasource_ID)),
		n_plots= length(unique(Plot_ID))
	)
metadata_metrics2 <- metadata_metrics # just to keep the original metadata file (see next section)
plotRealm<- dcast(metadata_metrics, Metric ~ Realm, value.var = "n_plots" )
names(plotRealm)<- c("Metric"  ,"F_plots",  "T_plots")
DsRealm<- dcast(metadata_metrics, Metric ~ Realm , value.var = "n_datasets")
names(DsRealm)<- c("Metric"  ,      "F_datasets" , "T_datasets")
metadata_metrics<- merge(DsRealm, plotRealm)
metadata_metrics$all_datasets<- rowSums(metadata_metrics[, c("F_datasets", "T_datasets")], na.rm = T)
metadata_metrics$all_plots<- rowSums(metadata_metrics[, c("F_plots", "T_plots")], na.rm = T)

#  order columns
metadata_metrics[    , c(1,6,7,3,5,2,4)   ]






# ALPHA METRICS#####
# make metadata 
metadata_metrics2$Metric<- as.character(metadata_metrics2$Metric)
metadata_metrics2$x <- NA # location for plotting in graphs
metadata_metrics2$y <- NA
metadata_metrics2$y[ metadata_metrics2$Realm == "Terrestrial"] <- 400
metadata_metrics2$y[ metadata_metrics2$Realm == "Freshwater"] <- 150
metadata_metrics2$x <- 0.01
metadata_metrics2$Metric[metadata_metrics2$Metric == "richness"]<- "Richness"
metadata_metrics2$Metric[metadata_metrics2$Metric == "abundance"]<- "Abundance"
metadata_metrics2$Metric[metadata_metrics2$Metric == "ENSPIE"]<- "Evenness"
metadata_metrics2$Metric[metadata_metrics2$Metric == "rarefiedRichness"]<- "Rarefied richness"
#metadata_metrics2$Metric[metadata_metrics2$Metric == "Shannon"]<- "ENS-Shannon"

metadata_metrics2$text<- paste(  metadata_metrics2$n_datasets, "|", metadata_metrics2$n_plots) # number of datasets and plots for graph
metadata_metrics2<- subset(metadata_metrics2, Metric %in% c("Abundance", "Richness", "Rarefied richness", "Evenness"  ))
metadata_metrics2$Metric<- factor(metadata_metrics2$Metric, levels = c ("Abundance", "Richness", "Rarefied richness", "ENS-Shannon", "Evenness" ))


# load inla outputs for univariate models
inlaRichSum<- as.data.frame(readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaRichnessSUMMARY.rds"))
richRandom<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaRichnessrandomSlopes.rds")
richMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaRichnessMarginal.rds")
richMarg$Metric<- "Richness"

inlaRarRichSum<- as.data.frame(readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaRarRichSUMMARY.rds"))
rarRichMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaRarRichMarginal.rds")
rarRichMarg$Metric<- "Rarefied richness"

inlaAbSum<- as.data.frame(readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaAbunSUMMARY.rds"))
abRandom<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaAbunrandomSlopes.rds")
abMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaAbunMarginal.rds" )
abMarg$Metric<- "Abundance"

inlapieSum<- as.data.frame(readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaENSPIESUMMARY.rds"))
pieMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaENSPIEMarginal.rds" )
pieMarg$Metric<- "Evenness"



# Fig 1 ######
univar<- rbind(abMarg, richMarg, rarRichMarg,  pieMarg)
univar<- subset(univar, Metric %in% c("Abundance", "Richness", "Rarefied richness", "Evenness"  ))
univar$Metric<- factor(univar$Metric, levels = c ("Abundance", "Richness", "Rarefied richness", "Evenness"  ))
# clean up the long tail of almost 0

richMarg$y  [ richMarg$Realm == "Terrestrial" & richMarg$x< -0.002826936 ]<- 0
richMarg$y[richMarg$y <1]
hist(richMarg$y)

brks<- c(-0.02, -0.01, -0.005, 0, 0.005, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")

ggplot(subset(univar, y>1)  , aes(x = x, y = y))+
	geom_vline(xintercept = -0.012 )+
	geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
	geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
	geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
	geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
	geom_vline(xintercept = 0)+
	facet_grid(rows = vars(Metric), switch = "y")+
	ylab ("")+  xlab("Trend slope  \n % change per year")+
	scale_fill_manual(values = col.scheme.realm)+
	scale_x_continuous(breaks = brks, labels = l, limits=c(-0.012,  0.012))+
	
	geom_text(aes(x = x, y = y, label = text), data = subset(metadata_metrics2, Realm == "Terrestrial" ), size = 3, color = "chocolate4")+
	geom_text(aes(x = x, y = y, label = text), data = subset(metadata_metrics2, Realm == "Freshwater" ), size = 3, color = "dodgerblue2")+
	
	theme_classic()+
	theme(axis.text.y=element_blank(),
				axis.ticks.y=element_blank(), 
				legend.key=element_blank(), 
				legend.position="bottom", 
				strip.text.y.left = element_text(size=10, angle=0, hjust = 1),
				strip.background = element_rect(colour = "white"))

ggsave(filename = "Fig 1a univar.png" , path = figure_path, width = 12, height = 12,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig 1a univar.pdf" , path = figure_path, width = 12, height = 12,  units = "cm",dpi = 300, device = "pdf")

# table of mean estimates 
Realms<- c("Freshwater", "Terrestrial" )
mnChangeEsts<- rbind(
cbind(Metric = "Abundance",         Realms, inlaAbSum[ 3:4, c(1,2,5, 13)] ), 
cbind(Metric = "Richness",          Realms, inlaRichSum[ 3:4, c(1,2,5, 13)] ), 
cbind(Metric = "Rarefied richness", Realms, inlaRarRichSum[ 3:4, c(1,2,5, 13)] ), 
cbind(Metric = "ENS-PIE",           Realms, inlapieSum[ 3:4, c(1,2,5, 13)] ) 
)

# convert to % change
mnChangeEsts$lower2.5Perc10Yr <- (10^(mnChangeEsts$`0.025quant`*10 )-1)   *100
mnChangeEsts$meanPerc10Yr <- (10^(mnChangeEsts$mean*10 )-1)   *100
mnChangeEsts$upper97.5Perc10Yr <- (10^(mnChangeEsts$`0.975quant`*10 )-1)   *100

mnChangeEsts$lower2.5PercYr <- (10^(mnChangeEsts$`0.025quant` )-1)   *100
mnChangeEsts$meanPercYr <- (10^(mnChangeEsts$mean )-1)   *100
mnChangeEsts$upper97.5PercYr <- (10^(mnChangeEsts$`0.975quant` )-1)   *100
mnChangeEsts$Realm2<- mnChangeEsts$Realms
mnChangeEsts$Metric2<- mnChangeEsts$Metric
mnChangeEsts









# Fig 3 BETA DIVERSITY #####
setwd("C:/Users/rk59zeqi/Documents/model outputs richness paper")
inlaJacSum<- as.data.frame(readRDS("inlaJaccardSUMMARY.rds"))
jacMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaJaccardMarginal.rds" )
jacMarg$Metric<- "Jaccard"

inlaJacDifSum<- as.data.frame(readRDS("InlaJacDifSUMMARY.rds"))
jacdifMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaJacDifMarginal.rds" )
jacdifMarg$Metric<- "Jaccard"

inlaHornSum<- as.data.frame(readRDS("inlaHornSUMMARY.rds"))
hornMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaHornMarginal.rds" )
hornMarg$Metric<- "Morisita-Horn"

inlaHornDifSum<- as.data.frame(readRDS("inlaHornDifSUMMARY.rds"))
horndifMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaHornDifMarginal.rds" )
horndifMarg$Metric<- "Morisita-Horn"


# use onlu corrected beta diversity 
betaMarg<- rbind(jacdifMarg, horndifMarg)
betaMarg$Metric<- ordered(betaMarg$Metric, 
												levels = (c("Jaccard", "Morisita-Horn" )))


ggplot(subset(betaMarg, y >1), aes(x = x, y = y))+
	#	geom_line( )+
	geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
	geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
	geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
	geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
	geom_vline(xintercept = 0)+
	facet_grid(rows = vars(Metric), switch="y")+
	ylab ("")+  xlab("Trend slope")+
	scale_fill_manual(values = col.scheme.realm)+
#	scale_x_continuous(breaks = brks, limits=c(0,  0.012))+
	theme_classic()+
	theme(axis.text.y=element_blank(),
				axis.ticks.y=element_blank(), 
				legend.key=element_blank(), 
				legend.position="bottom", 
				strip.text.y.left = element_text(size=10, angle=0, hjust = 1),
				strip.background = element_rect(colour = "white")
	)
ggsave(filename = "Fig 3 beta.png" , path = figure_path, width = 12, height = 6,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig 3 beta.pdf" , path = figure_path, width = 12, height = 6,  units = "cm",dpi = 300, device = "pdf")








# SAD changes #####
# number of dominant and rare species: (upper and lower 20% of abundance ) 
setwd("C:/Users/rk59zeqi/Documents/model outputs richness paper")

sads<- subset(completeData2021, Metric ==  "logNr020" |  Metric ==   "logNr2040"|  Metric == "logNr4060" |  Metric == "logNr6080"|  Metric ==  "logNr80100")
piv<- dcast(subset(sads, !is.na(Number)), Plot_ID+ Year ~ Metric, value.var = "Number", mean)
pivot_wider(sads, id_cols = Plot_ID, from = Metric, values_from = Number, values_fn = mean)

quantile80100Sum<- as.data.frame(readRDS("quantile80100SUMMARY.rds"))
q80100Marg<- readRDS("quantile80100Marginal.rds" )
q80100Marg$Metric<- "80-100%"

quantile6080Sum<- as.data.frame(readRDS("quantile6080SUMMARY.rds"))
q6080Marg<- readRDS("quantile6080Marginal.rds" )
q6080Marg$Metric<- "60-80%"

quantile4060Sum<- as.data.frame(readRDS("quantile4060SUMMARY.rds"))
q4060Marg<- readRDS("quantile4060Marginal.rds" )
q4060Marg$Metric<- "40-60%"

quantile2040Sum<- as.data.frame(readRDS("quantile2040SUMMARY.rds"))
q2040Marg<- readRDS("quantile2040Marginal.rds" )
q2040Marg$Metric<- "20-40%"

quantile020Sum<- as.data.frame(readRDS("quantile020SUMMARY.rds"))
q020Marg<- readRDS("quantile020Marginal.rds" )
q020Marg$Metric<- "0-20%"


quantilesData<- rbind(q020Marg, q2040Marg,q4060Marg, q6080Marg, q80100Marg)
quantilesData$Metric<- ordered(quantilesData$Metric, 
												levels = (c("0-20%", "20-40%",  "40-60%", "60-80%", "80-100%" )))


quantilesDataNeg<- quantilesData # duplicate for symetrical graphs
#quantilesDataNeg$x<- quantilesDataNeg$x+0.000000001
quantilesDataNeg[, 4:7] <- -quantilesDataNeg[, 4:7]
quantilesDataNeg$Realm[quantilesDataNeg$Realm == "Terrestrial"]<- "Terrestrial2"
quantilesDataNeg$Realm[quantilesDataNeg$Realm == "Freshwater"]<- "Freshwater2"

quantilesData<- rbind(quantilesData, quantilesDataNeg)

# cut off tails 
quantilesData <- quantilesData[quantilesData$x <0.01 & quantilesData$x >-0.005 , ]


brks<- c(-0.02, -0.01, -0.005, 0, 0.005, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")

ggplot(subset(quantilesData, y>1 | y< -1 ), aes(x = x, y = y))+
	#	geom_line( )+
	geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
	geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
	geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
	geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
	geom_vline(xintercept = 0)+
	coord_flip()+
	facet_grid(cols = vars(Metric), switch = "x")+
	ylab ("SAD interval")+  xlab("Trend slope  \n % change per year")+
	scale_fill_manual(values = col.scheme.realm2)+
	scale_x_continuous(breaks = brks,labels = l)+#, limits=c(-0.005,  0.01))+
	#ggtitle("Number of species per SAD interval")+
	theme_classic()+
	theme(axis.text.x=element_blank(),
				axis.ticks.x=element_blank(), 
				legend.key=element_blank(), 
				legend.position="bottom", 
				strip.background = element_rect(colour = "white")
	)

ggsave(filename = "Fig 2 SAD changes.png" , path = figure_path, width = 12, height = 12,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig 2 SAD changes.pdf" , path = figure_path, width = 12, height = 12,  units = "cm",dpi = 300, device = "pdf")


Realms<- c("Freshwater", "Terrestrial" )
quantChangeEsts<- rbind(
	cbind(Metric = "0-20%",         Realms, quantile020Sum[ 3:4, c(1,2,5, 13)] ), 
	cbind(Metric = "20-40%",          Realms, quantile2040Sum[ 3:4, c(1,2,5, 13)] ), 
	cbind(Metric = "40-60%", Realms, quantile4060Sum[ 3:4, c(1,2,5, 13)] ), 
	cbind(Metric = "60-80%",       Realms, quantile6080Sum[ 3:4, c(1,2,5, 13)] ), 
	cbind(Metric = "80-1000%",           Realms, quantile80100Sum[ 3:4, c(1,2,5, 13)] ) 
)


# calculate percent change
quantChangeEsts$lower2.5Perc10Yr <- (10^(quantChangeEsts$`0.025quant`*10 )-1)   *100
quantChangeEsts$meanPerc10Yr <- (10^(quantChangeEsts$mean*10 )-1)   *100
quantChangeEsts$upper97.5Perc10Yr <- (10^(quantChangeEsts$`0.975quant`*10 )-1)   *100

quantChangeEsts$lower2.5PercYr <- (10^(quantChangeEsts$`0.025quant` )-1)   *100
quantChangeEsts$meanPercYr <- (10^(quantChangeEsts$mean )-1)   *100
quantChangeEsts$upper97.5PercYr <- (10^(quantChangeEsts$`0.975quant` )-1)   *100
quantChangeEsts









# Fig S7 10% and 90% brackets #####

#Fig 
quantile90100Sum<- as.data.frame(readRDS("upper10percSUMMARY.rds"))
q90100Marg<- readRDS("upper10percMarginal.rds" )
q90100Marg$Metric<- "90-100%"


quantile010Sum<- as.data.frame(readRDS("lower10percSUMMARY.rds"))
q010Marg<- readRDS("lower10percMarginal.rds" )
q010Marg$Metric<- "0-10%"

decentilesData<- rbind(q010Marg, q90100Marg)
decentilesData <- decentilesData[decentilesData$x <0.01 & decentilesData$x >-0.005 , ]


decentilesDataNeg<- decentilesData
#decentilesDataNeg$x<- decentilesDataNeg$x+0.000000001
decentilesDataNeg[, 4:7] <- -decentilesDataNeg[, 4:7]
decentilesDataNeg$Realm[decentilesDataNeg$Realm == "Terrestrial"]<- "Terrestrial2"
decentilesDataNeg$Realm[decentilesDataNeg$Realm == "Freshwater"]<- "Freshwater2"

decentilesData<- rbind(decentilesData, decentilesDataNeg)

# cut off tails 
decentilesData <- decentilesData[decentilesData$x <0.01 & decentilesData$x >-0.005 , ]


ggplot(decentilesData, aes(x = x, y = y))+
	#	geom_line( )+
	geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
	geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
	geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
	geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
	geom_vline(xintercept = 0)+
	coord_flip()+
	facet_grid(cols = vars(Metric), switch = "x")+
	ylab ("SAD interval")+  xlab("Trend slope")+
	scale_fill_manual(values = col.scheme.realm2)+
	scale_y_continuous(breaks = brks)+#, limits=c(-0.005,  0.009))+
	#ggtitle("Number of species in upper and lower \n 10% brackets")+
	theme_classic()+
	theme(axis.text.x=element_blank(),
				axis.ticks.x=element_blank(), 
				legend.key=element_blank(), 
				legend.position="bottom", 
				strip.background = element_rect(colour = "white")
	)

ggsave(filename = "Fig S6 upper and lower decentile.png" , path = figure_path, width = 6, height = 12,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig S6 upper and lower decentile.pdf" , path = figure_path, width = 6, height = 12,  units = "cm",dpi = 300, device = "pdf")

Realms<- c("Freshwater", "Terrestrial" )
decChangeEsts<- rbind(
	cbind(Metric = "0-10%",         Realms, quantile010Sum[ 3:4, c(1,2,5, 13)] ), 
	cbind(Metric = "90-100%",      Realms, quantile90100Sum[ 3:4, c(1,2,5, 13)] ) 
)

decChangeEsts$lower2.5Perc10Yr <- (10^(decChangeEsts$`0.025quant`*10 )-1)   *100
decChangeEsts$meanPerc10Yr <- (10^(decChangeEsts$mean*10 )-1)   *100
decChangeEsts$upper97.5Perc10Yr <- (10^(decChangeEsts$`0.975quant`*10 )-1)   *100

decChangeEsts$lower2.5PercYr <- (10^(decChangeEsts$`0.025quant` )-1)   *100
decChangeEsts$meanPercYr <- (10^(decChangeEsts$mean )-1)   *100
decChangeEsts$upper97.5PercYr <- (10^(decChangeEsts$`0.975quant` )-1)   *100
decChangeEsts






#relations between abundance, PIE  and richness slopes #####
abRandom      <- readRDS("inlaAbunrandomSlopes.rds")
richRandom    <- readRDS("inlaRichnessrandomSlopes.rds")
enspieRandom  <- readRDS("inlaENSPIErandomSlopes.rds")
rarRichRandom <- readRDS("inlaRarRichrandomSlopes.rds")
jacRandom     <- readRDS("inlaJaccardrandomSlopes.rds")
jacDifRandom     <- readRDS("inlaJacDifrandomSlopes.rds")
hornRandom     <- readRDS("inlaHornrandomSlopes.rds")
hornDifRandom     <- readRDS("inlaHornDifrandomSlopes.rds")


richRandom$richnessSlope<- richRandom$slope
richRandom$richnessSlopeSD<- richRandom$`DataID_Slope_ sd`
abRandom$abundanceSlope <- abRandom$slope
abRandom$abundanceSlopeSD <- abRandom$`DataID_Slope_ sd`
enspieRandom$enspieSlope<- enspieRandom$slope
rarRichRandom$rarRichSlope <- rarRichRandom$slope
jacRandom$jacSlope     <- jacRandom$slope
jacDifRandom$jacDifSlope     <- jacDifRandom$slope
hornRandom$hornSlope     <- hornRandom$slope
hornDifRandom$hornDifSlope     <- hornDifRandom$slope

randomSlopes<- merge(richRandom[, c("Realm", "Datasource_ID", "Datasource_name", "richnessSlope", "richnessSlopeSD") ],  
										 abRandom[, c("Realm", "Datasource_ID", "Datasource_name", "abundanceSlope", "abundanceSlopeSD")], all = T)
randomSlopes<- merge(randomSlopes,  enspieRandom[, c("Realm", "Datasource_ID", "Datasource_name", "enspieSlope")], all = T)
randomSlopes<- merge(randomSlopes,  rarRichRandom[, c("Realm", "Datasource_ID", "Datasource_name", "rarRichSlope")], all = T)
head(randomSlopes)
randomSlopes<- merge(randomSlopes,  hornRandom[, c("Realm", "Datasource_ID", "Datasource_name", "hornSlope")], all = T)
randomSlopes<- merge(randomSlopes,  hornDifRandom[, c("Realm", "Datasource_ID", "Datasource_name", "hornDifSlope")], all = T)
randomSlopes<- merge(randomSlopes,  jacRandom[, c("Realm", "Datasource_ID", "Datasource_name", "jacSlope")], all = T)
randomSlopes<- merge(randomSlopes,  jacDifRandom[, c("Realm", "Datasource_ID", "Datasource_name", "jacDifSlope")], all = T)

# rename realms for fig 
randomSlopes$Realm2<- NA
randomSlopes$Realm2[randomSlopes$Realm == "Freshwater"] <- "Fw"
randomSlopes$Realm2[randomSlopes$Realm == "Terrestrial"] <- "Ter"
# assign colors: 



# min and max slopes
(10^(min(subset(randomSlopes, Realm == "Terrestrial")$richnessSlope, na.rm = T))-1)   *100
(10^(max(subset(randomSlopes, Realm == "Terrestrial")$richnessSlope, na.rm = T))-1)   *100
(10^(min(subset(randomSlopes, Realm == "Freshwater")$richnessSlope, na.rm = T))-1)   *100
(10^(max(subset(randomSlopes, Realm == "Freshwater")$richnessSlope, na.rm = T))-1)   *100


(10^(min(randomSlopes$abundanceSlope, na.rm = T))-1)   *100
(10^(max(randomSlopes$abundanceSlope, na.rm = T))-1)   *100


# correlation tests

cor.test(randomSlopes$richnessSlope, randomSlopes$abundanceSlope)
cor.test(randomSlopes$richnessSlope, randomSlopes$enspieSlope)
cor.test(randomSlopes$richnessSlope, randomSlopes$rarRichSlope)
cor.test(randomSlopes$richnessSlope, randomSlopes$hornDifSlope)

cor.test(randomSlopes$abundanceSlope, randomSlopes$enspieSlope)
cor.test(randomSlopes$abundanceSlope, randomSlopes$rarRichSlope)
cor.test(randomSlopes$enspieSlope, randomSlopes$rarRichSlope)

# plot all correlations
library(GGally, quietly = TRUE)
library(grid, quietly = TRUE)

labs <- c(
	richnessSlope = "Richness",
	abundanceSlope = "Abundance",
	rarRichSlope = "Rarefied richness",
	shanSlope = "ENS Shannon",
	enspieSlope = "Evenness",
#	hornSlope = "Moristita-Horn",
	hornDifSlope = "Morisita-Horn",
#	jacSlope = "Jaccard",
	jacDifSlope = "Jaccard"
)


# Load theme for ggplot2
theme_set(theme_bw())
# hack color scheme
ggplot <- function(...) ggplot2::ggplot(...) + 	scale_color_manual(values = col.scheme.realm3) +
	scale_fill_manual(values = col.scheme.realm3) 
unlockBinding("ggplot",parent.env(asNamespace("GGally")))
assign("ggplot",ggplot,parent.env(asNamespace("GGally")))

# Generate plot
ggpairs(randomSlopes, columns =  c(  "abundanceSlope","richnessSlope", "rarRichSlope" ,  "enspieSlope",  "jacDifSlope",  "hornDifSlope" ),
				axisLabels="show", 
				labeller = labeller(.default = labs), 
				mapping = ggplot2::aes(color = Realm2, alpha = 0.5))+
	geom_hline(yintercept=0,linetype="dashed")+
	geom_vline(xintercept=0,linetype="dashed")




ggsave(filename = "Fig S5 all correlations.png" , path = figure_path, width = 21, height = 21,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig S5 all correlations.pdf" , path = figure_path, width = 21, height = 21,  units = "cm",dpi = 300, device = "pdf")



out = 100
# run 100 randomized models with measurement error (sampling values from the random predictions 
predList<- NULL

myFun <- function(i,dat){
	dat<- dat[complete.cases(dat), ]; dim(dat)
	dat$myX <- rnorm(n=nrow(dat), mean=dat$abundanceSlope ,sd=dat$abundanceSlopeSD)
	dat$myY <- rnorm(n=nrow(dat), mean=dat$richnessSlope  ,sd=dat$richnessSlopeSD )
	
	
	
	predDF<- rbind(
		data.frame(Realm = "Terrestrial", 
							 myX = seq(from = min(dat$abundanceSlope [dat$Realm == "Terrestrial"], na.rm = T), to =max(dat$abundanceSlope [dat$Realm == "Terrestrial"], na.rm = T), length.out = out)), 
		data.frame(Realm = "Freshwater", 
							 myX = seq(from = min(dat$abundanceSlope [dat$Realm == "Freshwater"], na.rm = T), to =max(dat$abundanceSlope [dat$Realm == "Freshwater"], na.rm = T), length.out = out )))
	predDF$myY <- NA
	predDF$Datasource_ID <-0
#	predDF$x<- NA;        predDF$x.sd<- NA;            predDF$y<- NA;        predDF$y.sd<- NA 
	
	dat1<- rbind(dat[, names(predDF)], predDF)  	

	
	inla2 <- inla(myY ~ myX * Realm, data = dat1, 
								control.predictor = list(compute=TRUE),)
	preds<- list (cbind(dat1, inla2$summary.fitted.values)[is.na(dat1$myY),]) # take only predicted values for NA's
	names(preds)<- i
	
	predList <- append( predList, preds)
	return(predList)
	
}

predictions <- lapply(1:100, function(i) myFun(i,randomSlopes)) 
str(predictions)


# get regression coefficients for easier plotting 
regfun<- function(i,  mylist){
	lst<- as.data.frame(mylist[[i]][[1]])
	sm<- (lm(lst$mean ~ lst$myX + lst$Realm ) )
	
	coefDF = rbind(
		data.frame(sim= i, 
							 intercept=sm$coefficients[1],
							 coef=sm$coefficients[2], 
							 Realm = "Freshwater"),
		data.frame(sim= i, 
							 intercept=sm$coefficients[1] + sm$coefficients[3],
							 coef=sm$coefficients[2], 
							 Realm = "Terrestrial") )
	
	return(coefDF)
}

sampledCoefs <- lapply(1:length(predictions), function(i) regfun(i,predictions)) 
sampledCoefs <- do.call(rbind,sampledCoefs)



alpha<- 0.02


ggplot(randomSlopes, aes(x=abundanceSlope, y=richnessSlope, color =Realm)) +
	geom_point()+
	geom_errorbar(aes(ymin = richnessSlope -richnessSlopeSD , ymax = richnessSlope +richnessSlopeSD), alpha = 0.5)+
	geom_errorbarh(aes(xmin = abundanceSlope -abundanceSlopeSD , xmax = abundanceSlope +abundanceSlopeSD), alpha = 0.5)+
	
	geom_hline(yintercept=0,linetype="dashed")+
	geom_vline(xintercept=0,linetype="dashed")+
	scale_color_manual(values = col.scheme.realm) +
	scale_fill_manual(values = col.scheme.realm) +
	geom_abline(data = sampledCoefs,   # predicted lines 
							aes(intercept = intercept, slope = coef, color = Realm),
							alpha=0.1)+
	geom_abline(intercept=median(subset(sampledCoefs, Realm == "Terrestrial")$intercept), # median fit 
							slope=median(sampledCoefs$coef), colour="chocolate4", size = 1)+
	geom_abline(intercept=median(subset(sampledCoefs, Realm == "Freshwater")$intercept), # median fit 
							slope=median(sampledCoefs$coef), colour="dodgerblue2", size = 1)+
	xlab ("Abundance slope")+ ylab ("Richness slope")+
	theme_clean + 
	theme(  strip.background = element_blank(), 
					plot.title = element_text(hjust = 0.5), 
					legend.position = "none")#,

ggsave(filename = "Fig 1b regres.png" , path = figure_path, width = 12, height = 12,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig 1b regress.pdf" , path = figure_path, width = 12, height = 12,  units = "cm",dpi = 300, device = "pdf")

# make plots of marginals
ggplot(randomSlopes, aes( y=richnessSlope, color =Realm)) +
  geom_density()+
#	ggtitle ("richness")+
	scale_color_manual(values = col.scheme.realm) +
	scale_fill_manual(values = col.scheme.realm) +
	ylim(-0.015, 0.018)+
  theme_clean+
	theme(  strip.background = element_blank(), 
				plot.title = element_text(hjust = 0.5), 
				legend.position = "none")#,

ggsave(filename = "Fig 1b margRich.png" , path = figure_path, width = 3, height = 12,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig 1b margRich.pdf" , path = figure_path, width = 3, height = 12,  units = "cm",dpi = 300, device = "pdf")

ggplot(randomSlopes, aes( abundanceSlope, color =Realm)) +
	geom_density()+
#	ggtitle ("abundance")+
	scale_color_manual(values = col.scheme.realm) +
	scale_fill_manual(values = col.scheme.realm) +
	xlim(-0.05, 0.025)+
	theme_clean+
	theme(  strip.background = element_blank(), 
					plot.title = element_text(hjust = 0.5), 
					legend.position = "none")#,

ggsave(filename = "Fig 1b margAbun.png" , path = figure_path, width = 12, height = 3,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig 1b margAbun.pdf" , path = figure_path, width = 12, height = 3,  units = "cm",dpi = 300, device = "pdf")





# biivariate map #####
# richness results: assign 'significant' and non significant slopes at 80% CI 
richRandom$richlwr <- richRandom$`DataID_Slope_ 0.1quant` + richRandom$fixedSlp # add fixed and random slopes for lower CI
richRandom$richupr <-  richRandom$`DataID_Slope_ 0.9quant` + richRandom$fixedSlp # # add fixed and random slopes for upper CI
richRandom$richdir<- NA 
richRandom$richdir[richRandom$richupr <0 ]<- -1 # assign -1 to significant negative slopes
richRandom$richdir[richRandom$richlwr >0]<- 1 # assign +1 to significant positive slopes
richRandom$richdir[richRandom$richlwr<0 & richRandom$richupr>0]<-0 # assign 0 to non-significant slopes

# assign numerical 
abRandom$ablwr = abRandom$`DataID_Slope_ 0.1quant` + abRandom$fixedSlp # same as above
abRandom$abupr = abRandom$`DataID_Slope_ 0.9quant` + abRandom$fixedSlp 
abRandom$abdir<- NA 
abRandom$abdir[abRandom$abupr <0 ]<- -1 
abRandom$abdir[abRandom$ablwr >0]<- 1 
abRandom$abdir[abRandom$ablwr<0 & abRandom$abupr>0]<-0 

# add togteher richnes and abundance
randomSlopes2<- merge(richRandom[, c("Realm", "Datasource_ID", "Datasource_name", "richnessSlope", "richdir") ],  
										 abRandom[, c("Realm", "Datasource_ID", "Datasource_name", "abundanceSlope", "abdir")], all = T)



# assign categories to numbers
randomSlopes2$colo <- NA
randomSlopes2$colo[randomSlopes2$richdir == 0 & randomSlopes2$abdir == 0 ]  <- "none"
randomSlopes2$colo[randomSlopes2$richdir == 1 & randomSlopes2$abdir == 0 ]  <- "richpos0"
randomSlopes2$colo[randomSlopes2$richdir == -1 & randomSlopes2$abdir == 0 ] <- "richneg0"
randomSlopes2$colo[randomSlopes2$richdir == 0 & randomSlopes2$abdir == 1 ]  <- "abpos0"
randomSlopes2$colo[randomSlopes2$richdir == 1 & randomSlopes2$abdir == 1 ]  <- "bothpos"
randomSlopes2$colo[randomSlopes2$richdir == -1 & randomSlopes2$abdir == 1 ] <- "richnegabpos"
randomSlopes2$colo[randomSlopes2$richdir == 0 & randomSlopes2$abdir == -1 ]  <- "abneg0"
randomSlopes2$colo[randomSlopes2$richdir == 1 & randomSlopes2$abdir == -1 ]  <- "richposabneg"
randomSlopes2$colo[randomSlopes2$richdir == -1 & randomSlopes2$abdir == -1 ] <- "bothneg"

randomSlopes2<- merge(randomSlopes2, metadata_per_dataset)

unique(randomSlopes2$colo)
col.scheme.bivar<- c(	"none"       = '#a5add3',
											"richpos0"    = '#8c62aa',   
											"richneg0"    = '#ace4e4',
											"abpos0"      = '#5698b9',
											"bothpos"     = '#3b4994',
											"richnegabpos" = '#5ac8c8', 
											"abneg0"      = '#dfb0d6',
											"richposabneg"= '#be64ac',
										  "bothneg"     = '#e8e8e8')
										 
# test colorscheme and if colorings make sense
ggplot(subset(randomSlopes2, !is.na(colo)), aes(x=abundanceSlope, y=richnessSlope, color =colo)) +
	geom_point()+
	geom_hline(yintercept=0,linetype="dashed")+
	geom_vline(xintercept=0,linetype="dashed")+
	scale_color_manual(values = col.scheme.bivar) +
	scale_fill_manual(values = col.scheme.realm) +
	#	geom_smooth(method=lm) +
	scale_x_continuous(breaks = brks,labels = l, limits=c(-0.05,  0.03))+
	scale_y_continuous(breaks = brks,labels = l, limits=c(-0.05,  0.03))+ 	
	#geom_line(data = nwdat, aes(x = abundanceSlope, y = fit, colour = Realm),  linetype = 1)+
	#	geom_text(label=randomSlopes$Datasource_name)+
#	geom_ribbon(data = nwdat, inherit.aes = F,  aes (x = abundanceSlope , ymin = lwr, ymax = upr, fill = Realm ), alpha = 0.3)+
	theme_clean + 
	theme(  strip.background = element_blank(), 
					plot.title = element_text(hjust = 0.5))#,
# looks believable

library(rgdal)
library(sp)
library(broom)



#  ++++ RUN TWICE ####
pts.wgs <- randomSlopes2
pts.wgs$slope.lim<- pts.wgs$colo

pts.wgs <- SpatialPointsDataFrame(coords = data.frame(lon = pts.wgs$Mean_longitude,
																											lat = pts.wgs$Mean_latitude),
																	proj4string = CRS(WGS84),
																	data = pts.wgs)


setwd("C:\\Dropbox\\Insect Biomass Trends/csvs") # work
source("D:/work/2017 iDiv/2018 insect biomass/final-insect-diversity-trends1/map_preparation.R")
####   ++++ RUN TWICE UNTIL HERE ++++ ####


# scale slopes 
library(biscale)
legend <- bi_legend(pal = "DkBlue",
										dim = 3,
										xlab = "Abundance trend",
										ylab = "Richness trend",
										size = 8)

# plot on map  by Datasource # scale is set to be symmetrical over most extreme value
fw.wgs <-
	p.wgsLIGHT+
	geom_path(data=NE_graticules_rob, aes(long, lat, group=group),  color="grey50", size = 0.2) +
	geom_point(data = subset(pts.rob, Realm =="Freshwater" & !is.na(slope.lim) )@data ,  size = 2, #color = "grey30",pch = 21,
						 aes(x = x,   y = y,  color = slope.lim, group = NULL), 
						 position=position_jitter(h=1, w=1)) +  #
	scale_color_manual(values = col.scheme.bivar) +
	theme(legend.position = "none")+
	ggtitle("b. Freshwater fauna") +
	theme(panel.background = element_rect(fill = "grey40", colour = "black"))

fw.wgs


# terrestrial
terr.wgs <-
	p.wgsLIGHT+
	geom_path(data=NE_graticules_rob, aes(long, lat, group=group),  color="grey50", size = 0.2) +
	geom_point(data = subset(pts.rob, Realm =="Terrestrial" & !is.na(slope.lim))@data, size = 2, #pch = 21,color = "grey30" ,
						 aes(x = x,   y = y,  color = slope.lim, group = NULL) , 
						 position=position_jitter(h=1, w=1)) +
	scale_color_manual(values = col.scheme.bivar) +
	theme(legend.position = "none")+
	ggtitle("a. Terrestrial fauna")  +
	theme(panel.background = element_rect(fill = "grey40", colour = "black"))

terr.wgs

library(cowplot)
finalMap <- ggdraw() +
	draw_plot(terr.wgs, 0, 0.5, 1, 0.5) +
	draw_plot(fw.wgs, 0, 0, 1, 0.5) +
	draw_plot(legend, 0.025, .55, 0.18, 0.18)

finalMap
ggsave(filename = "Fig S1 map.png" , path = figure_path, width = 2500, height = 2900,  units = "px",dpi = 300, device = "png")
ggsave(filename = "Fig S1 map.pdf" , path = figure_path, width = 2500, height = 2900,  units = "px",dpi = 300, device = "pdf")






