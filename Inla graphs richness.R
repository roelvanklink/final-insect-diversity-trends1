rm(list=ls()) 

library(INLA)
library(brinla)
library(ggplot2)
library(ggnewscale)
library(tidyverse)
library(reshape2)
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/plotting functions.R")


setwd("C:/Users/roelv/Dropbox/Insect Biomass Trends/csvs") # home
setwd("C:\\Dropbox\\Insect Biomass Trends/csvs") # work
setwd("~/Dropbox/Insect Biomass Trends/csvs")#diana mac
figure_path<- "D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/Figures/"


load("completeData2021pure.Rdata"); dim(completeData2021pure) # 18.8.20 66249 lines,  1676 plots
#load("completeData2021.RData")
completeData2021pure$pltYr<- paste0(completeData2021pure$Plot_ID, "_", completeData2021pure$Year)
completeData2021pure$Continent[completeData2021pure$Continent == "Central America" | completeData2021pure$Continent == "South America" ] <- "Latin America"

exptPlots<- c(5, # alaska
							921, 922, 924,925, #smes
							643, 644, 646, 647, # hemlock removal
							137, 138, 139  #brazil fragmentation experiment
)
exptDatasources<- c(300,1364, 1357,1410) #Kellogg, Luiquillo CTE, Cedar creek big bio, some german grassland

completeData2021pure<- completeData2021pure[!completeData2021pure$Datasource_ID %in% exptDatasources, ]
completeData2021pure<- completeData2021pure[!completeData2021pure$Plot_ID %in% exptPlots, ]
dim(completeData2021pure)

# exclude plots with a trend in taxonomic resolution: 
bad_tax<- 
	c(849L, 132L, 1442L, 1527L, 10001L, 10003L, 10005L, 10006L, 10007L, 10008L, 10009L, 10011L, 10012L, 10015L, 10016L, 10017L, 10018L, 
		10019L, 10021L, 10024L, 10025L, 10026L, 10028L, 10029L, 10032L, 10033L, 10034L, 10035L, 10036L, 10038L, 10039L, 10044L, 10048L, 
		10049L, 10051L, 10052L, 10056L, 10057L, 10058L, 10059L, 10063L, 10066L, 10067L, 10068L, 10070L, 10072L, 10075L, 10079L, 10080L, 
		10081L, 10082L, 10084L, 10085L, 10087L, 10092L, 10093L, 10097L, 10099L, 10106L, 10108L, 10113L, 10114L, 10115L, 10119L, 10124L, 
		10125L, 10127L, 10128L, 10130L, 10135L, 10137L, 10138L, 10145L, 10146L, 10147L, 10149L, 10158L, 10160L, 10163L, 10164L, 10165L, 
		10168L, 10169L, 10170L, 10171L, 10173L, 10175L, 10178L, 10180L, 10181L, 10182L, 10185L, 10187L, 10188L, 10189L, 10192L, 10193L, 
		10195L, 10196L, 10198L, 10349L, 10200L, 10201L, 10204L, 10206L, 10208L, 10212L, 10215L, 10216L, 10219L, 10221L, 10223L, 10224L, 
		10225L, 10226L, 10227L, 10229L, 10230L, 10233L, 10236L, 10249L, 10250L, 10252L, 10253L, 10254L, 10255L, 10256L, 10257L, 10258L, 
		10259L, 10277L, 10279L, 10280L, 10281L, 10282L, 10283L, 10284L, 10285L, 10286L, 10287L, 10289L, 10290L, 10291L, 10292L, 10294L, 
		10295L, 10297L, 10298L, 10299L, 10301L, 10302L, 10303L, 10304L, 10305L, 10306L, 10307L, 10308L, 10309L, 10311L, 10312L, 10313L, 
		10314L, 10315L, 10316L, 10317L, 10318L, 10320L, 10321L, 10322L, 10323L, 10325L, 10326L, 10327L, 10328L, 10329L, 10331L, 10332L, 
		10334L, 10335L, 10336L, 10337L, 10338L, 10339L, 10340L, 10342L, 10343L, 10344L, 10346L, 10348L, 10469L, 10350L, 10351L, 10352L, 
		10353L, 10354L, 10355L, 10356L, 10357L, 10361L, 10362L, 10364L, 10365L, 10366L, 10368L, 10369L, 10370L, 10371L, 10373L, 10374L, 
		10375L, 10376L, 10378L, 10379L, 10380L, 10381L, 10382L, 10383L, 10384L, 10385L, 10386L, 10387L, 10388L, 10389L, 10390L, 10391L, 
		10392L, 10393L, 10394L, 10395L, 10398L, 10399L, 10400L, 10401L, 10403L, 10404L, 10407L, 10408L, 10409L, 10410L, 10412L, 10413L, 
		10414L, 10415L, 10416L, 10417L, 10418L, 10419L, 10421L, 10422L, 10424L, 10425L, 10426L, 10427L, 10428L, 10429L, 10430L, 10431L, 
		10432L, 10433L, 10434L, 10435L, 10436L, 10437L, 10438L, 10439L, 10475L, 10440L, 10441L, 10442L, 10443L, 10444L, 10445L, 10446L, 
		10448L, 10449L, 10485L, 10452L, 10454L, 10455L, 10457L, 10458L, 10459L, 10460L, 10463L, 10464L, 10465L, 10467L, 10468L, 10470L, 
		10474L, 10499L, 10491L, 10490L, 10498L, 10489L, 10506L, 10503L, 10511L, 10504L, 10500L, 10501L)

completeData2021pure<- completeData2021pure[!completeData2021pure$Plot_ID %in% bad_tax, ]

dim(completeData2021pure)
saveRDS(completeData2021pure, file = "C:\\Dropbox\\Insect Biomass Trends/csvs/Van Klink insects Full final dataframe.rds")



completeData2021<- completeData2021pure

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
subset(metadata_per_dataset, Time_span_yrs == max(metadata_per_dataset$Time_span_yrs))
subset(metadata_per_dataset, Nb_plots == max(metadata_per_dataset$Nb_plots))
subset(metadata_per_dataset, Nb_plots > 50)
sum(metadata_per_dataset$Nb_plots)

metadata_per_dataset_per_metric<- completeData2021 %>% 
	group_by(Datasource_ID, Unit) %>%
	summarise(
		Datasource_name = unique(Datasource_name), 
		count = 1)
md<- dcast(metadata_per_dataset_per_metric, Datasource_ID ~ Unit, fill = "0" )
md$ab<- ""; md$ab [md$abundance == 1]<- "A"
md$Rich<-  ""; md$Rich [md$richness == 1]<- "R"
md$comp<- "" ; md$comp [md$ENSPIE == 1]<- "C"
md$Metrics<- paste0(md$ab, md$Rich, md$comp)

metadata_per_dataset<- merge( metadata_per_dataset, md[, c(1, ncol(md))], all = T)
write.csv(metadata_per_dataset, file = "D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/Figures/Table S1.csv")


metadata_per_metric<- completeData2021 %>% 
	group_by(Unit, Realm) %>%
	summarise(
		Nb_datasets = length(unique(Datasource_ID	)), 
		Nb_plots = length(unique(Plot_ID))
	)
print(metadata_per_metric, n = Inf)

dcast(metadata_per_metric, Unit ~ Realm, value.var = "Nb_datasets")
dcast(metadata_per_metric, Unit ~ Realm, value.var = "Nb_plots")


startEndYears<- completeData2021 %>% 
  group_by(Plot_ID) %>%
  summarise(
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T), 
    nYrs = length(unique(Year)),
    )
startEndYears$pltStartYr<- paste0(startEndYears$Plot_ID, "_", startEndYears$Start_year)




cDrichness <- subset(completeData2021, Unit == "richness"); 
cDrichness <- subset(cDrichness, Datasource_ID != 1394); dim(cDrichness)
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
cDlogNr80   <- subset(completeData2021, Unit ==  "logNr80"   )       ; dim(cDlogNr80)   
cDlogNr20   <- subset(completeData2021, Unit == "logNr20"    ); dim(cDlogNr20)  
cDlogNr10   <- subset(completeData2021, Unit == "logNr10"    ); dim(cDlogNr10)  
cDlogNr90   <- subset(completeData2021, Unit ==  "logNr90"   ); dim(cDlogNr90)  
cDdom       <- subset(completeData2021, Unit == "dominanceRel"); dim(cDdom)  
cDshan  <- subset(completeData2021, Unit == "Shannon"); dim(cDshan)  
cDpielou <- subset(completeData2021, Unit == "Pielou"); dim(cDpilou)  
cDlogNr020   <- subset(completeData2021, Unit ==  "logNr020"   ); dim(cDlogNr020)  
cDlogNr2040   <- subset(completeData2021, Unit ==  "logNr2040"   ); dim(cDlogNr2040)  
cDlogNr4060   <- subset(completeData2021, Unit ==  "logNr4060"   ); dim(cDlogNr4060)  
cDlogNr6080   <- subset(completeData2021, Unit ==  "logNr6080"   ); dim(cDlogNr6080)  
cDlogNr80100   <- subset(completeData2021, Unit ==  "logNr80100"   ); dim(cDlogNr80100)  
cDlogQ1   <- subset(completeData2021, Unit ==  "logNrQ1"   ); dim(cDlogQ1)  
cDlogQ2   <- subset(completeData2021, Unit ==  "logNrQ2"   ); dim(cDlogQ2)  
cDlogQ3   <- subset(completeData2021, Unit ==  "logNrQ3"   ); dim(cDlogQ3)  
cDlogQ4   <- subset(completeData2021, Unit ==  "logNrQ4"   ); dim(cDlogQ4)  




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
load("C:\\Dropbox\\Insect Biomass Trends/csvs/metadata_per_dataset richness 20220228.RData")
load("C:\\Dropbox\\Insect Biomass Trends/csvs/metadata_per_plot richness 20220228.RData")
all.selectedIns<-  readRDS("all.selectedIns.RDS")

metaRich<- unique(cDrichness[, c("Datasource_ID", "Realm", "Continent")])
table(metaRich$Realm, metaRich$Continent)
metaPIE<- unique(cDenspie[, c("Datasource_ID", "Realm", "Continent")])
table(metaPIE$Realm, metaPIE$Continent)



theme_clean<- theme_grey() + theme(panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), 
                                 axis.line = element_line(colour = "black") , 
                                 legend.key=element_blank())

col.scheme.cont<-c( "Europe"="green3", "Latin America"= "magenta", "North America"= "orange","Asia" = "purple3", 
                    "Africa" = "blue", "Australia" = "red")
col.scheme.biom <-c( "Tropical"="green3",  "Drylands"= "orange", 
                     "Boreal/Alpine" = "blue", "Temperate" = "red")
col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4")
col.scheme.realm2 <- c(  "Freshwater"  = "dodgerblue2", "Freshwater2"  = "dodgerblue2",  "Terrestrial" = "chocolate4", "Terrestrial2" = "chocolate4")
col.scheme.realm3<-c(  "Fw"  = "dodgerblue2", "Ter" = "chocolate4")

#col.scheme.realm<- c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4", "Both realms" = "grey50")  #coral4
shps<- c("Freshwater" = 24, "Terrestrial" = 21 )#, "Both realms" = 22)
col.scheme.strat<-c( "Air" = "peru", "Herb layer" = "peru", "Soil surface" = "peru", "Trees" = "peru", 
                     "Underground" = "peru"  ,"Water" = "dodgerblue2")
col.scheme.PA <- c(  "yes"  = "darkgrey", "no" = "lightgrey")
col.scheme.AB <- c("biomass" = "red", "abundance" = "blue")

col.scheme.global<- c(  "Global"  = "grey10", "Observed" = "grey70")  #
col.scheme.black<- c(  "Global"  = "black", "Observed" = "black")  #

sz = 0.5
brks<- c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
e<- c("","","","","","","")
lims<- c(-0.03,0.036)

mypalett<- colorRampPalette  (c("#CC0000", "#FF6666", "cornsilk2", "dodgerblue2", "dodgerblue4"), space = "rgb")

brks<- c(-0.02, -0.01, 0, 0.01, 0.02)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")

setwd("C:/Users/rk59zeqi/Documents/model outputs richness paper")




#Table S1 Data availability: #####

# total datasets
length(unique(subset(completeData2021pure, Unit != "Biomass")$Datasource_ID)) #171


metadata_metrics<- completeData2021 %>%
	group_by (Unit, Realm) %>%
	summarise(
		n_datasets = length(unique(Datasource_ID)),
		n_plots= length(unique(Plot_ID))
	)
metadata_metrics2 <- metadata_metrics
plotRealm<- dcast(metadata_metrics, Unit ~ Realm, value.var = "n_plots" )
names(plotRealm)<- c("Metric"  ,"F_plots",  "T_plots")
DsRealm<- dcast(metadata_metrics, Unit ~ Realm , value.var = "n_datasets")
names(DsRealm)<- c("Metric"  ,      "F_datasets" , "T_datasets")
metadata_metrics<- merge(DsRealm, plotRealm)
metadata_metrics$all_datasets<- rowSums(metadata_metrics[, c("F_datasets", "T_datasets")], na.rm = T)
metadata_metrics$all_plots<- rowSums(metadata_metrics[, c("F_plots", "T_plots")], na.rm = T)

# relect rows and order columns
metadata_metrics[c(1, 44, 43, 45,9, 14, 18, 31)    , c(1,6,7,3,5,2,4)   ]


# Continental breakdown 
metadata_cont<- completeData2021 %>%
	group_by (Datasource_ID) %>%
	summarise(
		Metric = unique(Unit),
		Continent = unique(Continent)
		)

props<- prop.table(table(subset(metadata_cont, Metric == "abundance")$Continent))
props
props[3] + props[5] # europe and NA

props2<- prop.table(table(subset(metadata_cont, Metric == "Jaccard")$Continent))
props2
props2[2] + props2[4] # europe and NA



# ALPHA METRICS#####
# make metadata 
metadata_metrics2$x <- NA
metadata_metrics2$y <- NA
metadata_metrics2$y[ metadata_metrics2$Realm == "Terrestrial"] <- 400
metadata_metrics2$y[ metadata_metrics2$Realm == "Freshwater"] <- 150
metadata_metrics2$x <- 0.01
metadata_metrics2$Metric <- NA
metadata_metrics2$Metric[metadata_metrics2$Unit == "richness"]<- "Richness"
metadata_metrics2$Metric[metadata_metrics2$Unit == "abundance"]<- "Abundance"
metadata_metrics2$Metric[metadata_metrics2$Unit == "ENSPIE"]<- "Evenness"
metadata_metrics2$Metric[metadata_metrics2$Unit == "rarefiedRichness"]<- "Rarefied richness"
#metadata_metrics2$Metric[metadata_metrics2$Unit == "Shannon"]<- "ENS-Shannon"

metadata_metrics2$text<- paste(  metadata_metrics2$n_datasets, "|", metadata_metrics2$n_plots)
metadata_metrics2<- subset(metadata_metrics2, !is.na(Metric))
metadata_metrics2$Metric<- factor(metadata_metrics2$Metric, levels = c ("Abundance", "Richness", "Rarefied richness", "ENS-Shannon", "Evenness" ))



# Richness 
inlaRichSum<- as.data.frame(readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaRichnessSUMMARY.rds"))
richRandom<- readRDS("inlaRichnessrandomSlopes.rds")
richMarg<- readRDS("inlaRichnessMarginal.rds")
richMarg$Metric<- "Richness"
richFWsamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaRichnessFWSamples.rds" )
richTersamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaRichnessTerSamples.rds" )
dif<- richFWsamples - richTersamples
quantile(dif,c(0,0.025, 0.05, 0.1 ,0.5,0.9, 0.95, 0.975, 1))

inlaRarRichSum<- as.data.frame(readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaRarRichSUMMARY.rds"))
rarRichRandom<- readRDS("inlaRarRichrandomSlopes.rds")
rarRichMarg<- readRDS("inlaRarRichMarginal.rds")
rarRichMarg$Metric<- "Rarefied richness"
rarRichFWsamples<- readRDS("inlaRarRichFWSamples.rds" )
rarRichTersamples<- readRDS("inlaRarRichTerSamples.rds" )
dif<- rarRichFWsamples - rarRichTersamples
quantile(dif,c(0,0.025, 0.05, 0.1 ,0.5,0.9, 0.95, 0.975, 1))



inlaAbSum<- as.data.frame(readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaAbunSUMMARY.rds"))
abRandom<- readRDS("inlaAbunrandomSlopes.rds")
abMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaAbunMarginal.rds" )
abMarg$Metric<- "Abundance"
abFWsamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaAbunFWSamples.rds" )
abTersamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaAbunTerSamples.rds" )
dif<- abFWsamples - abTersamples
quantile(dif,c(0,0.025, 0.05, 0.1 ,0.5,0.9, 0.95, 0.975,1))

# ENSPIE load and get marginals 

inlapieSum<- as.data.frame(readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaENSPIESUMMARY.rds"))
pieMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaENSPIEMarginal.rds" )
pieMarg$Metric<- "Evenness"
pieFWsamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaENSPIEFWSamples.rds" )
pieTersamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaENSPIETerSamples.rds" )
dif<- pieFWsamples - pieTersamples
quantile(dif,c(0,0.025, 0.05, 0.1 ,0.5,0.9, 0.95, 0.975,1))

inlaShanSum<- as.data.frame(readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaShanSUMMARY.rds"))
shanMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/InlaShanMarginal.rds")
shanMarg$Metric<- "ENS-Shannon"
shanFWsamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaShanFWSamples.rds" )
shanTersamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaShanTerSamples.rds" )
dif<- shanFWsamples - shanTersamples
quantile(dif,c(0,0.025, 0.05, 0.1 ,0.5,0.9, 0.95, 0.975,1))



# Fig 1 ######


univar<- rbind(abMarg, richMarg, rarRichMarg,  pieMarg)
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
cbind(Metric = "ENS-Shannon",       Realms, inlaShanSum[ 3:4, c(1,2,5, 13)] ), 
cbind(Metric = "ENS-PIE",           Realms, inlapieSum[ 3:4, c(1,2,5, 13)] ) 
)

mnChangeEsts$lower2.5Perc10Yr <- (10^(mnChangeEsts$`0.025quant`*10 )-1)   *100
mnChangeEsts$meanPerc10Yr <- (10^(mnChangeEsts$mean*10 )-1)   *100
mnChangeEsts$upper97.5Perc10Yr <- (10^(mnChangeEsts$`0.975quant`*10 )-1)   *100

mnChangeEsts$lower2.5PercYr <- (10^(mnChangeEsts$`0.025quant` )-1)   *100
mnChangeEsts$meanPercYr <- (10^(mnChangeEsts$mean )-1)   *100
mnChangeEsts$upper97.5PercYr <- (10^(mnChangeEsts$`0.975quant` )-1)   *100
mnChangeEsts$Realm2<- mnChangeEsts$Realms
mnChangeEsts$Metric2<- mnChangeEsts$Metric
mnChangeEsts








# dominance #####
inlaDomSum<- as.data.frame(readRDS("dominanceRelSUMMARY.rds"))
domMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/dominanceRelMarginal.rds" )
abFWsamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/dominanceRelFWSamples.rds" )
abTersamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/dominanceRelTerSamples.rds" )
dif<- abFWsamples - abTersamples
quantile(dif,c(0,0.025, 0.05, 0.1 ,0.5,0.9, 0.95, 0.975,1))


ggplot(domMarg, aes(x = x, y = y))+
	#	geom_line( )+
	geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
	geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
	geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
	geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
	geom_vline(xintercept = 0)+
	ggtitle ("Dominance")+
	xlab("Slope")+	
	scale_fill_manual(values = col.scheme.realm)+
	theme_classic()
# This is rather unexpected 
# is it because it shoudl first be converted to ENS?  









# Fig 3 BETA DIVERSITY #####
setwd("C:/Users/rk59zeqi/Documents/model outputs richness paper")
inlaJacSum<- as.data.frame(readRDS("inlaJaccardSUMMARY.rds"))
jacMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaJaccardMarginal.rds" )
jacMarg$Metric<- "Jaccard"
jacFWsamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaJaccardFWSamples.rds" )
jacTersamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaJaccardTerSamples.rds" )
dif<- jacFWsamples - jacTersamples
quantile(dif,c(0,0.025, 0.05, 0.1 ,0.5,0.9, 0.95, 0.975,1))

inlaJacDifSum<- as.data.frame(readRDS("InlaJacDifSUMMARY.rds"))
jacdifMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaJacDifMarginal.rds" )
jacdifMarg$Metric<- "Jaccard"
jacdifFWsamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaJacDifFWSamples.rds" )
jacdifTersamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaJacDifTerSamples.rds" )
dif<- JacdifFWsamples - JacdifTersamples
quantile(dif,c(0,0.025, 0.05, 0.1 ,0.5,0.9, 0.95, 0.975,1))

inlaHornSum<- as.data.frame(readRDS("inlaHornSUMMARY.rds"))
hornMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaHornMarginal.rds" )
hornMarg$Metric<- "Morisita-Horn"
hornFWsamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaHornFWSamples.rds" )
hornTersamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaHornTerSamples.rds" )
dif<- hornFWsamples - hornTersamples
quantile(dif,c(0,0.025, 0.05, 0.1 ,0.5,0.9, 0.95, 0.975,1))

inlaHornDifSum<- as.data.frame(readRDS("inlaHornDifSUMMARY.rds"))
horndifMarg<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaHornDifMarginal.rds" )
horndifMarg$Metric<- "Morisita-Horn"
horndifFWsamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaHornDifFWSamples.rds" )
horndifTersamples<- readRDS("C:/Users/rk59zeqi/Documents/model outputs richness paper/inlaHornDifTerSamples.rds" )
dif<- horndifFWsamples - horndifTersamples
quantile(dif,c(0,0.025, 0.05, 0.1 ,0.5,0.9, 0.95, 0.975,1))



betaMarg<- rbind(jacMarg, jacdifMarg, hornMarg, horndifMarg)
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



# different visualization

FWslopeHorndif<- inlaHornDifSum$mean[3]
TerslopeHorndif<- inlaHornDifSum$mean[4]
FWslopeHorn<-  inlaHornSum$mean[3]
TerslopeHorn<- inlaHornSum$mean[4]
FWslopeJac<-  inlaJacSum$mean[3]
TerslopeJac<- inlaJacSum$mean[4]
FWslopeJacdif<-  inlaJacDifSum$mean[3]
TerslopeJacdif<- inlaJacDifSum$mean[4]


points<- data.frame(
	test = rep(c("Jaccard", "Morisita-Horn"), each = 2),
	x = c(0,80,0,80), 
		y = c(1,0,1,0))

lines<- data.frame(
	test = rep(c("Jaccard", "Morisita-Horn"), each = 4),
	es = c(0.5,0.5,1,1,0.5,0.5,1,1),
	metric = rep(c("Jaccard", "ES Jaccard", "Morisita-Horn", "ES Morisita-Horn"),each = 2),
	Realm = rep(c("Freshwater", "Terrestrial"),4),
	int = rep(0,8), 
	slp = c(FWslopeJac, TerslopeJac, FWslopeJacdif, TerslopeJacdif,FWslopeHorn, TerslopeHorn, FWslopeHorndif, TerslopeHorn) )


ggplot(points, aes(x , y  ) )+
	geom_point( color = "white" )+
	geom_abline(data = lines, aes(slope = slp, intercept = int, color = Realm, alpha = es ), size = 2)+ #
	xlim (0, 80)+ ylim(0,1)+
	scale_alpha(range = c(0.4, 1))+
	facet_wrap(.~test)+
	scale_color_manual(values = col.scheme.realm)+
	ylab("beta-diversity")+ xlab ("Year")+
	theme_classic()


ggplot(points, aes(x , y  ) )+
	geom_point( )+
	geom_abline(slope = FWslopeJac, intercept = 0, color = "dodgerblue2", size = 2)+
	geom_abline(slope = TerslopeJac, intercept = 0, color = "chocolate4", size = 2)+
	
	geom_abline(slope = FWslopeJacdif, intercept = 0, color = "dodgerblue2", size = 2, alpha = 0.5)+
	geom_abline(slope = TerslopeJacdif, intercept = 0, color = "chocolate4", size = 2, alpha = 0.5)+

	geom_abline(slope = FWslopeHorndif, intercept = 0, color = "dodgerblue2", size = 2, alpha = 0.5)+
	geom_abline(slope = TerslopeHorndif, intercept = 0, color = "chocolate4", size = 2, alpha = 0.5)+
	
	geom_abline(slope = FWslopeHorn, intercept = 0, color = "dodgerblue2", size = 2)+
	geom_abline(slope = TerslopeHorn, intercept = 0, color = "chocolate4", size = 2)+
	xlim (0, 80)+ ylim(0,1)+
	facet_wrap(.~test)+
	ylab("beta-diversity")+ xlab ("Year")+
	theme_classic()





# SAD changes #####
# number of dominant and rare species: (upper and lower 20% of abundance ) 
setwd("C:/Users/rk59zeqi/Documents/model outputs richness paper")

sads<- subset(completeData2021pure, Unit ==  "logNr020" |  Unit ==   "logNr2040"|  Unit == "logNr4060" |  Unit == "logNr6080"|  Unit ==  "logNr80100")
piv<- dcast(subset(sads, !is.na(Number)), Plot_ID+ Year ~ Unit, value.var = "Number", mean)
pivot_wider(sads, id_cols = Plot_ID, from = Unit, values_from = Number, values_fn = mean)

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


quantilesDataNeg<- quantilesData
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
shanRand      <- readRDS("InlaShanrandomSlopes.rds")
jacRandom     <- readRDS("inlaJaccardrandomSlopes.rds")
jacDifRandom     <- readRDS("inlaJacDifrandomSlopes.rds")
hornRandom     <- readRDS("inlaHornrandomSlopes.rds")
hornDifRandom     <- readRDS("inlaHornDifrandomSlopes.rds")


richRandom$richnessSlope<- richRandom$slope
richRandom$richnessSlopeSD<- richRandom$`DataID_Slope_ sd`
abRandom$abundanceSlope <- abRandom$slope
abRandom$abundanceSlopeSD <- abRandom$`DataID_Slope_ sd`
enspieRandom$enspieSlope<- enspieRandom$slope
shanRand$shanSlope<- shanRand$slope
rarRichRandom$rarRichSlope <- rarRichRandom$slope
jacRandom$jacSlope     <- jacRandom$slope
jacDifRandom$jacDifSlope     <- jacDifRandom$slope
hornRandom$hornSlope     <- hornRandom$slope
hornDifRandom$hornDifSlope     <- hornDifRandom$slope

randomSlopes<- merge(richRandom[, c("Realm", "Datasource_ID", "Datasource_name", "richnessSlope", "richnessSlopeSD") ],  
										 abRandom[, c("Realm", "Datasource_ID", "Datasource_name", "abundanceSlope", "abundanceSlopeSD")], all = T)
randomSlopes<- merge(randomSlopes,  enspieRandom[, c("Realm", "Datasource_ID", "Datasource_name", "enspieSlope")], all = T)
randomSlopes<- merge(randomSlopes,  shanRand[, c("Realm", "Datasource_ID", "Datasource_name", "shanSlope")], all = T)
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
cor.test(randomSlopes$richnessSlope, randomSlopes$shanSlope)
cor.test(randomSlopes$richnessSlope, randomSlopes$rarRichSlope)
cor.test(randomSlopes$richnessSlope, randomSlopes$hornDifSlope)

cor.test(randomSlopes$abundanceSlope, randomSlopes$enspieSlope)
cor.test(randomSlopes$abundanceSlope, randomSlopes$rarRichSlope)
cor.test(randomSlopes$abundanceSlope, randomSlopes$shanSlope)
cor.test(randomSlopes$enspieSlope, randomSlopes$rarRichSlope)
cor.test(randomSlopes$enspieSlope, randomSlopes$shanSlope)
cor.test(randomSlopes$shanSlope, randomSlopes$rarRichSlope)

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
ggpairs(randomSlopes, columns =  c(  "abundanceSlope","richnessSlope", "rarRichSlope" , "shanSlope" , "enspieSlope" ),
											 axisLabels="show", 
				labeller = labeller(.default = labs), 
											 mapping = ggplot2::aes(color = Realm2, alpha = 0.5))+
	geom_hline(yintercept=0,linetype="dashed")+
	geom_vline(xintercept=0,linetype="dashed")
	
ggpairs(randomSlopes, columns =  c(  "abundanceSlope","richnessSlope", "rarRichSlope" ,  "enspieSlope",  "jacDifSlope",  "hornDifSlope" ),
				axisLabels="show", 
				labeller = labeller(.default = labs), 
				mapping = ggplot2::aes(color = Realm2, alpha = 0.5))+
	geom_hline(yintercept=0,linetype="dashed")+
	geom_vline(xintercept=0,linetype="dashed")




ggsave(filename = "Fig S5 all correlations.png" , path = figure_path, width = 21, height = 21,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig S5 all correlations.pdf" , path = figure_path, width = 21, height = 21,  units = "cm",dpi = 300, device = "pdf")



ggplot(randomSlopes, aes(x=abundanceSlope, y=richnessSlope, color =Realm)) +
  geom_point()+
 geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
 	scale_color_manual(values = col.scheme.realm) +
	geom_smooth(method=lm) +
  geom_text(label=randomSlopes$Datasource_name)+
	 theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,

# is a linear regression line the most appropriate here? it makes a big difference what's on x and what's on y 

ggplot(randomSlopes, aes(x=richnessSlope, y=abundanceSlope, color =Realm)) +
	geom_point()+
	geom_hline(yintercept=0,linetype="dashed")+
	geom_vline(xintercept=0,linetype="dashed")+
	scale_color_manual(values = col.scheme.realm) +
	geom_smooth(method=lm) +
	geom_text(label=randomSlopes$Datasource_name)+
	theme_clean + 
	theme(  strip.background = element_blank(), 
					plot.title = element_text(hjust = 0.5))#,


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
	#geom_rug()+
	#	geom_smooth(method=lm) +
	#	scale_x_continuous(breaks = brks,labels = l, limits=c(-0.05,  0.03))+#
	#	scale_y_continuous(breaks = brks,labels = l, limits=c(-0.02,  0.016))+ 	
	# geom_ribbon(data = as.data.frame(predictions[[1]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[2]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[3]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[4]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[5]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[6]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[7]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[8]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[9]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[10]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[11]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[12]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[13]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[14]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[15]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[16]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[17]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[18]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[19]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[20]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[21]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[22]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[23]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[24]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[25]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[26]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[27]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[28]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[29]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[30]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[31]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[32]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[33]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[34]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[35]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[36]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[37]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[38]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[39]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
	# geom_ribbon(data = as.data.frame(predictions[[40]][[1]]), inherit.aes = F,  aes (x = myX , ymin = `0.025quant`, ymax = `0.975quant`, fill = Realm ), alpha = alpha)+
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



# cleaner version 
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
							alpha=0.3)+
	geom_abline(intercept=median(subset(sampledCoefs, Realm == "Terrestrial")$intercept), # median fit 
							slope=median(sampledCoefs$coef), colour="chocolate4", size = 1)+
	geom_abline(intercept=median(subset(sampledCoefs, Realm == "Freshwater")$intercept), # median fit 
							slope=median(sampledCoefs$coef), colour="dodgerblue2", size = 1)+
	xlab ("Abundance slope")+ ylab ("Richness slope")+
	theme_clean + 
	theme(  strip.background = element_blank(), 
					plot.title = element_text(hjust = 0.5), 
					legend.position = "none")#,

ggsave(filename = "Fig 1b regres clean.png" , path = figure_path, width = 12, height = 12,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig 1b regress clean.pdf" , path = figure_path, width = 12, height = 12,  units = "cm",dpi = 300, device = "pdf")



# old 
ggplot(randomSlopes, aes(x=abundanceSlope, y=richnessSlope, color =Realm)) +
	geom_point()+
	geom_hline(yintercept=0,linetype="dashed")+
	geom_vline(xintercept=0,linetype="dashed")+
	scale_color_manual(values = col.scheme.realm) +
	scale_fill_manual(values = col.scheme.realm) +
	#	geom_smooth(method=lm) +
	scale_x_continuous(breaks = brks,labels = l, limits=c(-0.05,  0.03))+
	scale_y_continuous(breaks = brks,labels = l, limits=c(-0.02,  0.016))+ 	
#			geom_line(data = nwdat, aes(x = abundanceSlope, y = fit, colour = Realm),  linetype = 1)+
	#	geom_text(label=randomSlopes$Datasource_name)+
#			geom_ribbon(data = nwdat, inherit.aes = F,  aes (x = abundanceSlope , ymin = lwr, ymax = upr, fill = Realm ), alpha = 0.3)+
	xlab ("Abundance slope")+ ylab ("Richness slope")+
	theme_clean + 
	theme(  strip.background = element_blank(), 
					plot.title = element_text(hjust = 0.5), 
					legend.position = "none")#,


# biivariate map #####
richRandom$richlwr <- richRandom$`DataID_Slope_ 0.1quant` + richRandom$fixedSlp
richRandom$richupr <-  richRandom$`DataID_Slope_ 0.9quant` + richRandom$fixedSlp 
richRandom$richdir<- NA 
richRandom$richdir[richRandom$richupr <0 ]<- -1 
richRandom$richdir[richRandom$richlwr >0]<- 1 
richRandom$richdir[richRandom$richlwr<0 & richRandom$richupr>0]<-0 


abRandom$ablwr = abRandom$`DataID_Slope_ 0.1quant` + abRandom$fixedSlp
abRandom$abupr = abRandom$`DataID_Slope_ 0.9quant` + abRandom$fixedSlp 
abRandom$abdir<- NA 
abRandom$abdir[abRandom$abupr <0 ]<- -1 
abRandom$abdir[abRandom$ablwr >0]<- 1 
abRandom$abdir[abRandom$ablwr<0 & abRandom$abupr>0]<-0 





randomSlopes2<- merge(richRandom[, c("Realm", "Datasource_ID", "Datasource_name", "richnessSlope", "richdir") ],  
										 abRandom[, c("Realm", "Datasource_ID", "Datasource_name", "abundanceSlope", "abdir")], all = T)


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
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/map_preparation.R")
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

#png("map fw.png", width=4400, height=1800, res = 360)
#fw.wgs
#dev.off()


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

#png("map terr.png", width=4400, height=2000, res = 360)
#terr.wgs
#dev.off()

library(cowplot)
finalMap <- ggdraw() +
	draw_plot(terr.wgs, 0, 0.5, 1, 0.5) +
	draw_plot(fw.wgs, 0, 0, 1, 0.5) +
	draw_plot(legend, 0.025, .55, 0.18, 0.18)

finalMap
ggsave(filename = "Fig S1 map.png" , path = figure_path, width = 2500, height = 2900,  units = "px",dpi = 300, device = "png")
ggsave(filename = "Fig S1 map.pdf" , path = figure_path, width = 2500, height = 2900,  units = "px",dpi = 300, device = "pdf")







#  Fig S7 fitted vs predicted values  Looks good!  #####
cDRichness$preds <- inlaRich$summary.fitted.values$mean  #need to have control.predictor = list(link = 1) in model
ggplot(cDRichness,aes(x=preds,y=log10(Number+1)))+
  geom_point (aes(color = Realm), size =sz)+
  geom_abline(slope=1,intercept=0)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab ("Predicted values") + ylab ("Observed values")+
  facet_wrap(.~Realm, scales = "free")+
 theme_clean + 
  theme( strip.background = element_blank())

metadata_realm<-  cDRichness %>% 
  group_by(Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID))) 

metadata_realm$Unit<- "Abundance &\n biomass"

Rslope<- inlaRealmSum[3:4,]
varsR<- data.frame(do.call(rbind, strsplit(rownames(Rslope), split = ":")))
Rslope<-cbind(Rslope, varsR)
Rslope$X3<- Rslope$X2
Rslope$Realm<-gsub("Realm", "", Rslope$X1)
Rslope$Unit<- "Abundance &\n biomass"
Rslope$AB <-Rslope$Unit
Rslope<- merge(Rslope, metadata_realm)
Rslope$P<- ps

# percentage change per year and per decade
data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr", "FW 10 yr", "Terr 10 yr"), 
  CI2.5 =  c((10^(inlaRichSum  [3:4,4]  )-1 ) *100, (10^(inlaRichSum  [3:4,4] *10)-1 )  *100),#0.025 CI
  mean =   c((10^(inlaRichSum  [3:4,1]  )-1)  *100, (10^(inlaRichSum  [3:4,1] *10)-1 )  *100), # proportional changes per year
  CI97.5 = c((10^(inlaRichSum  [3:4,12] )-1 ) *100, (10^(inlaRichSum  [3:4,12] *10)-1)  *100)# 0.975
)

# positive effect for fw, no trend for richness


table(inlaRich$cpo$failure)
cpo = inlaRich$cpo$cpo
ind <- 1:length(cpo)
df = data.frame(ind,cpo,
                realm=cDrichness$Realm,
                unit=cDrichness$Unit,
                dataset=cDrichness$Datasource_ID)

qplot(ind,cpo,data=subset(df,cpo<0.05))+
  geom_point(aes(colour=factor(dataset)))+
  facet_wrap(realm~unit) # some datasets have weird values

pit <- inlaRealm$cpo$pit
n = length(sort(pit))
samples <- (1:n)/(n+1)
plot(samples, sort(pit), xlab="uniform quantiles", ylab="Sorted PIT values")
abline(0,1)





# thrash #####



# this is all just exporation with no real hypothesis 
ggplot(randomSlopes, aes(x=richnessSlope, y=enspieSlope, color =Realm)) +
	geom_point()+
	geom_hline(yintercept=0,linetype="dashed")+
	geom_vline(xintercept=0,linetype="dashed")+
	scale_color_manual(values = col.scheme.realm) +
	geom_smooth(method=lm) +
	geom_text(label=randomSlopes$Datasource_name)+
	theme_clean + 
	theme(  strip.background = element_blank(), 
					plot.title = element_text(hjust = 0.5))#,


ggplot(randomSlopes, aes(x=abundanceSlope , y=enspieSlope, color =Realm)) +
	geom_point()+
	geom_hline(yintercept=0,linetype="dashed")+
	geom_vline(xintercept=0,linetype="dashed")+
	scale_color_manual(values = col.scheme.realm) +
	geom_smooth(method=lm) +
	geom_text(label=randomSlopes$Datasource_name)+
	theme_clean + 
	theme(  strip.background = element_blank(), 
					plot.title = element_text(hjust = 0.5))#,


ggplot(randomSlopes, aes(x=braySlope , y=enspieSlope, color =Realm)) +
	geom_point()+
	geom_hline(yintercept=0,linetype="dashed")+
	geom_vline(xintercept=0,linetype="dashed")+
	scale_color_manual(values = col.scheme.realm) +
	geom_smooth(method=lm) +
	geom_text(label=randomSlopes$Datasource_name)+
	theme_clean + 
	theme(  strip.background = element_blank(), 
					plot.title = element_text(hjust = 0.5))#,






# sample posteriors and do test to see if they differ  #####
my_list <- list("RealmFreshwater:cYear" = 1 , 
								"RealmTerrestrial:cYear" = 1)


lm1.samples = inla.posterior.sample(1000, inlaAb, selection = my_list)
#extract the bits we need using the line below
Ter.samp <- as.vector(inla.posterior.sample.eval(function(...) { `RealmTerrestrial:cYear` },lm1.samples))
quantile(Ter.samp,c(0,0.025, 0.1 ,0.5,0.8, 0.975,1)) #similar to inla model
#Ter.samp<- data.frame(Realm = "Terrestrial", #
#											sample = Ter.samp)
FW.samp <- as.vector(inla.posterior.sample.eval(function(...) { `RealmFreshwater:cYear` },lm1.samples))
#FW.samp<- data.frame(Realm = "Freshwater", #
#											sample = FW.samp)

dif<- Ter.samp - FW.samp
quantile(dif,c(0,0.025, 0.05, 0.1 ,0.5,0.9, 0.95, 0.975,1)) #similar to inla model



sam<- rbind(Ter.samp, FW.samp)


inla.test<- inla(sample ~ Realm , data = sam)
inla.test$summary.fixed





