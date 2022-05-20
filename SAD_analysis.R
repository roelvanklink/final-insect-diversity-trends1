# SAD_analysis
# From Roel:
# The file is called ‘Density values temp 20210805rds’  and is located in the dropbox under: Dropbox\Insect Biomass Trends\csvs
# 
# There are 4 types of densities: 
#   1)	Raw: densityMode = relative & correction = F 
#   2)	Corrected raw: added the part of the curve below 0 to the positive side of the curve:   densityMode = relative & correction = F
#   3)	Absolute raw: like Raw, but multiplied by the number of species. This is however strongly correlated to the richness trend, so in my opinion rather useless:   densityMode = absolute & correction = F
#   4)	Absolute corrected: Raw multiplied by the number of species + corrected for part below 0. Probably also highly correlated with  richness trend: densityMode = absolute & correction = F
#   
#   I’d start with the raw type, as it makes most sense to me. 
  
#libraries
library(tidyverse)
library(lme4)

#data - check this is the right one
# test data: 
#mydata <- readRDS("C://Dropbox/Insect Biomass Trends/csvs/Toy data Density values 20210805.rds"); dim(mydata)
completeDens2021<- read_rds("C://Dropbox/Insect Biomass Trends/csvs/allDensities 20211221.rds" ); dim(completeDens2021)

#Remove experimental plots (if any) 
exptPlots<- c(5, # alaska
							921, 922, 924,925, #smes
							643, 644, 646, 647, # hemlock removal
							137, 138, 139  #brazil fragmentation experiment
)
exptDatasources<- c(300,1364, 1357,1410) #Kellogg, Luquillo CTE, Cedar creek big bio, some german grassland

completeDens2021<- completeDens2021[!completeDens2021$Datasource_ID %in% exptDatasources, ]
completeDens2021<- completeDens2021[!completeDens2021$Plot_ID %in% exptPlots, ]
dim(completeDens2021)




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

completeDens2021<- completeDens2021[!completeDens2021$Plot_ID %in% bad_tax, ]



(completeDens2021) %>% select(starts_with("p", ignore.case = FALSE))

completeDens2021<- completeDens2021 %>% select(! place)

densitiesLong <- completeDens2021 %>% 
                #select(!X) %>%
               # filter(densityMode == "absolute" & correction == FALSE) %>%
                pivot_longer(cols = starts_with("p", ignore.case = FALSE),
                              names_to = "percentile",
                              values_to  ="value") %>%
                mutate(percentile = parse_number(percentile)) %>%
                as.data.frame()
              dim(densitiesLong)

dataAvailability<- densitiesLong %>% 
  group_by (Realm, Year) %>%
  summarize(
  numberOfDatasets = length(unique(Datasource_ID))
)
#saveRDS(densitiesLong, file = "C:/Dropbox/Insect Biomass Trends/csvs/densitiesLong.rds")








#mydata_long <- densitiesLong
#pivot data set

densitiesLongAbs<- subset(densitiesLong, densityMode == "absolute" & correction == T)
densitiesLongRel<- subset(densitiesLong, densityMode == "relative" & correction == T)

	

#plot a couple of examples

ggplot(subset(densitiesLongRel, Plot_ID == 117))+
  geom_line(aes(x = percentile, y = value, group = Year, color = Year), size=1.5)+
  scale_color_viridis_c()+
  theme_classic()

ggplot(subset(densitiesLongRel, Plot_ID == 2087))+
  geom_line(aes(x = percentile, y = value, group=Year, color = Year), size=1.5)+
  scale_color_viridis_c()+
  theme_classic()

#look at the distribution of the data
summary(densitiesLongRel$value)
hist(densitiesLongRel$value)
hist(log10(densitiesLongRel$value+1))
hist(log(densitiesLongRel$value+1))

#how do we have examples above 1??
mydata_long_above1 <- subset(densitiesLongRel, value>1)
head(mydata_long_above1)
length(unique(mydata_long_above1$Plot_ID))#237!!

#look at couple of examples when the density goes above zero
ggplot(subset(densitiesLongRel, Plot_ID == 905))+
  geom_line(aes(x = percentile, y = value, group=Year, color = Year), size=1.5)+
  scale_color_viridis_c()+
  theme_classic()

ggplot(subset(densitiesLongRel, Plot_ID == 2397))+
  geom_line(aes(x = percentile, y = value, group=Year, color = Year), size=1.5)+
  scale_color_viridis_c()+
  theme_classic()
#seems to be in cases when distribution was v skewed

#quick and dirty analysis

#trend at each percentile and data set
allTrends <- densitiesLongAbs %>% 
  group_by(percentile, Plot_ID) %>%
  nest_by() %>% 
  mutate(model = list(lm(log10(value+1) ~ Year, data = data))) %>% 
  summarise(broom::tidy(model)) %>%
  unnest(c(percentile, Plot_ID)) %>%
  filter(.,term == "Year")

ggplot(allTrends) +
  geom_boxplot(aes(x=as.factor(percentile),y=estimate))

ggplot(allTrends) +
  geom_boxplot(aes(x=as.factor(percentile),y=estimate),outlier.color = NA)+
  ylim(-0.03,0.03)+
  geom_hline(yintercept = 0, colour = "red")+
  theme_classic()


#using lmer  #####
library(lme4)
library(beepr)

# note that there location random effect is removed, because the models couldn't run
# this takes a long time to run 


# analysis with yr as factor to producs graphs
Fdat<- subset(densitiesLongRel, Realm == "Freshwater")
Tdat<- subset(densitiesLongRel, Realm == "Terrestrial")
yrs<- sort(unique(densitiesLongAbs$Year[complete.cases(densitiesLongAbs) ]))
TerEsts <-   Tdat %>%
	filter(Realm == "Terrestrial" ) %>%
	group_by(percentile) %>%
	nest_by() %>% 
	mutate(model = list(lmer(log10(value+1) ~ -1 + factor(Year) + (1|Datasource_ID/ Plot_ID), data = data))) %>% 
	summarise(data.frame(summary(model)$coef)) %>%
	unnest(percentile) %>%
	ungroup() #%>%
TerEsts$trEst <-  (10^TerEsts$Estimate)-1 
TerEsts$Year <-  rep(sort(unique(Tdat$Year)),length(unique(Tdat$percentile)))   
TerEsts$Realm <- "Terrestrial"


FwEsts <-  Fdat %>%
	#	filter(Realm == "Freshwater" & correction == FALSE) %>%
	group_by(percentile) %>%
	nest_by() %>% 
	mutate(model = list(lmer(log10(value+1) ~ -1 + factor(Year) + (1|Datasource_ID/ Plot_ID), data = data))) %>% 
	summarise(data.frame(summary(model)$coef)) %>%
	unnest(percentile) %>%
	ungroup() 
# mutate(Estimate = (10^Estimate)-1)
FwEsts$trEst <-  (10^FwEsts$Estimate)-1 
FwEsts$Year <-  rep(sort(unique(Fdat$Year)),length(unique(Fdat$percentile)))   
FwEsts$Realm <- "Freshwater"
allTrends <- rbind(TerEsts, FwEsts); dim(allTrends)

allTrends <- merge( dataAvailability, allTrends  )
allTrends <- arrange(allTrends, Realm, Year, percentile)

beep(2)
ggplot(subset(allTrends, numberOfDatasets>4  )) + #subset(,Year>=1980)
	geom_path(aes(x=percentile,y=Estimate,group = Year, color = Year), size=0.5)+
	scale_color_viridis_c(option= 'B')+
	xlab("SAD percentile")+ylab("Density")+
	facet_wrap(~Realm, ncol = 1 , strip.position="right" )+
	theme_classic()+
	theme(legend.position = "right")

ggsave(filename = "Fig 2b sads.png" , path = figure_path, width = 8, height = 12,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig 2b sads.pdf" , path = figure_path, width = 8, height = 12,  units = "cm",dpi = 300, device = "pdf")






#Get linear trends per percentile
allTrendsT <- Tdat %>% 
  group_by(percentile) %>%
  nest_by() %>% 
  mutate(model = list(lmer(log10(value + 1) ~   Year + (1| Datasource_ID/ Plot_ID), data = data))) %>% 
  summarise(data.frame(t(summary(model)$coef[2,]))) %>%
  unnest(percentile) %>%
  ungroup() # %>%
allTrendsT<- allTrendsT %>% mutate( 
					upper = Estimate + (Std..Error*2),
        	lower = Estimate - (Std..Error*2), 
					Realm = "Terrestrial")
         
allTrendsFW <- Fdat %>%
	group_by(percentile) %>%
	nest_by() %>% 
	mutate(model = list(lmer(log10(value + 1) ~   Year + (1| Datasource_ID/ Plot_ID), data = data))) %>% 
	summarise(data.frame(t(summary(model)$coef[2,]))) %>%
	unnest(percentile) %>%
	ungroup()  #%>%
	allTrendsFW<- allTrendsFW %>% mutate( upper = Estimate + (Std..Error*2),
																				lower = Estimate - (Std..Error*2), 
																				Realm = "Freshwater")

allTrends<- rbind(allTrendsFW, allTrendsT)
beep(1)
ggplot(allTrends) +
  geom_path(aes(x=percentile,y=Estimate, color = Realm))+
  geom_ribbon(aes(x=percentile,ymax=upper, ymin = lower,fill = Realm), alpha = 0.5)+
  geom_hline(yintercept = 0, colour = "black")+
	scale_fill_manual(values = col.scheme.realm)+
	scale_color_manual(values = col.scheme.realm)+
	theme_classic()+
  ylab("Trend")+xlab("SAD percentile")+ 
	ggtitle("Trendestimates year effect")

  

 
 # attempt at including location as random effect 
# not really working 
percentiles<- c(1:100)
  estimates<- NULL
  all.preds<- NULL
  pb <- txtProgressBar(min = 0, max = 100, style = 3)
for( i in 1:length(percentiles))  {
  perc<- percentiles[i]
  
  dat <- subset(densitiesLongRel, percentile == perc); dim(dat)
  predDat<-  unique(dat[, c("Realm","Year" )]) # dataframe of to be predicted values
  dim(predDat)

  model <- lmer(log10(value+1) ~ Realm + Realm : factor(Year) + (1|Datasource_ID/Location / Plot_ID), data = dat) #adding location blows up the model@ 
  model2<- lmer(log10(value+1) ~ Realm + Realm : Year         + (1|Datasource_ID/Location/ Plot_ID), data = dat)
  
  sum<- summary(model2)
  sum1<- as.data.frame(sum$coefficients[ 3:4, ])
  sum1$percentile<- perc
  sum1$Realm<- c("Freshwater", "Terrestrial")
  
  estimates<- rbind(estimates, sum1)
  
  predDat$predicted<- predict(model, newdata = predDat, re.form=NA, se.fit=T) 
  predDat$percentile <- perc
  
  all.preds<- rbind(all.preds, predDat)
  
  setTxtProgressBar(pb, i)

  }
  
  estimates$lower<- estimates$Estimate - estimates$`Std. Error`
  estimates$upper <- estimates$Estimate + estimates$`Std. Error`

ggplot(estimates)+
  geom_point   (aes(x = percentile, y = Estimate, color = Realm),  position=position_dodge(width= 1)) + 
  geom_errorbar(aes(x = percentile ,ymin= lower , ymax= upper , color = Realm), alpha = 1,
                  size = 0.5, width = 0, position=position_dodge(width = 1))+  
  geom_hline(yintercept=0,linetype="dashed") +
    theme_classic()




all.preds <- merge( all.preds, dataAvailability)
all.preds<- arrange(all.preds, percentile)
ggplot(subset(all.preds, numberOfDatasets > 4  )) + 

	geom_path(aes(x=percentile,y=predicted, group = Year, color = Year),size=0.5)+
	scale_color_viridis_c(option= 'B')+
	theme_classic()+
	xlab("SAD percentile")+ylab("Density")+
	facet_grid(rows = vars(Realm) )




predict(test, newdata = sumDat, re.form=NA) # re.form=NA sets all random effects to zero


sumDat<- data.frame( 
  Year = c(1960 , 1965, 1970, 1980, 1990, 1995, 2000, 2005, 2010, 2015, 
                        1960 , 1965, 1970, 1980, 1990, 1995, 2000, 2005, 2010, 2015), 
  Realm  = rep(c("Terrestrial", "Freshwater") , each = 10))


unique()
