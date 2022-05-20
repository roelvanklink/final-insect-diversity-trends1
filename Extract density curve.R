library(ggplot2)
library(reshape2)
library(vegan)
library(INLA)
library(beepr)
library(lme4)
library (lmerTest)
setwd("C:/Users/roelv/Dropbox/Insect Biomass Trends/csvs") # home
setwd("C:\\Dropbox\\Insect Biomass Trends/csvs") # work # work

    prepInla<- function (myData){
      myData$cYear <- myData$Year - floor(median(myData$Year))
      myData$iYear <- myData$Year - min(myData$Year) + 1
      myData$sum<- rowSums(myData[6:105])
      myData$Plot_ID_4INLA<- as.numeric(factor(myData$Plot_ID))   
      myData$Plot_ID_4INLAs <- myData$Plot_ID_4INLA+max(myData$Plot_ID_4INLA)
      myData$Datasource_ID_4INLA <- as.numeric(factor(myData$Datasource_ID))
      myData$Datasource_ID_4INLAs <- myData$Datasource_ID_4INLA+max(myData$Datasource_ID_4INLA)
      return(myData)
    }

theme_clean<- theme_grey() + theme(panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), 
                                 axis.line = element_line(colour = "black") , 
                                 legend.key=element_blank())
    

taxa<-read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/taxa5.0.csv"); dim(taxa)
plots<-read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/PlotData 5.0.csv"); dim(plots)
UKfwPlots<- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/UKfwSites.csv")
plots<- rbind(plots[, names(UKfwPlots)], UKfwPlots)

samples <-read.csv( file = "Sample_Info 5.0.csv"); dim(samples)
samples$Flag_taxonomy[samples$Datasource_ID == 1488 ]<- "FLAG" # temporary fix until a new version of sample ID is made 
database <-read.csv( file = "Data 5.0.csv"); dim(database)
database<- subset(database, Note != "remove");dim(database)
unique(database$Datasource_name)
studies<-read.csv(file = "studies 5.0.csv", header = T); dim(studies)
#studies1 <-read.table( file = "clipboard", header = T, sep = "\t"); dim(studies1) 
#write.csv(studies1, file = "studies 5.0.csv", row.names = F)
shps<- c("Freshwater" = 24, "Terrestrial" = 21, "Both realms" = 22)
col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4")


dat<- readRDS( file = "rawInsectsForRichnessAggregatedPerYear.RDS")

dat<- merge(dat, studies)
unique(dat[, c("Datasource_name", "Plot_ID", "Plot_name")])    


 
# take som erandom plots 
randomPlots<- c(162,  158, 1049, 146, 427, 1192, 559, 570, 501, 891, 264, 228, 866) 
randomPlots<- sample(unique(dat$Plot_ID), 16)
  
 ggplot(subset(n , Plot_ID %in% randomPlots), aes (x = log(Number), color = as.factor(Year )))+
# 	geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density() +
 	 scale_colour_viridis_d()+
 	facet_wrap(.~ Plot_name, 	scales = "free")

 ggplot(diamonds, aes(depth, colour = cut)) +
  geom_density() +
  xlim(55, 70)
 
 
 vec<- subset(n , Plot_ID == 515 & Year == 2000)$Number
d<- density(log10(vec), bw = "sj", from = mn, to =  mx, n = 1000)


 
 
 
 # grab values from the density estimator 
vec<- subset(n , Plot_ID == 515 )
mx<- max(log10(vec$Number))
mn<- min(log10(vec$Number[is.finite(log10(vec$Number))]))


piv<- dcast(vec, Year~Taxon, value.var = "Number", sum)
str(piv)
pivlist<- setNames(split(piv[, -(1)], seq(nrow(piv))), piv$Year)
pivlist<- lapply(pivlist, function(x) {x[x!=0]})


yr<- as.numeric(piv[1, 1])
line<- as.numeric(pivlist[[1]]) # grab a line from the pivot table
d<- density(log10(line),  from = mn, to =  mx, n = 100) # default bandwidth setting
S<- length(line)
perc<- d$y*S


line<- as.numeric(piv[2,]) # grab a line from the pivot table


for(i in 1: nrow(piv)){ 
  
  line<- as.numeric(piv[i, ])
  yr<- line[1]
  line<- as.numeric(line[-1])
  S<- specnumber(line)
 
  dens<- density(log10(line)[is.finite(log10(line))] ,  from = mn, to =  mx, n = 100)$y
  densAbs<-dens*S
  densAbs<- t(as.data.frame(densAbs))
  rownames(densAbs)<- yr
  densFits<- rbind(densFits, densAbs)
}



# try-out loop for absolute and relative abundance and for corrected and uncorrected fits 
randomPlots<- c(162,  158, 1049, 146, 427, 1192, 559, 570, 502, 891, 264, 228, 866) 
plt <-as.numeric(out[6763, ]$Plot_ID) 
randomPlots<- sample(unique(dat$Plot_ID), 50)
randomPlots<- unique(dat$Plot_ID)
length(randomPlots) # 1237 half of these are UK and SE fw data



BW <- 0.3 # choose bandwith for kernels
densFitsRel<- data.frame()
densFitsAbs<- data.frame()
densFitsCorRel<- data.frame()
densFitsCorAbs<- data.frame()
  for (j in 1: length(randomPlots)){

      plt<-randomPlots[j]
 
      Plot_ID <- plt
      vec<- subset(dat , Plot_ID == plt )
      mx<- max(log10(vec$Number))
      mn<- min(log10(vec$Number[is.finite(log10(vec$Number))]))
      Datasource_ID <- unique(vec$Datasource_ID)
      dif <- mx - mn

      Realm <- unique(vec$Realm)


      piv<- dcast(vec, Year~Taxon, value.var = "Number", sum)

  

  for(i in 1: nrow(piv)){ 
  
    line<- as.numeric(piv[i, ])
    Year<- line[1]
    line<- as.numeric(line[-1])
    line<- line[is.finite(log10(line))]
    S<- specnumber(line)
    n <- sum(line)
       if (S <= 1) next  # skip line if zero or one species present
  
    d<- density(log10(line) , bw = BW,  from = mn, to =  mx, n = 100)
    dens<- d$y
    bw<- d$bw
    dens<- t(as.data.frame(dens))
    colnames(dens)<- paste0("p", seq(1:100)) # percentiles
    densAbs<-dens*S
  
        densFitsAbs<- rbind(densFitsAbs, cbind(as.data.frame(cbind(Datasource_ID, Plot_ID, Realm)), as.data.frame(cbind( Year, bw, S, n, densAbs))))
        densFitsRel<- rbind(densFitsRel, cbind(as.data.frame(cbind(Datasource_ID, Plot_ID, Realm)), as.data.frame(cbind( Year, bw, S, n, dens))))
        
        # correct for missing part (left of min)
        d2<- 	 density(log10(line), bw = BW,  from = mn-dif, to = mx , n = 200)$y	 
        negd2 <- rev(d2)	 # reverse all estimates along whole line
        dsum2<- d2 + negd2  # sum these up to gett sum of all 
        dsumhalf2<- dsum2[101:200] # cut off negative values on x
        
        dCor<- as.data.frame(t(dsumhalf2))
        colnames(dCor)<- paste0("p", seq(1:100)) # percentiles
        dCorAbs<-dCor*S
        
        
        densFitsCorRel<- rbind(densFitsCorRel, cbind(as.data.frame(cbind(Datasource_ID, Plot_ID, Realm)), as.data.frame(cbind( Year, bw, S, n,  dCor))))
        densFitsCorAbs<- rbind(densFitsCorAbs, cbind(as.data.frame(cbind(Datasource_ID, Plot_ID, Realm)), as.data.frame(cbind( Year, bw, S, n, dCorAbs))))

}

}

out<- densFitsAbs # choose which output file to visualize
infs<- apply(log(out[, 6:105]), 2, function (x) sum(is.infinite(x))) # check of there are infinite values in the matrices (0's)
sum(infs); infs
indexInfs<- apply(log(out[, 6:105]), 2, function (x) which(is.infinite(x))) # which are infinite values in the matrices (0's)
sort(table(unlist(indexInfs, use.names = F)), decreasing = T) 
outAbs<-prepInla(out)

outRel<- densFitsRel
outRel<-prepInla(outRel)

    
    #plot(out$S, out$sum)

# check what the distributions look like
ggplot(as.data.frame(densFitsCorAbs), aes (x = log10(p1),  fill = (Realm )))+
 geom_histogram() +
  facet_wrap(.~ Plot_ID, scales = "free_x")
# log transformed looks slightly better

ggplot(as.data.frame(densFitsCorAbs), aes (x = log10(p90),  fill = Realm))+
 geom_histogram() +
  facet_wrap(.~ Plot_ID, scales = "free_x")

par(mfrow = c(4,3))
for(i in c(1,2,5,10,20,30,50,70,80,90,95,99)){
x<- out[, i+5]
plot(log10(x) ~ out$Year, main = names(out)[ i+5])}


# run some models ##### 
summary(lm(log10(p5) ~ yr, data =out ))
summary(lmer((p90) ~ cYear : Realm + Realm + (cYear | Plot_ID), data =out ))


# automate models: ####


vars<- c("p1","p5","p10","p20", "p30", "p40", "p50", "p60", "p70", "p80", "p90","p95","p99", "S", "n")
for( i in c(1:length(vars))){  #1: length(vars)
variable<- vars[i]

modelname<- paste0("smallInla", variable)
formul<-as.formula(paste(variable, " ~ cYear : Realm + Realm+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAs,iYear,model='iid')+
                    f(Datasource_ID_4INLAs,iYear,model='iid')"))

model <- inla( formul,
               control.compute = list(dic=TRUE,waic=TRUE, cpo = TRUE), 
              #control.inla = list(h = 0.484761), 
               control.predictor = list(link = 1) , verbose = F, 
              quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,    
               data=outRel)

	fixed = (model$summary.fixed)

	object<- get("model")
	assign(modelname, object)
print(modelname)	
}





smallInlaN<- inla(log10(n) ~ cYear : Realm + Realm+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAs,iYear,model='iid')+
                    f(Datasource_ID_4INLAs,iYear,model='iid'), #+
                #   f(iYear,model='ar1',replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE,waic=TRUE, cpo = TRUE), 
                    # control.predictor = list(link = 1) , verbose = F, 
                    quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,    
                    data=outRel); beep(2)
smallInlaN$summary.fixed
fixedN<- list(smallInlaN$summary.fixed)


ests<- rbind(cbind(Percentile = 1,  Realm = c("Freshwater", "Terrestrial")  , smallInlap1$summary.fixed[ 3:4, ]   ), 
             cbind(Percentile = 10, Realm = c("Freshwater", "Terrestrial")  , smallInlap10$summary.fixed[ 3:4, ]   ), 
             cbind(Percentile = 20, Realm = c("Freshwater", "Terrestrial")  , smallInlap20$summary.fixed[ 3:4, ]   ), 
             cbind(Percentile = 30, Realm = c("Freshwater", "Terrestrial")  , smallInlap30$summary.fixed[ 3:4, ]   ), 
             cbind(Percentile = 50, Realm = c("Freshwater", "Terrestrial")  , smallInlap50$summary.fixed[ 3:4, ]   ), 
             cbind(Percentile = 70, Realm = c("Freshwater", "Terrestrial")  , smallInlap70$summary.fixed[ 3:4, ]   ), 
             cbind(Percentile = 80, Realm = c("Freshwater", "Terrestrial")  , smallInlap80$summary.fixed[ 3:4, ]   ), 
             cbind(Percentile = 90, Realm = c("Freshwater", "Terrestrial")  , smallInlap90$summary.fixed[ 3:4, ]   ), 
             cbind(Percentile = 99, Realm = c("Freshwater", "Terrestrial")  , smallInlap99$summary.fixed[ 3:4, ]   ))

ggplot(ests) +
          geom_errorbar(aes(x=Percentile, ymin=`0.025quant` , ymax= `0.975quant`, color = Realm),alpha = 0.5,
                        size = 1, width=0,   position=position_dodge(width= 5))+  
          geom_errorbar(aes(x=Percentile ,ymin=`0.05quant`,ymax= `0.95quant`, color = Realm),alpha = 0.75,
                        size = 2, width = 0,  position=position_dodge(width= 5))+  
          geom_errorbar(aes(x=Percentile ,ymin=`0.1quant`,ymax= `0.9quant`, color = Realm), alpha = 1,
                        size = 3, width=0, position=position_dodge(width= 5))+  
          geom_point(aes(x=Percentile ,   y=mean,  shape = Realm), 
                     size = 2.5, position=  position_dodge(width = 5), color = "black", fill = "black",  alpha=1 )+
          scale_color_manual(values = col.scheme.realm)+
          scale_fill_manual(values = col.scheme.realm)+
          scale_shape_manual(values = shps)+
          geom_hline(yintercept=0,linetype="dashed") +
#          geom_text(aes(x = Continent , y = 0.03, label = text, fill = Realm),  
#                    position = position_dodge(width = 0.7), size = 3, color = 1) +
 #         coord_flip()+
  #        scale_y_continuous(breaks = brks,labels = l, limits=c(-0.03,0.034))+
   #       xlab ("")+ ylab("")+ #Trend slope | \n % change per year
          theme_clean #+
  #        theme(legend.key=element_blank(),
   #             legend.position='none', 
    #            axis.text.x=element_blank()) +
     #     geom_text(aes(x = 5.3 , y = -0.025, label = "B"),  
    #                size = 6, color = 1) 



ggplot(as.data.frame(dat), aes (y = log10(p10), x = Year,  color = Realm))+
 geom_point() +
  facet_wrap(.~ Plot_ID, scales = "free")




# check if model estimates f



  
hist(log10(line), 4, ylim = c(0,300))
lines(d, col = "red" )
abline(v = 0.01*mx , col = "yellow")
abline(v = 0.1*mx , col = "orange")
abline(v = 0.2*mx , col = "red")
abline(v = 0.3*mx , col = "purple")
abline(v = 0.5*mx , col = "blue")
abline(v = 0.7*mx , col = "purple")
abline(v = 0.8*mx , col = "red")
abline(v = 0.9*mx , col = "orange")
abline(v = 0.99*mx , col = "yellow")
 mean(d$y)
 median(d$y)
d$x[which(d$y == max(d$y))] # the x value at which y is max # this is the most freqent 'bin', equivalent to the mode 
d$y[200] # 20% quantile  

vec<- subset(n , Plot_ID ==153)
min(log10(vec$Number))
max(log10(vec$Number))

ggplot(vec, aes (x = log10(Number), color = as.factor(Year )))+
 geom_density() +
 	 scale_colour_viridis_d()



# this will work if I calculate the density estimates for the gradient from the 
# minimum to the maximum observed abundance value for any species over the whole time series 


plot(density(log10(line),  bw = "sj", from = mn, to =  mx, n = ))
hist(log10(line), 30)

hist(log10(line), 14, freq = F )
lines(density(log10(line),  bw = 0.2, from = mn, to =  mx, n = 100))
abline(v = 0.01*mx , col = "yellow")
abline(v = 0.1*mx , col = "orange")
abline(v = 0.2*mx , col = "red")
abline(v = 0.3*mx , col = "purple")
abline(v = 0.5*mx , col = "blue")
abline(v = 0.7*mx , col = "purple")
abline(v = 0.8*mx , col = "red")
abline(v = 0.9*mx , col = "orange")
abline(v = 0.99*mx , col = "yellow")





# plot some specific lines: 
 tst<- subset(dat, Plot_ID == 1698 & Year == 2003)
 line<- tst$Number[is.finite(log10( tst$Number))]
 
# manual calculation of negative part of the density curve  	 
 d<- 	 density(log10(line+1),  bw = 0.3,from = (mn - dif), to = mx ,  n = 200)$y	 
 negd <- rev(d)	 
 dsum<- d + negd
 
# this kind of works.  
hist(log10(line+1), xlim = c(-mx, mx), ylim = c(0,5),  freq = F)  
lines(d, x = seq(-mx, mx, length.out = 200), col = "red" )
lines(negd, x = seq(-mx, mx, length.out = 200), col = "blue" )
lines(dsum, x = seq(-mx, mx, length.out = 200), col = "black" )

 d<- 	 density(log10(line),  from = mn-dif, to = mx ,  n = 200)$y*S	 
 negd <- rev(d)	 
 dsum<- d + negd
 dif <- mx - mn
 
hist(log10(line+1), xlim = c(mn -dif, mx), 5,  ylim = c(0,20),  freq = T)  
lines(d, x = seq(mn - dif, mx, length.out = 200), col = "red" )
lines(negd, x = seq(mn-dif, mx,  length.out = 200), col = "blue" )
lines(dsum, x = seq(mn - dif, mx, length.out = 200), col = "black" )


dsumhalf<- dsum[101:200] # this is important! 
hist(log10(line), xlim = c(0, mx), 20,  ylim = c(0,300),  freq = T)  
lines(d[101:200], x = seq(0, mx, length.out = 100), col = "red" )
lines(negd[101:200], x = seq(0, mx, length.out = 100), col = "blue" )
lines(dsumhalf, x = seq(0, mx, length.out = 100), col = "black" )


  
  
d2<- 	 density(log10(line), from = -mx, to = mx , n = 100)$y	 
negd2 <- rev(d2)	 
dsum2<- d2 + negd
dsumhalf2<- dsum2[101:200] # this is important! 

 


# generate random line of abundances
samples<- unique(dat[, c("Plot_ID", "Year")]) 

randomLine<- function(){
smp<<-  samples[  sample(rownames(samples), 1), ]
randomsmp<<- subset (dat, Plot_ID == smp$Plot_ID & Year == smp$Year)
line <- randomsmp$Number
name<<- paste( unique(randomsmp$Datasource_name), unique(randomsmp$Plot_name))
line<<- line[is.finite(log10(line))]
S   <<- length(line)
mx  <<- max(log10(line))+0.5
}


hist(log10(line), xlim = c(0, mx), 8,  ylim = c(0,1),  freq = F)  
lines(dsumhalf)
lines(density(log10(line), from = 0, to = mx , n = 10)$y,  x = seq(0, mx, length.out = 10), col =  1)	 
lines(density(log10(line), from = 0, to = mx , n = 100)$y, x = seq(0, mx, length.out = 100), col = 2)	 
lines(density(log10(line), from = 0, to = mx , n = 200)$y, x = seq(0, mx, length.out = 200), col = 3)	 
lines(density(log10(line), from = 0, to = mx , n = 500)$y, x = seq(0, mx, length.out = 500), col = 4)	 
 	 
 # differet bandwiths	 
hist(log10(line), xlim = c(0, mx), 8,  ylim = c(0,1),  freq = F)  
lines(density(log10(line), from = 0, to = mx , bw = 0.2, n = 100)$y,  x = seq(0, mx, length.out = 100), col =  "black")	 
lines(density(log10(line), from = 0, to = mx , bw = 0.3, n = 100 )$y, x = seq(0, mx, length.out = 100), col = "red")	 
lines(density(log10(line), from = 0, to = mx , bw = "SJ", n = 100)$y, x = seq(0, mx, length.out = 100), col = "green")	 
lines(density(log10(line), from = 0, to = mx            , n = 100)$y, x = seq(0, mx, length.out = 100), col = "blue")	 
 	 

par(mfrow = c(4,4))
randomLine()

plt<- subset(dat, Plot_ID == sample(samples$Plot_ID, 1))
yrs<- sort(unique(plt$Year))

for(i in 1: 8) {

  randomsmp <- subset(plt, Year == yrs[i])
line <- randomsmp$Number
name<<-  unique(randomsmp$Year[])
line<- line[is.finite(log10(line))]
S   <<- length(line)
mx  <<- max(log10(line))+0.5
 # different bandwiths	 on S-corrected data
hist(log10(line), xlim = c(min(log10(line)), mx), ylim = c(0,1), 16, main = name ,   freq = T)  
lines(density(log10(line), from = 0, to = mx , bw = 0.2, n = 100)$y,  x = seq(0, mx, length.out = 100), col =  "black")	 
lines(density(log10(line), from = 0, to = mx , bw = 0.3, n = 100 )$y, x = seq(0, mx, length.out = 100), col = "red")	 
lines(density(log10(line), from = 0, to = mx , bw = "SJ", n = 100)$y, x = seq(0, mx, length.out = 100), col = "green")	 
lines(density(log10(line), from = 0, to = mx            , n = 100)$y, x = seq(0, mx, length.out = 100), col = "blue")	 
}

summary(lm()
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
## TRASH #####


n<- dat[dat$Number !=0 , ]

 dim(n)
 sum(n$Number)
 n50<- sample_n(n, 50)
 hist(log(n$Number))
 hist(log(n50$Number))
 

 library(e1071)
 skewness(log(n50$Number))
 
 
 datall <- NULL
 for (i in 1:10) {
 
 n10<- sample_n(n, 10)
 
 n50<- sample_n(n, 50)
n50<- dcast(n50, Taxon~ "Number", value.var = "Number", sum)
  n100<- sample_n(n, 100)
n100<- dcast(n100, Taxon~ "Number", value.var = "Number", sum)
 n500<- sample_n(n, 500)
n500<- dcast(n500, Taxon~ "Number", value.var = "Number", sum)
 n1000<- sample_n(n, 1000)
n1000<- dcast(n1000, Taxon~ "Number", value.var = "Number", sum)
 n2500<- sample_n(n, 2500)
n2500<- dcast(n2500, Taxon~ "Number", value.var = "Number", sum)
 n5000<- sample_n(n, 5000)
n5000<- dcast(n5000, Taxon~ "Number", value.var = "Number", sum)
 
 
dat<-  data.frame(
 n=  c(10, 50, 100,500,1000, 2500, 5000), 
 skew = c(skewness(log(n10$Number)), 
 				 skewness(log(n50$Number)), 
 				 skewness(log(n100$Number)), 
 				 skewness(log(n500$Number)), 
 				 skewness(log(n1000$Number)), 
 				 skewness(log(n2500$Number)), 
 				 skewness(log(n5000$Number)) 
 ))
 
datall<- rbind(datall, dat)
 }
 
 plot(datall$n, datall$skew)
hist(log(n5000$Number))


distr<-  Hungary %>% 
  group_by( Plot_ID, Year) %>%
  summarise(
    kurt = kurtosis(log(Number+1)),
    skew = skewness(log(Number+1)), 
    n = sum(Number))


distr<-   n %>% 
  group_by( Plot_ID, Year) %>%
  summarise(
    kurt = kurtosis(log(Number)),
    skew = skewness(log(Number)), 
    n = sum(Number), 
    S = length(unique(Taxon)))

 plot(distr$Year, distr$skew)
 plot(distr$Year, distr$kurt)
 
 # does skewness depend on N? (but there is no strong trend in N in this dataset)
 plot(distr$n, distr$skew)

 hist(log(n5000$Number))


 ggplot(distr, aes(x = Year, y = skew, color = as.factor(Plot_ID)))+
 	geom_point()+
 	stat_smooth(se = F)
 
ggplot(distr, aes(x = Year, y = kurt, color = as.factor(Plot_ID)))+
 	geom_point()+
 	stat_smooth(se = F)

 ggplot(distr, aes(x = Year, y = n, color = as.factor(Plot_ID)))+
 	geom_point()+
 	stat_smooth(se = F)

 ggplot(distr, aes(x = Year, y = S, color = as.factor(Plot_ID)))+
 	geom_point()+
 	stat_smooth(se = F)

 ggplot(subset(n , Plot_ID == 515), aes (x = log(Number)))+
 	geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(alpha=.2, fill="#FF6666") +
 	facet_wrap(Year ~ .)
 

 
# S and the density * S are heavily correlated! #####
par(mfrow = c(5,4)) 
 
plot(log(outAbs$S), log(outAbs$p1), main = paste(outAbs$Datasource_ID[1], outAbs$Plot_ID[1] ))
plot(log(outAbs$S), log(outAbs$p10))
plot(log(outAbs$S), log(outAbs$p50))
plot(log(outAbs$S), log(outAbs$p90))
 
# look at some random plots  
 smp<-   sample(randomPlots, 1)
randomsmp<- subset (outAbs, Plot_ID == smp)
plot(log(randomsmp$S), log(randomsmp$p1), main = paste(randomsmp$Datasource_ID[1], randomsmp$Plot_ID[1] ))
plot(log(randomsmp$S), log(randomsmp$p10))
plot(log(randomsmp$S), log(randomsmp$p50))
plot(log(randomsmp$S), log(randomsmp$p90))
 
 