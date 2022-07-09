
#x is a full inla object
# data is the completeData dataframe used to generate the model output (x)
# unit is the biodiversity change metric under study  as it should be written on the axis as it should be 


makeGraphData<- function(x, dat, unit){
	metadata<-  dat %>% 
  group_by(Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)), 
    Unit = unit) 


# get probabilities of including 0 , one sided test
ps<- NULL
for(i in 3: nrow(x$summary.fixed)){
  p<-inla.pmarginal(0, x$marginals.fixed[[i]])
  ps<- c(ps, p) };ps



Slope<- as.data.frame(x$summary.fixed)[3:4,]

vars<-data.frame(do.call(rbind, strsplit(rownames(Slope), split = ":")))
Slope<-cbind(Slope, vars)
Slope$X1<-gsub("Realm", "", Slope$X1)

Slope$P <- round(ps, 3)
Slope$ptxt<- paste0("p = ", round(Slope$P, 3))
Slope<- merge(metadata, Slope,  by.y= "X1", by.x = "Realm")
Slope$text = paste0("(", Slope$Datasources, " | ", Slope$Plots, ")")


return(Slope)
	

}




ExtractRandEf<-function(x, dat){
	
RandEfDataset <- 	unique(dat[,c("Datasource_ID", "Datasource_name", "Datasource_ID_4INLA", "Datasource_ID_4INLAs", "Realm")])
#pull out random intercepts and slopes:

#data source ID
intercepts     <- model$summary.random$Datasource_ID_4INLA
slopes         <- model$summary.random$Datasource_ID_4INLAs
slopes_Location<-model$summary.random$Location_4INLAs
slopes_plot    <-model$summary.random$Plot_ID_4INLAs
names(intercepts)[2:ncol(intercepts)]      <- paste("DataID_Intercept_", names(intercepts)[2:ncol(intercepts)]) # names for dataset intercepts
names(slopes)[2:ncol(intercepts)]          <- paste("DataID_Slope_", names(slopes)[2:ncol(intercepts)])             # names for dataset slopes
names(slopes_Location)[2:ncol(intercepts)] <- paste("Loc_slp_", names(slopes_Location)[2:ncol(intercepts)]) # names for Location slopes
names(slopes_plot)[2:ncol(intercepts)]     <- paste("Plot_slp_", names(slopes_plot)[2:ncol(intercepts)])        # names for plot slopes

# datasource level slopes for Fig 1
RandEfDataset <- merge(RandEfDataset, intercepts, by.x="Datasource_ID_4INLA", by.y="ID")
RandEfDataset <- merge(RandEfDataset, slopes, by.x="Datasource_ID_4INLAs", by.y="ID")

# add up fixed slope and random slopes

fx<-data.frame(Realm = c("Freshwater", "Terrestrial"), # df for fixed effects. because we use the moel with only 'year' no differences between realms
               fixedSlp = model$summary.fixed$mean[3:4], 
               fixedIntercept = (model$summary.fixed$mean[1:2]  ) )
RandEfDataset<- merge(RandEfDataset, fx, by = "Realm" )
RandEfDataset$slope <- RandEfDataset$'DataID_Slope_ mean'+ RandEfDataset$fixedSlp # sum of fixed and random slopes  
#RandEfDataset$intercept <- RandEfDataset$'DataID_Intercept_ mean'+ RandEfDataset$fixedIntercept # sum of fixed and random intercepts  

return(RandEfDataset)
}


