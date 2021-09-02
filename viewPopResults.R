library(rsyncrosim)
library(ggplot2)
theme_set(theme_bw())

#Here is the sort of thing I wish to show from the demographic model results.
#Can't easily be done in the UI because there are many replicate trajectories for each iteration (i.e. input landscape)

#Set paths, library, and scenario
cDir = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFModel/UI"
sourceData = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFData"
libName = "ROFDemo2"
exampleScn = 17 #example result scenario

#Load result tables
cLib = ssimLibrary(paste0(cDir,"/",libName))
rScn = scenario(cLib,exampleScn) 
distMetrics= datasheet(rScn,"OutputDisturbanceMetrics")
popMetrics= datasheet(rScn,"OutputPopulationMetrics")
popMetrics$MetricTypeID=as.character(popMetrics$MetricTypeID)
popMetrics$Replicate=paste0("x",popMetrics$Replicate)
popMetrics$Amount[popMetrics$MetricTypeID=="N"]=log10(popMetrics$Amount[popMetrics$MetricTypeID=="N"]+0.001)
popMetrics$MetricTypeID[popMetrics$MetricTypeID=="N"]="log10N"

#See changes in disturbance metrics over time
base1 <- ggplot(data = distMetrics, 
                aes(x = Timestep, y = Amount))+
  geom_line(size = 0.5)+facet_grid(MetricTypeID~Iteration,scales="free")+
  xlab("Time")+
  ylab("Response")+
  theme(legend.position = "none")
base1

#See changes in demographic metrics over time
base2 <- ggplot(data = popMetrics, 
                aes(x = Timestep, y = Amount, group=Replicate,colour=Replicate))+
  geom_line(size = 0.5)+facet_grid(MetricTypeID~Iteration,scales="free")+
  xlab("Time")+
  ylab("Response")+
  theme(legend.position = "none")
base2

