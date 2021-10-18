#install.packages("C:/Users/HughesJo/Documents/rsyncrosim_1.3.0.tar.gz",repos=NULL,type="source")
library(rsyncrosim)

cDir = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFModel/UI"

sourceData = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFData/Used"

inPath = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFModel/SpaDESOutputs/v1/ROF_CCSM4_RCP45_res125_rep01/outputs/ROF_CCSM4_RCP45_res125_rep01/ROF_CCSM4_RCP45_res125_rep01.qs"

libName = "ROFDemo"

#delete(paste0(cDir,"/",libName,".ssim"),force=T)

cLib = ssimLibrary(paste0(cDir,"/",libName),package="ROFSim")

cProj= project(cLib,"Demo")

datasheet(cProj)

#TO DO: extract this info from input range map
cSheet="ROFSim_CaribouRange"
cc=data.frame(Name=c("James Bay","Missisa","Ozhiski","Nipigon","Pagwachuan"))
saveDatasheet(cProj,cc,name=cSheet)
datasheet(cProj,cSheet)

cSheet="ROFSim_SpaDESSimObject"
cc=data.frame(Name=c("burnMap","biomassMap","rstLCC","standAgeMap"),Description=c("cumulative burn map","total biomass (g/m^2) filtered by cohortData","Map of land cover classes","Map of time since transition"))
saveDatasheet(cProj,cc,name=cSheet)
datasheet(cProj,cSheet)

cSheet="ROFSim_Rasters"
cc=data.frame(Name=c("Caribou Ranges","Harvest","Anthropogenic Disturbance","Natural Disturbances","Provincial Land Cover","Stand Age"))
cc$SpaDESSimObject[cc$Name=="Stand Age"]="standAgeMap"
saveDatasheet(cProj,cc,name=cSheet)
datasheet(cProj,cSheet,optional=T)

############
#scenarios - run control
rcScnS = scenario(cProj,"Run Control 2020, 2100")
#TO DO: Ask Val how to get "Total Iterations" option
cSheet="ROFSim_RunControl"
cc=data.frame(MinimumIteration=1,MaximumIteration=2,MinimumTimestep=2020,MaximumTimestep=2100,OutputFrequency=10)
saveDatasheet(rcScnS,cc,name=cSheet)
datasheet(rcScnS,cSheet,optional=T)

rcScn = scenario(cProj,"Run Control 2020")
cSheet="ROFSim_RunControl"
cc=data.frame(MinimumIteration=1,MaximumIteration=1,MinimumTimestep=2020,MaximumTimestep=2020)
saveDatasheet(rcScn,cc,name=cSheet)
datasheet(rcScn,cSheet,optional=T)


############
#scenarios - caribou - current

cbScn = scenario(cProj,"Caribou - current")

cSheet="core_Pipeline"
cc=data.frame(StageNameID="Caribou Habitat",RunOrder=1)
saveDatasheet(cbScn,cc,name=cSheet)
datasheet(cbScn,cSheet)

cSheet="ROFSim_RunCaribouRange"
cc=data.frame(Range="Missisa",CoeffRange="Missisa")
saveDatasheet(cbScn,cc,name=cSheet)
datasheet(cbScn,cSheet)

cSheet="ROFSim_CaribouDataSource"
cc=data.frame(LandCoverRasterID="Provincial Land Cover",ProjectShapeFileID="Ranges",EskerShapeFileID="Eskers",LinearFeatureShapeFileID="Linear Features",NaturalDisturbanceRasterID="Natural Disturbances",HarvestRasterID="Harvest",AnthropogenicRasterID="Anthropogenic Disturbance")
saveDatasheet(cbScn,cc,name=cSheet)
datasheet(cbScn,cSheet)

cSheet="ROFSim_CaribouModelOptions"
cc=data.frame(RunDistMetrics=T,RunCaribouHabitat=T,RunDemographicModel=T,padProjPoly=T)
saveDatasheet(cbScn,cc,name=cSheet)
datasheet(cbScn,cSheet)

cSheet="ROFSim_RasterFile"
cc=data.frame(RastersID="Natural Disturbances",Filename=paste0(sourceData,"/fireAFFES2020_250.tif"))
cc=rbind(cc,data.frame(RastersID="Harvest",Filename=paste0(sourceData,"/harvMNRF2018_250.tif")))
cc=rbind(cc,data.frame(RastersID="Provincial Land Cover",Filename=paste0(sourceData,"/plc250.tif")))
saveDatasheet(cbScn,cc,name=cSheet,append=F)
datasheet(cbScn,cSheet)

cSheet="ROFSim_ExternalFile"
cc=data.frame(PolygonsID="Eskers",File=paste0(sourceData,"/esker.shp"))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/rail.shp")))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/util2020.shp")))
cc=rbind(cc,data.frame(PolygonsID="Ranges",File=paste0(sourceData,"/project_ranges.shp")))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/road_ORNMNRFROF2020.shp")))
saveDatasheet(cbScn,cc,name=cSheet,append=F)
datasheet(cbScn,cSheet)

dependency(cbScn,rcScn)
datasheet(cbScn)

cbRes = run(cbScn)

#############
#Caribou with change in anthropogenic disturbance
cbcScn = scenario(cProj,"Caribou - anthro",sourceScenario=cbScn)
dependency(cbcScn,rcScn,remove=T,force=T)
dependency(cbcScn,rcScnS)

cSheet="ROFSim_RasterFile"
cc=data.frame(RastersID="Natural Disturbances",Filename=paste0(sourceData,"/fireAFFES2020_250.tif"))
cc=rbind(cc,data.frame(RastersID="Harvest",Filename=paste0(sourceData,"/harvMNRF2018_250.tif")))
cc=rbind(cc,data.frame(RastersID="Provincial Land Cover",Filename=paste0(sourceData,"/plc250.tif")))
cc$Timestep=NA
cc=rbind(cc,data.frame(RastersID="Anthropogenic Disturbance",Timestep=2040,Filename=paste0(sourceData,"/mines_ras250.tif")))
saveDatasheet(cbcScn,cc,name=cSheet,append=F)
datasheet(cbcScn,cSheet)

cSheet="ROFSim_ExternalFile"
cc=data.frame(PolygonsID="Eskers",File=paste0(sourceData,"/esker.shp"))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/rail.shp")))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/util2020.shp")))
cc=rbind(cc,data.frame(PolygonsID="Ranges",File=paste0(sourceData,"/project_ranges.shp")))
cc$Timestep=NA
cc=rbind(cc,data.frame(PolygonsID="Linear Features",Timestep=2020,File=paste0(sourceData,"/road_ORNMNRFROF2020.shp")))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",Timestep=2030,File=paste0(sourceData,"/RoF_MNRF_2020.shp")))
saveDatasheet(cbcScn,cc,name=cSheet,append=F)
datasheet(cbcScn,cSheet)

cbcRes = run(cbcScn)

############
#scenarios - import SpaDES
spScn = scenario(cProj,"Import SpaDES")

cSheet="core_Pipeline"
cc=data.frame(StageNameID="Spades Import",RunOrder=1)
saveDatasheet(spScn,cc,name=cSheet)
datasheet(spScn,cSheet)

dependency(spScn,rcScnS)

cSheet="ROFSim_SpaDESGeneral"
cc=data.frame(Iteration=c(1,2),Filename=c(inPath,gsub("rep01","rep03",inPath,fixed=T)))
saveDatasheet(spScn,cc,name=cSheet)
datasheet(spScn,cSheet)

cSheet="ROFSim_SpaDESRuntimeRasters"
cc=data.frame(RastersID=c("Stand Age"))
saveDatasheet(spScn,cc,name=cSheet)
datasheet(spScn,cSheet)

datasheet(spScn,cSheet,optional=T)

spRes = run(spScn)
#TO DO: figure out how to landcover legend table, stand age colours, etc.

############
#scenarios - make LCC from SpaDES
siScn = scenario(cProj,"Make LCC from SpaDES")
dependency(siScn,spScn)
mergeDependencies(siScn)=T

cSheet="core_Pipeline"
cc=data.frame(StageNameID="Generate LCC from Cohort Data",RunOrder=1)
saveDatasheet(siScn,cc,name=cSheet)
datasheet(siScn,cSheet)

lccRes = run(siScn)

###############
#Spades caribou scenario
cbsScn = scenario(cProj,"Caribou - spades - anthro",sourceScenario=cbcScn)
#cbsScn = scenario(cProj,"Caribou - spades - anthro")
cSheet="ROFSim_RasterFile"
cc=data.frame(RastersID="Harvest",Filename=paste0(sourceData,"/harvMNRF2018_250.tif"))
cc$Timestep=NA
cc=rbind(cc,data.frame(RastersID="Anthropogenic Disturbance",Timestep=2040,Filename=paste0(sourceData,"/mines_ras250.tif")))
saveDatasheet(cbsScn,cc,name=cSheet,append=F)
datasheet(cbsScn,cSheet)

cSheet="ROFSim_CaribouDataSource"
cc=data.frame(LandCoverRasterID="Provincial Land Cover",ProjectShapeFileID="Ranges",EskerShapeFileID="Eskers",LinearFeatureShapeFileID="Linear Features",NaturalDisturbanceRasterID="Stand Age",HarvestRasterID="Harvest",AnthropogenicRasterID="Anthropogenic Disturbance")
saveDatasheet(cbsScn,cc,name=cSheet,append=F)
datasheet(cbsScn,cSheet)

dependency(cbsScn,spRes)
dependency(cbsScn,lccRes)
mergeDependencies(cbsScn)=T

cbsRes = run(cbsScn)

##########
#add legend to landcover - after map is created in UI

#lccRes = scenario(cProj,10)
#install.packages("RColorBrewer")
library(RColorBrewer)
library(dplyr)
library(readr)
library(raster)

#example map
filepath(cLib)
scenarioId(lccRes)
mapPath = paste0(filepath(cLib),".input/Scenario-",scenarioId(lccRes),"/ROFSim_RasterFile/PLC_it_1_ts_2020.tif")
iMap = raster(mapPath)
fTab = freq(iMap)
fTab

# name of the map that needs a legend
mapName <- "ChangeView"

# Setting path to custom legend
legendPath <- "."
fileName <- "colormap_mapID_ROFSim_InputRastersMap-IDtemplateB.txt"
fileNameMod <- "colormap_mapID_ROFSim_InputRastersMap-ID.txt"

# get the list of charts to identify which one needs a legend
myCharts <- datasheet(cProj, 
                      name = "corestime_Maps", 
                      includeKey = T)
myCharts

myCharts$Criteria

# ID value for map that needs legend is
myChart <- filter(myCharts, Name == mapName)
mapId <- myChart$MapID[1]
mapId <- paste0("map", as.character(mapId))
rasterId <- myChart$Criteria[1]
rasterId=gsub("Map2","Map",rasterId,fixed=T)
rasterId <- c(44,18)#abs(parse_number(strsplit(rasterId,"|",fixed=T)[[1]]))
#empirically, 18 and 45 sort of work.

newFileName <- gsub("mapID", mapId, fileNameMod)
newFileNames=list()
if(length(rasterId)==1){
  newFileNames[["a"]] <- gsub("ID", as.character(rasterId), newFileName)
}else{
  for(rr in rasterId){
    newFileNames[[as.character(rr)]] <- gsub("ID", as.character(rr), newFileName)
  }
}

# get the legend directory for the library
libProperties <- ssimLibrary(cLib, summary = T)
legendDir <- filter(libProperties, property == "External input files:")
legendDir <- as.character(legendDir$value)
legendDir <- paste0(legendDir,"\\Project-",as.character(projectId(cProj)))

#edit legend file
lTab = read.csv(paste(legendPath, fileName, sep="/"))
names(lTab)=c("ID","RGB1","RGB2","RGB3","C","Label")
lTab$ID = as.numeric(lTab$ID)
lTab=subset(lTab,!is.na(RGB1))

omitRare = subset(data.frame(fTab),count<1)
names(omitRare)=c("ID","frequency")
merge(lTab,omitRare)

lTab
#evergreens -
evergreens = subset(lTab,is.element(Label,c("Coniferous Forest","Coniferous Swamp")))
evergreens = evergreens[order(-evergreens$ID),]
evergreenCols = brewer.pal(nrow(evergreens)+1,name="Greens")
evergreenCols = col2rgb(evergreenCols)
evergreenCols= evergreenCols[,-1]
evergreens$RGB1=evergreenCols[1,]
evergreens$RGB2=evergreenCols[2,]
evergreens$RGB3=evergreenCols[3,]
#evergreens$RGB1=0
#evergreens$RGB2=109
#evergreens$RGB3=44

lTab
#mixed
mix = subset(lTab,is.element(Label,c("Mixed Forest","Sparse Forest","Treed Fen","Tree Bog")))
mix = mix[order(-mix$ID),]
mixCols = brewer.pal(nrow(mix)+1,name="Oranges")
mixCols = col2rgb(mixCols)
mixCols=mixCols[,-1]
mix$RGB1=mixCols[1,]
mix$RGB2=mixCols[2,]
mix$RGB3=mixCols[3,]

#decid
decid = subset(lTab,is.element(Label,c("Deciduous Forest","Deciduous Swamp")))
decid = decid[order(-decid$ID),]
decidCols = brewer.pal(nrow(decid)+1,name="Purples")
decidCols = col2rgb(decidCols)
decidCols=decidCols[,-1]
decid$RGB1=decidCols[1,]
decid$RGB2=decidCols[2,]
decid$RGB3=decidCols[3,]

combo = rbind(evergreens,mix,decid)

lTab
#wet
wet = subset(lTab,is.element(Label,c("Water - Deep or Clear","Water - Shallow or Sedimented","Mudflats"))|grepl("Marsh",Label)|grepl("Swamp",Label)|grepl("Fen",Label)|grepl("Bog",Label))
wet=subset(wet,!is.element(ID,combo$ID))
wet = wet[order(-wet$ID),]
wetCols = brewer.pal(nrow(wet)+1,name="Blues")
wetCols = col2rgb(wetCols)
wetCols=wetCols[,-1]
wet$RGB1=wetCols[1,]
wet$RGB2=wetCols[2,]
wet$RGB3=wetCols[3,]


combo = rbind(evergreens,mix,decid,wet)

remainder = subset(lTab,!is.element(ID,combo$ID))
remainder

#dryLow = subset(remainder,!grepl("No Data",remainder$Label,fixed=T)&!grepl("Burns",remainder$Label,fixed=T)&!grepl("Regenerating Depletion",remainder$Label,fixed=T)&!grepl("Cloud and Shadow",remainder$Label,fixed=T)&!grepl("Other",remainder$Label,fixed=T)&!grepl("Tundra Heath",remainder$Label,fixed=T))
#dryLow = dryLow[order(dryLow$ID),]
#dryCols = brewer.pal(nrow(dryLow)+1,name="Greys")
#dryCols = col2rgb(dryCols)
#dryCols=dryCols[,-1]
#dryLow$RGB1=dryCols[1,]
#dryLow$RGB2=dryCols[2,]
#dryLow$RGB3=dryCols[3,]

combo = rbind(evergreens,mix,decid,wet)

remainder = subset(lTab,!is.element(ID,combo$ID))
remainder
remainder$RGB1 = 0
remainder$RGB2 = 0
remainder$RGB3 = 0

remainder$RGB1[grepl("Burns",remainder$Label)] = 197
remainder$RGB2[grepl("Burns",remainder$Label)] = 27
remainder$RGB3[grepl("Burns",remainder$Label)] = 138

remainder$RGB1[grepl("Regenerating Depletion",remainder$Label)] = 250
remainder$RGB2[grepl("Regenerating Depletion",remainder$Label)] = 159
remainder$RGB3[grepl("Regenerating Depletion",remainder$Label)] = 181


combo = rbind(evergreens,mix,decid,wet,remainder)

lines = c("# Syncrosim Generated Provincial Land Cover Color Map (QGIS-compatible),,,,,",
  "INTERPOLATION:DISCRETE")

lines = c(lines,paste(combo$ID,combo$RGB1,combo$RGB2,combo$RGB3,combo$C,combo$Label,sep=","))

fileConn<-file(paste(legendPath, fileNameMod, sep="/"))
writeLines(lines, fileConn)
close(fileConn)


# copy the custom legend to the legend directory
sourceFile <- paste(legendPath, fileNameMod, sep="/")
for(nn in newFileNames){
  destFile <- paste(legendDir, nn, sep = "/")
  file.copy(sourceFile, destFile, overwrite = T)
}

# delete the temp folder to get rid of any cached bitmaps
tempDir <- filter(libProperties, property == "Temporary files:")
tempDir <- as.character(tempDir$value)
unlink(tempDir, recursive = T, force = T)

