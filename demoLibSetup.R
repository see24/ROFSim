#install.packages("C:/Users/HughesJo/Documents/rsyncrosim_1.3.0.tar.gz",repos=NULL,type="source")
library(rsyncrosim)

cDir = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFModel/UI"

sourceData = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFData/Used"

inPath = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFModel/SpaDESOutputs/v1/ROF_CCSM4_RCP45_res125_rep01/outputs/ROF_CCSM4_RCP45_res125_rep01/ROF_CCSM4_RCP45_res125_rep01.qs"

libName = "ROFDemo4"

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
cc=data.frame(RunDistMetrics=T,RunCaribouHabitat=F,RunDemographicModel=T)
#delete(cbScn,datasheet=cSheet,force=T)
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
cSheet="ROFSim_RasterFile"
cc=data.frame(RastersID="Harvest",Filename=paste0(sourceData,"/harvMNRF2018_250.tif"))
cc$Timestep=NA
cc=rbind(cc,data.frame(RastersID="Anthropogenic Disturbance",Timestep=2040,Filename=paste0(sourceData,"/mines_ras250.tif")))
saveDatasheet(cbsScn,cc,name=cSheet,append=F)
datasheet(cbsScn,cSheet)

cSheet="ROFSim_CaribouDataSource"
cc=data.frame(LandCoverRasterID="Provincial Land Cover",ProjectShapeFileID="Ranges",EskerShapeFileID="Eskers",LinearFeatureShapeFileID="Linear Features",NaturalDisturbanceRasterID="Stand Age",HarvestRasterID="Harvest",AnthroDisturbanceRasterID="Anthropogenic Disturbance")
saveDatasheet(cbsScn,cc,name=cSheet,append=F)
datasheet(cbsScn,cSheet)

dependency(cbsScn,spRes)
dependency(cbsScn,lccRes)
mergeDependencies(cbsScn)=T
