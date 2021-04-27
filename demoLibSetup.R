
library(rsyncrosim)

cDir = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFModel/UI"

sourceData = "C:/Users/HughesJo/Documents/InitialWork/OntarioFarNorth/RoFData"

delete(paste0(cDir,"/ROFDemo3.ssim"),force=T)

#TO DO: 
#Set subscenarios as in ROFDemo

libName = "ROFDemo3"

cLib = ssimLibrary(paste0(cDir,"/",libName),package="ROFSim")

cProj= project(cLib,"Demo1")

cSheet="ROFSim_CaribouRange"
cc=data.frame(Name=c("James Bay","Missisa","Ozhiski","Nipigon","Pagwachuan"))
saveDatasheet(cProj,cc,name=cSheet)
datasheet(cProj,cSheet)

cSheet="ROFSim_MetricType"
cc=data.frame(Name=c("anthroBuff","natDist","totalDist"))
saveDatasheet(cProj,cc,name=cSheet)
datasheet(cProj,cSheet)

cSheet="ROFSim_Season"
cc=data.frame(Name=c("Winter","Spring","Summer","Fall"))
saveDatasheet(cProj,cc,name=cSheet)
datasheet(cProj,cSheet)

cSheet="ROFSim_SpaDESSimObject"
cc=data.frame(Name=c("burnMap"))
saveDatasheet(cProj,cc,name=cSheet)
datasheet(cProj,cSheet)

cSheet="ROFSim_Rasters"
cc=data.frame(Name=c("Fire Disturbance","Harvest","Provincial Land Cover"))
cc$SpaDESSimObject[cc$Name=="Fire Disturbance"]="burnMap"
saveDatasheet(cProj,cc,name=cSheet)
datasheet(cProj,cSheet,optional=T)

cSheet="ROFSim_Polygons"
cc=data.frame(Name=c("Eskers","Linear Features","Ranges"))
saveDatasheet(cProj,cc,name=cSheet)
datasheet(cProj,cSheet)

############
#scenarios - run control
rcScn = scenario(cProj,"Run Control")

cSheet="ROFSim_RunControl"
cc=data.frame(MinimumIteration=1,MaximumIteration=1,MinimumTimestep=2020,MaximumTimestep=2020)
saveDatasheet(rcScn,cc,name=cSheet)
datasheet(rcScn,cSheet)

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
cc=data.frame(LandCoverRasterID="Provincial Land Cover",ProjectShapeFileID="Ranges",EskerShapeFileID="Eskers",LinearFeatureShapeFileID="Linear Features",NaturalDisturbanceRasterID="Fire Disturbance",HarvestRasterID="Harvest")
saveDatasheet(cbScn,cc,name=cSheet)
datasheet(cbScn,cSheet)

cSheet="ROFSim_HabitatModelOptions"
cc=data.frame(RunDistMetrics=T,RunCaribouHabitat=F,ECCCBufferWidth=500,PadProjPoly=F,PadFocal=F,doScale=F)
saveDatasheet(cbScn,cc,name=cSheet)
datasheet(cbScn,cSheet)

cSheet="ROFSim_RasterFile"
cc=data.frame(RastersID="Fire Disturbance",Filename=paste0(sourceData,"/fireAFFES2020_250.tif"))
cc=rbind(cc,data.frame(RastersID="Harvest",Filename=paste0(sourceData,"/harvMNRF2018_250.tif")))
cc=rbind(cc,data.frame(RastersID="Provincial Land Cover",Filename=paste0(sourceData,"/plc250.tif")))
delete(cbScn,datasheet=cSheet,force=T)
saveDatasheet(cbScn,cc,name=cSheet)
datasheet(cbScn,cSheet)


cSheet="ROFSim_ExternalFile"
cc=data.frame(PolygonsID="Eskers",File=paste0(sourceData,"/esker.shp"))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/rail.shp")))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/road_ORNMNRFROF2020.shp")))
cc=rbind(cc,data.frame(PolygonsID="Linear Features",File=paste0(sourceData,"/util2020.shp")))
cc=rbind(cc,data.frame(PolygonsID="Ranges",File=paste0(sourceData,"/project_ranges.shp")))
delete(cbScn,datasheet=cSheet,force=T)
saveDatasheet(cbScn,cc,name=cSheet)
datasheet(cbScn,cSheet)

dependency(cbScn,rcScn)
datasheet(cbScn)

############
#scenarios - SpaDES import

siScn = scenario(cProj,"SpaDES import")
