# ROFSim - Transformer 3 - Run Caribou Model

# Packages ----------------------------------------------------------------

library(rsyncrosim)
library(caribouMetrics)
library(raster)
library(sf)
library(dplyr)
library(tidyr)

# Load Environment --------------------------------------------------------

e <- ssimEnvironment()
myLib <- ssimLibrary()
mySce <- scenario()

# Source helper functions -------------------------------------------------

source(file.path(e$PackageDirectory, "helpers.R"))

# Get all datasheets ------------------------------------------------------

myDatasheetsNames <- c("RasterFile", 
                       "ExternalFile", 
                       "RunCaribouRange", 
                       "HabitatModelOptions",
                       "CaribouDataSource")

loadDatasheet <- function(name){
  sheet <- tryCatch(
    {
      datasheet(mySce, name = name, lookupsAsFactors = FALSE, 
                optional = TRUE)
    },
    error = function(cond){
      return(NULL)
    }, 
    warning = function(cond){
      return(NULL)
    }
  )
}

allParams <- lapply(myDatasheetsNames, loadDatasheet)
names(allParams) <- myDatasheetsNames

# Modify the source data table
allParams$CaribouDataSourceWide <- allParams$CaribouDataSource %>% 
  pivot_longer(values_to = "VarID", names_to = "CaribouVarID", 
               cols=tidyselect::all_of(names(allParams$CaribouDataSource))) %>% 
  rowwise() %>% 
  mutate(type=ifelse(grepl("Raster", CaribouVarID, fixed=TRUE), "raster", "shapefile")) %>% 
  ungroup() %>% drop_na() %>% as.data.frame()

# Get variables -----------------------------------------------------------

if (nrow(allParams$RasterFile > 0)){
  allParams$RasterFile <- allParams$RasterFile %>% 
    left_join(filter(allParams$CaribouDataSourceWide, type=="raster"), 
              by = c("RastersID" = "VarID")) %>% 
    as.data.frame()
}

if (nrow(allParams$ExternalFile > 0)){
  allParams$ExternalFile <- allParams$ExternalFile %>% 
    left_join(filter(allParams$CaribouDataSourceWide, type == "shapefile"), 
              by = c("PolygonsID" = "VarID")) %>% 
    as.data.frame()
}

# Filter Timesteps --------------------------------------------------------

uniqueIterFromData <- 
  unique(c(allParams$ExternalFile$Iteration, 
           allParams$RasterFile$Iteration))
uniqueIterFromData <- uniqueIterFromData[!is.na(uniqueIterFromData)]

uniqueTsFromData <- 
  unique(c(allParams$ExternalFile$Timestep, 
           allParams$RasterFile$Timestep))
uniqueTsFromData <- uniqueTsFromData[!is.na(uniqueTsFromData)]

iterationSet <- GLOBAL_MinIteration:GLOBAL_MaxIteration
iterationSet <- iterationSet[iterationSet %in% uniqueIterFromData]

timestepSet <- GLOBAL_MinTimestep:GLOBAL_MaxTimestep
timestepSet <- timestepSet[timestepSet %in% uniqueTsFromData]

# Run model ---------------------------------------------------------------

envBeginSimulation(length(iterationSet) * length(timestepSet))

# Empty list to start
habitatUseAll <- NULL

# iteration <- iterationSet[1]
# timestep <- timestepSet[1]

for (iteration in iterationSet) {
  
  for (timestep in timestepSet) {
    #iteration=1;timestep=2011
    
    envReportProgress(iteration, timestep)
    
    # Filter inputs based on iteration and timestep
    InputRasters <- filterInputs(allParams$RasterFile, 
                                 iteration, timestep, min(timestepSet))
    InputVectors <- filterInputs(allParams$ExternalFile,
                                 iteration, timestep, min(timestepSet))
    
    # Call the main function with all arguments extracted from datasheets
    plcRas <-  tryCatch({
      raster(filter(InputRasters, CaribouVarID == "LandCoverRasterID")$File)
    }, error = function(cond) { stop("land cover can't be null") })
    
    # Reclass landcover if needed
    # TO DO: allow user to input plcLU table (same format at plcToResType in caribouMetrics package). If table is specified, reclass regardless of whether the number of classes is <9.
    if ((max(values(plcRas), na.rm = TRUE) <= 9)){
      warning(paste0("Assuming landcover classes are: ",paste(paste(resTypeCode$ResourceType,resTypeCode$code),collapse=",")))
    }else if((max(values(plcRas), na.rm = TRUE) == 29)){
      #TO DO: add PLC legend file to caribouMetrics package, and report here.
      warning(paste0("Assuming Ontario provincial landcover classes: ",paste(paste(plcToResType$ResourceType,plcToResType$PLCCode),collapse=",")))
      plcRas <- reclassPLC(plcRas,plcToResType)
    }else if((max(values(plcRas), na.rm = TRUE) == 39)){
      warning(paste0("Assuming national landcover classes: ",paste(paste(lccToResType$ResourceType,lccToResType$PLCCode),collapse=",")))
      plcRas <- reclassPLC(plcRas,lccToResType)
    }else{
      stop("Landcover classification not recognized. Please specify...")
    }
    
    eskerRas <- tryCatch({
      raster(filter(InputRasters, CaribouVarID == "EskerRasterID")$File)
    }, error = function(cond) { NULL })
    eskerPol <- tryCatch({
      st_read(filter(InputVectors, CaribouVarID == "EskerShapeFileID")$File)
    }, error = function(cond) { NULL })
    
    if(is.null(eskerPol)){
      eskerFinal <- eskerRas
    } else {
      eskerFinal <- eskerPol
    }
    
    ageRas <- tryCatch({
      raster(filter(InputRasters, CaribouVarID == "AgeRasterID")$File)
    }, error = function(cond) { NULL })
    
    natDistRas <- tryCatch({
      raster(filter(InputRasters, CaribouVarID == "NaturalDisturbanceRasterID")$File)
    }, error = function(cond) { NULL })
    
    anthroDistRas <- tryCatch({
      raster(filter(InputRasters, CaribouVarID == "AnthropogenicRasterID")$File)
    }, error = function(cond) { NULL })
    
    harvRas <- tryCatch({
      raster(filter(InputRasters, CaribouVarID == "HarvestRasterID")$File)
    }, error = function(cond) { NULL })
    
    linFeatRas <- tryCatch({
      filtered <- filter(InputRasters, CaribouVarID == "LinearFeatureRasterID")$File
      linFeatListRas <- lapply(filtered, raster)
    }, error = function(cond) { NULL })
    linFeatPol <- tryCatch({
      filtered <- filter(InputVectors, CaribouVarID == "LinearFeatureShapeFileID")$File
      linFeatListPol <- lapply(filtered, st_read)
    }, error = function(cond) { NULL })
    
    if(length(linFeatPol) == 0){
      linFeatFinal <- linFeatListRas
    } else {
      linFeatFinal <- linFeatListPol
    }
    
    projectPol <- tryCatch({
      st_read(filter(InputVectors, CaribouVarID == "ProjectShapeFileID")$File) %>% 
        # TODO implement better checks: verify if Range/RANGE_NAME are there 
        rename(Range = RANGE_NAME)
    }, error = function(cond) { NULL })
    
    # Rename range in expected format
    renamedRange <- rename(allParams$RunCaribouRange, coefRange = CoeffRange)
    
    projectPoltmp <- projectPol %>% 
      filter(Range %in% renamedRange$Range) 

    #SOON TO DO: handle missing inputs
    #TO DO: handle polygon inputs for natural disturbance, anthro disturbance, and harvest
    #TO DO: think though to improve computational efficiency.
    res <- caribouHabitat(
      landCover = plcRas , 
      esker = eskerFinal, 
      natDist = natDistRas,
      anthroDist = anthroDistRas,
      harv = harvRas,
      linFeat = linFeatFinal, 
      projectPoly = projectPoltmp,
      caribouRange = renamedRange,       # Caribou Range
      # Options
      padProjPoly = optArg(allParams$HabitatModelOptions$PadProjPoly),
      padFocal = optArg(allParams$HabitatModelOptions$PadFocal),
      doScale = optArg(allParams$HabitatModelOptions$doScale),
      # outputs are saved afterwards
      eskerSave = NULL,
      linFeatSave = NULL,
      saveOutput = NULL
    )
    
    #UI TO DO: add option to calculate and output disturbance metrics
    #UI TO DO: make habitat use calculation optional
    #TO DO: check that disturbanceMetrics calculations handle multiple ranges properly
    #UI TO DO: make buffer width an argument
    
    doDistMetrics=F
    #SOON TO DO: figure out why this is failing - likely not coping well with missing inputs...
    #TO DO: accept anthropogenic disturbance polygons or rasters, and behave properly when they are missing.
    if(doDistMetrics){
      fullDist <- disturbanceMetrics(
        landCover=!is.na(plcRas),
        natDist = natDistRas,
        harv = harvRas,
        linFeat = linFeatFinal,
        projectPoly = projectPoltmp,
        padFocal = optArg(allParams$HabitatModelOptions$PadFocal),
        bufferWidth =  500 
      )
    }
    
    ## Save to DATA folder
    #TO DO: add option to save elements of res@processedData
    writeRaster(res@habitatUse, bylayer = TRUE, format = "GTiff",
                suffix = paste(names(res@habitatUse), 
                               paste(renamedRange$Range, collapse = "_"),
                               paste0("it_",iteration), 
                               paste0("ts_",timestep), sep = "_"),
                filename = file.path(e$TransferDirectory, "OutputHabitatUse"), 
                overwrite = TRUE)
    
    # Build df and save the datasheet
    habitatUseDf <- data.frame(SeasonID = names(res@habitatUse), 
                               Iteration = iteration,
                               Timestep = timestep)
    habitatUseDf$FileName <- file.path(e$TransferDirectory, 
                                       paste0(paste("OutputHabitatUse",
                                                    habitatUseDf$Season,
                                                    paste(renamedRange$Range, collapse = "_"),
                                                    "it", habitatUseDf$Iteration, 
                                                    "ts", habitatUseDf$Timestep,
                                                    sep= "_"), ".tif"))
    habitatUseDf <- habitatUseDf %>% 
      expand_grid(RangeID = renamedRange$Range)
    
    habitatUseAll[[paste0("it_",iteration)]][[paste0("ts_",timestep)]] <- 
      habitatUseDf
    
  }
}


habitatUseMerged <- bind_rows(unlist(habitatUseAll, recursive = F))

saveDatasheet(ssimObject = mySce, name = "OutputSpatialHabitat", data = habitatUseMerged)
envEndSimulation()
