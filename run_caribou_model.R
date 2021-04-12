## ROF SIM Prototype Package
## Second Transformer
## Running Caribou RSF Model

print("Secondary Transformer: running caribou model")

# Load Packages
library(rsyncrosim)
library(caribouMetrics)
library(raster)
library(sf)
library(dplyr)
library(tidyr)

# Load environment
e <- ssimEnvironment()
myLib <- ssimLibrary()
mySce <- scenario()

# Source the helpers
source(file.path(e$PackageDirectory, "helpers.R"))

# Get datasheets
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
           allParams$RasterFile$Iteration, 
           allParams$DataSummary$Iteration))
uniqueIterFromData <- uniqueIterFromData[!is.na(uniqueIterFromData)]

uniqueTsFromData <- 
  unique(c(allParams$ExternalFile$Timestep, 
           allParams$RasterFile$Timestep, 
           allParams$DataSummary$Timestep))
uniqueTsFromData <- uniqueTsFromData[!is.na(uniqueTsFromData)]

iterationSet <- GLOBAL_MinIteration:GLOBAL_MaxIteration
iterationSet <- iterationSet[iterationSet %in% uniqueIterFromData]

timestepSet <- GLOBAL_MinTimestep:GLOBAL_MaxTimestep
timestepSet <- timestepSet[timestepSet %in% uniqueTsFromData]

#Simulation
envBeginSimulation(length(iterationSet) * length(timestepSet))

# Run model ---------------------------------------------------------------

# Empty list to start
habitatUseAll <- NULL

# iteration <- iterationSet[1]
# timestep <- timestepSet[1]

for (iteration in iterationSet) {
  
  for (timestep in timestepSet) {
    
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
    
    # Verify if reclassing PLC is needed
    if (!(max(values(plcRas), na.rm = TRUE) <= 9)){
      plcRas <- reclassPLC(plcRas)
    }
    
    # Change resolution if needed
    if (res(plcRas)[1] != 250){
      aggFact <- 250/res(plcRas)[1]
      plcRas <- raster::aggregate(plcRas, aggFact, fun = raster::modal)
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
      # TODO This is a bold assumption, but the current implementation of 
      # Caribou metrics is not flexible to unnamed features
      names(linFeatListRas) <- c("rail", "roads", "utilities")[1:length(linFeatListRas)]
    }, error = function(cond) { NULL })
    linFeatPol <- tryCatch({
      filtered <- filter(InputVectors, CaribouVarID == "LinearFeatureShapeFileID")$File
      linFeatListPol <- lapply(filtered, st_read)
      # TODO This is a bold assumption, but the current implementation of 
      # Caribou metrics is not flexible to unnamed features
      names(linFeatListPol) <- c("rail", "roads", "utilities")[1:length(linFeatListPol)]
    }, error = function(cond) { NULL })
    
    if(length(linFeatPol) == 0){
      linFeatFinal <- linFeatListRas
    } else {
      linFeatFinal <- linFeatListPol
    }
    
    projectPol <- tryCatch({
      st_read(filter(InputVectors, CaribouVarID == "ProjectShapeFileID")$File) %>% 
        # TODO implement better checks: verify if Range/RANGE_NAME aare there 
        rename(Range = RANGE_NAME)
    }, error = function(cond) { NULL })
    
    # Rename range in expected format
    renamedRange <- rename(allParams$RunCaribouRange, coefRange = CoeffRange)
    
    for(row in seq_len(nrow(renamedRange))) {
      
      theRangeDf <- renamedRange[row,]
      
      projectPoltmp <- projectPol %>% 
        filter(Range %in% theRangeDf$Range) 
      
      res <- caribouHabitat(
        
        landCover = plcRas , 
        
        esker = eskerFinal, 
        
        # TODO Implement this input once link with spades is defined
        # updatedLC = friRas,
        
        # Commented out due to #33
        age = ageRas, 
        
        natDist = natDistRas,
        
        anthroDist = anthroDistRas,
        
        harv = harvRas,
        
        linFeat = linFeatFinal, 
        
        projectPoly = projectPoltmp,
        
        # Caribou Range
        caribouRange = theRangeDf,
        
        # Options
        padProjPoly = optArg(allParams$HabitatModelOptions$PadProjPoly),
        padFocal = optArg(allParams$HabitatModelOptions$PadFocal),
        doScale = optArg(allParams$HabitatModelOptions$doScale),
        
        # outputs are saved afterwards
        eskerSave = NULL,
        linFeatSave = NULL,
        saveOutput = NULL
        
      )
      
      ## Save to DATA folder
      writeRaster(res@habitatUse, bylayer = TRUE, format = "GTiff",
                  suffix = paste(names(res@habitatUse), 
                                 theRangeDf$Range,
                                 paste0("it_",iteration), 
                                 paste0("ts_",timestep), sep = "_"),
                  filename = file.path(e$TransferDirectory, "OutputHabitatUse"), 
                  overwrite = TRUE)
      
      # Build df and save the datasheet
      habitatUseDf <- data.frame(SeasonID = names(res@habitatUse), 
                                 Iteration = iteration,
                                 Timestep = timestep,
                                 RangeID = theRangeDf$Range)
      habitatUseDf$FileName <- file.path(e$TransferDirectory, 
                                         paste0(paste("OutputHabitatUse",
                                                      habitatUseDf$Season,
                                                      theRangeDf$Range,
                                                      "it", habitatUseDf$Iteration, 
                                                      "ts", habitatUseDf$Timestep,
                                                      sep= "_"), ".tif"))
      
      habitatUseAll[[paste0("it_",iteration)]][[paste0("ts_",timestep)]][[theRangeDf$Range]] <- 
        habitatUseDf
      
    }
  }
}

while(class(habitatUseAll[[1]]) == "list"){
  habitatUseAll <- unlist(habitatUseAll, recursive = F)
}

if (length(habitatUseAll) > 1) {
  habitatUseMerged <- bind(habitatUseAll)
} else{
  habitatUseMerged <- habitatUseAll[[1]]
}

saveDatasheet(ssimObject = mySce, name = "OutputSpatialHabitat", data = habitatUseMerged)
envEndSimulation()