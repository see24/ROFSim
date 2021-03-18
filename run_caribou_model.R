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

# Load environment
e <- ssimEnvironment()
myLib <- ssimLibrary()
mySce <- scenario()

# Source the helpers
source(file.path(e$PackageDirectory, "helpers.R"))

# Get datasheets
myDatasheetsNames <- c("DataSummary", 
                       "RasterFile", 
                       "ExternalFile", 
                       "RunCaribouRange", 
                       "HabitatModelOptions",
                       "Crosswalk")

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

# Get variables -----------------------------------------------------------

if (nrow(allParams$RasterFile > 0)){
  allParams$RasterFile <- allParams$RasterFile %>% left_join(allParams$Crosswalk) %>% 
    mutate(RasterVariableID = ifelse(is.na(CaribouVariableID), 
                                     RasterVariableID, CaribouVariableID)) %>% 
    select(-c(CaribouVariableID, VariableID, FileVariableID)) %>% 
    group_by(Iteration, Timestep, RasterVariableID) %>% 
    group_modify(~ if(nrow(.x)>1){ .x[!is.na(.x$Source), ]} else { .x})
}

if (nrow(allParams$DataSummary > 0)){
  allParams$DataSummary <- allParams$DataSummary %>% left_join(allParams$Crosswalk) %>% 
    mutate(VariableID = ifelse(is.na(CaribouVariableID), 
                               VariableID, CaribouVariableID)) %>% 
    select(-c(CaribouVariableID, RasterVariableID, FileVariableID)) %>%  
    group_by(Iteration, Timestep, VariableID) %>% 
    group_modify(~ if(nrow(.x)>1){ .x[!is.na(.x$Source), ]} else { .x})
}

if (nrow(allParams$ExternalFile > 0)){
  allParams$ExternalFile <- allParams$ExternalFile %>% left_join(allParams$Crosswalk) %>% 
    mutate(FileVariableID = ifelse(is.na(CaribouVariableID), 
                                   FileVariableID, CaribouVariableID)) %>% 
    select(-c(CaribouVariableID, VariableID, RasterVariableID)) %>% 
    group_by(Iteration, Timestep, FileVariableID) %>% 
    group_modify(~ if(nrow(.x)>1){ .x[!is.na(.x$Source), ]} else { .x})
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

for (iteration in iterationSet) {
  
  for (timestep in timestepSet) {
    
    envReportProgress(iteration, timestep)
    
    # Filter inputs based on iteration and timestep
    # rasters
    InputRasters <- filterInputs(allParams$RasterFile, 
                                 iteration, timestep)
    InputVectors <- filterInputs(allParams$ExternalFile,
                                 iteration, timestep)
    
    # Call the main function with all arguments extracted from datasheets
    res <- caribouHabitat(# Rasters
      plc = raster(InputRasters$PlcFileName),
      fri = raster(InputRasters$FriFileName), 
      age = raster(InputRasters$AgeFileName), 
      natDist = raster(InputRasters$NatDistFileName), 
      anthroDist = raster(InputRasters$AnthroDistFileName), 
      harv = raster(InputRasters$HarvFileName), 
      
      # Vectors
      projectPoly = st_read(InputVectors$ProjectPolyFileName), 
      
      # String
      caribouRange = allParams$ROFSim_CaribouRange$Range, 
      
      # Rasters or vectors
      esker = selectInputs(InputRasters, InputVectors, "EskerFileName"),
      linFeat = selectInputs(InputRasters, InputVectors, "LinFeatFileName"),
      
      # Look up table
      friLU = allParams$ROFSim_FriLookUpTable[,c(2,1)], # Necessary as expecting this order
      
      # Model options
      padProjPoly = optArg(allParams$ROFSim_ModelOptions$PadProjPoly),
      padFocal = optArg(allParams$ROFSim_ModelOptions$PadFocal),
      
      # outputs are saved afterwards
      eskerSave = NULL,
      linFeatSave = NULL,
      saveOutput = NULL,
      
      # TEMPORARY, for test purposes only
      winArea = 500)
    
    ## Save to DATA folder
    writeRaster(res@habitatUse, bylayer = TRUE, format = "GTiff",
                suffix = paste(names(res@habitatUse), 
                               paste0("it_",iteration), 
                               paste0("ts_",timestep), sep = "_"),
                filename = file.path(e$TransferDirectory, "OutputHabitatUse"), 
                overwrite = TRUE)
    
    # Build df and save the datasheet
    habitatUseDf <- data.frame(Season = names(res@habitatUse), 
                               Iteration = iteration,
                               Timestep = timestep,
                               Range = allParams$ROFSim_CaribouRange$Range)
    habitatUseDf$HabitatUse <- file.path(e$TransferDirectory, 
                                         paste0(paste("OutputHabitatUse",
                                                      habitatUseDf$Season,
                                                      "it", habitatUseDf$Iteration, 
                                                      "ts", habitatUseDf$Timestep,
                                                      sep= "_"), ".tif"))
    
    habitatUseAll[[paste0("it_",iteration)]][[paste0("ts_",timestep)]] <- habitatUseDf
    
  }
}

if (length(habitatUseAll) > 1) {
  habitatUseMerged <- bind(unlist(habitatUseAll, recursive = F))
} else{
  habitatUseMerged <- unlist(habitatUseAll, recursive = F)[[1]]
}
saveDatasheet(ssimObject = mySce, name = "ROFSim_OutputHabitatUse", data = habitatUseMerged)
