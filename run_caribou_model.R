## ROF SIM Prototype Package
## Second Transformer
## Running Caribou RSF Model

print("Secondary Transformer: running caribou model")

# Load Packages
library(rsyncrosim)
library(caribouMetrics)
library(raster)
library(sf)

# Load environment
e <- ssimEnvironment()
myLib <- ssimLibrary()
mySce <- scenario()

# Access all datasheets of importance
myDatasheets <- datasheet(mySce)
# Only select datasheets from the ROF package 
subFilter <- sapply(X = myDatasheets$name, FUN = grepl, pattern="^(ROF)\\w+")
myDatasheetsFiltered <- myDatasheets[subFilter,]
myDatasheetsNames <- myDatasheetsFiltered$name
# Remove the SpadesObject and outputs datasheet which can be empty
myDatasheetsNamesFiltered <- 
  myDatasheetsNames[!(myDatasheetsNames %in% c("ROFSim_InputSpadesObject",
                                               "ROFSim_OutputHabitatUse"))]

allParams <- lapply(myDatasheetsNamesFiltered, 
                    FUN = datasheet, 
                    ssimObject = mySce)
names(allParams) <- myDatasheetsNamesFiltered

# Verify if linear inputs are vectors or rasters

# Eskers
if(is.na(allParams$ROFSim_SpatialInputsRasters$EskerFileNameRas)){
  if(is.na(allParams$ROFSim_SpatialInputsVectors$EskerFileNameVec)){
    stop("Both esker vector and raster inputs are unspecified - please specify one")
  } else {
    eskerFile <- st_read(allParams$ROFSim_SpatialInputsVectors$EskerFileNameVec)
  }
} else {
  eskerFile <- raster(allParams$ROFSim_SpatialInputsRasters$EskerFileNameRas)
}

# Linear features
if(is.na(allParams$ROFSim_SpatialInputsRasters$LinFeatFileNameRas)){
  if(is.na(allParams$ROFSim_SpatialInputsVectors$LinFeatFileNameVec)){
    stop("Both esker vector and raster inputs are unspecified - please specify one")
  } else {
    linFeatFile <- st_read(allParams$ROFSim_SpatialInputsVectors$LinFeatFileNameVec)
  }
} else {
  linFeatFile <- raster(allParams$ROFSim_SpatialInputsRasters$LinFeatFileNameRas)
}

# Verify that Caribou Range is correctly specified
# TODO

# Function to process optional arguments
optArg <- function(arg){
  if(is.na(arg)){
    arg <- NULL
  }
  arg
}

# Call the main function with all arguments extracted from datasheets
res <- caribouHabitat(# Rasters
  plc = raster(allParams$ROFSim_SpatialInputsRasters$PlcFileName),
  fri = raster(allParams$ROFSim_SpatialInputsRasters$FriFileName), 
  age = raster(allParams$ROFSim_SpatialInputsRasters$AgeFileName), 
  natDist = raster(allParams$ROFSim_SpatialInputsRasters$NatDistFileName), 
  anthroDist = raster(allParams$ROFSim_SpatialInputsRasters$AnthroDistFileName), 
  harv = raster(allParams$ROFSim_SpatialInputsRasters$HarvFileName), 
  
  # Vectors
  projectPoly = st_read(allParams$ROFSim_SpatialInputsVectors$ProjectPolyFileName), 
  
  # String
  caribouRange = allParams$ROFSim_CaribouRangeID$CaribouRangeID, 
  
  # Rasters or vectors
  esker = eskerFile,
  linFeat = linFeatFile,
  
  # Look up table
  friLU = allParams$ROFSim_FriLookUpTable,
  
  # Model options
  eskerSave = optArg(allParams$ROFSim_ModelOptions$EskerSave),
  linFeatSave = optArg(allParams$ROFSim_ModelOptions$LinFeatSave),
  padProjPoly = optArg(allParams$ROFSim_ModelOptions$PadProjPoly),
  padFocal = optArg(allParams$ROFSim_ModelOptions$PadFocal),
  saveOutput = optArg(allParams$ROFSim_ModelOptions$OutputSave), 
  
  # TEMPORARY, for test purposes only
  winArea = 500)

