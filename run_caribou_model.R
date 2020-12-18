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
                    ssimObject = mySce, 
                    lookupsAsFactors = FALSE)
names(allParams) <- myDatasheetsNamesFiltered

# Verify if linear inputs are vectors or rasters

# Eskers
if(is.null(allParams$ROFSim_SpatialInputsRasters$EskerFileNameRas)){
  if(is.null(allParams$ROFSim_SpatialInputsVectors$EskerFileNameVec)){
    stop("Both esker vector and raster inputs are unspecified - please specify one")
  } else {
    eskerFile <- st_read(allParams$ROFSim_SpatialInputsVectors$EskerFileNameVec)
  }
} else {
  if(!is.null(allParams$ROFSim_SpatialInputsVectors$EskerFileNameVec)){
    message("Both raster and vector outputs have been specified. Loading raster.")
  }
  eskerFile <- raster(allParams$ROFSim_SpatialInputsRasters$EskerFileNameRas)
}

# Linear features
if(is.null(allParams$ROFSim_SpatialInputsRasters$LinFeatFileNameRas)){
  if(is.null(allParams$ROFSim_SpatialInputsVectors$LinFeatFileNameVec)){
    stop("Both esker vector and raster inputs are unspecified - please specify one")
  } else {
    linFeatFile <- st_read(allParams$ROFSim_SpatialInputsVectors$LinFeatFileNameVec)
  }
} else {
  if(!is.null(allParams$ROFSim_SpatialInputsVectors$LinFeatFileNameVec)){
    message("Both raster and vector outputs have been specified. Loading raster.")
  }
  linFeatFile <- raster(allParams$ROFSim_SpatialInputsRasters$LinFeatFileNameRas)
}

# Function to process optional arguments
optArg <- function(arg){
  if(length(arg)==0){
    arg <- FALSE
  } else if (arg == "Yes"){
    arg <- TRUE
  } else if (arg == "No"){
    arg <- FALSE
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
  caribouRange = allParams$ROFSim_CaribouRange$Range, 
  
  # Rasters or vectors
  esker = eskerFile,
  linFeat = linFeatFile,
  
  # Look up table
  friLU = allParams$ROFSim_FriLookUpTable,
  
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
            suffix = paste(1:4, names(res@habitatUse), sep = "_"),
            filename = file.path(e$TransferDirectory, "OutputHabitatUse"), 
            overwrite = TRUE)

# Build df and save the datasheet
habitatUseDf <- data.frame(Season = names(res@habitatUse), 
                           HabitatUse = list.files(e$TransferDirectory, full.names = FALSE, 
                                                   pattern = ".tif"))
saveDatasheet(ssimObject = mySce, name = "ROFSim_OutputHabitatUse", data = habitatUseDf)
