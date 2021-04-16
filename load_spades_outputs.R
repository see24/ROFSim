## ROF SIM Prototype Package

## Importing spades outputs

# Load Packages
library(rsyncrosim)
library(raster)

library(qs)
library(data.table)
library(magrittr)
library(dplyr)

library(SpaDES.core)
library(SpaDES.tools)

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

# Set transformer name
transformerName <- "Spades Import"

# Figure out variables to extract from spades -----------------------------

# Get all vars data

allRasters <- datasheet(mySce, "ROFSim_Rasters", lookupsAsFactors = FALSE)

allPolygons <- datasheet(mySce, "ROFSim_Polygons", lookupsAsFactors = FALSE)

# Find out which variables to extract

spadesRasterVars <- datasheet(mySce, "ROFSim_SpaDESRuntimeRasters", 
                              lookupsAsFactors = FALSE)
if(nrow(spadesRasterVars)>0){
  spadesRasterVars <- spadesRasterVars %>% 
    left_join(allRasters, by = c("RastersID" = "Name")) %>% 
    rename(var = RastersID) %>% 
    mutate(type = "raster")
}

spadesFileVars <- datasheet(mySce, "ROFSim_SpaDESRuntimeFiles", 
                            lookupsAsFactors = FALSE)
if(nrow(spadesFileVars)>0){
  spadesFileVars <- spadesFileVars %>% 
    left_join(allPolygons, by = c("PolygonsID" = "Name")) %>% 
    rename(var = FileVariableID) %>% 
    mutate(type = "file")
}

allVars <- rbind(spadesRasterVars, spadesFileVars)

# Verify all required vars have a matching sim object
if(sum(is.na(allVars$SpaDESSimObject))>0){
  stop("Crosswalk between variable and spades object not provided.")
}

# SpaDES Info -------------------------------------------------------------

# Get the spades datasheet 
spadesDatasheet <- datasheet(ssimObject = mySce, name = "ROFSim_SpaDESGeneral")

# If datasheet is not empty, get the path
if(nrow(spadesDatasheet) == 0){
  stop("No SpaDES file specified.")
} else {
  if (is.na(spadesDatasheet$Filename)){
    stop("No SpaDES file specified.")
  } else {
    spadesObjectPath <- spadesDatasheet$Filename
  }
}

# Run control -------------------------------------------------------------

runControlSheet <- datasheet(mySce, "RunControl")

if (nrow(runControlSheet) == 0){
  # Extract run control information
  runControl <- list(start = start(spadesObject), 
                     end = end(spadesObject), 
                     current = time(spadesObject))
  
  runControlSheet <- addRow(runControlSheet, 
                            list(MinimumIteration = 1, 
                                 MaximumIteration = 1, 
                                 MinimumTimestep = runControl$start, 
                                 MaximumTimestep = runControl$end))
  saveDatasheet(mySce, runControlSheet, "RunControl")
}

timestepSet <- runControlSheet$MinimumTimestep:runControlSheet$MaximumTimestep
# iterationSet <- runControlSheet$MinimumIteration:runControlSheet$MaximumIteration
# FOR NOW, ITER IS SET
theIter <- 1

# Filter spades outputs info ----------------------------------------------

# Load the spades object 
# TODO adding a warnings about memory could be usefull
spadesObject <- qs::qread(spadesObjectPath)

# Verify vars required are present in the spades object
testPresent <- !(allVars$SpaDESSimObject %in% names(spadesObject))
if(sum(testPresent)>0){
  stop("Incorrect or missing spades object(s).")
}

# Filter them
outputs <- outputs(spadesObject) %>% 
  make_paths_relative("outputs") %>% 
  filter(objectName %in% allVars$SpaDESSimObject) %>% 
  filter(saveTime %in% timestepSet) %>% 
  rename(Timestep = saveTime)

# For now, reconstruct the relative paths based on basenames
outputs$file <- file.path(dirname(spadesDatasheet$Filename), basename(outputs$file))

# Extract variables -------------------------------------------------------

# Transfer dir
tmp <- e$TransferDirectory

# Get empty datasheets
rasterFiles <- datasheet(mySce, "RasterFile", lookupsAsFactors = FALSE, 
                         empty = TRUE, optional = TRUE)
extFiles <- datasheet(mySce, "ExternalFile", lookupsAsFactors = FALSE,
                      empty = TRUE, optional = TRUE)


for (rowVar in seq_len(length.out = nrow(allVars))){
  
  theVar <- as.character(allVars$SpaDESSimObject[rowVar])
  theVarName <- as.character(allVars$var[rowVar])
  theVarType <- allVars$type[rowVar]
  
  # Re-filter based on specific var this time
  outputsFiltered <- outputs %>% 
    filter(objectName == theVar)
  
  for (rowOut in seq_len(length.out = nrow(outputsFiltered))){
    
    theTs <- outputsFiltered$Timestep[rowOut]
    
    if (theVarType == "raster") {
      
      tmpObj <- raster(outputsFiltered$file[rowOut]) >= 1
      
      # Make file name
      filePath <- file.path(tmp, paste0(theVar, "_", paste(paste0("it_",theIter), 
                                                           paste0("ts_",theTs), sep = "_"), 
                                        ".tif"))
      
      # Write tmp file
      writeRaster(tmpObj, filePath, overwrite = TRUE)
      
      # Populate sheet
      rasterFiles <- addRow(rasterFiles, list(Iteration = theIter,
                                              Timestep = theTs,
                                              RastersID = theVarName, 
                                              Filename = filePath,
                                              TransformerID = transformerName))
      
    } else if (theVarType == "file") {
      
      tmpObj <- st_read(outputsFiltered[rowOut])
      
      # TODO fix this case, how to get the file here?
      filePath <- file.path(tmp, paste0(theVar, "_", paste(paste0("it_",theIter), 
                                                           paste0("ts_",theTs), sep = "_"), 
                                        ".ext"))
      
      # Write tmp file
      writeRaster(tmpObj, filePath)
      
      # Populate sheet
      extFiles <- addRow(extFiles, list(Iteration = theIter,
                                        Timestep = theTs,
                                        PolygonsID = theVarName, 
                                        Filename = filePath,
                                        TransformerID = transformerName))
      
    }
  }

}

saveIfNotEmpty <- function(sheet, name){
  if(nrow(sheet) != 0){
    saveDatasheet(mySce, sheet, name, append = TRUE)
  }
}

mapply(list(rasterFiles, extFiles), 
       FUN = saveIfNotEmpty, 
       name = list("RasterFile", "ExternalFile"))

# -------------------------------------------------------------------------
