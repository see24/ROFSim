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

allRasters <- datasheet(mySce, "ROFSim_Rasters")

allPolygons <- datasheet(mySce, "ROFSim_Polygons")

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

# Spades processing -------------------------------------------------------

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

# Load the spades object 
# TODO adding a warnings about memory could be usefull
spadesObject <- qs::qread(spadesObjectPath)

# Verify vars required are present in the spades object
testPresent <- !(allVars$SpaDESSimObject %in% names(spadesObject))
if(sum(testPresent)>0){
  stop("Incorrect or missing spades object(s).")
}

# Extract info ------------------------------------------------------------

# Extract run control information
runControl <- list(start = start(spadesObject), 
                   end = end(spadesObject), 
                   current = time(spadesObject))

runControlSheet <- datasheet(mySce, "RunControl", empty = TRUE)
runControlSheet <- addRow(runControlSheet, 
                          list(MinimumIteration = 1, 
                               MaximumIteration = 1, 
                               MinimumTimestep = runControl$start, 
                               MaximumTimestep = runControl$end))
saveDatasheet(mySce, runControlSheet, "RunControl")

# Extract the other vars
tmp <- e$TransferDirectory

# For now, harcode Iteration + Timestep
theIter <- 1
theTs <- runControl$current

# Get empty datasheets
rasterFiles <- datasheet(mySce, "RasterFile", lookupsAsFactors = FALSE, 
                         empty = TRUE, optional = TRUE)
extFiles <- datasheet(mySce, "ExternalFile", lookupsAsFactors = FALSE,
                      empty = TRUE, optional = TRUE)

for (rowVar in seq_len(length.out = nrow(allVars))){
  
  theVar <- as.character(allVars$SpaDESSimObject[rowVar])
  theVarName <- as.character(allVars$var[rowVar])
  theVarType <- allVars$type[rowVar]
  
  object <- spadesObject[[theVar]]
  
  if (theVarType == "raster") {
    
    # Get info
    filePath <- file.path(tmp, paste0(theVar, "_", paste(paste0("it_",theIter), 
                                                         paste0("ts_",theTs), sep = "_"), 
                                      ".tif"))
    
    # Write tmp file
    writeRaster(object, filePath, overwrite = TRUE)
    
    # Populate sheet
    rasterFiles <- addRow(rasterFiles, list(Iteration = theIter,
                                            Timestep = theTs,
                                            RastersID = theVarName, 
                                            Filename = filePath,
                                            TransformerID = transformerName))
    
  } else if (theVarType == "file") {
    
    # TODO fix this case, how to get the file here?
    filePath <- file.path(tmp, paste0(theVar, "_", paste(paste0("it_",theIter), 
                                                         paste0("ts_",theTs), sep = "_"), 
                                      ".ext"))
    
    # Write tmp file
    writeRaster(object, filePath)
    
    # Populate sheet
    extFiles <- addRow(extFiles, list(Iteration = theIter,
                                      Timestep = theTs,
                                      PolygonsID = theVarName, 
                                      Filename = filePath,
                                      TransformerID = transformerName))
    
  }
  
  
}

saveIfNotEmpty <- function(sheet, name){
  if(nrow(sheet) != 0){
    saveDatasheet(mySce, sheet, name)
  }
}

mapply(list(rasterFiles, extFiles), 
       FUN = saveIfNotEmpty, 
       name = list("RasterFile", "ExternalFile"))

# -------------------------------------------------------------------------
