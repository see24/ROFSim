## ROF SIM Prototype Package
### Primary Transformer
## Importing spades outputs

print("Primary Transformer: processing spades outputs")

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

# Figure out variables to extract from spades -----------------------------

# Get all vars data

spadesVars <- datasheet(mySce, "ROFSim_SpaDESRuntimeVariables", 
                        lookupsAsFactors = FALSE) 
if(nrow(spadesVars)>0){
  spadesVars <- spadesVars %>% 
    rename(var = VariableID) %>% 
    mutate(type = "var")
}

spadesRasterVars <- datasheet(mySce, "ROFSim_SpaDESRuntimeRasters", 
                              lookupsAsFactors = FALSE)
if(nrow(spadesRasterVars)>0){
  spadesRasterVars <- spadesRasterVars %>% 
    rename(var = RasterVariableID) %>% 
    mutate(type = "raster")
}

spadesFileVars <- datasheet(mySce, "ROFSim_SpaDESRuntimeFiles", 
                            lookupsAsFactors = FALSE)
if(nrow(spadesFileVars)>0){
  spadesFileVars <- spadesFileVars %>% 
    rename(var = FileVariableID) %>% 
    mutate(type = "file")
}

allVars <- rbind(spadesVars, spadesRasterVars, spadesFileVars)

# Spades processing -------------------------------------------------------

# Get the spades datasheet 
spadesDatasheet <- datasheet(ssimObject = mySce, name = "ROFSim_SpaDESImportSettings")

# If datahseet is not empty, get the path
if(nrow(spadesDatasheet) == 0){
  stop("No SpaDES object specified.")
} else {
  if (is.na(spadesDatasheet$Filename)){
    stop("No SpaDES object specified.")
  } else {
    spadesObjectPath <- spadesDatasheet$Filename
  }
}

# Load the spades object 
# TODO adding a warnings about memory could be usefull
spadesObject <- qs::qread(spadesObjectPath)

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
tmp <- tempdir(check = TRUE)

# For now, harcode Iteration + Timestep
theIter <- 1
theTs <- runControl$current

for (rowVar in seq_len(length.out = nrow(allVars))){
  
  theVar <- allVars$var[rowVar]
  theVarType <- allVars$type[rowVar]
  
  object <- spadesObject[[theVar]]
  
  if (theVarType == "var"){
    
    # Get info
    targetSheet <- "DataSummary"
    filePath <- file.path(tmp, paste0(theVar, ".csv"))
    
    # Write tmp file
    write.csv(object, filePath)
    
    # Populate sheet
    theSheet <- datasheet(mySce, targetSheet, empty = TRUE, optional = TRUE)
    addRow(theSheet, list(Iteration = theIter,
                          Timestep = theTs,
                          VariableID = theVar, 
                          File = filePath,
                          Source = "load_spades_outputs"))
  
  } else if (theVarType == "raster") {
    
    # Get info
    targetSheet <- "RasterFile"
    filePath <- file.path(tmp, paste0(theVar, ".tif"))
    
    # Write tmp file
    writeRaster(object, filePath, overwrite = TRUE)
    
    # Populate sheet
    theSheet <- datasheet(mySce, targetSheet, empty = TRUE, optional = TRUE)
    addRow(theSheet, list(Iteration = theIter,
                          Timestep = theTs,
                          RasterVariableID = theVar, 
                          File = filePath,
                          Source = "load_spades_outputs"))
  
  } else if (theVarType == "file") {
    
    # TODO fix this case, how to get the file here?
    targetSheet <- "ExternalFile"
    filePath <- file.path(tmp, paste0(theVar, ".ext"))
    
    # Write tmp file
    writeRaster(object, filePath)
    
    # Populate sheet
    theSheet <- datasheet(mySce, targetSheet, empty = TRUE, optional = TRUE)
    addRow(theSheet, list(Iteration = theIter,
                          Timestep = theTs,
                          FileVariableID = theVar, 
                          File = filePath,
                          Source = "load_spades_outputs"))
  
  }
  
  saveDatasheet(mySce, theSheet, targetSheet)
  
}

# unlink(tmp, recursive = TRUE)

# -------------------------------------------------------------------------

# # Get cohort and pixel group
# # "a cohort is a particular combo of species and ages"
# cohort_data <- spadesObject$cohortData
# pixelGroupMap <- spadesObject$pixelGroupMap
# 
# # Subset, age and dominant species
# cohort_data_summary <-
#   cohort_data[,list(ageMax = max(age),
#                     biomass = sum(B)),
#               by = c("speciesCode", "pixelGroup")] ## takes max age within cohort
# 
# # THIS WILL ALLOW PROGRAMMAGIC ASSIGNMENT OF REDUCTION METHOD BASED ON VAR NAME
# cohort_data[ , lapply(.SD, assign_reduction_function), 
#              by = c("speciesCode", "pixelGroup"), .SD=test]
# 
# cohort_data_summary <-
#   cohort_data[,test,
#               by = c("speciesCode", "pixelGroup")]
# head(cohort_data_summary)
# 
# tmp <- list()
# for (species in unique(cohort_data$speciesCode)){
#   tmp[[species]] <-
#     rasterizeReduced(reduced = cohort_data_summary[speciesCode == species],
#                      fullRaster = pixelGroupMap,
#                      mapcode = "pixelGroup",
#                      newRasterCols =  c("ageMax", "biomass"))
# }
