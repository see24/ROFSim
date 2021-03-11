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

# Get time information
# Start, End, Current
runControl <- c(start(spadesObject), end(spadesObject), time(spadesObject))

tmp <- tempdir(check = TRUE)

for (rowVar in seq_len(length.out = nrow(allVars))){
  
  theVar <- allVars$var[rowVar]
  theVarType <- allVars$type[rowVar]
  
  object <- spadesObject[[theVar]]
  
  if (theVarType == "var"){
    
    # Get info
    targetSheet <- "DataSummary"
    filePath <- file.path(temp, paste0(var, ".csv"))
    
    # Write tmp file
    write.csv(object, filePath)
    
    # Populate Sheet
  
  } else if (theVarType == "raster") {
    
    # Get info
    targetSheet <- "RasterFile"
    filePath <- file.path(temp, paste0(var, ".tif"))
    
    # Write tmp file
    writeRaster(object, filePath)
    
    # Populate sheet
    theSheet <- datasheet(mySce, targetSheet)
    addRow(theSheet, list())
  
  } else if (theVarType == "file") {
    
    targetSheet <- "ExternalFile"
    # TODO file path?
  
  }
  
  
  
}

unlink(tmp, recursive = TRUE)

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
