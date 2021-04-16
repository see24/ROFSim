## ROF SIM Prototype Package

## Make land cover from cohort data

# Load Packages
library(rsyncrosim)
library(raster)

library(qs)
library(data.table)
library(magrittr)
library(dplyr)

library(SpaDES.core)
library(SpaDES.tools)

library(caribouMetrics)

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

# Source Craig's function + my helpers
source(file.path(e$PackageDirectory, "makeLCCFromCohortData_helper.R"))
source(file.path(e$PackageDirectory, "helpers.R"))

lccClassTable = data.table(
  standLeading = c("pureCon_dense", "pureCon_open", "pureCon_sparse",
                   "pureCon_sparse",
                   "pureDec_dense", "pureDec_open", "pureDec_sparse",
                   "mixed_dense", "mixed_open", "mixed_sparse"), 
  LCCclass = c(1,6,8,32,
               2,11,11, 
               3,13,13)) # HARDCODED TO MATCH INPUTS

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

# Get outputs table
outputs <- outputs(spadesObject) %>% 
  make_paths_relative("outputs") %>% 
  filter(objectName %in% c("cohortData", "pixelGroupMap")) %>% 
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

# Get LCC
rstLCC <- spadesObject$rasterToMatch

outputSheet <- data.frame()

for (ts in sort(unique(outputs$Timestep))){
  
  outputsFiltered <- outputs %>% 
    filter(Timestep == ts)
  
  if(nrow(outputsFiltered) != 2){ stop("invalid number of outputs") }
  
  cohort_data <- as.data.table(qs::qread(outputsFiltered %>% 
                                           filter(objectName == "cohortData") %>% 
                                           pull(file)))
  pixelGroupMap <- raster(outputsFiltered %>% 
                            filter(objectName == "pixelGroupMap") %>% 
                            pull(file))
  names(pixelGroupMap) <- "pixelGroup"
  rstLCC <- raster::resample(rstLCC,
                             pixelGroupMap)
  
  # Make file name
  filePath <- file.path(tmp, paste0("PLC", "_", paste(paste0("it_",theIter), 
                                                      paste0("ts_",ts), sep = "_"), 
                                    ".tif"))
  
  # Populate sheet
  updated_LCC_tmp <- makeLCCfromCohortData(cohortData = cohort_data,
                                           pixelGroupMap = pixelGroupMap,
                                           rstLCC = rstLCC,
                                           lccClassTable = lccClassTable)
  
  
  writeRaster(updated_LCC_tmp, overwrite = TRUE,
              filename = filePath)
  
  tmpsheet <- data.frame(RastersID = "Provincial Land Cover", 
                         File = filePath, 
                         TransformerID = "Generate LCC from Cohort Data")
  
  outputSheet <- bind_rows(outputSheet, tmpsheet)
    
}

saveDatasheet(mySce, outputSheet, "RasterFile")
