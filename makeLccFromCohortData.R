# ROFSim - Transformer 2 - Generate LCC from cohort

# Set transformer name
transformerName <-"Generate LCC from Cohort Data"

# Packages ----------------------------------------------------------------

library(rsyncrosim)
library(raster)
library(qs)
library(data.table)
library(magrittr)
library(dplyr)
library(SpaDES.core)
library(SpaDES.tools)
library(caribouMetrics)

# Load Environment --------------------------------------------------------

e <- ssimEnvironment()
myLib <- ssimLibrary()
mySce <- scenario()

# Get all datasheets ------------------------------------------------------

# Access all datasheets of importance
myDatasheets <- datasheet(mySce)

# Only select datasheets from the ROF package
subFilter <- sapply(X = myDatasheets$name, FUN = grepl, pattern="^(ROF)\\w+")
myDatasheetsFiltered <- myDatasheets[subFilter,]
myDatasheetsNames <- myDatasheetsFiltered$name

# Source Functions --------------------------------------------------------

source(file.path(e$PackageDirectory, "makeLCCFromCohortData_helper.R"))
source(file.path(e$PackageDirectory, "helpers.R"))

# Hardcoded reclass table
lccClassTable = data.table(
  standLeading = c("pureCon_dense", "pureCon_open", "pureCon_sparse",
                   "pureCon_sparse",
                   "pureDec_dense", "pureDec_open", "pureDec_sparse",
                   "mixed_dense", "mixed_open", "mixed_sparse"), 
  LCCclass = c(1,1,8,8,
               2,2,2, 
               7,7,7)) # HARDCODED TO MATCH RESOURCE TYPES

# SpaDES Info -------------------------------------------------------------

# Get the spades datasheet 
spadesDatasheet <- datasheet(ssimObject = mySce, name = "ROFSim_SpaDESGeneral")

# If datasheet is not empty, get the path
if(nrow(spadesDatasheet) == 0){
  
  stop("No SpaDES files specified.")
  
} else {
  
  if (sum(is.na(spadesDatasheet$Filename))>0){
    
    stop("No SpaDES files specified.")
    
  }
  
}

# Run control -------------------------------------------------------------

runControlSheet <- datasheet(mySce, "RunControl")

if (nrow(runControlSheet) == 0){
  stop("Run Control datasheet is empty.")
}

# Iterations
iterationSet <- runControlSheet$MinimumIteration:runControlSheet$MaximumIteration

if(sum(!(iterationSet %in% unique(spadesDatasheet$Iteration)))>0){
  stop("Iterations required by run control are not encoded in SpaDES datasheet")
} else{
  spadesDatasheet <- spadesDatasheet %>% 
    filter(Iteration %in% iterationSet)
}

# Timesteps
timestepSet <- seq(from = runControlSheet$MinimumTimestep,
                   to = runControlSheet$MaximumTimestep, 
                   by = runControlSheet$OutputFrequency)

# Extract variables -------------------------------------------------------

# Function for reclassifying LCC05 values to resource types
LCC05ToResType <- function(FromList, ToVal, landcover){
  reclassDT <- data.table("Is" = FromList,
                          "becomes" = rep(ToVal, length(FromList)))
  
  reclassedLCC <- raster::reclassify(landcover, reclassDT)
  
  return(reclassedLCC)
}

# Transfer dir
tmp <- e$TransferDirectory

# Get empty datasheets
rasterFiles <- datasheet(mySce, "RasterFile", lookupsAsFactors = FALSE, 
                         empty = TRUE, optional = TRUE)

# Empty output sheet
outputSheet <- data.frame()

for (theIter in iterationSet){
  #theIter=2
  # Load the spades object 
  spadesObjectPath <- spadesDatasheet %>% 
    filter(Iteration == theIter) %>% 
    pull(Filename)
  #sort( sapply(ls(),function(x){object.size(get(x))})) 
  rstLCC=NULL;spadesObject=NULL
  spadesObject <- qs::qread(spadesObjectPath)
  
  # Filter them
  outputs <- outputs(spadesObject) %>% 
    make_paths_relative("outputs") %>% 
    filter(objectName %in% c("cohortData", "pixelGroupMap")) %>% 
    filter(saveTime %in% timestepSet) %>% 
    rename(Timestep = saveTime)
  
  # For now, reconstruct the relative paths based on basenames
  outputs$file <- file.path(dirname(spadesObjectPath), basename(outputs$file))
  
  rstLCC <- spadesObject$rasterToMatch
  
  for (ts in sort(unique(outputs$Timestep))){
    #ts=2100
    
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
    
    rm(cohort_data);rm(pixelGroupMap);rm(updated_LCC_tmp)
    #sort(sapply(ls(), function(x) {object.size(get(x)) }))
    
    tmpsheet <- data.frame(Iteration = theIter, 
                           Timestep = ts, 
                           RastersID = "Provincial Land Cover", 
                           Filename = filePath, 
                           TransformerID = transformerName)
    
    outputSheet <- bind_rows(outputSheet, tmpsheet)
    
  }
  rstLCC=NULL
  #sort( sapply(ls(),function(x){object.size(get(x))})) 
}

saveDatasheet(mySce, outputSheet, "RasterFile", append = TRUE)
