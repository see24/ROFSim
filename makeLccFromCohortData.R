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
  LCCclass = c(1,1,8,8,
               2,2,2, 
               7,7,7))) # HARDCODED TO MATCH INPUTS

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

  # From the latest update from Craig
  # Reclassify values to resource types
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  LCC05ToResType <- function(FromList, ToVal, landcover){
    reclassDT <- data.table("Is" = FromList,
                            "becomes" = rep(ToVal, length(FromList)))
    
    reclassedLCC <- raster::reclassify(landcover, reclassDT)
    
    return(reclassedLCC)
  }
  
  CON_ls <- c(1,6,7) #1
  correctLCC <- LCC05ToResType(CON_ls, 1, updated_LCC_tmp)
  
  DEC_ls <- c(2,11,12) #2
  correctLCC <- LCC05ToResType(DEC_ls, 2, correctLCC)
  
  DTN_ls <- c(34,35) #3
  correctLCC <- LCC05ToResType(DTN_ls, 3, correctLCC)
  
  LGOP_ls <- c(19,31,32) #4
  correctLCC <- LCC05ToResType(LGOP_ls, 4, correctLCC)
  
  LGTP_ls <- c(8,10) #5
  correctLCC <- LCC05ToResType(LGTP_ls, 5, correctLCC)
  
  LGW_ls <- c(37,38) #6
  correctLCC <- LCC05ToResType(LGW_ls, 6, correctLCC)
  
  MIX_ls <- c(3,4,5,13,14,15) #7
  correctLCC <- LCC05ToResType(MIX_ls, 7, correctLCC)
  
  ST_ls <- c(9,20) #8
  correctLCC <- LCC05ToResType(ST_ls, 8, correctLCC)
  
  OTHER_ls <- c(16,17,18,21,22,23,24,25,26,27,28,29,30,33,36,39) #9
  correctLCC <- LCC05ToResType(OTHER_ls, 9, correctLCC)
  
  writeRaster(correctLCC, overwrite = TRUE,
              filename = filePath)
  
  tmpsheet <- data.frame(Iteration = theIter, 
                         Timestep = ts, 
                         RastersID = "Provincial Land Cover", 
                         Filename = filePath, 
                         TransformerID = "Generate LCC from Cohort Data")
  
  outputSheet <- bind_rows(outputSheet, tmpsheet)
    
}

saveDatasheet(mySce, outputSheet, "RasterFile", append = TRUE)
