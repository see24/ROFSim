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

# Source Craig's function
source(file.path(e$PackageDirectory, "makeLccFromCohortData_helper.R"))

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

cohort_data <- spadesObject$cohortData
pixelGroupMap <- spadesObject$pixelGroupMap
rstLCC <- spadesObject$rstLCC

lccClassTable = data.table(
  standLeading = c("pureCon_dense", "pureCon_open", "pureCon_sparse",
                   "pureCon_sparse",
                   "pureDec_dense", "pureDec_open", "pureDec_sparse",
                   "mixed_dense", "mixed_open", "mixed_sparse"),
  LCCclass = c(1,6,8,32,
               2,11,11,
               3,13,13)) # HARDCODED TO MATCH LCC05

# Call the function -------------------------------------------------------

rasterFiles <- datasheet(mySce, "RasterFile", optional = TRUE)

if(nrow(rasterFiles) != 0){

  sheetSubset <- subset(rasterFiles, RasterVariableID == "rstLCC")
  restofSheet <- subset(rasterFiles, RasterVariableID != "rstLCC")
  newVar <- "landCover"

  if(nrow(sheetSubset) != 0){

    updated_LCC_list <- vector(mode = "list", length = nrow(sheetSubset))

    for (lccRow in seq_len(length.out = nrow(sheetSubset))){

      theIter <- sheetSubset[lccRow,]$Iteration
      theTs <- sheetSubset[lccRow,]$Timestep

      filePath <- file.path(e$TransferDirectory,
                            paste0(newVar, "_", paste(paste0("it_",theIter),
                                                      paste0("ts_",theTs), sep = "_"),
                                   ".tif"))

      updated_LCC_tmp <- makeLCCfromCohortData(cohortData = cohort_data,
                                               pixelGroupMap = pixelGroupMap,
                                               rstLCC = rstLCC,
                                               lccClassTable = lccClassTable)
      writeRaster(updated_LCC_tmp, overwrite = TRUE,
                  filename = filePath)

      sheetSubset[lccRow,]$RasterVariableID <- newVar
      sheetSubset[lccRow,]$File <- filePath
      sheetSubset[lccRow,]$Source <- "makeLccFromCohortData"

    }
  }
}

fullSheet <- rbind(sheetSubset, restofSheet)
saveDatasheet(mySce, fullSheet, "RasterFile")
