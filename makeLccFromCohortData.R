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