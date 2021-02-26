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
allSpatialVars <- datasheet(mySce, "ROFSim_SpatialVariable") %>% 
  mutate(type = "spatial")
allTabularVars <- datasheet(mySce, "ROFSim_TabularVariable") %>% 
  mutate(type = "tabular")

AllVars <- rbind(allSpatialVars, allTabularVars)

spatialVarsSources <- datasheet(mySce, "ROFSim_SpatialVariableSources")
tabularVarsSources <- datasheet(mySce, "ROFSim_TabularVariableSources")

# Spades processing -------------------------------------------------------

# Get the spades datasheet 
spadesDatasheed <- datasheet(ssimObject = mySce, name = "ROFSim_InputSpadesObject")

# If datahseet is not empty, get the path
if(nrow(spadesDatasheed) == 0){
  stop("No spades objects specified")
} else {
  spadesObjectPath <- spadesDatasheed[1,1]
}

# Load the spades object 
# TODO adding a warnings about memory could be usefull
spadesObject <- qs::qread(spadesObjectPath)

# Extract info ------------------------------------------------------------

# Get cohort and pixel group
# "a cohort is a particular combo of species and ages"
cohort_data <- spadesObject$cohortData
pixelGroupMap <- spadesObject$pixelGroupMap

# Unique values

# TODO how do we decide which vars to extract?

# Subset, age and dominant species
cohort_data_summary <-
  cohort_data[,list(ageMax = max(age),
                    biomass = sum(B)),
              by = c("speciesCode", "pixelGroup")] ## takes max age within cohort

tmp <- list()
for (species in unique(cohort_data$speciesCode)){
  tmp[[species]] <-
    rasterizeReduced(reduced = cohort_data_summary[speciesCode == species],
                     fullRaster = pixelGroupMap,
                     mapcode = "pixelGroup",
                     newRasterCols =  c("ageMax", "biomass"))
}
