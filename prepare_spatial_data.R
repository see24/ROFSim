# ROFSim - Transformer 0 - Prepare Spatial Data

# Packages ----------------------------------------------------------------

library(rsyncrosim)
library(caribouMetrics)
library(raster)
library(sf)
library(dplyr)
library(tidyr)

# Packages ----------------------------------------------------------------

library(rsyncrosim)
library(caribouMetrics)
library(raster)
library(sf)
library(dplyr)
library(tidyr)

localDebug = F
if(!localDebug){
  # Load environment
  e <- ssimEnvironment()
  myLib <- ssimLibrary()
  mySce <- scenario()
  # Source the helpers
  source(file.path(e$PackageDirectory, "helpers.R"))
  
}else{
  e=list()
  e$PackageDirectory = "C:/Users/HughesJo/Documents/SyncroSim/Packages/ROFSim"
  t = try(source(file.path(e$PackageDirectory, "helpers.R")),silent=T) #this will throw Error in .local(.Object, ...) : A library name is required. Don't worry about it.
  source("./scripts/loadSSimLocalForDebug.R") #run outside of SSim for debugging caribouMetrics package
}

# Get all datasheets ------------------------------------------------------

myDatasheetsNames <- c("RasterFile", 
                       "ExternalFile", 
                       "RunCaribouRange", 
                       "CaribouModelOptions",
                       "CaribouDataSource")

loadDatasheet <- function(name){
  sheet <- tryCatch(
    {
      datasheet(mySce, name = name, lookupsAsFactors = FALSE, 
                optional = TRUE)
    },
    error = function(cond){
      return(NULL)
    }, 
    warning = function(cond){
      return(NULL)
    }
  )
}

allParams <- lapply(myDatasheetsNames, loadDatasheet)
names(allParams) <- myDatasheetsNames

# Modify the source data table
allParams$CaribouDataSourceWide <- allParams$CaribouDataSource %>% 
  pivot_longer(values_to = "VarID", names_to = "CaribouVarID", 
               cols=tidyselect::all_of(names(allParams$CaribouDataSource))) %>% 
  rowwise() %>% 
  mutate(type=ifelse(grepl("Raster", CaribouVarID, fixed=TRUE), "raster", "shapefile")) %>% 
  ungroup() %>% drop_na() %>% as.data.frame()

# Get variables -----------------------------------------------------------

if (nrow(allParams$RasterFile > 0)){
  allParams$RasterFile <- allParams$RasterFile %>% 
    left_join(filter(allParams$CaribouDataSourceWide, type=="raster"), 
              by = c("RastersID" = "VarID")) %>% 
    as.data.frame()
}

if (nrow(allParams$ExternalFile > 0)){
  allParams$ExternalFile <- allParams$ExternalFile %>% 
    left_join(filter(allParams$CaribouDataSourceWide, type == "shapefile"), 
              by = c("PolygonsID" = "VarID")) %>% 
    as.data.frame()
}