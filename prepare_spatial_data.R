# ROFSim - Transformer 0 - Prepare Spatial Data

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

# not sure what the point is this just adds the type but type is already separate
# if (nrow(allParams$RasterFile > 0)){
#   allParams$RasterFile <- allParams$RasterFile %>% 
#     left_join(filter(allParams$CaribouDataSourceWide, type=="raster"), 
#               by = c("RastersID" = "VarID")) %>% 
#     as.data.frame()
# }
# 
# if (nrow(allParams$ExternalFile > 0)){
#   allParams$ExternalFile <- allParams$ExternalFile %>% 
#     left_join(filter(allParams$CaribouDataSourceWide, type == "shapefile"), 
#               by = c("PolygonsID" = "VarID")) %>% 
#     as.data.frame()
# }

# get landcover from first timestep
landCoverPth <- filter(allParams$RasterFile, RastersID == "Provincial Land Cover") %>%
  slice(1) %>% 
  pull(Filename)

# get projectPoly
projectPolyPth <- filter(allParams$ExternalFile, PolygonsID == "Ranges") %>%
  pull(File)

# get linear features and make sublists that are appropriate for timesteps 

# TODO: need an indicator of whether a type of linear feature existed before ie
# if there are roads and rail at ts 1 and new roads are added at ts 2 then the
# rail should be combined with both. could maybe just always combine linearFeats
# that are time step 0 and user needs to know to give a specific timestep if
# they want to overwrite it later. But still need to know what it overrides!
linFeatsList <- filter(allParams$ExternalFile, PolygonsID == "Linear Features") %>% 
  mutate(Timestep = ifelse(is.na(Timestep), 0, Timestep)) %>%
  split(.$Timestep)




# make other filenames into named list
polyFilenames <- allParams$ExternalFile %>% 
  filter(!PolygonsID %in% c("Linear Features", "Ranges")) %>% 
  pull(File) %>% as.list() 

names(polyFilenames) <- paste0(gsub(" ", "_", allParams$ExternalFile$PolygonsID), 
                               "iter_", allParams$ExternalFile$Iteration,
                               "ts_", allParams$ExternalFile$Timestep)

rastFilenames <- allParams$RasterFile %>% 
  filter(Filename != landCoverPth) %>% 
  pull(Filename) %>% as.list() 

names(rastFilenames) <- paste0(gsub(" ", "_", allParams$RasterFile$RastersID), 
                               "iter_", allParams$RasterFile$Iteration,
                               "ts_", allParams$RasterFile$Timestep)
