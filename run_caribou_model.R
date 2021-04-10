## ROF SIM Prototype Package
## Second Transformer
## Running Caribou RSF Model

print("Secondary Transformer: running caribou model")

# Load Packages
library(rsyncrosim)
library(caribouMetrics)
library(raster)
library(sf)
library(dplyr)
library(tidyr)

# Load environment
e <- ssimEnvironment()
myLib <- ssimLibrary()
mySce <- scenario()

# Source the helpers
source(file.path(e$PackageDirectory, "helpers.R"))

# Get datasheets
myDatasheetsNames <- c("RasterFile", 
                       "ExternalFile", 
                       "RunCaribouRange", 
                       "HabitatModelOptions",
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
    # mutate(RasterVariableID = ifelse(is.na(CaribouVarID), 
    #                                  RasterVariableID, CaribouVarID)) %>% 
    # select(-c(CaribouVarID, VariableID, FileVariableID)) %>% 
    # group_by(Iteration, Timestep, RastersID) %>% 
    # group_modify(~ if(nrow(.x)>1){ .x[is.na(.x$Source), ]} else { .x}) %>% 
    # ungroup %>% 
    as.data.frame()
}

if (nrow(allParams$ExternalFile > 0)){
  allParams$ExternalFile <- allParams$ExternalFile %>% 
    left_join(filter(allParams$CaribouDataSourceWide, type == "shapefile"), 
              by = c("PolygonsID" = "VarID")) %>% 
    # mutate(FileVariableID = ifelse(is.na(CaribouVariableID), 
    #                                FileVariableID, CaribouVariableID)) %>% 
    # select(-c(CaribouVariableID, VariableID, RasterVariableID)) %>% 
    # group_by(Iteration, Timestep, FileVariableID) %>% 
    # group_modify(~ if(nrow(.x)>1){ .x[!is.na(.x$Source), ]} else { .x}) %>% 
    # ungroup %>% 
    as.data.frame()
}

# if (nrow(allParams$DataSummary > 0)){
#   allParams$DataSummary <- allParams$DataSummary %>% left_join(allParams$Crosswalk) %>% 
#     mutate(VariableID = ifelse(is.na(CaribouVariableID), 
#                                VariableID, CaribouVariableID)) %>% 
#     select(-c(CaribouVariableID, RasterVariableID, FileVariableID)) %>%  
#     group_by(Iteration, Timestep, VariableID) %>% 
#     group_modify(~ if(nrow(.x)>1){ .x[is.na(.x$Source), ]} else { .x}) %>% 
#     ungroup %>% as.data.frame()
# }

# Filter Timesteps --------------------------------------------------------

uniqueIterFromData <- 
  unique(c(allParams$ExternalFile$Iteration, 
           allParams$RasterFile$Iteration, 
           allParams$DataSummary$Iteration))
uniqueIterFromData <- uniqueIterFromData[!is.na(uniqueIterFromData)]

uniqueTsFromData <- 
  unique(c(allParams$ExternalFile$Timestep, 
           allParams$RasterFile$Timestep, 
           allParams$DataSummary$Timestep))
uniqueTsFromData <- uniqueTsFromData[!is.na(uniqueTsFromData)]

iterationSet <- GLOBAL_MinIteration:GLOBAL_MaxIteration
iterationSet <- iterationSet[iterationSet %in% uniqueIterFromData]

timestepSet <- GLOBAL_MinTimestep:GLOBAL_MaxTimestep
timestepSet <- timestepSet[timestepSet %in% uniqueTsFromData]

#Simulation
envBeginSimulation(length(iterationSet) * length(timestepSet))

# Run model ---------------------------------------------------------------

# Empty list to start
habitatUseAll <- NULL

# iteration <- iterationSet[1]
# timestep <- timestepSet[1]

for (iteration in iterationSet) {
  
  for (timestep in timestepSet) {
    
    envReportProgress(iteration, timestep)
    
    # Filter inputs based on iteration and timestep
    # rasters
    InputRasters <- filterInputs(allParams$RasterFile, 
                                 iteration, timestep, min(timestepSet))
    InputVectors <- filterInputs(allParams$ExternalFile,
                                 iteration, timestep, min(timestepSet))
    
    # Call the main function with all arguments extracted from datasheets
    plc <- raster(filter(InputRasters, CaribouVarID == "LandCoverRasterID")$File)
    
    eskerDras <-  tryCatch({
      raster(filter(InputRasters, CaribouVarID == "esker")$File)
      }, error = function(cond) { NULL })

    ageD = raster(filter(InputRasters, CaribouVarID == "AgeasterID")$File)

    natDistD = raster(filter(InputRasters, RasterVariableID == "natDist")$File)

    anthroDistD = raster(filter(InputRasters, RasterVariableID == "anthroDist")$File)

    harvD = raster(filter(InputRasters, RasterVariableID == "harv")$File)

    linFeatDras = raster(filter(InputRasters, RasterVariableID == "linFeat")$File)

    projectPolyD = st_read(filter(InputVectors, FileVariableID == "projectPoly")$File,
                           quiet = TRUE)
    
    res <- caribouHabitat(
      
      landCover = plcD , 
      
      esker = st_read("D://ROF/inputs/esker.shp"), 
      
      updatedLC = friD , 
      
      age = ageD, 
      
      natDist = natDistD, 
      
      anthroDist = anthroDistD, 
      
      harv = harvD,
      
      linFeat = st_read("D://ROF/inputs/linFeat.shp"), 
      
      projectPoly = projectPolyD,
      
      # Caribou Range
      caribouRange = allParams$RunCaribouRange$Range, 
      
      # Options
      padProjPoly = optArg(allParams$HabitatModelOptions$PadProjPoly),
      padFocal = optArg(allParams$HabitatModelOptions$PadFocal),
      
      # outputs are saved afterwards
      eskerSave = NULL,
      linFeatSave = NULL,
      saveOutput = NULL,
      
      # TEMPORARY, for test purposes only
      winArea = 500
    )
    
    ## Save to DATA folder
    writeRaster(res@habitatUse, bylayer = TRUE, format = "GTiff",
                suffix = paste(names(res@habitatUse), 
                               paste0("it_",iteration), 
                               paste0("ts_",timestep), sep = "_"),
                filename = file.path(e$TransferDirectory, "OutputHabitatUse"), 
                overwrite = TRUE)
    
    # Build df and save the datasheet
    habitatUseDf <- data.frame(SeasonID = names(res@habitatUse), 
                               Iteration = iteration,
                               Timestep = timestep,
                               RangeID = allParams$RunCaribouRange$Range)
    habitatUseDf$FileName <- file.path(e$TransferDirectory, 
                                       paste0(paste("OutputHabitatUse",
                                                    habitatUseDf$Season,
                                                    "it", habitatUseDf$Iteration, 
                                                    "ts", habitatUseDf$Timestep,
                                                    sep= "_"), ".tif"))
    
    habitatUseAll[[paste0("it_",iteration)]][[paste0("ts_",timestep)]] <- habitatUseDf
    
  }
}

if (length(habitatUseAll) > 1) {
  habitatUseMerged <- bind(unlist(habitatUseAll, recursive = F))
} else{
  habitatUseMerged <- unlist(habitatUseAll, recursive = F)[[1]]
}
saveDatasheet(ssimObject = mySce, name = "OutputSpatialHabitat", data = habitatUseMerged)
