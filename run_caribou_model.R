## ROF SIM Prototype Package
## Second Transformer
## Running Caribou RSF Model

print("Secondary Transformer: running caribou model")

# Load Packages
library(rsyncrosim)
library(caribouMetrics)
library(raster)
library(sf)

# Load environment
e <- ssimEnvironment()
myLib <- ssimLibrary()
mySce <- scenario()

# Source the helpers
source(file.path(e$PackageDirectory, "helpers.R"))

# Access all datasheets of importance
myDatasheets <- datasheet(mySce)
# Only select datasheets from the ROF package 
subFilter <- sapply(X = myDatasheets$name, FUN = grepl, pattern="^(ROF)\\w+")
myDatasheetsFiltered <- myDatasheets[subFilter,]
myDatasheetsNames <- myDatasheetsFiltered$name
# Remove the SpadesObject and outputs datasheet which can be empty
myDatasheetsNamesFiltered <- 
  myDatasheetsNames[!(myDatasheetsNames %in% c("ROFSim_InputSpadesObject",
                                               "ROFSim_OutputHabitatUse"))]

allParams <- lapply(myDatasheetsNamesFiltered, 
                    FUN = datasheet, 
                    ssimObject = mySce, 
                    lookupsAsFactors = FALSE)
names(allParams) <- myDatasheetsNamesFiltered

# Function to filter inputs based on iterations and timesteps

# params <- data.frame(data = c("NA_NA","2_2", "NA_4","3_10", "3_34", "NA_7", "4_1"), 
#                      Iteration = c(NA,2,NA,3,3,NA,4), 
#                      Timestep = c(NA,2,4,10,34,7,10))
# filterInputs(params, 5, 33)
# iter <- 5
# ts <- 33

# Function to select raster or vectors for esker + linfeat

# Set of timesteps to analyse
timestepSet <- GLOBAL_MinTimestep:GLOBAL_MaxTimestep
iterationSet <- GLOBAL_MinIteration:GLOBAL_MaxIteration

#Simulation
envBeginSimulation(GLOBAL_TotalIterations * GLOBAL_TotalTimesteps)

# Empty list to start

habitatUseAll <- NULL

for (iteration in iterationSet) {
  
  for (timestep in timestepSet) {
    
    envReportProgress(iteration, timestep)
    
    # Filter inputs based on iteration and timestep
    # rasters
    InputRasters <- filterInputs(allParams$ROFSim_SpatialInputsRasters, 
                                 iteration, timestep)
    InputVectors <- filterInputs(allParams$ROFSim_SpatialInputsVectors,
                                 iteration, timestep)
    
    # Call the main function with all arguments extracted from datasheets
    res <- caribouHabitat(# Rasters
      plc = raster(InputRasters$PlcFileName),
      fri = raster(InputRasters$FriFileName), 
      age = raster(InputRasters$AgeFileName), 
      natDist = raster(InputRasters$NatDistFileName), 
      anthroDist = raster(InputRasters$AnthroDistFileName), 
      harv = raster(InputRasters$HarvFileName), 
      
      # Vectors
      projectPoly = st_read(InputVectors$ProjectPolyFileName), 
      
      # String
      caribouRange = allParams$ROFSim_CaribouRange$Range, 
      
      # Rasters or vectors
      esker = selectInputs(InputRasters, InputVectors, "EskerFileName"),
      linFeat = selectInputs(InputRasters, InputVectors, "LinFeatFileName"),
      
      # Look up table
      friLU = allParams$ROFSim_FriLookUpTable[,c(2,1)], # Necessary as expecting this order
      
      # Model options
      padProjPoly = optArg(allParams$ROFSim_ModelOptions$PadProjPoly),
      padFocal = optArg(allParams$ROFSim_ModelOptions$PadFocal),
      
      # outputs are saved afterwards
      eskerSave = NULL,
      linFeatSave = NULL,
      saveOutput = NULL,
      
      # TEMPORARY, for test purposes only
      winArea = 500)
    
    ## Save to DATA folder
    writeRaster(res@habitatUse, bylayer = TRUE, format = "GTiff",
                suffix = paste(names(res@habitatUse), 
                               paste0("it_",iteration), 
                               paste0("ts_",timestep), sep = "_"),
                filename = file.path(e$TransferDirectory, "OutputHabitatUse"), 
                overwrite = TRUE)
    
    # Build df and save the datasheet
    habitatUseDf <- data.frame(Season = names(res@habitatUse), 
                               Iteration = iteration,
                               Timestep = timestep,
                               Range = allParams$ROFSim_CaribouRange$Range)
    habitatUseDf$HabitatUse <- file.path(e$TransferDirectory, 
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
saveDatasheet(ssimObject = mySce, name = "ROFSim_OutputHabitatUse", data = habitatUseMerged)
