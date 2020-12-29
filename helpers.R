# from the stconnect package

library(rsyncrosim)

# Function to process optional arguments
optArg <- function(arg){
  if(length(arg)==0){
    arg <- FALSE
  } else if (arg == "Yes"){
    arg <- TRUE
  } else if (arg == "No"){
    arg <- FALSE
  }
  arg
}

# CreateRasterFileName <- function(prefix, iteration, timestep, extension) {
#   return(sprintf("%s.it%s.ts%s.%s", prefix, iteration, timestep, extension))
# }

GetDataSheetExpectData <- function(name, ssimObj) {
  ds = datasheet(ssimObj, name)
  if (nrow(ds) == 0) { stop(paste0("No data for: ", name)) }
  return(ds)
}

GetSingleValueExpectData <- function(df, name) {
  v = df[, name]
  if (is.na(v)) { stop(paste0("Missing data for: ", name)) }
  return(v)
}

e = ssimEnvironment()
GLOBAL_Session = session()
GLOBAL_Library = ssimLibrary(session = GLOBAL_Session)
GLOBAL_Project = project(GLOBAL_Library, project = as.integer(e$ProjectId))
GLOBAL_Scenario = scenario(GLOBAL_Library, scenario = as.integer(e$ScenarioId))
GLOBAL_RunControl = GetDataSheetExpectData("ROFSim_RunControl", GLOBAL_Scenario)
GLOBAL_MaxIteration = GetSingleValueExpectData(GLOBAL_RunControl, "MaximumIteration")
GLOBAL_MinIteration = GetSingleValueExpectData(GLOBAL_RunControl, "MinimumIteration")
GLOBAL_MinTimestep = GetSingleValueExpectData(GLOBAL_RunControl, "MinimumTimestep")
GLOBAL_MaxTimestep = GetSingleValueExpectData(GLOBAL_RunControl, "MaximumTimestep")
GLOBAL_TotalIterations = (GLOBAL_MaxIteration - GLOBAL_MinIteration + 1)
GLOBAL_TotalTimesteps = (GLOBAL_MaxTimestep - GLOBAL_MinTimestep + 1)