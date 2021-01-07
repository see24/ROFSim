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

## Functions for wildcard

filterInputs <- function(params, iter, ts, min_ts = 1){
  
  # Cases where One or Both columns are missing
  if(!sum(is.element(names(params), "Iteration"))){
    print("No Iteration column")
    if(!sum(is.element(names(params), "Timestep"))){
      print("No Timestep column either")
      if(nrow(params) > 1){
        stop("No timestep nor iteration specified, yet multiple inputs are provided")
      } else{
        return(params)
      }
    } else{
      print("Only Iteration is missing, assuming current iteration")
      params$Iteration <- iter
    }
  } else if(!sum(is.element(names(params), "Timestep"))){
    print("Only Timestep is missing, assuming current timestep")
    params$Timestep <- ts
  }
  
  # Cases when more than one NA timestep inputs are provided (need to check before filling wildcards)
  NACount <- sum(is.na(params$Timestep))
  if(sum(NACount > 1 & NACount < nrow(params))){
    stop("More than one NA input for timesteps are provided, cannot align with timestep")
  }
  
  # Fill in the NAs for filtering
  params$Iteration <- fillWildcardITER(params$Iteration, iter)
  params$Timestep <- fillWildcardTS(params$Timestep, ts, min_ts)
  
  # Cases when there is more than one unique input
  iterAndTs <- params[, c("Iteration", "Timestep")]
  if(nrow(unique(iterAndTs)) != nrow(iterAndTs)){
    stop("More than one unique combination of timesteps and iteration")
  }
  
  # Now we are reading to select the right inputs
  # If the iteration or timestep doesnt match, we select the one closest one under
  # Try a perfect match, otherwise find next best match
  theSubset <- subset(params, Iteration == iter & Timestep == ts)
  if(nrow(theSubset) == 1){
    return(theSubset)
  } else {
    # Subset by iter first
    theSubset <- subset(params, Iteration == iter)
    if (nrow(theSubset) == 1){
      # Only return the one row if timestep is coherent
      if(theSubset$Timestep > ts){
        # Otherwise default tp 1:1
        theSubset <- subset(params, Iteration == 1 & Timestep == min_ts)
        return(theSubset)
      } else{
        return(theSubset)
      }
    } else if(nrow(theSubset) > 1) {
      # If more than one, select the timestep that is just under
      nearestval <- suppressWarnings(max(subset(theSubset$Timestep, 
                                                theSubset$Timestep<ts)))
      theSubset <- subset(theSubset, Timestep == nearestval)
      if(nrow(theSubset) == 0){
        # If there are none, then default to 1:1
        theSubset <- subset(params, Iteration == 1 & Timestep == min_ts)
        return(theSubset)
      }
    } else{
      # if anything fails, default to 1:1
      theSubset <- subset(params, Iteration == 1 & Timestep == min_ts)
      return(theSubset)
    }
  }
  return(theSubset)
}

# which.min(abs(x - your.number))

# Function to fill NA for wildcards

fillWildcardTS <- function(x, fill, min_ts){
  NACount <- sum(is.na(x))
  if (NACount == length(x)){
    # If all empty, assume current timestep
    x[which(is.na(x))] <- fill
  } else {
    # otherwise, assume NAs are timestep 1
    x[which(is.na(x))] <- min_ts
  }
  return(x)
}

fillWildcardITER <- function(x, fill){
  # Fill NAs to iteration 1
  x[which(is.na(x))] <- 1
  return(x)
}

selectInputs <- function(rasters, vectors, column){
  
  columnRas <- paste0(column, "Ras")
  columnVec <- paste0(column, "Vec")
  
  if(is.null(rasters[[columnRas]])){
    if(is.null(vectors[[columnVec]])){
      stop("Both esker vector and raster inputs are unspecified - please specify one")
    } else {
      theFile <- st_read(vectors[[columnVec]])
    }
  } else {
    if(!is.null(vectors[[columnVec]])){
      message("Both raster and vector outputs have been specified. Loading raster.")
    }
    theFile <- raster(rasters[[columnRas]])
  }
  return(theFile)
}
