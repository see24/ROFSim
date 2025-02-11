# from the stconnect package

library(rsyncrosim)

# RSYNCROSIM helpers ------------------------------------------------------

# Function to process optional arguments
optArg <- function(arg){
  if(length(arg)==0){
    arg <- NULL
    #Comment: seems like this should return NULL or NA, not F
  }else if (is.na(arg)){
      return(NULL)
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
  if (nrow(ds) == 0) { warning(paste0("No data for: ", name)) }
  return(ds)
}

GetSingleValueExpectData <- function(df, name) {
  v = df[, name]
  if (is.na(v)) { warning(paste0("Missing data for: ", name)) }
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

# ROFSIM helpers ----------------------------------------------------------

## Functions for wildcard

filterInputs <- function(params, iter, ts, useMostRecent=F,min_ts = 1){
  #params=subset(allParams$RasterFile,!is.na(Timestep));iter=1;ts=2050;min_ts=2020;useMostRecent="RasterID"
  # Cases where One or Both columns are missing
  if(!sum(is.element(names(params), "Iteration"))){
    print("No Iteration column")
    if(!sum(is.element(names(params), "Timestep"))){
      print("No Timestep column either")
      if(nrow(params) > 1){
        stop("No timestep nor iteration specified, yet multiple inputs are provided")
      } else {
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
  
  # Fill in the NAs for filtering
  params$Iteration <- fillWildcardITER(params$Iteration, iter)
  params$Timestep <- fillWildcardTS(params$Timestep, ts, min_ts)

  prevs <- subset(params, Iteration == iter & Timestep < ts)
  theSubset <- subset(params, Iteration == iter & Timestep == ts)
  
  if((useMostRecent!=F)&(nrow(prevs)>0)){
    if(nrow(theSubset)==0){
      missingLayers=unique(prevs[[useMostRecent]])
    }else{
      missingLayers=setdiff(prevs[[useMostRecent]],theSubset[[useMostRecent]])
    }  
    if(length(missingLayers)>0){
      for(mm in missingLayers){
        #mm="Linear Features"
        prevs$match=prevs[[useMostRecent]]
        useP = subset(prevs,match==mm)
        useP$match=NULL
        #For each iteration, select most recent timestep
        useSet = unique(subset(useP,select=c(Timestep,Iteration)))
        useSet=useSet%>%
          group_by(Iteration) %>%
          summarise(Timestep = max(Timestep))
        useP=merge(useP,data.frame(useSet))
        useP$Timestep=ts
        theSubset=rbind(theSubset,useP)
      }
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
    # otherwise, assume NAs are timestep minimum
    #TO DO: figure out what I just broke by changing this assumption.
    x[which(is.na(x))] <- fill#min_ts
  }
  return(x)
}

fillWildcardITER <- function(x, fill){
  # Fill NAs to iteration 1
  x[which(is.na(x))] <- fill
  return(x)
}

# Function to discriminate raster/vectors inputs
# TODO this might become obsolete
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

make_paths_relative <- function(theTable, folder){
  projDir <- theTable[["file"]][[1]] %>%
    strsplit(., folder) %>%
    `[[`(1) %>%
    `[`(1) %>%
    paste0(folder)
  theTable[["file"]] <- theTable[["file"]] %>%
    gsub(pattern = paste0(projDir, "/"), replacement = "", x = .)
  return(theTable)
}

# find_spades_files <- function(path){
# 
#   model_basename <- basename(path)
#   files_in_folder <- tools::file_path_sans_ext(list.files(path))
# 
#   file_name <- which(model_basename %in% files_in_folder)
# 
#   if(length(file_name) == 1){
#     return(file_name)
#   } else {
#     stop("SpaDES file not found")
#   }
# 
# }
