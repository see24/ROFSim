makeLCCfromCohortData <- function(cohortData,
                                  pixelGroupMap,
                                  rstLCC, 
                                  lccClassTable){
  
  library(LandR)
  library(data.table)
  library(raster)
  
  ### Step 1: Define vegetation type ###
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # A) Assign type for each species based on equivalence table (using KNN)
  sppEquivalencies <- LandR::sppEquivalencies_CA
  
  for (x in 1:length(unique(cohortData$speciesCode))) {
    cohortData[speciesCode == unique(cohortData$speciesCode)[x],
               Type := sppEquivalencies[KNN == unique(cohortData$speciesCode)[x]]$Type]
  }
  
  cohortData[Type == "Deciduous"]$Type <- "deciduous"
  cohortData[Type == "Conifer"]$Type <- "conifer"
  
  # B) Calculate species cover based on percent biomass
  cohortData[, coverIndex := B]
  cohortData[, totalCoverIndex := sum(coverIndex), by = c("pixelGroup")]
  cohortData[, treeTypeCoverIndex := sum(coverIndex), by = c("pixelGroup", "Type")]
  cohortData[, percTree := treeTypeCoverIndex/totalCoverIndex, 
             by = c("pixelGroup", "Type") ]
  
  # B.1) Simplify and dcast cohortData to be able to compare the percentages
  cohortDataSim <- unique(cohortData[, c("pixelGroup", "Type", "percTree")])
  cohortDataD <- dcast(data = cohortDataSim, formula = pixelGroup ~ Type, 
                       fill = 0)
  
  # C) Mark pure and mixed stands based on a 75% threshold
  cohortDataD[, pureDec := fifelse(deciduous >= 0.75, 1, 0)]
  cohortDataD[, pureCon := fifelse(conifer >= 0.75, 1, 0)]
  cohortDataD[, standLeading := colnames(.SD)[max.col(.SD, ties.method="first")], 
              .SDcols = c("pureDec", "pureCon")]
  cohortDataD[, standLeading := fifelse(pureDec+pureCon == 0, "mixed", standLeading)]
  
  # D) Simplifying
  cohortDataSim <- unique(cohortDataD[, c("pixelGroup", "standLeading")])
  
  ### Step 2: Define level of openness ###
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # A) Take original LCC classes and divide them into "dense" or "sparse"
  sparsenessMap <- rstLCC
  
  sparsenessMap[!sparsenessMap[] %in% lccClassTable[["LCCclass"]]] <- NA
  
  sparse <- lccClassTable[["LCCclass"]][grep(pattern = "sparse", x = lccClassTable[["standLeading"]])]
  open <- lccClassTable[["LCCclass"]][grep(pattern = "open", x = lccClassTable[["standLeading"]])]
  dense <- lccClassTable[["LCCclass"]][grep(pattern = "dense", x = lccClassTable[["standLeading"]])]
  
  # dense = 1; open = 2; sparse =  3
  sparsenessMap[sparsenessMap[] %in% dense] <- -1
  sparsenessMap[sparsenessMap[] %in% open] <- -2
  sparsenessMap[sparsenessMap[] %in% sparse] <- -3
  
  sparsenessMap <- -sparsenessMap
  
  sparsenessMap <- ratify(sparsenessMap)
  rat <- raster::levels(sparsenessMap)[[1]]
  rat$sparseness <- c("dense", "open", "sparse")
  levels(sparsenessMap) <- rat
  names(sparsenessMap) <- "sparsenessMap"
  sparsenessMapDT <- unique(na.omit(data.table::data.table(getValues(stack(sparsenessMap, 
                                                                           pixelGroupMap)))))
  
  finalDT  <- merge(cohortDataSim, sparsenessMapDT, all.x = TRUE)
  
  finalDT <- merge(finalDT, data.table(sparsenessMap = c(1,2,3), 
                                       sparseness = c("dense", "open", "sparse")),
                   by = "sparsenessMap", all.x = TRUE)
  finalDT[, standLeading  := paste(standLeading, sparseness, sep = "_")]
  
  # Because we have 2 categories of pureCon_sparse (8 and 32 -- which is treed lichen bog or treed wetland),
  # we need to remove one from the lccClassTable
  lccClassTable <- unique(lccClassTable, by = "standLeading")
  finalDT <- merge(finalDT, lccClassTable, by = "standLeading", all.x = TRUE)
  
  # Get the new classes to the LCC where they are supposed to be
  newLCCClass <- SpaDES.tools::rasterizeReduced(reduced = finalDT, 
                                                fullRaster = pixelGroupMap, 
                                                newRasterCols = "LCCclass", 
                                                mapcode = "pixelGroup")
  
  DT <- data.table(pixelID = 1:ncell(newLCCClass),
                   getValues(stack(rstLCC, newLCCClass)))
  names(DT) <- c("pixelID", "LCC", "newLCC")
  DT[, updatedLCC := fifelse(!is.na(newLCC), newLCC, LCC)]
  updatedLCCras <- raster::setValues(x = raster(rstLCC), 
                                     values = DT[["updatedLCC"]])
  
  return(updatedLCCras)
}
