#' Reactive image object set
#' 
#' @name CciaImageSet
#' @description Reactive image object set
#'
#' @examples
#' TODO
#' @export
CciaImageSet <- R6::R6Class(
  "CciaImageSet",
  inherit = ReactivePersistentObjectSet,
  
  ## private
  private = list(
  ),
  
  ### public
  public = list(
    #' @description Init
    #' @param stateFile character with file path to state file
    #' @param ... passed to super$initialize
    initialize = function(stateFile, ...) {
      super$initialize(stateFile = stateFile, ...)
      
      # create directories for processing
      for (x in cciaConf()$dirs$tasks) {
        dir.create(file.path(dirname(stateFile), x),
                   showWarnings = FALSE)
      }
    },
    
    #' @description Propagate population map to others
    #' @param popType character for population type
    #' @param fromUID character for unique ID from
    #' @param toUIDs character for unique ID to
    #' @param invalidate boolean to invalidate object
    #' @param saveState boolean to save state
    propagatePopMap = function(popType, fromUID, toUIDs = NULL, invalidate = FALSE,
                               saveState = FALSE) {
      # get reference object
      refObj <- self$cciaObjects()[[fromUID]]
      
      # get to uIDs
      if (is.null(toUIDs)) {
        toUIDs <- names(self$cciaObjects())
      }
        
      # check that reference is not copied
      toUIDs = toUIDs[toUIDs != fromUID]
      
      # get mapping from image
      if (private$isReactive() == TRUE) {
        popMap <- refObj()$imPopMap(popType, includeFiltered = TRUE)
      } else {
        popMap <- refObj$imPopMap(popType, includeFiltered = TRUE)
      }
      
      # set mapping
      for (x in self$cciaObjects(uIDs = toUIDs)) {
        if (private$isReactive() == TRUE) {
          x()$setImPopMap(popType, popMap, invalidate = invalidate)
          
          if (saveState == TRUE)
            x()$saveState(saveData = FALSE)
        } else {
          x$setImPopMap(popType, popMap, invalidate = invalidate)
          
          if (saveState == TRUE)
            x$saveState(saveData = FALSE)
        }
      }
    },
    
    #' @description Propagate flow gating to others
    #' @param fromUID character for unique ID from
    #' @param toUIDs character for unique ID to
    #' @param invalidate boolean to invalidate object
    #' @param saveState boolean to save state
    #' @param ... passed to FlowGatingSet$copyGatesFrom
    propagateFlowGating = function(fromUID, toUIDs = NULL, invalidate = FALSE,
                                   saveState = FALSE, ...) {
      # get reference object
      refObj <- self$cciaObjects()[[fromUID]]
      
      # get to uIDs
      if (is.null(toUIDs)) {
        toUIDs <- names(self$cciaObjects())
      }
      
      # check that reference is not copied
      toUIDs = toUIDs[toUIDs != fromUID]
      
      # get GatingSet from image
      if (private$isReactive() == TRUE) {
        gsFrom <- refObj()$flowGatingSet()
      } else {
        gsFrom <- refObj$flowGatingSet()
      }
      
      # set gating
      for (x in self$cciaObjects(uIDs = toUIDs)) {
        if (private$isReactive() == TRUE) {
          x()$flowGatingSet()$copyGatesFrom(
            gsFrom, invalidate = invalidate, ...)
          
          if (saveState == TRUE)
            x()$saveState(saveData = FALSE)
        } else {
          x$flowGatingSet()$copyGatesFrom(
            gsFrom, invalidate = invalidate, ...)
          
          if (saveState == TRUE)
            x$saveState(saveData = FALSE)
        }
      }
    },
    
    #' @description Channels for objects
    #' @param cciaObjects list of ReactivePersistentObject
    cciaObjectsChannelNames = function(cciaObjects = NULL) {
      if (is.null(cciaObjects)) {
        cciaObjects <- self$cciaObjects()
      }
      
      objChannelNames <- c()
      
      # go through objects and get all attributes
      cciaObjects <- private$reqCciaObjectList(cciaObjects)
      
      if (length(cciaObjects) > 0) {
        for (curObj in cciaObjects) {
          objChannelNames <- c(
            objChannelNames,
            names(curObj()$imChannelNames())
          )
        }
      }
      
      unique(objChannelNames)
    },
    
    #' @description Edit channel names for objects
    #' @param objChannelNum integer for channel number
    #' @param objChannelNames list of character channel names
    #' @param cciaObjects list of ReactivePersistentObject
    editChannelNamesForCciaObjects = function(objChannelNum, objChannelNames, cciaObjects = NULL, ...) {
      # get objects
      if (is.null(cciaObjects)){
        cciaObjects <- self$cciaObjects()
      }
      
      cciaObjects <- private$reqCciaObjectList(cciaObjects)
      
      # go through objects
      if (length(cciaObjects) > 0) {
        for (curObjName in names(cciaObjects)) {
          cciaObjects[[curObjName]]()$editChannelName(
            objChannelNum, objChannelNames[[curObjName]], ...)
        }
      }
    },
    
    #' @description ppps for images
    #' @param removeNULL boolean to remove NULL
    #' @param uIDs list of character for unique IDs
    #' @param ... passed to CciaImage$ppp
    ppp = function(removeNULL = TRUE, uIDs = NULL, ...) {
      # get ppps from images
      if (private$isReactive() == TRUE) {
        ppps <- lapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) {
            message(sprintf("[ppp] >> %s", x()$getUID()))
            x()$ppp(...)
          }
        )
      } else {
        ppps <- lapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) {
            message(sprintf("[ppp] >> %s", x$getUID()))
            x$ppp(...)
          }
        )
      }
      
      # remove null
      if (removeNULL == TRUE) {
        ppps <- ppps[lengths(ppps) != 0]
      }
      
      ppps
    },
    
    #' @description Spatial DTs for images
    #' @param asDT boolean to convert to data.table
    #' @param removeNULL boolean to remove NULL
    #' @param uIDs list of character for unique IDs
    #' @param ... passed to CciaImage$spatialDT
    spatialDT = function(
      asDT = TRUE, removeNULL = TRUE, uIDs = NULL, ...) {
      # get DTs from images
      if (private$isReactive() == TRUE) {
        spatialDTs <- lapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) {
            message(sprintf("[spatialDT] >> %s", x()$getUID()))
            x()$spatialDT(...)
          }
        )
      } else {
        spatialDTs <- lapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) {
            message(sprintf("[spatialDT] >> %s", x$getUID()))
            x$spatialDT(...)
          }
        )
      }
      
      # remove null
      if (removeNULL == TRUE) {
        spatialDTs <- spatialDTs[lengths(spatialDTs) != 0]
      }
      
      # bind together
      if (asDT == TRUE) {
        spatialDTs <- data.table::rbindlist(spatialDTs, fill = TRUE, idcol = "uID")
      }
      
      spatialDTs
    },
    
    #' @description pop paths for images
    #' @param uIDs list of character for unique IDs
    #' @param combinePaths boolean to compile paths into one list
    #' @param ... passed to CciaImage$popPaths
    popPaths = function(uIDs = NULL, combinePaths = TRUE, ...) {
      # get paths from images
      if (private$isReactive() == TRUE) {
        popPaths <- lapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) {
            message(sprintf("[popDT] >> %s", x()$getUID()))
            x()$popPaths(...)
          }
        )
      } else {
        # popPaths <- lapply(
        popPaths <- parallel::mclapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) {
            message(sprintf("[popDT] >> %s", x$getUID()))
            x$popPaths(...)
            # }
            # }, mc.cores = parallel::detectCores()
          }, mc.cores = 2
        )
      }
      
      if (combinePaths == TRUE) {
        popPaths <- unique(unname(unlist(popPaths)))
      }
      
      popPaths
    },
    
    #' @description popDTs for images
    #' @param asDT boolean to convert to data.table
    #' @param removeNULL boolean to remove NULL
    #' @param uIDs list of character for unique IDs
    #' @param colsToNormalise list of character for columns to normalise
    #' @param batchGroup character to identify batch groups
    #' @param normPercentile numeric for percentile to normalise
    #' @param mc.cores numeric for workers
    #' @param ... passed to CciaImage$popDT
    popDT = function(asDT = TRUE, removeNULL = TRUE, uIDs = NULL,
                     colsToNormalise = c(), batchGroup = "uID", normPercentile = 0.998,
                     mc.cores = 2, ...) {
      # get DTs from images
      if (private$isReactive() == TRUE) {
        popDTs <- lapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) {
            message(sprintf("[popDT] >> %s", x()$getUID()))
            x()$popDT(...)
          }
        )
      } else {
        # popDTs <- lapply(
        popDTs <- parallel::mclapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) {
            message(sprintf("[popDT] >> %s", x$getUID()))
            x$popDT(...)
          # }
          # }, mc.cores = parallel::detectCores() - 2
          }, mc.cores = mc.cores
        )
      }
      
      # remove null
      if (removeNULL == TRUE) {
        popDTs <- popDTs[lengths(popDTs) != 0]
      }
      
      # bind together
      if (asDT == TRUE) {
        popDTs <- data.table::rbindlist(popDTs, fill = TRUE, idcol = "uID")
        
        # normalise columns per batch group?
        if (length(colsToNormalise) > 0) {
          # add experimental information for grouping?
          if (!batchGroup %in% colnames(popDTs)) {
            # popCols <- colnames(popDT)
            
            # add experimental info
            popDTs <- popDTs[
              as.data.table(self$summary(withSelf = FALSE, fields = c("Attr"))),
              on = "uID"]
          }
          
          # normalise DT
          popDTs <- .normaliseDT(
            popDTs,
            colsToNormalise = colsToNormalise,
            batchGroup = batchGroup,
            normPercentile = normPercentile
            )
          
          # remove experimental info again?
          # popDT <- popDT[, ..popCols]
        }
      }
      
      popDTs
    },
    
    #' @description Tracks for images
    #' @param pop character for population
    #' @param removeNULL boolean to remove NULL
    #' @param uIDs list of character for unique IDs
    #' @param ... passed to CciaImage$tracks
    tracks = function(pop, removeNULL = TRUE,
                      uIDs = NULL, ...) {
      # get tracks from images
      if (private$isReactive() == TRUE) {
        imTracks <- lapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) {
            message(sprintf("[tracks] >> %s", x()$getUID()))
            x()$tracks(pop, ...)
          } 
        )
      } else {
        imTracks <- lapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) {
            message(sprintf("[tracks] >> %s", x$getUID()))
            x$tracks(pop, ...)
          } 
        )
      }
      
      # remove null
      if (removeNULL == TRUE) {
        imTracks <- imTracks[lengths(imTracks) != 0]
      }
      
      imTracks
    },
    
    #' @description Tracks measurements for images
    #' @param measures list of character for track measurements
    #' @param removeNULL boolean to remove NULL
    #' @param uIDs list of character for unique IDs
    #' @param ... passed to CciaImage$tracksMeasures
    tracksMeasures = function(measures,
                              asDT = TRUE, removeNULL = TRUE, uIDs = NULL,
                              ...) {
      # get tracksInfo from images
      if (private$isReactive() == TRUE) {
        imTracksMeasures <- lapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) x()$tracksMeasures(
            measures, ...)
        )
      } else {
        imTracksMeasures <- lapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) x$tracksMeasures(
            measures, ...)
        )
      }
      
      # remove null
      if (removeNULL == TRUE) {
        imTracksMeasures <- imTracksMeasures[lengths(imTracksMeasures) != 0]
      }
      
      # bind together
      if (asDT == TRUE) {
        imTracksMeasures <- data.table::rbindlist(imTracksMeasures, fill = TRUE, idcol = "uID")
        
        if (nrow(imTracksMeasures) > 0) {
          # adjust track id column type
          imTracksMeasures[, track_id := as.double(track_id)]
        }
      }
      
      imTracksMeasures
    },
    
    #' @description Tracks info for images
    #' @param trackStatsNames list of character for track statistics
    #' @param parentPop character for parent population
    #' @param removeNULL boolean to remove NULL
    #' @param uIDs list of character for unique IDs
    #' @param ... passed to CciaImage$tracksInfo
    tracksInfo = function(trackStatsNames, parentPop,
                          asDT = TRUE, removeNULL = TRUE, uIDs = NULL,
                          ...) {
      # get tracksInfo from images
      if (private$isReactive() == TRUE) {
        imTracksInfo <- lapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) x()$tracksInfo(
            trackStatsNames, parentPop, ...)
        )
      } else {
        imTracksInfo <- lapply(
          self$cciaObjects(uIDs = uIDs),
          function(x) x$tracksInfo(
            trackStatsNames, parentPop, ...)
        )
      }
      
      # remove null
      if (removeNULL == TRUE) {
        imTracksInfo <- imTracksInfo[lengths(imTracksInfo) != 0]
      }
      
      # bind together
      if (asDT == TRUE) {
        imTracksInfo <- data.table::rbindlist(imTracksInfo, fill = TRUE, idcol = "uID")
        
        if (nrow(imTracksInfo) > 0) {
          # adjust track id column type
          imTracksInfo[, track_id := as.double(track_id)]
        }
      }
      
      imTracksInfo
    },
    
    #' @description Segmented cells object
    #' @param cellTypeString character for cell type
    #' @param ... passed to self$popDT
    segmentedCells = function(cellTypeString = "pop", ...) {
      # get populations
      popDT <- self$popDT(...)
      
      # need to add prefixes for columns
      for (x in .flowCorrectChannelNames(
        # assuming all have the same channels
        self$cciaObjects()[[1]]$imChannelNames())) {
        if (x %in% colnames(popDT))
          setnames(popDT, x, paste0("intensity_", x))
      }
      
      # get morphology columns
      morphCols <- colnames(popDT)[!grepl(sprintf('^%s', paste(c(
          "centroid", "intensity", "UMAP",
          names(cciaConf()$parameters$popTypes), "bbox"),
          collapse = "|")), colnames(popDT))]
      
      # TODO this might be have to be extended
      morphCols <- morphCols[!morphCols %in% c(
        "uID", "label", "pop", "clusters", "value_sum")]
      
      for (x in morphCols) {
        if (x %in% colnames(popDT))
          setnames(popDT, x, paste0("morphology_", x))
      }
      
      # add identifiers
      popDT[, uID.label := paste(uID, label, sep = ".")]
      
      # get experimental info and merge
      exp.info <- as.data.table(cciaObj$summary(
        withSelf = FALSE, fields = c("Attr")
      ))
      
      # add dummy info if only one or none is given
      if (ncol(exp.info) == 1) {
        exp.info$DUMMY_A <- "Whale"
        exp.info$DUMMY_B <- "Lemon"
      } else if (ncol(exp.info) == 2) {
        exp.info$DUMMY <- "Lemon"
      }
      
      # merge
      popDT <- merge(popDT, exp.info)
      
      for (x in colnames(exp.info)[colnames(exp.info) != "uID"]) {
        setnames(popDT, x, paste0("exp_", x))
      }
      
      # generate segmented cells object
      spicyR::SegmentedCells(
        as.data.frame(popDT), cellProfiler = FALSE,
        spatialCoords = c("centroid_x", "centroid_y"),
        cellTypeString = cellTypeString,
        intensityString = "intensity_",
        morphologyString = "morphology_",
        phenotypeString = "exp_",
        cellIDString = "label",
        cellAnnotations = NULL,
        imageCellIDString = "uID.label",
        imageIDString = "uID",
        verbose = TRUE)
    },
    
    #' @description Load objects
    #' @param reset boolean to reset data
    #' @param forceDataReload boolean to force reload data
    #' @param ... passed to super$retrieveState
    retrieveState = function(reset = FALSE, forceDataReload = FALSE, ...) {
      super$retrieveState(...)
      
      # reset data?
      if (reset == TRUE) {
        self$resetData()
      }
      
      # retrieve data for all children
      if (forceDataReload == TRUE) {
        self$forceDataReload()
      }
    },
    
    #' @description Reset data
    resetData = function() {
      # go through all objects and reset data
      for (cciaObj in self$cciaObjects()) {
        cciaObj()$resetData()
      }
    },
    
    #' @description Force reload data
    forceDataReload = function() {
      # go through all objects and reload data
      for (cciaObj in self$cciaObjects()) {
        cciaObj()$loadData(forceReload = TRUE)
      }
    },
    
    ## setters
    setFlowAutospillPath = function(x, valueName = NULL, setDefault = TRUE,
                                    invalidate = TRUE, reset = FALSE) {
      objMeta <- self$getCciaMeta()
      
      objMeta <- .setVersionedVarInList(
        objMeta, "flowAutospillPath", x, valueName = valueName,
        setDefault = setDefault, reset = reset)
      
      self$setCciaMeta(objMeta, invalidate = invalidate)
    },
    
    ## getters
    flowAutospillPath = function(valueName = NULL, absolutePath = TRUE) {
      retVal = NULL
      
      retVal <- .getVersionedVarInList(
        self$getCciaMeta(), "flowAutospillPath", valueName = valueName)
      
      # add task directory
      retVal <- file.path(
        cciaConf()$dirs$tasks$data, "autospill", retVal,
        "table_spillover", "autospill_spillover.csv")
      
      # get absolute path?
      if (!is.null(retVal)) {
        if (absolutePath == TRUE) {
          retVal <- self$persistentObjectDirectoryFile(retVal)
        }
      }
      
      retVal
    },
    
    flowAutospillMatrix = function(valueName = NULL) {
      # get absolute path
      aspPath <- self$flowAutospillPath(valueName = valueName, absolutePath = TRUE)
      
      # Generate compensation matrix with autospill
      # https://autospill.vib.be/public/#/results/7418326b0127a8a587c276a1bcb39608
      comp_matrix <- read.csv(aspPath, header = TRUE)
      
      # move first column to rownames
      rownames(comp_matrix) <- comp_matrix[, 1]
      comp_matrix[, 1] <- NULL
      
      # adjust column names
      colnames(comp_matrix) <- rownames(comp_matrix)
      
      flowCore::compensation(comp_matrix)
    }
  )
)
