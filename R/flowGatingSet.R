#' Populations represented in GatingSet
#' 
#' @name FlowGatingSet
#' @description Populations represented in GatingSet
#'
#' @examples
#' TODO
#' @export
FlowGatingSet <- R6::R6Class(
  "FlowGatingSet",
  inherit = PopulationUtils,
  
  ## private
  private = list(
    # checks
    backendOpt = "skip",
    
    #' @description Add rectangle gate
    #' @param gateCoords list of (N,2) gate coordinates
    #' @param x character of 'X'-coordinate
    #' @param y character of 'Y'-coordinate
    createRectangleGate = function(gateCoords, x, y) {
      # create matrix
      mat <- matrix(
        unlist(gateCoords), ncol = 2,
        dimnames = list(c("min", "max"), c(x, y)))
      
      # make new rectangle gate
      rg1 <- rectangleGate(.gate = mat)
      flist <- list(rg1)
      names(flist) <- flowWorkspace::sampleNames(self$getPopObj())
      
      flist
    },
    
    #' @description Add path gate
    #' @param ... .flowPolygonGate
    createPathGate = function(...) {
      flist <- .flowPolygonGate(...)
      
      # add names
      names(flist) <- flowWorkspace::sampleNames(self$getPopObj())
      flist
    },
    
    ## setters
    setBackendOpt = function(x) {
      private$backendOpt <- x
    },
    
    ## getters
    getBackendOpt = function() {
      private$backendOpt
    }
  ),
  
  ### public
  public = list(
    #' @description Init
    #' @param objFilepath character for object file path
    #' @param imChannels list of character for channel names
    #' @param valueName character for value name
    initialize = function(objFilepath, imChannels, valueName = "default") {
      super$initialize(objFilepath, imChannels)
      
      # set gating set
      private$setPopObj(load_gs(objFilepath), invalidate = FALSE)
      
      # init channel limits
      # private$initImChannelLimits()
      
      # set value name
      private$setValueNames(c(valueName), invalidate = FALSE)
    },
    
    #' @description Gated populations paths
    #' @param includeRoot boolean to include root
    popPaths = function (includeRoot = FALSE, popOrder = 'tsort') {
      pops <- flowWorkspace::gs_get_pop_paths(self$getPopObj(), order = popOrder)
      
      if (includeRoot == FALSE) {
        pops <- pops[pops != "root"]
      }
      
      pops
    },
    
    #' @description Population labels
    popLabels = function () {
      popPaths <- self$popPaths()
      
      # to through pops and assign labels unique
      # if a cell is in two or more endpoints
      # it will only get one population name
      # This can happen if the user does not 
      # gate sequential or the gates are overlapping
      popDT <- self$popDT()[, c("pop", "label")]
      
      for (i in popPaths[popPaths != "root"]) {
        # join
        popDT[self$popDT(i)[, c("pop", "label")],
              on = "label", pop := i.pop]
      }
      
      popDT
    },
    
    #' @description Number of gates for population
    #' @param parentPop character for parent population
    #' @param channels list of character (N,2) for channels
    popGateNum = function(parentPop, channels) {
      length(.flowDirectLeavesForPopWithAxis(
        self$getPopObj(), parentPop, channels
      )) 
    },
    
    #' @description Gate IDs for population
    #' @param popPath character for population path
    popGateIDs = function(popPath) {
      # get direct leaves
      directLeavesOnPlot <- self$popDirectLeaves(
        self$popParent(popPath), channels = self$popChannels(popPath))
      
      # assign gate IDs in order of creationg
      gateIDs <- seq(length(directLeavesOnPlot))
      names(gateIDs) <- directLeavesOnPlot
      
      gateIDs
    },
    
    #' @description Gate ID for population
    #' @param popPath character for population path
    popGateID = function(popPath) {
      gateIDs <- self$popGateIDs(popPath)
      
      gateIDs[[popPath]]
    },
    
    #' @description Parent for population
    #' @param pop character for population path
    #' @param root character to identify root population
    #' @param normaliseRoot boolean to normalise root
    popParent = function(pop, root = "", normaliseRoot = FALSE) {
      parentPath <- .flowPopParent(pop, root)
      
      # normalise root to '/'
      if (normaliseRoot == TRUE) {
        parentPath <- .flowNormRootPath(parentPath)
      }
      
      parentPath
    },
    
    #' @description Filtered list from gate coords
    #' @param gateCoords list of (N,2) gate coordinates
    #' @param x character of 'X'-coordinate
    #' @param y character of 'Y'-coordinate
    flistFromGateCoords = function(gateCoords, x, y) {
      flist <- NULL
      
      # call respective gating function
      if (names(gateCoords) == "rect")
        flist <- private$createRectangleGate(gateCoords$rect, x, y)
      else if (names(gateCoords) == "path")
        flist <- private$createPathGate(gateCoords$path, x, y)
      
      flist
    },
    
    #' @description Add population
    #' @param popName character for population name
    #' @param gateCoords list of (N,2) gate coordinates
    #' @param x character of 'X'-coordinate
    #' @param y character of 'Y'-coordinate
    #' @param parentPop character for parent population
    #' @param flist flow list object
    #' @param invalidate boolean to invalidate object
    addPop = function(popName, gateCoords, x, y, parentPop = "root",
                      flist = NULL, invalidate = TRUE) {
      if (is.null(flist)) {
        flist <- self$flistFromGateCoords(gateCoords, x, y)
      } else {
        # get x and y
        x <- names(flist@parameters)[1]
        y <- names(flist@parameters)[2]
      }
      
      # add pop to set
      gs_pop_add(self$getPopObj(), flist,
                 name = popName,
                 parent = parentPop)
      
      # how many gates are there for this specific axis combination?
      gateID <- self$popGateNum(parentPop, c(x, y))
      
      private$invalidate(invalidate = invalidate)
      
      gateID
    },
    
    #' @description Set population
    #' @param popPath character for population path
    #' @param gateCoords list of (N,2) gate coordinates
    #' @param x character of 'X'-coordinate
    #' @param y character of 'Y'-coordinate
    #' @param flist flow list object
    #' @param invalidate boolean to invalidate object
    setPop = function(popPath, gateCoords, x, y, flist = NULL, invalidate = TRUE) {
      # create gate
      if (is.null(flist))
        flist <- self$flistFromGateCoords(gateCoords, x, y)
      
      # add pop to set
      gs_pop_set_gate(self$getPopObj(), popPath, flist)
      
      private$invalidate(invalidate = invalidate)
    },
    
    #' @description Get population gate
    #' @param popPath character for population path
    getPopGate = function(popPath) {
      # get population gate
      .flowGateForPop(self$getPopObj(), popPath)
    },
    
    #' @description Get population stats
    #' @param popPath character for population path
    #' @param ... passed to .flowStatsForPop
    getPopStats = function(popPath, ...) {
      # get population gate
      .flowStatsForPop(self$getPopObj(), popPath, ...)
    },
    
    #' @description Delete population
    #' @param popPath character for population path
    #' @param invalidate boolean to invalidate object
    delPop = function(popPath, invalidate = TRUE) {
      popDeleted <- TRUE
      
      # remove from gating set
      tryCatch(
        expr = {
          gs_pop_remove(self$getPopObj(), popPath)
          private$invalidate(invalidate = invalidate)
        },
        error = function(e){ 
          popDeleted <<- FALSE
        },
        warning = function(w){
          popDeleted <<- FALSE
        }
      )
      
      popDeleted
    },
    
    #' @description Rename population
    #' @param popPath character for population path
    #' @param popName character for new population name
    #' @param invalidate boolean to invalidate object
    renamePop = function(popPath, popName, invalidate = TRUE) {
      newPath <- NULL
      
      # rename population 
      tryCatch(
        expr = {
          # get parent
          popParent <- self$popParent(popPath)
          
          # is there already a population with that name?
          directLeaves <- self$popDirectLeaves(popParent, namesOnly = TRUE)
          
          if (!popName %in% directLeaves) {
            # rename
            gs_pop_set_name(self$getPopObj(), popPath, popName)
            
            newPath <- paste(popParent, popName, sep = "/")
            private$invalidate(invalidate = invalidate)
          }
        },
        error = function(e){ 
        },
        warning = function(w){
        }
      )
      
      newPath
    },
    
    #' @description Is population root?
    #' @param pop character for population path
    popIsRoot = function(pop) {
      .flowPopIsRoot(pop)
    },
    
    #' @description Does the population exists in leaves?
    #' @param pops list of character for populations
    #' @param parent character for parent population
    #' @param directLeaves boolean to only include direct leaves
    #' @param includeParent boolean to include parent
    popsInLeaves = function(pops, parent = "root", directLeaves = FALSE,
                            includeParent = FALSE) {
      # get leaves
      if (directLeaves == TRUE) {
        leaves <- self$popDirectLeaves(parent)
      } else {
        leaves <- self$popLeaves(parent)
      }
      
      # are the pops in the list?
      leavesExist <- pops %in% leaves
      
      # exclude root
      leavesExist[pops == "root"] <- TRUE
      
      # add parent?
      if (includeParent == TRUE) {
        leavesExist[pops == parent] <- TRUE
      }
      
      leavesExist
    },
    
    #' @description Leaves of population
    #' @param popPath character for population
    popLeaves = function(popPath) {
      .flowLeavesForPop(self$getPopObj(), popPath)
    },
    
    #' @description Direct leaves of population
    #' @param popPath character for population
    #' @param channels list of character (2) for channels
    #' @param namesOnly boolean to return only population names not full path
    popDirectLeaves = function(popPath, channels = NULL, namesOnly = FALSE) {
      retVal <- NULL
      
      if (!is.null(channels)) {
        retVal <- .flowDirectLeavesForPopWithAxis(
          self$getPopObj(), popPath, channels)
      } else {
        retVal <- .flowDirectLeavesForPop(self$getPopObj(), popPath)
      }
      
      # only population names?
      if (namesOnly == TRUE) {
        if (popPath == "")
          popPath <- "/"
        
        # get names
        splitNames <- unlist(stringr::str_split(retVal, popPath))
        splitNames <- splitNames[splitNames != ""]
        
        # remote leading '/'
        retVal <- stringr::str_replace(splitNames, "/", "")
      }
      
      retVal
    },
    
    #' @description Channels of population
    #' @param popPath character for population
    popChannels = function(popPath) {
      .flowChannelsForPop(self$getPopObj(), popPath)
    },
    
    #' @description Check whether population is gated on the channels
    #' @param popPath character for population
    #' @param channels list of character (2) for channels
    popGatedOnChannels = function(popPath, channels) {
      .flowMatchGatingParamsForPop(
        self$getPopObj(),
        popPath, channels
      )
    },
    
    #' @description Get gate for population
    #' @param popPath character for population
    popGate = function(popPath) {
      .flowGateForPop(
        self$getPopObj(), popPath)
    },
    
    #' @description Default pops
    defaultPops = function() {
      "root"
    },
    
    #' @description Population data.table
    #' @param pops list of character for populations
    #' @param popCols list of character for columns
    #' @param dropNA boolean to drop NA
    #' @param dropPop boolean to drop population
    #' @param copyDT boolean to copy data.table
    popDT = function(pops = "root", popCols = NULL, dropNA = FALSE, dropPop = FALSE) {
      popList <- list()
      
      # make sure that columns are flow names
      popCols <- .flowCorrectChannelNames(popCols)
      
      # go through populations and build datatable
      for (x in pops) {
        popList[[x]] <- .flowFortifyGs(self$getPopObj(), cols = popCols, subset = x)
      }
      
      DT <- data.table::rbindlist(popList, idcol = TRUE)
      
      # rename populations
      if (nrow(DT) > 0) {
        setnames(DT, ".id", "pop")
      }
      
      DT
    },
    
    #' @description Change parent name
    #' @param popPath character for population
    #' @param parentPath character for parent population
    changeParentName = function(popPath, parentPath) {
      .flowChangeParentName(popPath, parentPath)
    },
    
    #' @description Recompute gating
    #' @param invalidate boolean to invalidate object
    recompute = function(invalidate = TRUE) {
      flowWorkspace::recompute(self$getPopObj())
      
      # invalidate
      private$invalidate(invalidate = invalidate)
    },
    
    #' @description change column names
    #' @param colNames list of character with new column names
    #' @param invalidate boolean to invalidate object
    renameColumns = function(colNames, invalidate = TRUE) {
      # flowWorkspace::colnames(self$getPopObj()) <- colNames
      # flowWorkspace::colnames(private$popObj) <- colNames
      
      oldNames <- colnames(self$getPopObj())
      newNames <- colNames
      
      # add label if not present
      if (!"label" %in% newNames) {
        newNames <- c(newNames, "label")
      }
      
      # create mapping
      channelMap <- data.frame(old = oldNames, new = newNames)
      
      # apply mapping
      private$setPopObj(
        flowWorkspace::gs_update_channels(self$getPopObj(), channelMap))
      
      # make sure data is saved
      private$setBackendOpt("move")
      
      # invalidate
      private$invalidate(invalidate = invalidate)
    },
    
    #' @description Copy from other GatingSet
    #' @param gsFrom FlowGatingSet to copy from
    #' @param removeAll boolean to remove all populations
    #' @param recompute boolean to recompute after
    #' @param invalidate boolean to invalidate object
    copyGatesFrom = function(gsFrom, removeAll = TRUE, recompute = FALSE,
                             invalidate = TRUE) {
      # remove pops if necessary
      if (removeAll == TRUE && length(self$popPaths()) > 0) {
        # delete from root
        # TODO this probably has to be a bit more gentle
        # than just removing everything .. ?
        for (y in self$popDirectLeaves(popPath = "root"))
          self$delPop(y, invalidate = invalidate)
      }
      
      # add pops
      for (x in gsFrom$popPaths()) {
        self$addPop(popName = .flowTrimPath(x, pathLevels = 0),
                    parentPop = .flowPopParent(x, root = "root"),
                    flist = gsFrom$getPopGate(x),
                    invalidate = invalidate)
      }
      
      if (recompute == TRUE)
        self$recompute(invalidate = invalidate)
    },
    
    #' @description Save gating set
    save = function() {
      # remove directory if exists
      if (dir.exists(self$getPopObjFilepath()) && private$getBackendOpt() == "skip")
        unlink(self$getPopObjFilepath())
      
      flowWorkspace::save_gs(
        self$getPopObj(), self$getPopObjFilepath(), backend_opt = private$getBackendOpt())
      
      # reset backend opt to skip
      private$setBackendOpt("skip")
    },
    
    #' @description Return whether utils are based on label props
    isLabelPropsStore = function() {
      FALSE
    }
    
    ## setters
    
    ## getters
  )
)
