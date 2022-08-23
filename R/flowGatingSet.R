#' Populations represented in GatingSet
#' 
#' @name FlowGatingSet
#' @description Populations represented in GatingSet
#' @import flowWorkspace
#'
#' @examples
#' TODO
#' @export
FlowGatingSet <- R6::R6Class(
  "FlowGatingSet",
  inherit = PopulationUtils,
  
  ## private
  private = list(
    gatingSet = NULL,
    
    # plotting parameters
    flowChannels = NULL,
    flowChannelLimits = NULL,
    
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
      names(flist) <- sampleNames(self$getPopObj())
      
      flist
    },
    
    #' @description Add path gate
    #' @param gateCoords list of (N,2) gate coordinates
    #' @param x character of 'X'-coordinate
    #' @param y character of 'Y'-coordinate
    createPathGate = function(gateCoords, x, y) {
      # create matrix
      # https://stackoverflow.com/a/43425453/13766165
      mat <- t(do.call(cbind, gateCoords))
      colnames(mat) <- c(x, y)
      
      # make new polygon gate
      pg1 <- polygonGate(.gate = mat)
      
      flist <- list(pg1)
      names(flist) <- sampleNames(self$getPopObj())
      
      flist
    }
    
    ## setters
    
    ## getters
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
      pops <- gs_get_pop_paths(self$getPopObj(), order = popOrder)
      
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
    #' @param invalidate boolean to invalidate object
    getPopGate = function(popPath) {
      # get population gate
      .flowGateForPop(self$getPopObj(), popPath)
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
    #' @param cols list of character for columns
    #' @param dropNA boolean to drop NA
    #' @param dropPop boolean to drop population
    #' @param copyDT boolean to copy data.table
    popDT = function(pops = "root", cols = NULL, dropNA = FALSE, dropPop = FALSE) {
      popList <- list()
      
      # go through populations and build datatable
      for (x in pops) {
        popList[[x]] <- .flowFortifyGs(self$getPopObj(), x, cols = cols)
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
      recompute(self$getPopObj())
      
      # invalidate
      private$invalidate(invalidate = invalidate)
    },
    
    #' @description Copy from other GatingSet
    #' @param gsFrom FlowGatingSet to copy from
    #' @param removeAll boolean to remove all populations
    #' @param recompute boolean to recompute after
    #' @param invalidate boolean to invalidate object
    copyGatesFrom = function(gsFrom, removeAll = TRUE, recompute = TRUE,
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
      if (dir.exists(self$getPopObjFilepath()))
        unlink(self$getPopObjFilepath())
      
      save_gs(self$getPopObj(),
              self$getPopObjFilepath(),
              backend_opt = "skip")
    },
    
    #' @description Return whether utils are based on label props
    isLabelPropsStore = function() {
      FALSE
    }
    
    ## setters
    
    ## getters
  )
)
