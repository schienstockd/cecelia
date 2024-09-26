#' GEneric population utils that have to be implemented for individual use cases.
#' 
#' @name PopulationUtils
#' @description Generic population utils
#'
#' @examples
#' TODO
#' @export
PopulationUtils <- R6::R6Class(
  "PopulationUtils",
  inherit = ReactiveObject,
  
  ## private
  private = list(
    popObj = NULL,
    popObjFilepath = NULL,
    imChannels = NULL,
    imChannelLimits = NULL,
    
    # value names
    valueNames = NULL,
    
    #' @description Init channel limits
    #' TODO not use this
    initImChannelLimits = function() {
      # get limits from root
      # TODO is there a faster version of this?
      popDT <- self$popDT()
      
      limits <- list()
      
      # get columns for limits
      limitColumns <- c(self$getImChannels(), cciaConf()$fcs$propsToAdd)
      
      # get limits for channels
      for (x in limitColumns) {
        if (x %in% colnames(popDT)) {
          limits[[x]] <- list(
            min = min(popDT[,get(x)]),
            max = max(popDT[,get(x)])
          )
        }
      }
      
      # set limits
      private$setImChannelLimits(limits, invalidate = FALSE)
    },
    
    ## setters
    setPopObj = function(x, invalidate = TRUE) {
      private$popObj <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setPopObjFilepath = function(x, invalidate = TRUE) {
      private$popObjFilepath <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setImChannels = function(x, invalidate = TRUE) {
      private$imChannels <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setImChannelLimits = function(x, invalidate = TRUE) {
      private$imChannelLimits <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setValueNames = function(x, invalidate = TRUE) {
      private$valueNames <- x
      private$invalidate(invalidate = invalidate)
    }
    
    ## getters
  ),
  
  ### public
  public = list(
    #' @description Init
    #' @param popObjFilepath character for object filepath
    #' @param imChannels list fo character for channel names
    initialize = function(popObjFilepath, imChannels) {
      private$setPopObjFilepath(popObjFilepath, invalidate = FALSE)
      
      # set channel names
      private$setImChannels(.flowCorrectChannelNames(imChannels),
                            invalidate = FALSE)
    },
    
    #' @description
    #' save populations as csv files
    #' this should be done everytime the population
    #' is changing - otherwise napari will
    #' not know what the populations are
    #' 
    #' TODO this is probably not the best way to do this and it only evolved
    #' because populations were originally only defined in a GatingSet with
    #' flowWorkspace. Now that populations are also defined in Anndata - I should
    #' probably think about a more flexible approach.
    #' 
    #' @param pops list of character for populations
    #' @param popsDir character for population directory
    #' @param popDT data.table for populations
    #' @param purge boolean to purge populations
    #' @param labels list of integer for labels
    savePops = function(pops, popsDir, popDT = NULL, purge = FALSE, labels = NULL) {
      # make directory
      dir.create(popsDir, recursive = TRUE)
      
      # remove all populations from directory
      if (purge == TRUE) {
        unlink(file.path(popsDir, "*"))
      }
      
      # get populations
      if (is.null(popDT)) {
        # popDT <- self$popDT(pops, popCols = c(self$getImChannels()[1], "label"))
        popDT <- self$popDT(pops, popCols = c(self$getImChannels()[1], "label", "track_id"))
      }
      
      # select with DT 
      # https://stackoverflow.com/questions/27511604/dplyr-on-data-table-am-i-really-using-data-table
      
      # filter on label IDs
      if (!is.null(labels)) {
        popDT <- popDT[label %in% labels]
      }
      
      # check whether pops are different from IDs
      if (is.null(names(pops))) {
        names(pops) <- pops
      }
      
      # TODO this should not occur in any case
      names(pops)[!is.na(stringr::str_match(names(pops), "/"))] = ""
      
      # remove populations without a name
      # TODO this can be the case for multi-file pops
      # where the root population is not saved
      # as an explicit population in the map
      pops <- pops[names(pops) != ""]
      
      if (nrow(popDT) > 0) {
        # define id columns
        idCol <- c("label", "track_id", if ("track_id" %in% colnames(popDT)) "value_name" else NULL)
        idCol <- idCol[idCol %in% colnames(popDT)]
        
        # go through populations and save as CSV
        for (i in names(pops)) {
          x <- pops[[i]]
          
          # save as csv
          write.csv(
            # popDT[pop == x,][, .(label)],
            popDT[pop == x,][, ..idCol],
            file.path(popsDir, paste0(i, ".csv")),
            row.names = FALSE
          )
        }
      }
    },
    
    #' @description Unlink population csv files
    #' @param pops list of character for populations
    #' @param popsDir character to population directory
    unlinkPops = function(pops, popsDir) {
      # go through populations and delete CSV
      for (i in names(pops)) {
        unlink(file.path(popsDir, paste0(i, ".csv")))
      }
    },
    
    #' @description Population data.table
    #' @param pops list of character for populations
    #' @param popCols list of character for columns
    #' @param dropNA boolean to drop NA
    #' @param dropPop boolean to drop population
    #' @param copyDT boolean to copy data.table
    popDT = function(pops = NULL, popCols = NULL, dropNA = FALSE, dropPop = FALSE) {
      stop("Implement popDT()")
    },
    
    #' @description Save populations back to disk
    save = function() {
      stop("Implement save()")
    },
    
    #' @description Populations paths
    #' @param includeRoot boolean to include root
    popPaths = function(includeRoot = FALSE) {
      stop("Implement popPaths()")
    },
    
    #' @description Pop labels
    popLabels = function() {
      stop("Implement popLabels()")
    },
    
    #' @description Default pops
    defaultPops = function() {
      stop("Implement defaultPops()")
    },
    
    #' @description Whether utils are based on label props
    #' for clusters, pull information from the main DT
    isLabelPropsStore = function() {
      TRUE
    },
    
    ## setters
    
    ## getters
    getPopObj = function(x = NULL) {
      if (!is.null(x)) {
        if (x %in% names(private$popObj)) private$popObj[[x]] else NULL
      } else {
        private$popObj
      }
    },
    
    getPopObjFilepath = function() {
      private$popObjFilepath
    },
    
    getImChannels = function() {
      private$imChannels
    },
    
    getImChannelLimits = function() {
      private$imChannelLimits
    },
    
    getValueNames = function() {
      private$valueNames
    }
  )
)
