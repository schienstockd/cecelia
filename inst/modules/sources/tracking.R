# Base class for tracking methods
Tracking <- R6::R6Class(
  "Tracking",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "tracking"
    },
    
    # reset image information
    resetImageInfo = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # reset values
      # cciaObj$setImLabelsFilepath(NULL)
      
      # save object
      cciaObj$saveState()
    },
    
    # update image information
    updateImageInfo = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # add population to popMap
      popType <- "live"
      parentPops <- c(self$funParams()$valueName)
      pops <- list(
        "tracked" = list(
          filterMeasure = "track_id",
          filterValues = 0,
          filterFun = "gt"
          # TODO add colour choice
          # For now, this is random
        )
      )
      
      # remove populations
      cciaObj$delPopsByPath(
        popType,
        pops = levels(interaction(parentPops, names(pops), sep = "/")),
        includeFiltered = TRUE
      )
      
      # add populations
      cciaObj$addFilteredPops(popType, parentPops, pops,
                              valueName = self$funParams()$valueName)
      
      # save to disk
      cciaObj$savePops(popType, purge = TRUE, includeFiltered = TRUE)
      
      # save object
      cciaObj$saveState()
    }
  )
)