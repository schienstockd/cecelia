CellNeighbours <- R6::R6Class(
  "CellNeighbours",
  inherit = SpatialAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "cellNeighbours",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },

    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start region detection")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        popType = self$funParams()$popType,
        pops = self$funParams()$pops,
        neighbourMethod = self$funParams()$neighbourMethod,
        neighbourRadius = self$funParams()$neighbourRadius,
        nRings = self$funParams()$nRings
      )
      
      # save pops before call
      if (self$funParams()$savePops == TRUE) {
        cciaObj$savePopMap(self$funParams()$popType, includeFiltered = TRUE)
        cciaObj$savePops(self$funParams()$popType,
                         pops = c(self$funParams()$pops),
                         includeFiltered = TRUE)
      }
        
      # call python
      self$pyScript("cell_neighbours", params)
      
      # update datapath
      cciaObj$setImNeighboursFilepath(
        paste0(self$funParams()$valueName, ".sq", cciaConf()$files$ext$anndata),
        valueName = self$funParams()$valueName
      )
      
      # save object
      cciaObj$saveState()
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
      
      TRUE
    }
  )
)
