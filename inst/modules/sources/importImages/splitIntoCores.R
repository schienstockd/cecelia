SplitIntoCores <- R6::R6Class(
  "SplitIntoCores",
  inherit = Import,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "splitIntoCores",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # create dir
      dir.create(self$envParams()$dirs$zero)
      self$initLog()
      
      self$writeLog(">> Split into cores")
      
      # prepare params
      params <- list(
        oriPath = cciaObj$oriFilepath(),
        normaliseImage = self$funParams()$normaliseImage,
        medianFilter = self$funParams()$medianFilter,
        otsuAdjust = self$funParams()$otsuAdjust,
        closingFilter = self$funParams()$closingFilter,
        smallObjectsSize = self$funParams()$smallObjectsSize,
        labelExpansion = self$funParams()$labelExpansion,
        condaEnvPath = cciaCondaPath()
      )
      
      # call python
      self$pyScript("split_into_cores", params)
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
