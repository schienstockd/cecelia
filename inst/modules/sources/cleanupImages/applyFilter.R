ApplyFilter <- R6::R6Class(
  "ApplyFilter",
  inherit = CleanupImages,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "applyFilter",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start filtering")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          basename(cciaObj$imFilepath(valueName = self$funParams()$valueName))
        ),
        filterFun = self$funParams()$filterFun,
        filterValue = self$funParams()$filterValue,
        imCorrectionPath = file.path(
          self$envParams()$dirs$zero,
          "ccidCorrected.zarr"
        ) 
      )
      
      # call python
      self$pyScript("apply_filter", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
