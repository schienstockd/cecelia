CellposeCorrect <- R6::R6Class(
  "CellposeCorrect",
  inherit = CleanupImages,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "cellposeCorrect",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start Cellpose correction")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get visibility
      cpVisibilities <- self$funParamVisibilities("cpParams", onlyVisible = TRUE)
      
      if (!is.null(cpVisibilities))
        cpParams <- cpParams[names(cpVisibilities)]
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          basename(cciaObj$imFilepath(valueName = self$funParams()$valueName))
        ),
        cpParams = cpParams,
        imCorrectionPath = file.path(
          self$envParams()$dirs$zero,
          "ccidCpCorrected.zarr"
        )
      )
      
      # call python
      self$pyScript("cellpose_correct", params)
      
      # update image information
      self$updateImageInfo(filename = "ccidCpCorrected", valueName = "cpCorrected")
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
