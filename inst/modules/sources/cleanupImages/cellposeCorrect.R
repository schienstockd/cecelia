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
      
      # convert channels names to numbers
      models <- lapply(
        self$funParams()$models, function(x) {
          x$modelChannels <- sapply(
            x$modelChannels, function(y) {
              unname(which(cciaObj$imChannelNames() == y)) - 1
            }, USE.NAMES = FALSE
          )
          
          x
        })
      
      # get visibility
      modelVisibilities <- self$funParamVisibilities("models", onlyVisible = TRUE)
      
      if (!is.null(modelVisibilities))
        models <- models[names(modelVisibilities)]
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          basename(cciaObj$imFilepath(valueName = self$funParams()$valueName))
        ),
        models = models,
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
