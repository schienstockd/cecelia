DriftCorrect <- R6::R6Class(
  "driftCorrect",
  inherit = CleanupImages,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "driftCorrect",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start drift correction")
      
      # get object
      cciaObj <- self$cciaTaskObject()

      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          # basename(cciaObj$imFilepath(valueName = "default"))
          basename(cciaObj$imFilepath(valueName = self$funParams()$valueName))
        ),
        driftChannel = self$funParams()$driftChannel,
        applyDriftCorrection = self$funParams()$applyDriftCorrection,
        applyGaussianToOthers = self$funParams()$applyGaussianToOthers,
        imCorrectionPath = file.path(self$envParams()$dirs$zero,
                                     "ccidDriftCorrected.zarr"),
        driftNormalisation = self$funParams()$driftNormalisation
      )
      
      # call python
      self$pyScript("drift_correct", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo(
        filename = "ccidDriftCorrected", valueName = "driftCorrected")
    }
  )
)
