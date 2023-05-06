SlidingWindowCorrect <- R6::R6Class(
  "SlidingWindowCorrect",
  inherit = CleanupImages,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "slidingWindowCorrect",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start sliding window correction")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$writeLog(self$funParams()$valueName)
      self$writeLog(cciaObj$imFilepath(valueName = self$funParams()$valueName))
      self$writeLog(cciaObj$imFilepath(valueName = "corrected"))
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          # basename(cciaObj$imFilepath(valueName = "default"))
          basename(cciaObj$imFilepath(valueName = self$funParams()$valueName))
        ),
        imCorrectionPath = file.path(
          self$envParams()$dirs$zero, "ccidSlidingWindow.zarr"),
        slidingWindow = self$funParams()$slidingWindow
      )
      
      # call python
      self$pyScript("sliding_window_correct", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo(
        filename = "ccidSlidingWindow", valueName = "slidingWindow", 
      )
    }
  )
)
