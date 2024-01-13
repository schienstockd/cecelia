RemoveStripes <- R6::R6Class(
  "RemoveStripes",
  inherit = CleanupImages,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "removeStripes",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start stripes correction")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # convert channels names to numbers
      imChannels <- NULL
      
      if (length(self$funParams()$imChannels) > 0)
        imChannelNames <- self$funParams()$imChannels
      else
        imChannelNames <- cciaObj$imChannelNames()
      
      imChannels <- sapply(
        imChannelNames, function(x) {
          as.integer(unname(which(cciaObj$imChannelNames() == x)) - 1)
        }, USE.NAMES = FALSE)
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = cciaObj$imFilepath(valueName = self$funParams()$valueName),
        imCorrectionPath = file.path(
          self$envParams()$dirs$zero, "ccidNoStripes.zarr"),
        stripePerc = self$funParams()$stripePerc,
        imChannels = imChannels
      )
      
      # call python
      self$pyScript("remove_stripes", params)
      
      # update image information
      self$updateImageInfo(filename = "ccidNoStripes", valueName = "noStripes")
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
