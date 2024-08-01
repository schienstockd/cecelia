Crop <- R6::R6Class(
  "crop",
  inherit = CleanupImages,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "crop",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start cropping")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # convert channels names to numbers
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
        imPath = file.path(
          self$envParams()$dirs$zero,
          basename(cciaObj$imFilepath(valueName = self$funParams()$valueName))
        ),
        toCommonArea = self$funParams()$toCommonArea,
        imChannels = imChannels,
        imCorrectionPath = file.path(
          self$envParams()$dirs$zero,
          "ccidCropped.zarr"
        ) 
      )
      
      # call python
      self$pyScript("crop", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo(
        filename = "ccidCropped", valueName = "cropped", 
      )
    }
  )
)
