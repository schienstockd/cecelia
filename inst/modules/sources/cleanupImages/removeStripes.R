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
          self$envParams()$dirs$zero, "ccidN2V.zarr"),
        modelDir = file.path(
          cciaObj$persistentObjectDirectory(uID = cecelia:::CCID_IMAGE_COLLECTION), 
          cciaConf()$dirs$tasks$models, cciaConf()$dirs$models$n2v),
        modelMapping = modelMapping
      )
      
      # call python
      self$pyScript("n2v_correct", params)
      
      # update image information
      self$updateImageInfo(filename = "ccidN2V", valueName = "n2v")
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
