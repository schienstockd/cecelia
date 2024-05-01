AvgCorrect <- R6::R6Class(
  "AvgCorrect",
  inherit = CleanupImages,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "avgCorrect",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start average correction")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # convert channels
      avgChannels <- sapply(
        self$funParams()$avgChannels, function(x) {
            unname(which(cciaObj$imChannelNames() == x)) - 1
          }, USE.NAMES = FALSE, simplify = FALSE)
      
      # get channels to add
      imChannels <- cciaObj$imChannelNames(valueName = self$funParams()$valueName)
      if (length(imChannels) == 0)
        imChannels <- cciaObj$imChannelNames()
      
      channelsToAdd <- paste("Avg", self$funParams()$avgChannels)
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          basename(cciaObj$imFilepath(valueName = self$funParams()$valueName))
        ),
        avgChannels = avgChannels,
        tileXY = self$funParams()$tileXY,
        tileOffset = self$funParams()$tileOffset,
        imCorrectionPath = file.path(
          self$envParams()$dirs$zero,
          "ccidAvgCorrected.zarr"
        )
      )
      
      # call python
      self$pyScript("avg_correct", params)
      
      # update image information
      self$updateImageInfo(
        filename = "ccidAvgCorrected", valueName = "avgCorrected",
        addChannels = channelsToAdd)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
