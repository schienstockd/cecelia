AfDriftCorrect <- R6::R6Class(
  "AfDriftCorrect",
  inherit = CleanupImages,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "afDriftCorrect",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start autofluorescence and drift correction")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # convert AF combination list to integer
      afCombinations <- self$funParams()$afCombinations
      afCombinations <- lapply(
        afCombinations, function(x) {
          x$divisionChannels <- sapply(x$divisionChannels, as.integer)
          
          x
        })
      
      # get visibility
      afVisibilities <- self$funParamVisibilities("afCombinations", onlyVisible = TRUE)
      
      if (!is.null(afVisibilities))
        afCombinations <- afCombinations[names(afVisibilities)]
      
      # get channels to add from inverse
      imChannels <- cciaObj$imChannelNames(valueName = self$funParams()$valueName)
      if (length(imChannels) == 0)
        imChannels <- cciaObj$imChannelNames()
      
      channelsToAdd <- mapply(
        function(x, i) {
          if (x$generateInverse == TRUE)
            paste("Inverse", i)
          else
            NA
        }, afCombinations, imChannels, SIMPLIFY = FALSE
      )
      channelsToAdd <- channelsToAdd[!is.na(channelsToAdd)]
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          # basename(cciaObj$imFilepath(valueName = "default"))
          basename(cciaObj$imFilepath(valueName = self$funParams()$valueName))
        ),
        afCombinations = afCombinations,
        driftChannel = self$funParams()$driftChannel,
        applyDriftCorrection = self$funParams()$applyDriftCorrection,
        applyGaussian = self$funParams()$applyGaussian,
        applyGaussianToOthers = self$funParams()$applyGaussianToOthers,
        imCorrectionPath = file.path(
          self$envParams()$dirs$zero,
          "ccidCorrected.zarr"
        ),
        driftNormalisation = self$funParams()$driftNormalisation
      )
      
      # call python
      self$pyScript("af_drift_correct", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      # self$updateImageInfo(addChannels = c("AF generated"))
      self$updateImageInfo(addChannels = channelsToAdd)
    }
  )
)
