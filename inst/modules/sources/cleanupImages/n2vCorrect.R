N2vCorrect <- R6::R6Class(
  "N2vCorrect",
  inherit = CleanupImages,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "n2vCorrect",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start Noise2Void correction")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # convert model channels to numbers and get model file paths
      modelMapping <- mapply(
        function(x, i) {
          # channel names
          if (length(x$modelChannels) > 0) {
            x$modelChannels <- sapply(
              x$modelChannels, function(y) {
                unname(which(cciaObj$imChannelNames() == y)) - 1
              }, USE.NAMES = FALSE
            )
            
            x
          } else {
            NULL
          }
        },
        self$funParams()$modelMapping, names(self$funParams()$modelMapping),
        SIMPLIFY = FALSE 
        )
      modelMapping <- modelMapping[lengths(modelMapping) > 0]
      
      # get visibility
      modelVisibilities <- self$funParamVisibilities("modelMapping", onlyVisible = TRUE)
      
      if (!is.null(modelVisibilities))
        modelMapping <- modelMapping[names(modelVisibilities)]
      
      # TODO this takes a long time as it loads all objects
      # n2vFiles <- self$cciaImageCollection()$n2vFiles(fullPath = TRUE)
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = cciaObj$imFilepath(valueName = self$funParams()$valueName),
        imCorrectionPath = file.path(
          self$envParams()$dirs$zero, "ccidN2V.zarr"),
        modelDir = file.path(
          cciaObj$persistentObjectDirectory(uID = cecelia:::CCID_IMAGE_COLLECTION), 
          cciaConf()$dirs$tasks$models, cciaConf()$dirs$models$n2v),
        modelMapping = modelMapping,
        tilingMult = self$funParams()$tilingMult
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
