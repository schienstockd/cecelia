# stardist
Stardist <- R6::R6Class(
  "Stardist",
  inherit = Segment,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "stardist",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start StarDist segmentation")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$writeLog("Run prediction")
      
      # convert channels names to numbers
      nucleiChannels <- sapply(
        self$funParams()$nucleiChannels,
        function (x) {
          unname(which(cciaObj$imChannelNames() == x)) - 1
        },
        USE.NAMES = FALSE
      )
      
      self$writeLog(self$funParams()$labelExpansion)
      self$writeLog(self$funParams()$nucleiChannels)
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          basename(cciaObj$imFilepath())
        ),
        minCellSize = self$funParams()$minCellSize,
        labelExpansion = self$funParams()$labelExpansion,
        labelErosion = self$funParams()$labelErosion,
        nucleiChannels = nucleiChannels,
        haloSize = self$funParams()$haloSize,
        haloWholeCell = self$funParams()$haloWholeCell,
        blockSize = self$funParams()$blockSize,
        overlap = self$funParams()$overlap,
        blockSizeZ = self$funParams()$blockSizeZ,
        overlapZ = self$funParams()$overlapZ,
        context = self$funParams()$context,
        clearDepth = self$funParams()$clearDepth,
        clearTouchingBorder = self$funParams()$clearTouchingBorder,
        updateMeasures = self$funParams()$updateMeasures,
        segment = self$funParams()$segment,
        saveMeshes = self$funParams()$saveMeshes,
        saveMeasures = self$funParams()$saveMeasures,
        extendedMeasures = self$funParams()$extendedMeasures
      )
      
      # add optional parameters
      params <- self$addFunParamsToList(
        params, cciaConf()$tasks$segment$optionalParams
      )
      
      # call python
      self$pyScript("sd_wrapper", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
