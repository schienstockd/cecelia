# Donblo
Donblo <- R6::R6Class(
  "Donblo",
  inherit = Segment,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "donblo",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      # assuming that there will be only one segmentation per image
      # otherwise this value would need to be entered by the user
      self$resetImageInfo(valueName = "default")
      
      self$initLog()
      self$writeLog("Start Donblo segmentation")
      self$writeLog(self$funParams()$blobChannels)
      self$writeLog(self$funParams()$donutChannels)

      # get object
      cciaObj <- self$cciaTaskObject()
  
      self$writeLog("Run Donblo")
      
      # convert channels names to numbers
      blobChannels <- sapply(
        self$funParams()$blobChannels,
        function (x) {
          unname(which(cciaObj$imChannelNames() == x)) - 1
        },
        USE.NAMES = FALSE
      )
      donutChannels <- sapply(
        self$funParams()$donutChannels,
        function (x) {
          unname(which(cciaObj$imChannelNames() == x)) - 1
        },
        USE.NAMES = FALSE
      )
  
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          basename(cciaObj$imFilepath())
        ),
        fijiPath = cciaConf()$imagej$path,
        scriptsPath = cciaConf()$imagej$scripts,
        cellRadius = self$funParams()$cellRadius,
        gaussianFilter = self$funParams()$gaussianFilter,
        medianFilter = self$funParams()$medianFilter,
        maximumFilter = self$funParams()$maximumFilter,
        minimumFilter = self$funParams()$minimumFilter,
        detectionThreshAdj = self$funParams()$detectionThreshAdj,
        rollingRadius = self$funParams()$rollingRadius,
        minCellSize = self$funParams()$minCellSize,
        labelExpansion = self$funParams()$labelExpansion,
        labelErosion = self$funParams()$labelErosion,
        blobChannels = blobChannels,
        donutChannels = donutChannels,
        blockSize = self$funParams()$blockSize,
        overlap = self$funParams()$overlap,
        haloSize = self$funParams()$haloSize,
        haloWholeCell = self$funParams()$haloWholeCell,
        blockSizeZ = self$funParams()$blockSizeZ,
        overlapZ = self$funParams()$overlapZ,
        context = self$funParams()$context,
        clearDepth = self$funParams()$clearDepth,
        clearTouchingBorder = self$funParams()$clearTouchingBorder,
        updateMeasures = self$funParams()$updateMeasures,
        segment = self$funParams()$segment,
        saveMeshes = self$funParams()$saveMeshes,
        extendedMeasures = self$funParams()$extendedMeasures
        # postVarianceFilter = self$funParams()$postVarianceFilter
      )
      
      # add optional parameters
      params <- self$addFunParamsToList(
        params, cciaConf()$tasks$segment$optionalParams
      )
      
      # call python
      self$pyScript("donblo", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
