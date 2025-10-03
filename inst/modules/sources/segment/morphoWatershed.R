# Morphological Watershed
MorphoWatershed <- R6::R6Class(
  "MorphoWatershed",
  inherit = Segment,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "morphoWatershed",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo(self$funParams()$valueName)
      
      self$initLog()
      self$writeLog("Start Morphological Watersched segmentation")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$writeLog("Run prediction")
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          basename(cciaObj$imFilepath())
        ),
        seedChannel = as.numeric(self$funParams()$seedChannel),
        seedThresholdRel = self$funParams()$seedThresholdRel,
        seedThresholdAbs = self$funParams()$seedThresholdAbs,
        cellRadius = self$funParams()$cellRadius,
        zSpread = self$funParams()$zSpread,
        cellMinDistance = self$funParams()$cellMinDistance,
        cellSizeMax = self$funParams()$cellSizeMax,
        gaussianFilter = self$funParams()$gaussianFilter,
        maximumFilter = self$funParams()$maximumFilter,
        medianFilter = self$funParams()$medianFilter,
        minimumFilter = self$funParams()$minimumFilter,
        timepoints = seq(
          self$funParams()$timepoints[[1]],
          self$funParams()$timepoints[[2]]
        )
      )
      
      # add optional parameters
      params <- self$addFunParamsToList(
        params, cciaConf()$tasks$segment$optionalParams
      )
      
      # call python
      self$pyScript("morpho_watershed", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo(self$funParams()$valueName)
    }
  )
)
