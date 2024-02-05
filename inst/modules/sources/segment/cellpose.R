# Cellpose
Cellpose <- R6::R6Class(
  "Cellpose",
  inherit = Segment,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "cellpose",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # check whether to use suffixes or not
      labelSuffixes <- list()
      
      if (all(c("cyto", "nuc") %in% sapply(self$funParams()$models, function(x) x$matchAs))) {
        labelSuffixes <- c("cyto", "nuc")
      }
      
      # reset image information
      # self$resetImageInfo()
      labelSuffixes <- self$resetImageInfo(labelSuffixes = labelSuffixes)
      
      self$initLog()
      self$writeLog("Start Cellpose segmentation")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$writeLog("Run prediction")
      
      # use gpu?
      useGPU <- FALSE
      
      if ("useGPU" %in% names(self$envParams()$conf))
        useGPU <- self$envParams()$conf$useGPU
      
      self$writeLog(paste(">> Use GPU", useGPU))
      
      # convert channels names to numbers
      models <- lapply(
        self$funParams()$models, function(x) {
          x$cellChannels <- sapply(
            x$cellChannels, function(y) {
              unname(which(cciaObj$imChannelNames() == y)) - 1
            }, USE.NAMES = FALSE
          )
          if (length(x$nucChannels) > 0)
            x$nucChannels <- sapply(
              x$nucChannels, function(y) {
                unname(which(cciaObj$imChannelNames() == y)) - 1
              }, USE.NAMES = FALSE
            )
          
          x
        })
      
      # sort ranks by and filter
      models <- models[self$funParams()$modelsRanks]
      
      # get visibility
      modelVisibilities <- self$funParamVisibilities("models")
      
      if (!is.null(modelVisibilities))
        models <- models[names(modelVisibilities)[modelVisibilities == TRUE]]
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          basename(cciaObj$imFilepath())
        ),
        models = models,
        labelSuffixes = labelSuffixes,
        useOmni = FALSE,
        useGPU = useGPU,
        matchThreshold = self$funParams()$matchThreshold,
        removeUnmatched = self$funParams()$removeUnmatched,
        minCellSize = self$funParams()$minCellSize,
        haloSize = self$funParams()$haloSize,
        haloWholeCell = self$funParams()$haloWholeCell,
        labelExpansion = self$funParams()$labelExpansion,
        labelErosion = self$funParams()$labelErosion,
        blockSize = self$funParams()$blockSize,
        overlap = self$funParams()$overlap,
        blockSizeZ = self$funParams()$blockSizeZ,
        overlapZ = self$funParams()$overlapZ,
        context = self$funParams()$context,
        labelOverlap = self$funParams()$labelOverlap,
        clearDepth = self$funParams()$clearDepth,
        clearTouchingBorder = self$funParams()$clearTouchingBorder,
        updateMeasures = self$funParams()$updateMeasures,
        segment = self$funParams()$segment,
        measure = self$funParams()$measure,
        useDask = self$funParam("useDask", FALSE),
        saveMeshes = self$funParams()$saveMeshes,
        saveMeasures = self$funParams()$saveMeasures,
        extendedMeasures = self$funParams()$extendedMeasures,
        calcMedianIntensities = self$funParams()$calcMedianIntensities,
        integrateTime = self$funParams()$integrateTime,
        integrateTimeMode = if ("integrateTimeMode" %in% names(self$funParams())) self$funParams()$integrateTimeMode else "max",
        normaliseToWhole = self$funParams()$normaliseToWhole
      )
      
      # add optional parameters
      params <- self$addFunParamsToList(
        params, cciaConf()$tasks$segment$optionalParams
      )
      
      # call python
      self$pyScript("cellpose_wrapper", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo(labelSuffixes = labelSuffixes)
    }
  )
)
