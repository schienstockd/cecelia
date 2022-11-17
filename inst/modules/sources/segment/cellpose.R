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
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start Cellpose segmentation")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$writeLog("Run prediction")
      
      # convert channels names to numbers
      models <- lapply(
        self$funParams()$models, function(x) {
          x$cellChannels <- sapply(
            x$cellChannels, function(y) {
              unname(which(cciaObj$imChannelNames() == y)) - 1
            }, USE.NAMES = FALSE
          )
          
          x
        })
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          basename(cciaObj$imFilepath())
        ),
        models = models,
        useOmni = FALSE,
        useGPU = if ("useGPU" %in% names(self$envParams()$conf))
          self$envParams()$conf$useGPU
        else
          FALSE,
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
      self$pyScript("cellpose_wrapper", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
