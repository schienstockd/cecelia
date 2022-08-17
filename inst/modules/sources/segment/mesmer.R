# mesmer
# https://github.com/vanvalenlab/deepcell-tf/blob/master/notebooks/applications/Mesmer-Application.ipynb
Mesmer <- R6::R6Class(
  "Mesmer",
  inherit = Segment,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "mesmer",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start Mesmer segmentation")
      
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
      cytoChannels <- sapply(
        self$funParams()$cytoChannels,
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
        nucleiChannels = nucleiChannels,
        cytoChannels = cytoChannels,
        normalisePercentile = self$funParams()$normalisePercentile,
        minCellSize = self$funParams()$minCellSize,
        labelErosion = self$funParams()$labelErosion,
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
        extendedMeasures = self$funParams()$extendedMeasures
      )
      
      # add optional parameters
      params <- self$addFunParamsToList(
        params, cciaConf()$tasks$segment$optionalParams
      )
      
      # call python
      self$pyScript("mesmer_wrapper", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
