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
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo(suffixes = c("cyto", "nuc"))
      
      self$initLog()
      self$writeLog("Start Mesmer segmentation")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$writeLog("Run prediction")
      
      # convert channels names to numbers
      models <- lapply(
        self$funParams()$models, function(x) {
          x$nucleiChannels <- sapply(
            x$nucleiChannels, function(y) {
              unname(which(cciaObj$imChannelNames() == y)) - 1
            }, USE.NAMES = FALSE
          )
          x$cytoChannels <- sapply(
            x$cytoChannels, function(y) {
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
        saveMeasures = self$funParams()$saveMeasures,
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
      self$updateImageInfo(labelSuffixes = c("nuc", "cyto"))
    }
  )
)
