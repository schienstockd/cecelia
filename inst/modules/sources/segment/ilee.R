# ILEE
Ilee <- R6::R6Class(
  "Ilee",
  inherit = Segment,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "ilee",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start ILEE segmentation")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$writeLog("Run prediction")
      
      # use gpu?
      useGPU <- FALSE
      
      if ("useGPU" %in% names(self$envParams()$conf))
        useGPU <- self$envParams()$conf$useGPU
      
      self$writeLog(paste(">> Use GPU", useGPU))
      
      # convert channels names to numbers
      filamentChannels <- sapply(
        self$funParams()$filamentChannels,
        function (x) {
          unname(which(cciaObj$imChannelNames() == x)) - 1
        },
        USE.NAMES = FALSE
      )
      
      integrateTimeMode = if ("integrateTimeMode" %in% names(self$funParams())) self$funParams()$integrateTimeMode else "avg"
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          basename(cciaObj$imFilepath())
        ),
        useGPU = useGPU,
        blockSize = self$funParams()$blockSize,
        overlap = self$funParams()$overlap,
        blockSizeZ = self$funParams()$blockSizeZ,
        overlapZ = self$funParams()$overlapZ,
        context = self$funParams()$context,
        labelOverlap = self$funParams()$labelOverlap,
        clearDepth = self$funParams()$clearDepth,
        clearTouchingBorder = self$funParams()$clearTouchingBorder,
        useDask = self$funParam("useDask", FALSE),
        integrateTime = self$funParams()$integrateTime,
        integrateTimeMode = integrateTimeMode,
        filamentChannels = filamentChannels,
        normalise = self$funParams()$normalise,
        k1 = self$funParams()$k1,
        k2 = self$funParams()$k2
      )
      
      # add optional parameters
      params <- self$addFunParamsToList(
        params, cciaConf()$tasks$segment$optionalParams
      )
      
      # call python
      self$pyScript("ilee_wrapper", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo(labelChannels = filamentChannels, integrateTimeMode = integrateTimeMode)
    }
  )
)
