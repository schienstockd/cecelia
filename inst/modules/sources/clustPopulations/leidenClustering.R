LeidenClustering <- R6::R6Class(
  "LeidenClustering",
  inherit = ClustPopulations,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "leidenClustering",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start Leiden clustering")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$writeLog("Run clustering")
      
      # get channels from first uID
      imChannels <- cciaObj$cciaObjects(uIDs = self$funParams()$uIDs)[[1]]$imChannelNames()
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        popType = self$funParams()$popType,
        popsToCluster = self$funParams()$popsToCluster,
        keepPops = self$funParams()$keepPops,
        resolution = self$funParams()$resolution,
        mergeUmap = self$funParams()$mergeUmap,
        clusterChannels = self$funParams()$clusterChannels,
        objectMeasures = self$funParams()$objectMeasures,
        normaliseAxis = self$funParams()$normaliseAxis,
        normaliseToMedian = self$funParams()$normaliseToMedian,
        normalisePercentile = self$funParams()$normalisePercentile,
        normalisePercentileBottom = self$funParams()$normalisePercentileBottom,
        normaliseIndividually = self$funParams()$normaliseIndividually,
        intensityMeasure = attr(imChannels, "measure"),
        transformation = self$funParams()$transformation,
        # TODO why is this not converted to numeric automatically?
        logBase = as.numeric(self$funParams()$logBase),
        uIDs = if ("uIDs" %in% names(self$funParams()))
          self$funParams()$uIDs
        else
          list()
      )
      
      # include filtered?
      includeFiltered <- TRUE
      if ("includeFiltered" %in% names(self$funParams()) &&
          self$funParams()$includeFiltered == FALSE)
        includeFiltered <- FALSE
      
      # save pops before call
      if (self$funParams()$savePops == TRUE && length(self$funParams()$popsToCluster) > 0) {
        self$writeLog(">> Save populations")
        
        if (cciaObj$getCciaClass() == "CciaImageSet") {
          for (x in cciaObj$cciaObjects(uIDs = self$funParams()$uIDs)) {
            self$writeLog(x$getUID())
            
            x$savePopMap(self$funParams()$popType, includeFiltered = includeFiltered)
            x$savePops(self$funParams()$popType,
                       pops = self$funParams()$popsToCluster,
                       includeFiltered = includeFiltered)
          }
        } else {
          self$writeLog(cciaObj$getUID())
          
          cciaObj$savePopMap(self$funParams()$popType, includeFiltered = includeFiltered)
          cciaObj$savePops(self$funParams()$popType,
                           pops = self$funParams()$popsToCluster,
                           includeFiltered = includeFiltered)
        }
      }
      
      # call python
      self$pyScript("leiden_clust", params)
      
      # TODO set populations by default to all resulting pops if reLeiden is used
      if (self$funParams()$keepPops == TRUE &&
          length(self$funParams()$popsToCluster) > 0 &&
          self$funParams()$popType == "clust") {
        # get pop map
        
        # get maximum cluster
        
        # assign new clusters to first pops to clust by default
      }
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
