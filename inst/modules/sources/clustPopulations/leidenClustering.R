source(file.path(
  cfg$tasks$sources, "clustPopulations.R")
)

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
        sep = CCID_CLASS_SEP
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
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        popType = self$funParams()$popType,
        popsToCluster = self$funParams()$popsToCluster,
        keepPops = self$funParams()$keepPops,
        resolution = self$funParams()$resolution,
        clusterChannels = self$funParams()$clusterChannels,
        normaliseAxis = self$funParams()$normaliseAxis,
        normaliseToMedian = self$funParams()$normaliseToMedian,
        normalisePercentile = self$funParams()$normalisePercentile,
        normalisePercentileBottom = self$funParams()$normalisePercentileBottom,
        normaliseIndividually = self$funParams()$normaliseIndividually,
        transformation = self$funParams()$transformation,
        # TODO why is this not converted to numeric automatically?
        logBase = as.numeric(self$funParams()$logBase),
        uIDs = if ("uIDs" %in% names(self$funParams()))
          self$funParams()$uIDs
        else
          list()
      )
      
      # save pops before call
      if (self$funParams()$savePops == TRUE && length(self$funParams()$popsToCluster) > 0) {
        if (cciaObj$getCciaClass() == "CciaImageSet") {
          for (x in cciaObj$cciaObjects(uIDs = self$funParams()$uIDs)) {
            x$savePopMap(self$funParams()$popType, includeFiltered = TRUE)
            x$savePops(self$funParams()$popType,
                       pops = self$funParams()$popsToCluster,
                       includeFiltered = TRUE)
          }
        } else {
          cciaObj$savePopMap(self$funParams()$popType, includeFiltered = TRUE)
          cciaObj$savePops(self$funParams()$popType,
                           pops = self$funParams()$popsToCluster,
                           includeFiltered = TRUE)
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