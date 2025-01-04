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
      
      self$writeLog(paste("Run clustering with", self$funParams()$correctBatch ,"Correction"))
      
      # get channels from first uID for measure attribute
      imChannels <- cciaObj$cciaObjects(uIDs = self$funParams()$uIDs)[[1]]$imChannelNames()
      
      # channels an be in different order for each image
      clusterChannels <- lapply(
        cciaObj$cciaObjects(uIDs = self$funParams()$uIDs), function(x) {
          lapply(
            self$funParams()$clusterChannels, function(y) {
              y$channels <- sapply(
                y$channels, function(z) {
                  unname(which(x$imChannelNames() == z)) - 1
                }, USE.NAMES = FALSE
              )
              
              y
            })
        })
      
      # get reference channel
      refChannel <- lapply(
        cciaObj$cciaObjects(uIDs = self$funParams()$uIDs), function(x) {
          unname(which(x$imChannelNames() == self$funParams()$refChannel)) - 1
        })
      
      # use gpu?
      useGPU <- FALSE
      
      if ("useGPU" %in% names(self$envParams()$conf))
        useGPU <- self$envParams()$conf$useGPU
      
      self$writeLog(paste(">> Use GPU", useGPU))
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        popType = self$funParams()$popType,
        popsToCluster = self$funParams()$popsToCluster,
        refPops = self$funParams()$refPops,
        keepPops = self$funParams()$keepPops,
        resolution = self$funParams()$resolution,
        mergeUmap = self$funParams()$mergeUmap,
        clusterChannels = clusterChannels,
        refChannel = refChannel,
        objectMeasures = self$funParams()$objectMeasures,
        normaliseAxis = self$funParams()$normaliseAxis,
        normaliseToMedian = self$funParams()$normaliseToMedian,
        normalisePercentile = self$funParams()$normalisePercentile,
        normalisePercentileBottom = self$funParams()$normalisePercentileBottom,
        normaliseIndividually = self$funParams()$normaliseIndividually,
        correctBatch = self$funParams()$correctBatch,
        intensityMeasure = attr(imChannels, "measure"),
        transformation = self$funParams()$transformation,
        # TODO why is this not converted to numeric automatically?
        logBase = as.numeric(self$funParams()$logBase),
        pagaThreshold = self$funParams()$pagaThreshold,
        usePaga = self$funParams()$usePaga,
        useGPU = useGPU,
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
