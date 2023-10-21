# Base class for segmentation methods
Segment <- R6::R6Class(
  "Segment",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "segment"
    },
    
    # reset image information before segmentation
    resetImageInfo = function(valueName = NULL, labelSuffixes = list()) {
      if (is.null(valueName)) {
        valueName <- self$funParams()$valueName
      }
      
      ### reset information for analysis depending on segmentation ###
      # TODO - is there a better option to do this.. ?
      self$runTasks(c(
        "gatePopulations.resetImageInfo",
        "clustPopulations.resetImageInfo"
      ))
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # set spaceholder name
      spaceholder <- "SPACEHOLDER"
      # if (self$funParams()$callingFun %in% c("mesmer", "cellpose")) {
      if (length(labelSuffixes) > 0) {
        # attr(spaceholder, "suffixes") <- c("cyto", "nuc")
        attr(spaceholder, "suffixes") <- labelSuffixes
      }
      
      # add halo
      if ("haloSize" %in% names(self$funParams()) && self$funParams()$haloSize > 0) {
        if (is.null(attr(spaceholder, "suffixes"))) {
          attr(spaceholder, "suffixes") <- c("halo")
        } else {
          attr(spaceholder, "suffixes") <- c(attr(spaceholder, "suffixes"), "halo")
        }
      }
      
      # reset values
      cciaObj$setImLabelsFilepath(spaceholder,
                                  valueName = valueName,
                                  setDefault = FALSE)
      
      if (length(cciaObj$imLabelPropsFilepath(valueName = valueName)) < 1) {
        cciaObj$setImLabelPropsFilepath("SPACEHOLDER",
                                        valueName = valueName,
                                        setDefault = FALSE)
      }
      
      # save object
      cciaObj$saveState()
      
      attr(spaceholder, "suffixes")
    },
    
    # update labels filepath
    updateLabelsFilepath = function(valueName = NULL, labelSuffixes = c(),
                                    labelChannels = c(), integrateTimeMode = NULL) {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get channel names
      channelNames <- cciaObj$imChannelNames()
      
      # add other measurements?
      if ("calcMedianIntensities" %in% names(self$funParams()) && self$funParams()$calcMedianIntensities == TRUE) {
        attr(channelNames, "measure") <- "median"
      } else {
        attr(channelNames, "measure") <- "mean"
      }
      
      # reset types
      attr(channelNames, "types") <- NULL
      
      labelsPath <- paste0(valueName, cciaConf()$files$ext$labels)
      
      # add channels
      if (length(labelChannels) > 0) {
        attr(labelsPath, "channels") <- labelChannels
      }
      
      # add time mode integration
      if (!is.null(integrateTimeMode)) {
        attr(labelsPath, "integrateTimeMode") <- integrateTimeMode
      }
      
      # add labels and properties
      # TODO why does this not work sometimes?
      if (length(labelSuffixes) > 0) {
        # set combined labels
        attr(labelsPath, "suffixes") <- unlist(labelSuffixes)
        
        # set types for channel names
        channelTypes <- unlist(labelSuffixes)
        
        attr(channelNames, "types") <- channelTypes
      }
      
      cciaObj$setImLabelsFilepath(labelsPath, valueName = valueName)
      
      # set channel names
      cciaObj$setImChannelNames(channelNames, valueName = valueName)
      
      # load data from disk when loading the object
      # instead of pushing into object which makes it big
      # and slow to load
      cciaObj$setImLabelPropsFilepath(
        paste0(valueName, cciaConf()$files$ext$labelProps),
        valueName = valueName
      )
      
      # save object
      cciaObj$saveState()
    },
    
    # update image information after segmentation
    updateImageInfo = function(valueName = NULL, valueNames = NULL, ...) {
      # get value names if set, otherwise single value
      if (!is.null(valueNames) && length(valueNames) > 0) {
        # go through value names
        for (x in valueNames) {
          self$updateLabelsFilepath(valueName = x, ...)
        }
      } else {
        if (is.null(valueName)) {
          valueName <- self$funParams()$valueName
        }
        
        self$updateLabelsFilepath(valueName = valueName, ...)
      }
    }
  )
)
