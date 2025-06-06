# Base class for cleaning up images
CleanupImages <- R6::R6Class(
  "CleanupImages",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "cleanupImages"
    },
    
    # reset image information before segmentation
    resetImageInfo = function(valueName = "corrected", uID = NULL) {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get object from set
      if (!is.null(uID))
        cciaObj <- cciaObj$cciaObjects()[[uID]]
      
      # TODO is this required .. ?
      # if (cciaObj$getCciaClass() == "CciaImage") {
      #   # reset channel information
      #   cciaObj$setImFilepath(NULL, valueName = valueName)
      #   cciaObj$setImChannelNames(NULL, valueName = valueName)
      #   
      #   # save object
      #   cciaObj$saveState()
      # }
    },
    
    # update image information after segmentation
    updateImageInfo = function(addChannels = NULL, filename = "ccidCorrected",
                               valueName = "corrected", uID = NULL) {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get object from set
      if (!is.null(uID))
        cciaObj <- cciaObj$cciaObjects()[[uID]]
      
      if (cciaObj$getCciaClass() == "CciaImage") {
        # set filename
        cciaObj$setImFilepath(paste0(filename, ".zarr"), valueName = valueName)
        
        # get previous value name
        prevValueName <- NULL
        prevChannelNames <- NULL
        if ("valueName" %in% names(self$funParams())) {
          prevValueName <- self$funParams()$valueName
          
          if (prevValueName %in% cciaObj$valueNames("imChannelNames")) {
            prevChannelNames <- cciaObj$imChannelNames(valueName = prevValueName)
          } else {
            prevChannelNames <- cciaObj$imChannelNames()
          }
        }
        
        # update channel information
        if (length(addChannels) > 0) {
          # is the number of channels correct?
          # TODO this will always fall back to default
          if (cciaObj$omeXMLPixels(reset = TRUE)$SizeC >= (length(prevChannelNames) + length(addChannels))) {
            newChannelNames <- c(prevChannelNames, addChannels)
            names(newChannelNames) <- c(names(prevChannelNames),
                                        sprintf("Chn%d", length(newChannelNames)))
            
            cciaObj$setImChannelNames(newChannelNames, valueName = valueName)
          }
        }
      
        # save object
        cciaObj$saveState()
      }
    }
  )
)