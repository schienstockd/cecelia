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
        
        # update channel information
        if (length(addChannels) > 0) {
          # is the number of channels correct?
          if (cciaObj$omeXMLPixels(reset = TRUE)$SizeC >= (length(cciaObj$imChannelNames(valueName = "default")) + length(addChannels))) {
            newChannelNames <- c(cciaObj$imChannelNames(valueName = "default"), addChannels)
            names(newChannelNames) <- c(names(cciaObj$imChannelNames(valueName = "default")),
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