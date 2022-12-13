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
    resetImageInfo = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # reset channel information
      cciaObj$setImFilepath(NULL, valueName = "corrected")
      cciaObj$setImChannelNames(NULL, valueName = "corrected")
      
      # save object
      cciaObj$saveState()
    },
    
    # update image information after segmentation
    updateImageInfo = function(addChannels = NULL) {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # set filename
      cciaObj$setImFilepath("ccidCorrected.zarr", valueName = "corrected")
      
      # update channel information
      if (length(addChannels) > 0) {
        
        
        # is the number of channels correct?
        if (cciaObj$omeXMLPixels(reset = TRUE)$SizeC >= (length(cciaObj$imChannelNames()) + length(addChannels))) {
          newChannelNames <- c(cciaObj$imChannelNames(), addChannels)
          names(newChannelNames) <- c(names(cciaObj$imChannelNames()),
                                      sprintf("Chn%d", length(newChannelNames)))
          
          cciaObj$setImChannelNames(newChannelNames,
                                    valueName = "corrected")
        }
      }
      
      # save object
      cciaObj$saveState()
    }
  )
)