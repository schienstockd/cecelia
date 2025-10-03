# Base class for mapping methods
ClustRegions <- R6::R6Class(
  "ClustRegions",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "clustRegions"
    },
    
    # reset image information before mapping
    resetImageInfo = function() {
    },
    
    # update image information after mapping
    updateImageInfo = function() {
    }
  )
)
