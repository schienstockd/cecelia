# Base class for training models for image segmentation
TrainModels <- R6::R6Class(
  "TrainModels",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "trainModels"
    },
    
    # reset image information
    resetImageInfo = function() {
    },
    
    # update image information
    updateImageInfo = function() {
    }
  )
)