# Base class for behaviour
BehaviourAnalysis <- R6::R6Class(
  "BehaviourAnalysis",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "behaviourAnalysis"
    },
    
    # reset image information
    resetImageInfo = function() {
      # # get object
      # cciaObj <- self$cciaTaskObject()
      # 
      # # save object
      # cciaObj$saveState()
    },
    
    # update image information
    updateImageInfo = function() {
      # # get object
      # cciaObj <- self$cciaTaskObject()
      # 
      # # save object
      # cciaObj$saveState()
    }
  )
)