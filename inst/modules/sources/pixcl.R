# Base class for pixel classification
Pixcl <- R6::R6Class(
  "Pixcl",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "pixcl"
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