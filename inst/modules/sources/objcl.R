# Base class for object classification
Objcl <- R6::R6Class(
  "Objcl",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "objcl"
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