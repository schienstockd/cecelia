# Base class for signal analysis
SignalAnalysis <- R6::R6Class(
  "SignalAnalysis",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "signalAnalysis"
    },
    
    # reset image information
    resetImageInfo = function() {
      # # get object
      # cciaObj <- self$cciaTaskObject()
      # 
      # # get calling function
      # if (self$funParams()$callingFun == " ... ") {
      # }
      # 
      # # save object
      # cciaObj$saveState()
    },
    
    # update image information
    updateImageInfo = function() {
      # # get object
      # cciaObj <- self$cciaTaskObject()
      # 
      # # get calling function
      # if (self$funParams()$callingFun == " ... ") {
      # } 
      #     
      # # save object
      # cciaObj$saveState()
    }
  )
)