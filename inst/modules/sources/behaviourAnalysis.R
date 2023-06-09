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
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # create path
      clustPath <- sprintf("%s.sc%s", self$funParams()$valueName, cciaConf()$files$ext$labelProps)
      
      # update anndata path
      if ("cciaObjects" %in% names(cciaObj)) {
        # remember that this was part of a bigger clustering
        attr(clustPath, "savedIn") <- cciaObj$getUID()
        
        for (x in cciaObj$cciaObjects(uIDs = self$funParams()$uIDs)) {
          x$setImAnndataFilepath(
            clustPath,
            valueName = self$funParams()$valueName
          )
          
          x$saveState()
        }
      } else {
        cciaObj$setImAnndataFilepath(
          clustPath,
          valueName = self$funParams()$valueName
        )
        
        # save object
        cciaObj$saveState()
      }
    }
  )
)