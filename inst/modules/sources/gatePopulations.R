# Base class for gating methods
GatePopulations <- R6::R6Class(
  "GatePopulations",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "gatePopulations"
    },
    
    # reset image information before gating
    resetImageInfo = function(gatingSet = NULL) {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      if (cciaObj$getCciaClass() == "CciaImage") {
        # unlink datafiles and reset values
        if (!is.null(gatingSet)) {
          if (!purrr::is_empty(cciaObj$imGatingSetFilepath(valueName = gatingSet))) {
            unlink(cciaObj$imGatingSetFilepath(valueName = gatingSet), recursive = TRUE)
          }
          
          cciaObj$setImGatingSetFilepath(NULL, valueName = gatingSet, setDefault = FALSE)
        }
        
        # reset populations
        cciaObj$setImPopMap("flow", NULL)
        
        # reset plots
        cciaObj$setImPopGatePlot("flow", plotParams = NULL)
      }
      
      # save object
      cciaObj$saveState()
    },
    
    # update image information after gating
    updateImageInfo = function(gatingSet = NULL) {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # update paths
      if (!is.null(gatingSet)) {
        if (cciaObj$getCciaClass() == "CciaImageSet") {
          # set the gating set as the gating set saved in the image set
          # TODO is that a good idea .. ?
          for (x in cciaObj$cciaObjects(uIDs = self$funParams()$uIDs)) {
            # set location UID
            gsPath <- paste0(gatingSet, cciaConf()$files$ext$gatingSet)
            attr(gsPath, "locUID") <- cciaObj$getUID()
            
            x$setImGatingSetFilepath(
              gsPath,
              valueName = gatingSet
            )
            
            # save object
            x$saveState()
          }
        } else {
          cciaObj$setImGatingSetFilepath(
            # taskDirFiles("data", paste0(gatingSet, cciaConf()$files$ext$gatingSet)),
            paste0(gatingSet, cciaConf()$files$ext$gatingSet),
            valueName = gatingSet
          )
        }
      }
      
      # save object
      cciaObj$saveState()
    }
  )
)
